namespace Rosdex.Services.ProductManager.Domain

type CategoryId = int

type ProductId = int

type Product = {
    Id : ProductId
    // ..
}

type OfferId = int

type Offer = {
    Id : OfferId
    Name : string
    // ..
}

type JobId = System.Guid

type JobInfo = {
    Id : JobId
    CreatedAt : System.DateTime
}

type 'a Job = {
    Info : JobInfo
    State : 'a
}

module Job =

    let info { Job.Info = info } = info

    let state { Job.State = state } = state

// Category prediction service

module CategoryPrediction =
    type JobId = System.Guid

    [<RequireQualifiedAccess>]
    type State =
        | Created
        | Performing
        | Done
        | Error

    type Job =  State Job

// Cards building

module CardsBuilding =

    type CreatedJob = {
        Input : Offer list
    }

    type InCategoryPredictionJob = {
        InProcess : Offer list
        PredictionId : CategoryPrediction.JobId
        //Processed : (Offer * CategoryId option) list
    }

    type CategoryPredictedJob = {
        PredictedOffers : (Offer * CategoryId option) list
    }

    type State =
        // TODO? Created -> Parsed -> JobInCat...?
        | Created of CreatedJob
        | JobInCategoryPrediction of InCategoryPredictionJob
        | JobWithPredictedCategory of CategoryPredictedJob
        | Failed of JobInfo * Message : string

    type Job = State Job

    //// TODO? Events?
    //type Event =
    //    | JobSendedToCategoryPrediction of CategoryPredictionJobId
    //    | PredictedCategory of (OfferId * CategoryId option) list
    //    | Failed of Message : string

    // TODO: Error
    type CommandHandler = {
        TrySendToCategoryPrediction :
            Offer list
                -> CategoryPrediction.JobId option Async
        TryFetchCategoryPredictionResult :
            CategoryPrediction.JobId
                -> Map<OfferId, CategoryId> option Async
    }

    type Command =
        // TODO? Тащить ли Id как в примере?
        | SendToCategoryPrediction // of SubList : OfferId list
        | FetchCategoryPredictionResult

    // TODO
    // Развернуть Errors
    module CommandHandler =
        let handleSendToCategoryPrediction commandHanlder state = async {
            match state with
            | Created job ->
                let! id =
                    job.Input
                    |> commandHanlder.TrySendToCategoryPrediction
                return
                    Result.ofOption "Cannot send to predict." id
                    |> Result.map (fun id ->
                        JobInCategoryPrediction {
                            InProcess = job.Input
                            PredictionId = id
                        })
            | _ ->
                return
                    state
                    |> sprintf """State (%A) must be "Created"."""
                    |> Error
            }

        // TODO
        // ? Лишние OfferId в map
        // ? Частичная обработка
        let handleFetchCategoryPredictionResult commandHandler state = async {
            match state with
            | JobInCategoryPrediction p ->
                let! map =
                    p.PredictionId
                    |> commandHandler.TryFetchCategoryPredictionResult
                match map with
                | Some map ->
                    return Ok <| JobWithPredictedCategory {
                        PredictedOffers =
                            p.InProcess
                            |> List.map (fun p ->
                                p, p.Id |> map.TryFind)
                        }
                | None ->
                    return Error ""
            | _ ->
                return
                    state
                    |> sprintf """State (%A) must be "JobInCategoryPrediction"."""
                    |> Error
            }

        let handle commandHandler state command =
            match command with
            | SendToCategoryPrediction ->
                handleSendToCategoryPrediction commandHandler state
            | FetchCategoryPredictionResult ->
                handleFetchCategoryPredictionResult commandHandler state

    type CommandHandler with
        member this.Handle state command =
            CommandHandler.handle this state command