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
        PredictionJob : CategoryPrediction.Job
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
                -> CategoryPrediction.Job option Async
        TryCheckCategoryPrediction :
            CategoryPrediction.JobId
                -> CategoryPrediction.Job option Async
        TryFetchCategoryPredictionResult :
            CategoryPrediction.JobId
                -> Map<OfferId, CategoryId> option Async
    }

    type Command =
        // TODO? Тащить ли Id как в примере?
        | SendToCategoryPrediction // of SubList : OfferId list
        | CheckCategoryPrediction
        | FetchCategoryPredictionResult

    // TODO
    // Развернуть Errors
    module CommandHandler =
        let handleSendToCategoryPrediction commandHanlder state = async {
            match state with
            | Created job ->
                let! predictionJob =
                    job.Input
                    |> commandHanlder.TrySendToCategoryPrediction
                return
                    Result.ofOption "Cannot send to predict." predictionJob
                    |> Result.map (fun predictionJob ->
                        JobInCategoryPrediction {
                            InProcess = job.Input
                            PredictionJob = predictionJob
                        })
            | _ ->
                return
                    state
                    |> sprintf """State (%A) must be "Created"."""
                    |> Error
            }

        let handleCheckCategoryPredictionResult commandHandler state = async {
            match state with
            | JobInCategoryPrediction job ->
                // TODO: Реакции на ошибки -> Failed.
                let! predictionJob =
                    job.PredictionJob.Info.Id
                    |> commandHandler.TryCheckCategoryPrediction
                match predictionJob with
                | Some p ->
                    return Ok <| JobInCategoryPrediction {
                        job with PredictionJob = p
                    }
                | None -> return Error ""
            | _ ->
                return
                    state
                    |> sprintf """State (%A) must be "JobInCategoryPrediction"."""
                    |> Error
            }

        // TODO
        // ? Лишние OfferId в map
        // ? Частичная обработка
        let handleFetchCategoryPredictionResult commandHandler state = async {
            match state with
            | JobInCategoryPrediction job
                when job.PredictionJob.State = CategoryPrediction.State.Done
                ->
                let! map =
                    job.PredictionJob.Info.Id
                    |> commandHandler.TryFetchCategoryPredictionResult
                match map with
                | Some map ->
                    return Ok <| JobWithPredictedCategory {
                        PredictedOffers =
                            job.InProcess
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

        let handle commandHandler command state=
            (match command with
            | SendToCategoryPrediction ->
                handleSendToCategoryPrediction
            | CheckCategoryPrediction ->
                handleCheckCategoryPredictionResult
            | FetchCategoryPredictionResult ->
                handleFetchCategoryPredictionResult)
                commandHandler state

    type CommandHandler with
        member this.Handle command state =
            CommandHandler.handle this command state 