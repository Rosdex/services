namespace Rosdex.Services.ProductMananger.Domain

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

type Job = {
    Id : JobId
    CreatedAt : System.DateTime
}

module Job =

    let id { Job.Id = id } = id

    // Тащить ли Job?
    type CreatedJob = {
        Job : Job
        Input : Offer list
    }

    type CategoryPredictionJobId = System.Guid

    type InCategoryPredictionJob = {
        Job : Job
        InProcess : Offer list
        PredictionId : CategoryPredictionJobId
        //Processed : (Offer * CategoryId option) list
    }

    type CategoryPredictedJob = {
        Job : Job
        PredictedOffers : (Offer * CategoryId option) list
    }

    type State =
        // TODO? Created -> Parsed -> JobInCat...?
        | Created of CreatedJob
        | JobInCategoryPrediction of InCategoryPredictionJob
        | JobWithPredictedCategory of CategoryPredictedJob
        | Failed of Job * Message : string

    module State =
        let job = function
            | Created p -> p.Job
            | JobInCategoryPrediction p -> p.Job
            | JobWithPredictedCategory p -> p.Job
            | Failed (p, _) -> p

    //// TODO? Events?
    //type Event =
    //    | JobSendedToCategoryPrediction of CategoryPredictionJobId
    //    | PredictedCategory of (OfferId * CategoryId option) list
    //    | Failed of Message : string

    // TODO: Error
    type CommandHandler = {
        SendToCategoryPrediction :
            Offer list
                -> CategoryPredictionJobId Async
        FetchCategoryPredictionResult :
            CategoryPredictionJobId
                -> Map<OfferId, CategoryId> Async
    }

    type Command =
        // TODO? Тащить ли Id как в примере?
        | SendToCategoryPrediction // of SubList : OfferId list
        | FetchCategoryPredictionResult

    // TODO
    // Развернуть Errors
    module CommandHandler =
        let handleSendToCategoryPrediction commandHanlder state =
            match state with
            | Created job ->
                async {
                    let! id =
                        job.Input
                        |> commandHanlder.SendToCategoryPrediction
                    return JobInCategoryPrediction {
                        Job = job.Job
                        InProcess = job.Input
                        PredictionId = id
                        }
                    }
                |> Ok
            | _ ->
                state
                |> State.job
                |> sprintf """Job (%A) state must be "Created"."""
                |> Error

        // TODO
        // ? Лишние OfferId в map
        // ? Частичная обработка
        let handleFetchCategoryPredictionResult commandHandler state =
            match state with
            | JobInCategoryPrediction p ->
                async {
                    let! map =
                        p.PredictionId
                        |> commandHandler.FetchCategoryPredictionResult
                    return JobWithPredictedCategory {
                        Job = p.Job
                        PredictedOffers =
                            p.InProcess
                            |> List.map (fun p ->
                                p, p.Id |> map.TryFind)
                    }}
                |> Ok
            | _ ->
                state
                |> State.job
                |> sprintf """Job (%A) state must be "JobInCategoryPrediction"."""
                |> Error

        let handle commandHandler state command =
            match command with
            | SendToCategoryPrediction ->
                handleSendToCategoryPrediction commandHandler state
            | FetchCategoryPredictionResult ->
                handleFetchCategoryPredictionResult commandHandler state