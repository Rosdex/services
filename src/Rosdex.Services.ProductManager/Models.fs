namespace Rosdex.Services.ProductManager.Models

[<CLIMutable>]
type Message =
    {
        Text : string
    }

open Rosdex.Services.ProductManager.Domain

type Job = CardsBuilding.Job
type State = CardsBuilding.State

type JobStorageApi = {
    GetAll :
        unit -> Job list Async
    TryGet :
        JobId -> Job option Async
    CreateNew :
        Offer list -> Job Async
    // TODO:
    // ? Заменить на TryApply :
    //      JobId
    //      -> (CardBuilding.State -> CardBuilding.State)
    //      -> CardBuilding.Job option Async
    TryUpdate :
        // TODO: Error
        JobId -> CardsBuilding.State -> CardsBuilding.Job option Async
}

module AgentHelpers =
    type 'a oneWayFrom = 'a AsyncReplyChannel
    type twoWay<'input, 'reply> = 'input * 'reply AsyncReplyChannel

module JobStorageApi =
    module AgentBased =
        open AgentHelpers

        let tuple input ch = input, ch

        type AgentMessage =
            | GetAll of oneWayFrom<Job list>
            | TryGet of twoWay<JobId, Job option>
            | CreateNew of twoWay<Offer list, Job>
            | TryUpdate of twoWay<JobId * State, Job option>

        let ofAgent (agent : MailboxProcessor<_>) =
            {
                GetAll = fun () ->
                    agent.PostAndAsyncReply GetAll
                TryGet = fun id ->
                    agent.PostAndAsyncReply (tuple id >> TryGet)
                CreateNew = fun offers ->
                    agent.PostAndAsyncReply (tuple offers >> CreateNew)
                TryUpdate = fun id state ->
                    agent.PostAndAsyncReply (tuple (id, state) >> TryUpdate)
            }

    // TODO: Вынести
    module InMemory =
        open AgentBased

        let apply dict = function
            | GetAll rc ->
                dict
                    |> Map.toList
                    |> List.map snd
                    |> rc.Reply
                dict
            | TryGet (id, rc) ->
                dict
                    |> Map.tryFind id
                    |> rc.Reply
                dict
            | CreateNew (offers, rc) ->
                let result =
                    {
                        Info =
                            {   Id = JobId.NewGuid();
                                CreatedAt = System.DateTime.Now }
                        State = State.Created { Input = offers }
                    }
                rc.Reply result
                result
                    |> Map.add result.Info.Id
                    <| dict
            | TryUpdate ((id, state), rc) ->
                dict.TryFind id
                    |> Option.map (fun p ->
                        let result = { p with State = state }
                        rc.Reply (Some result)
                        Map.add id result dict)
                    |> Option.defaultValue dict

        let agent initDict =
            MailboxProcessor.Start (fun inbox ->
                let rec loop dict = async {
                    let! message = inbox.Receive()
                    return!
                        apply dict message
                        |> loop
                    }
                loop initDict)

        let create = agent >> ofAgent

type CategoryPredictionServiceClient = {
    Init :
        Offer list
        -> CategoryPrediction.Job Async
    TryGetJob :
        CategoryPrediction.JobId
        -> CategoryPrediction.Job option Async
    TryGetResult :
        CategoryPrediction.JobId
        -> (OfferId * CategoryId option) list option Async
}

module CategoryPredictionServiceClient =
    open CategoryPrediction

    module AgentBased =
        open AgentHelpers

        type AgentMessage =
            | Init of
                twoWay<Offer list, Job>
            | TryGetJob of
                twoWay<JobId, Job option>
            | TryGetResult of
                twoWay<JobId, (OfferId * CategoryId option) list option>

        let ofAgent (agent : MailboxProcessor<_>) =
            let tuple a b = a, b
            {
                Init = fun offers ->
                    tuple offers
                    >> Init
                    |> agent.PostAndAsyncReply
                TryGetJob = fun id ->
                    tuple id
                    >> TryGetJob
                    |> agent.PostAndAsyncReply
                TryGetResult = fun id ->
                    tuple id
                    >> TryGetResult
                    |> agent.PostAndAsyncReply
            }

module CommandHandler =
    open Rosdex.Services.ProductManager.Domain.CardsBuilding

    let ofCategoryPredictionServiceClient client : CommandHandler =
        {
            SendToCategoryPrediction = fun offers -> async {
                let! job = client.Init offers
                return job.Info.Id
            }
            TryFetchCategoryPredictionResult = fun id -> async {
                let! result = client.TryGetResult id
                return result
                    |> Option.map (
                        List.choose (fun (offer, cat) ->
                            cat |> Option.map (fun p -> offer, p))
                        >> Map.ofList)
            }
        }