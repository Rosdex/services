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