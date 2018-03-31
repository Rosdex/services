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

module CategoryPredictionService =
    open CategoryPrediction

    type Error =
        | JobNotFound
        | ResultIsNotReady
        | NetError of StatusCode : int
        | FormatError

    type Client = {
        TryInit :
            Offer list
            -> Result<Job, Error> Async
        TryGetJob :
            JobId
            -> Result<Job, Error> Async
        TryGetResult :
            JobId
            -> Result<(OfferId * CategoryId option) list, Error> Async
    }

    module Client =

        module AgentBased =
            open AgentHelpers

            type twoWayWithError<'input, 'reply> =
                twoWay<'input, Result<'reply, Error>>

            type AgentMessage =
                | TryInit of
                    twoWayWithError<Offer list, Job>
                | TryGetJob of
                    twoWayWithError<JobId, Job>
                | TryGetResult of
                    twoWayWithError<JobId, (OfferId * CategoryId option) list>

            let ofAgent (agent : MailboxProcessor<_>) =
                let tuple a b = a, b
                {
                    TryInit = fun offers ->
                        tuple offers
                        >> TryInit
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

        module JsonStub =
            open AgentBased
            open Hopac
            open HttpFs.Client
            open Microsoft.FSharpLu.Json

            type JobSchema = {
                id : System.Guid
                status : State
                created_at : System.DateTime
                // ..
            }

            module JobSchema =
                let toJob schema =
                    {
                        Info =
                            {
                                Id = schema.id
                                CreatedAt = schema.created_at
                            }
                        State = schema.status
                    }

                let tryExtract (response : Response) =
                    response.body
                    |> Compact.tryDeserializeStream<JobSchema> |> function
                        | Choice1Of2 p -> Ok p
                        | _ -> Error FormatError

            let handleNetError (response : Response) =
                match response.statusCode with
                | 200 -> Ok response
                | p -> p |> NetError |> Error

            let tryExtractJob =
                JobSchema.tryExtract
                >> Result.map JobSchema.toJob

            let tryInitJob endpoint offers = async {
                use! response =
                    Request.createUrl Post endpoint
                    // TODO: SCV
                    |> Request.bodyString (Compact.serialize offers)
                    |> getResponse
                    |> Job.toAsync
                return
                    response
                    |> handleNetError
                    |> Result.bind tryExtractJob
                }

            let tryGetJob endpoint (id : JobId) = async {
                use! response =
                    id
                    |> sprintf "%s/%O" endpoint
                    |> Request.createUrl Get
                    |> getResponse
                    |> Job.toAsync
                return
                    response
                    |> handleNetError
                    |> Result.bind tryExtractJob
                }

            let tryGetResult endpoint (id : JobId) = async {
                use! response =
                    id
                    |> sprintf "%s/%O/result" endpoint
                    |> Request.createUrl Get
                    |> getResponse
                    |> Job.toAsync
                return
                    response
                    |> handleNetError
                    |> Result.bind (fun p ->
                        p.body
                        |> Compact.tryDeserializeStream
                        |> Result.ofChoice
                        |> Result.mapError (fun _ -> FormatError))
                }

            let agent endpoint = MailboxProcessor.Start (fun inbox ->
                let rec loop () = async {
                    let! message = inbox.Receive()
                    match message with
                    | TryInit (offers, rc) ->
                        let! result = tryInitJob endpoint offers
                        rc.Reply result
                    | TryGetJob (id, rc) ->
                        let! result = tryGetJob endpoint id
                        rc.Reply result
                    | TryGetResult (id, rc) ->
                        let! result = tryGetResult endpoint id
                        rc.Reply result
                    return! loop ()
                    }
                loop ()
                )

            let create =
                agent >> ofAgent

module CommandHandler =
    open Rosdex.Services.ProductManager.Domain.CardsBuilding
    open CategoryPredictionService

    // TODO: Bubble Error
    let ofCategoryPredictionServiceClient client : CommandHandler =
        {
            SendToCategoryPrediction = fun offers -> async {
                let! job = client.TryInit offers
                return
                    match job with
                    | Ok p -> p.Info.Id
                    | Error p ->
                        failwithf "Not implemented yet. %A" p
            }
            TryFetchCategoryPredictionResult = fun id -> async {
                let! result = client.TryGetResult id
                return
                    match result with
                    | Ok predicted ->
                        predicted
                        |> List.choose (fun (offer, cat) ->
                            cat |> Option.map (fun p -> offer, p))
                        |> Map.ofList
                        |> Some
                    | Error _ -> None
            }
        }