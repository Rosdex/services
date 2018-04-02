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
    TryApply :
        JobId -> (State -> State Async) -> Job option Async
    TryUpdate :
        // TODO: Error
        JobId -> State -> Job option Async
}

module AgentHelpers =
    type 'a oneWayFrom = 'a AsyncReplyChannel
    type twoWay<'input, 'reply> = 'input * 'reply AsyncReplyChannel

module JobStorageApi =
    module AgentHelpers =
        open AgentHelpers

        let tuple input ch = input, ch

        type AgentMessage =
            | GetAll of oneWayFrom<Job list>
            | TryGet of twoWay<JobId, Job option>
            | CreateNew of twoWay<Offer list, Job>
            | TryApply of twoWay<JobId * (State -> State Async), Job option>
            | TryUpdate of twoWay<JobId * State, Job option>

        let ofAgent (agent : MailboxProcessor<_>) =
            {
                GetAll = fun () ->
                    agent.PostAndAsyncReply GetAll
                TryGet = fun id ->
                    agent.PostAndAsyncReply (tuple id >> TryGet)
                CreateNew = fun offers ->
                    agent.PostAndAsyncReply (tuple offers >> CreateNew)
                TryApply = fun id applying ->
                    agent.PostAndAsyncReply (tuple (id, applying) >> TryApply)
                TryUpdate = fun id state ->
                    agent.PostAndAsyncReply (tuple (id, state) >> TryUpdate)
            }

    // TODO: Вынести
    module InMemory =
        open AgentHelpers

        let apply dict = function
            | GetAll rc ->
                dict
                    |> Map.toList
                    |> List.map snd
                    |> rc.Reply
                dict |> async.Return
            | TryGet (id, rc) ->
                dict
                    |> Map.tryFind id
                    |> rc.Reply
                dict |> async.Return
            | CreateNew (offers, rc) ->
                let result =
                    {
                        Info =
                            {   Id = JobId.NewGuid();
                                CreatedAt = System.DateTime.Now }
                        State = State.Created { Input = offers }
                    }
                rc.Reply result
                dict.Add (result.Info.Id, result)
                    |> async.Return
            | TryApply ((id, applying), rc) ->
                dict.TryFind id
                    |> Option.map (fun job -> async {
                        let! newState = applying job.State
                        let result = {
                            job with State = newState
                        }
                        rc.Reply (Some result)
                        return Map.add id result dict
                    })
                    |> Option.defaultValue (async.Return dict)
            | TryUpdate ((id, state), rc) ->
                dict.TryFind id
                    |> Option.map (fun p ->
                        let result = { p with State = state }
                        rc.Reply (Some result)
                        Map.add id result dict)
                    |> Option.defaultValue dict
                    |> async.Return

        let agent initDict =
            MailboxProcessor.Start (fun inbox ->
                let rec loop dict = async {
                    let! message = inbox.Receive()
                    let! dict =  apply dict message
                    return! loop dict
                    }
                loop initDict)

        let create = agent >> ofAgent

module CategoryPredictionService =
    open CategoryPrediction

    type Error =
        | JobNotFound of JobId
        | ResultIsNotReady
        | ResponseError of StatusCode : int
        | Exception of System.Exception // TODO: ?
        | FormatError of string

    type Client = {
        TryInit :
            Offer list
            -> Result<Job, Error> Async
        TryGetJob :
            JobId
            -> Result<Job, Error> Async
        TryGetResult :
            JobId
            -> Result<(OfferId * CategoryId) list, Error> Async
    }

    module Client =

        module AgentHelpers =
            open AgentHelpers

            type twoWayWithError<'input, 'reply> =
                twoWay<'input, Result<'reply, Error>>

            type AgentMessage =
                | TryInit of
                    twoWayWithError<Offer list, Job>
                | TryGetJob of
                    twoWayWithError<JobId, Job>
                | TryGetResult of
                    twoWayWithError<JobId, (OfferId * CategoryId) list>

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

        module AgentBased =
            open AgentHelpers
            open Hopac
            open HttpFs.Client
            open Microsoft.FSharpLu.Json

            module CsvOffers =
                open Rosdex.Parser.Csv
                open Rosdex.Parser.Csv.Operators

                let offerCsvWriter : Offer CsvWriter = {
                    CsvWriter.Config = CsvConfig.default'
                    Fields =
                        [
                            (fun p -> p.Id) => "Id"
                            (fun p -> p.Name) => "Name"
                        ]
                }

                let serialize (stream : System.IO.TextWriter) offers =
                    offers
                    |> List.iter (
                        CsvWriter.tryStringifyItem offerCsvWriter
                        >> function
                            | Ok p -> stream.WriteLine p
                            | Error _ -> ())

                // TODO: ?
                let tryExtract (stream : System.IO.Stream) =
                    use reader = new System.IO.StreamReader(stream)
                    try
                        [
                            while not reader.EndOfStream do
                                match reader.ReadLine() |> String.split ',' |> List.map int with
                                | [id; categoryId] -> yield id, categoryId
                                | _ -> failwith "Wrong csv format!"
                        ]
                        // Ошибка ли это?
                        |> fun p ->
                            match p with
                            | [] -> printfn "Ooops, empty list!"; p
                            | _ -> p
                        |> Ok
                    with
                        | ex -> Error ex.Message

            type JobSchema = {
                [<Newtonsoft.Json.JsonRequired>] uuid : System.Guid
                [<Newtonsoft.Json.JsonRequired>] status : State
                created_at : System.DateTime
                // ..
            }

            module JobSchema =
                open Newtonsoft.Json

                type IgnoreMissingMember =
                    static member settings =
                        let r = Compact.Internal.Settings.settings
                        r.MissingMemberHandling <- MissingMemberHandling.Ignore
                        r
                    static member formatting =
                        Compact.Internal.Settings.formatting

                type IMM = With<IgnoreMissingMember>

                let toJob schema =
                    {
                        Info =
                            {
                                Id = schema.uuid
                                CreatedAt = schema.created_at
                            }
                        State = schema.status
                    }

                let tryExtract (response : Response) =
                    response.body
                    |> IMM.tryDeserializeStream<JobSchema> |> function
                        | Choice1Of2 p -> Ok p
                        | Choice2Of2 p -> Error (FormatError p)

            let handleNotFound id (response : Response) =
                match response.statusCode with
                | 404 -> Error (JobNotFound id)
                | _ -> Ok response

            let handleNetError (response : Response) =
                match response.statusCode with
                | 200 -> Ok response
                | p -> p |> ResponseError |> Error

            let tryExtractJob =
                JobSchema.tryExtract
                >> Result.map JobSchema.toJob

            let tryInitJob endpoint offers = async {
                let! response =
                    endpoint
                    |> Request.createUrl Post
                    // TODO: Send stream
                    |> Request.body (
                        let str =
                            let stream = new System.IO.StringWriter()
                            CsvOffers.serialize stream offers
                            stream.ToString()
                        BodyForm [
                            FormFile ("file",
                                ("file.csv",
                                    ContentType.create ("application", "vnd.ms-excel"),
                                    FileData.Plain str))
                        ])
                    |> tryGetResponse
                    |> Job.toAsync
                return
                    response
                    |> Result.ofChoice
                    |> Result.mapError (fun p -> Exception p)
                    |> Result.bind handleNetError
                    |> Result.bind tryExtractJob
                }

            let tryGetJob endpoint (id : JobId) = async {
                let! response =
                    id
                    |> sprintf "%s%O" endpoint
                    |> Request.createUrl Get
                    |> tryGetResponse
                    |> Job.toAsync
                return
                    response
                    |> Result.ofChoice
                    |> Result.mapError (fun p -> Exception p)
                    |> Result.bind (handleNotFound id)
                    |> Result.bind handleNetError
                    |> Result.bind tryExtractJob
                }

            let tryGetResult endpoint (id : JobId) = async {
                let! response =
                    id
                    |> sprintf "%s%O/result" endpoint
                    |> Request.createUrl Get
                    |> tryGetResponse
                    |> Job.toAsync
                return
                    response
                    |> Result.ofChoice
                    |> Result.mapError (fun p -> Exception p)
                    // TODO: not ready
                    |> Result.bind (handleNotFound id)
                    |> Result.bind handleNetError
                    |> Result.bind (fun p ->
                        p.body
                        |> CsvOffers.tryExtract
                        |> Result.mapError FormatError)
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
        let handleError result =
            match result with
            | Ok p -> Some p
            | Error err -> printfn "%A" err; None
        {
            TrySendToCategoryPrediction = fun offers -> async {
                let! job = client.TryInit offers
                return job |> handleError
            }
            TryCheckCategoryPrediction = fun id -> async {
                let! job = client.TryGetJob id
                return job |> handleError
            }
            TryFetchCategoryPredictionResult = fun id -> async {
                let! result = client.TryGetResult id
                return
                    result
                    |> handleError
                    |> Option.map Map.ofList
            }
        }