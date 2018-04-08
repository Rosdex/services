namespace Rosdex.Services.ProductManager.Models

[<CLIMutable>]
type Message =
    {
        Text : string
    }

open Rosdex.Services.ProductManager.Domain

module AgentHelpers =
    type 'a oneWayFrom = 'a AsyncReplyChannel
    type twoWay<'input, 'reply> = 'input * 'reply AsyncReplyChannel

module JobStorage =
    open CardsBuilding

    type Client = {
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

    module AgentBased =
        open AgentHelpers

        type AgentMessage =
            | GetAll of oneWayFrom<Job list>
            | TryGet of twoWay<JobId, Job option>
            | CreateNew of twoWay<Offer list, Job>
            | TryApply of twoWay<JobId * (State -> State Async), Job option>
            | TryUpdate of twoWay<JobId * State, Job option>

        let ofAgent (agent : MailboxProcessor<_>) =
            let tuple input ch = input, ch
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

        module InMemory =
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
        | ResultIsNotReady of JobId
        | ResponseError of StatusCode : int
        | Exception of exn // TODO: ?
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

    open AgentHelpers

    type private twoWayWithError<'input, 'reply> =
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

    module Csv =
        open Rosdex.Parser.Csv
        open Rosdex.Parser.Csv.Operators

        let offersWriter : Offer CsvWriter = {
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
                CsvWriter.tryStringifyItem offersWriter
                >> function
                    | Ok p -> stream.WriteLine p
                    | Error _ -> ())

        let parseResult (stream : System.IO.Stream) =
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

    module AgentBased =
        open Hopac
        open HttpFs.Client

        module Job =
            open Microsoft.FSharpLu.Json
            open Newtonsoft.Json

            type JobSchema = {
                [<JsonRequired>] uuid : System.Guid
                [<JsonRequired>] status : State
                created_at : System.DateTime
                // ..
            }

            type IgnoreMissingMember =
                static member settings =
                    let r = Compact.Internal.Settings.settings
                    r.MissingMemberHandling <- MissingMemberHandling.Ignore
                    r
                static member formatting =
                    Compact.Internal.Settings.formatting

            type CompactIMM = With<IgnoreMissingMember>

            let tryExtractSchema (response : Response) =
                response.body
                |> CompactIMM.tryDeserializeStream<JobSchema>
                |> Result.ofChoice
                |> Result.mapError FormatError

            let toJob schema = {
                Info = {    Id = schema.uuid
                            CreatedAt = schema.created_at }
                State = schema.status
            }

            let tryExtract =
                tryExtractSchema
                >> Result.map toJob

        module Handlers =
            let exc choice =
                choice
                |> Result.ofChoice
                |> Result.mapError Exception

            let statusCode code error (response : Response)=
                if response.statusCode = code
                then Some error
                else None

            let notFound id = statusCode 404 (JobNotFound id)

            let resultNotReady id = statusCode 405 (ResultIsNotReady id)

            let netError (response : Response) =
                if response.statusCode >= 300
                then response.statusCode |> ResponseError |> Some
                else None

            let optionHandler handler response =
                response
                |> handler
                |> function
                    | Some err -> Error err
                    | None -> Ok response

            let defaultWith handlers chResponse =
                chResponse
                |> exc
                |> Result.bind (optionHandler (fun res ->
                    handlers |> List.tryPick (fun f -> f res)))
                |> Result.bind (optionHandler netError)

        type 'a Requesting = {
            BuildRequest : unit -> Request
            SpecialHandlers : (Response -> Error option) list
            BindModel : Response -> Result<'a, Error>
        }

        module Requesting =
            let execute query = async {
                let! response =
                    query.BuildRequest ()
                    |> tryGetResponse
                    |> Job.toAsync
                return response
                    |> Handlers.defaultWith query.SpecialHandlers
                    |> Result.bind query.BindModel
            }

        module Requestings =
            let tryInitJob endpoint offers = {
                BuildRequest = fun () ->
                    endpoint
                    |> Request.createUrl Post
                    // TODO: Send stream
                    |> Request.body (
                        let str =
                            let stream = new System.IO.StringWriter()
                            Csv.serialize stream offers
                            stream.ToString()
                        BodyForm [
                            FormFile ("file",
                                ("file.csv",
                                    ContentType.create ("application", "vnd.ms-excel"),
                                    FileData.Plain str))
                        ])
                SpecialHandlers = []
                BindModel = Job.tryExtract
            }

            let tryGetJob endpoint id = {
                Requesting.BuildRequest = fun () ->
                    id
                    |> sprintf "%s%O" endpoint
                    |> Request.createUrl Get
                SpecialHandlers = [ Handlers.notFound id ]
                BindModel = Job.tryExtract
            }

            let tryGetResult endpoint id = {
                Requesting.BuildRequest = fun () ->
                    id
                    |> sprintf "%s%O/result" endpoint
                    |> Request.createUrl Get
                SpecialHandlers =
                    [   Handlers.notFound id
                        Handlers.resultNotReady id ]
                BindModel = fun p ->
                    p.body
                    |> Csv.parseResult
                    |> Result.mapError FormatError
            }

        let agent endpoint = MailboxProcessor.Start (fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | TryInit (offers, rc) ->
                    let! result =
                        Requestings.tryInitJob endpoint offers
                        |> Requesting.execute
                    rc.Reply result
                | TryGetJob (id, rc) ->
                    let! result =
                        Requestings.tryGetJob endpoint id
                        |> Requesting.execute
                    rc.Reply result
                | TryGetResult (id, rc) ->
                    let! result =
                        Requestings.tryGetResult endpoint id
                        |> Requesting.execute
                    rc.Reply result
                return! loop ()
                }
            loop ()
            )

        let create =
            agent >> ofAgent

module CommandHandler =
    open Rosdex.Services.ProductManager.Domain.CardsBuilding

    // TODO: Bubble Error
    let ofCategoryPredictionServiceClient
            (client : CategoryPredictionService.Client)
            : CommandHandler =
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