module Rosdex.Services.ProductManager.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Rosdex.Services.ProductManager.HttpHandlers
open Rosdex.Services.ProductManager.Models

// ---------------------------------
// Web app
// ---------------------------------

let logInput : HttpHandler =
    fun next ctx -> task {
        sprintf "At %O: %O | %O | %O"
            System.DateTime.Now
            ctx.Request.Method
            ctx.Request.Path
            ctx.Request.QueryString
        |> ctx.GetLogger("io").LogDebug
        return! next ctx
        }

let logOutput : HttpHandler =
    fun next ctx -> task {
        let! result = next ctx
        result
        |> Option.iter (fun p ->
            sprintf "At %O: %O | %O | %O"
                System.DateTime.Now
                p.Response.StatusCode
                p.Response.ContentType
                p.Response.ContentLength
            |> ctx.GetLogger("io").LogDebug)
        return result
        }

let jobStorageApi = JobStorage.AgentBased.InMemory.create Map.empty

let webApp =
    logInput
    >=> logOutput
    >=> choose [
        subRoute "/api" (
            choose [
                subRoute "/products/cards-building" (ProductsManager.CardsBuilding.endpoint jobStorageApi)
                GET >=> choose [
                    route "/hello" >=> handleGetHello
                ]
            ])
        setStatusCode 404 >=> text "Not Found"
    ]

module CategoryPredictionJobSubscriber =
    open Domain.CardsBuilding

    module Default =
        let categoryPredictionJobsEndpoint =
            "http://localhost:5001/jobs/"

    let handler categoryPredictionEndpoint =
        categoryPredictionEndpoint
        |> CategoryPredictionService.AgentBased.create
        |> CommandHandler.ofCategoryPredictionServiceClient

    let agent handler =
        MailboxProcessor.Start (fun inbox ->
            let rec loop () = async {
                let! id, command = inbox.Receive()
                try
                    let! job = jobStorageApi.TryApply id (fun state -> async {
                        let! result = CommandHandler.handle handler command state
                        return
                            match result with
                            | Ok newState -> newState
                            | Error p -> printfn "%A" p; state
                        })
                    match job with
                    | Some _ -> ()
                    | None ->
                        printfn "Job %O is not found." id
                with
                    | ex -> printfn "%s" ex.Message
                return! loop ()
            }
            loop ()
        )

    type CPState = Domain.CategoryPrediction.State

    let rec loop intervalMs (agent : MailboxProcessor<_>) =
        async {
            do! Async.Sleep intervalMs
            try
                let! jobs = jobStorageApi.GetAll ()
                jobs
                |> List.choose (fun job ->
                    match job.State with
                    | State.Created _ ->
                        Some SendToCategoryPrediction
                    | State.JobInCategoryPrediction p
                        when p.PredictionJob.State = CPState.Done ->
                        Some FetchCategoryPredictionResult
                    | State.JobInCategoryPrediction _ ->
                        Some CheckCategoryPrediction
                    | _ -> None
                    // ≈сли Job, то при малом интервале начинает забивать задачами с устаревшими данными
                    |> Option.map (fun p -> job.Info.Id, p))
                |> List.map (fun p -> printfn "%A" p; p)
                |> List.iter agent.Post
            with
                | ex -> printfn "%s" ex.Message
            return! loop intervalMs agent
        }

    let build intervalMs categoryPredictionEndpoint =
        categoryPredictionEndpoint
        |> handler
        |> agent
        |> loop intervalMs

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:54496") // »з launchSettings.json
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseGiraffe(webApp)

module Configuration =
    open Microsoft.FSharpLu.Json
    open Giraffe.Serialization.Json

    let useFSharpLuJson (services : IServiceCollection) =
        Compact.Internal.Settings.settings
        |> NewtonsoftJsonSerializer
        |> services.AddSingleton<IJsonSerializer>
        |> ignore

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

    Configuration.useFSharpLuJson services

let configureLogging (builder : ILoggingBuilder) =
    let filter name logLevel =
        match logLevel, name with
        | LogLevel.Error, _
        | LogLevel.Debug, "io"
            -> true
        | _ -> false
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

open Microsoft.Extensions.Configuration
open System.IO

[<EntryPoint>]
let main args =
    args
        |> Seq.pairwise
        |> Seq.tryFind (fst >> String.equalsCaseInsensitive "-cpje")
        |> Option.map snd
        |> Option.defaultValue CategoryPredictionJobSubscriber.Default.categoryPredictionJobsEndpoint
        |> fun p ->
            printfn "Category prediction service jobs endpoint: %s" p;
            p
        |> CategoryPredictionJobSubscriber.build (10 * 1000)
        |> Async.Start
    let config =
        ConfigurationBuilder()
            .SetBasePath(Directory.GetCurrentDirectory())
            .AddJsonFile("hosting.json", true)
            .Build()
    WebHostBuilder()
        .UseKestrel()
        .UseIISIntegration()
        .UseConfiguration(config)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0