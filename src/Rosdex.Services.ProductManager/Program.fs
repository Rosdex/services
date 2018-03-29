module Rosdex.Services.ProductManager.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Rosdex.Services.ProductManager.HttpHandlers

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

let webApp =
    logInput
    >=> logOutput
    >=> choose [
        subRoute "/api" (
            choose [
                GET >=> choose [
                    route "/hello" >=> handleGetHello
                ]
            ])
        setStatusCode 404 >=> text "Not Found"
    ]

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
    builder.WithOrigins("http://localhost:8080")
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

[<EntryPoint>]
let main _ =
    WebHostBuilder()
        .UseKestrel()
        .UseIISIntegration()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0