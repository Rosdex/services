namespace Rosdex.Services.ProductManager

module HttpHandlers =

    open Microsoft.AspNetCore.Http
    open Giraffe
    open Rosdex.Services.ProductManager.Models
    open Rosdex.Services.ProductManager.Domain

    let handleGetHello =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let response = {
                    Text = "Hello world, from Giraffe!"
                }
                return! json response next ctx
            }

    module ProductsManager =

        module CardsBuilding =
            let handlePostProducts storageApi offers : HttpHandler =
                fun next ctx -> task {
                    let! response =
                        storageApi.CreateNew offers
                        |> Async.StartAsTask
                    return! json response next ctx
                    }
            let handleGetAllProducts storageApi : HttpHandler =
                fun next ctx -> task {
                    let! response =
                        storageApi.GetAll ()
                        |> Async.StartAsTask
                    return! json response next ctx
                    }
            let handleGetProduct storageApi id : HttpHandler =
                fun next ctx -> task {
                    let! response =
                        storageApi.TryGetJob id
                        |> Async.StartAsTask
                    match response with
                    | Some p ->
                        return! json p next ctx
                    | None ->
                        // TODO: ?
                        return! json { Text = "Not found" } next ctx
                    }

            let endpoint storageApi =
                choose [
                    GET >=> choose [
                        routef "/%s" (
                            // TODO: Handle error
                            System.Guid.Parse
                            >> handleGetProduct storageApi)
                        // TODO: Кошерно ли это?
                        route ""
                            >=> handleGetAllProducts storageApi
                    ]
                    POST
                        >=> route ""
                        // TODO: Handle error
                        >=> bindJson<Offer list> (handlePostProducts storageApi)
                ]