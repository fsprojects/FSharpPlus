#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module Samples.MonadTrans
#endif

open System
open FSharpPlus
open FSharpPlus.Data

let fn : ResultT<Reader<int,Result<_,string>>> = 
    monad {
       let! x1 = lift ask
       let! x2 = 
        if x1 > 0 then 
            result 1
            else ResultT (result (Error "Negative value")) 
       return ((+) x1 x2)
    }

let x = (fn |> ResultT.run |> Reader.run) 10
// Result<int,string> = Ok 11
let y = (fn |> ResultT.run |> Reader.run) -1
// Result<int,string> = Error "Negative value"            

module Suave=
    // setup something that reminds us of what Suave can work with
    // this is an overly simplified model of Suave in order to show how OptionT can be used 
    // in conjunction with generic Kleisli composition (fish) operator
    type WebPart<'a> = 'a -> OptionT<Async<'a option>>
    let inline succeed x = async.Return (Some x)
    module WebPart=
        /// Comment from <a href="https://github.com/SuaveIO/suave/blob/v2.4.3/src/Suave/WebPart.fsi#L39-L42">WebPart.fsi</a>
        /// Entry-point for composing the applicative routes of the http application,
        /// by iterating the options, applying the context, arg, to the predicate
        /// from the list of options, until there's a match/a Some(x) which can be
        /// run.
        let choose (options : WebPart<'a> list) =fun x -> choice (List.map ( (|>) x) options)

    module Http=
        type HttpResponse = { status : int; content:string }
        type HttpRequest = { url : Uri; ``method``:string }
        
        type HttpContext = { request:HttpRequest; response:HttpResponse }
    module Successful=
        open Http
        let OK s = OptionT << fun ctx -> { ctx with response = { ctx.response with status = 200; content = s }} |> succeed
    module Filters=
        open Http
        let ``method`` (m : string) =
            OptionT << fun (x : HttpContext) -> async.Return (if (m = x.request.``method``) then Some x else None)
        let GET  (x : HttpContext) = ``method`` "GET" x
        let POST (x : HttpContext) = ``method`` "POST" x
  
        let path s =
            OptionT << fun (x : HttpContext) -> async.Return (if (s = x.request.url.AbsolutePath) then Some x else None)
    
    open Successful
    open Filters
    let app() =
        let overview =
            GET >=> (fun (ctx) ->
                monad {
                  return! OK "overview" ctx
                })
        let register =
            POST >=> fun (ctx) ->
                monad {
                  return! OK "register" ctx
                }
        WebPart.choose [ path "/" >=> (OK "/")
                         path "/note" >=> register
                         path "/notes" >=> overview ]
