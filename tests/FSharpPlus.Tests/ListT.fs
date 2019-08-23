module FSharpPlus.Tests.ListT

open System
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework
open FsCheck
open Helpers
open System.Collections.Generic
open System.Threading.Tasks

module BasicTests =
    [<Test>]
    let wrap_unwrap () =
        let c = listT (async.Return (['a'..'g']))
        let res = c |> ListT.run |> listT |> ListT.run |> extract
        let exp = c |> ListT.run |> extract
        CollectionAssert.AreEqual (res, exp)
  
    [<Test>]
    let infiniteLists () =
        let (infinite: ListT<Lazy<_>>) = ListT.unfold (fun x -> monad { return (Some (x, x + 1) ) }) 0
        let finite = take 12 infinite
        let res = finite <|> infinite
        CollectionAssert.AreEqual (res |> take 13 |> ListT.run |> extract, [0;1;2;3;4;5;6;7;8;9;10;11;0])
        
    // Compile tests
    let binds () =
        let res1 = listT [| [1..4] |] >>= fun x -> listT [| [x * 2] |]
        let res2 = listT (Task.FromResult [1..4]) |> ListT.bind (fun x -> listT (Task.FromResult  [x * 2]))
        let res3 = listT (ResizeArray [ [1..4] ]) |> ListT.bind (fun x -> listT (ResizeArray [ [x * 2] ]))
        let res4 = listT (lazy [1..4]) |> ListT.bind (fun x -> listT (lazy ( [x * 2])))
        let (res5: ListT<_ seq>) = listT (seq [ [1..4] ]) |> ListT.bind (fun x -> listT (seq [ [x * 2] ]))
        () // Note: seq needs type annotation, the non-sealead types don't work with generic >>= (internal error, unsolved type var)
        
    let bind_for_ideantity () =
        let res = listT (Identity [1..4]) >>= fun x -> listT (Identity [x * 2])
        ()    
        
    let computation_expressions () =
        let oneTwoThree : ListT<_> = monad.plus { 
            do! lift <| Async.Sleep 10
            yield 1
            do! lift <| Async.Sleep 50
            yield 2 
            yield 3}
        ()

    let applicative_with_options () =
        let x = (+) <!> listT None <*> listT (Some [1;2;3;4])
        () // It doesn't work with asyncs
