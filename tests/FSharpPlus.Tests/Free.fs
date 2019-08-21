namespace FSharpPlus.Tests

open System
open FSharpPlus
open FSharpPlus.Builders
open FSharpPlus.Data
open NUnit.Framework

open Helpers
open SideEffects

module Free =
    // compile tests only for now
    
    // primitive functor types
    let aFreeOfListInt = Roll [Roll [Roll [Pure 2]]]
    let aFreeOfListFloat = aFreeOfListInt >>= (fun x -> let a = Roll [ Pure "99" ] in a) >>= (fun x -> Roll [ Pure 90.4 ])
    
    // user defined functor types
    let aFreeOfIdentityInt = Roll (Identity (Pure 1)) >>= (fun x -> Roll (Identity (Pure 42)))


module Sample1 =
    // 
    // Free monad-interpreter in F# from http://www.fssnip.net/7SX/title/Freemonad-interpreter
    // (based on: http://programmers.stackexchange.com/a/242803/145941)

    type DSL<'next> =
        | Get of key: string *       (string -> 'next)
        | Set of key: string * value: string *  'next

    let mapDSL: ('a -> 'b) -> DSL<'a> -> DSL<'b> = 
        fun     f          ->
            function
            | Get (k,    c) -> Get (k,      c >> f)
            | Set (k, v, c) -> Set (k, v, f c     )

    type DSL<'next> with
        static member Map (x, f) = mapDSL f x

    type FreeDSL<'a> = Free<DSL<'a>,'a>

    let ex1  = Set ("alma", "bela", (Get ("alma", id)))
    let exF1 = Roll (Set ("alma", "bela", (Roll (Get ("alma", (fun s -> Pure s))))))

    let liftFree: DSL<'a> -> FreeDSL<'a> =
        fun       action  -> Roll (mapDSL Pure action)

    let get key       = liftFree (Get (key, id))
    let set key value = liftFree (Set (key, value, ()))

    let exF2 = set "foo" "bar" >>= fun _ -> get "foo"

    let exF3 = monad {
        let! value  = get "foo"
        do! set "bar" value
        get "bar" |> ignore
    }


    let rec interpreter: ('a -> unit) -> FreeDSL<'a> -> unit =
        fun              receiver        free        ->
            match free with

            | Roll (Get(key,        nextF)) -> printfn "Get %s" key
                                               nextF (sprintf "'get.%s'" key) |> interpreter receiver
            | Roll (Set(key, value, next )) -> printfn "Set %s = %s" key value
                                               next                           |> interpreter receiver
            | Pure v                        -> printfn "return(%A)" v
                                               receiver v


    interpreter (printfn "Received: %A") exF1
    interpreter (printfn "Received: %A") exF2
    interpreter (printfn "Received: %A") exF3
