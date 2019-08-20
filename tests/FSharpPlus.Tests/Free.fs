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
    let aFreeOfListFloat = aFreeOfListInt >>= (fun x -> Roll [ Pure "99" ]) >>= (fun x -> Roll [ Pure 90.4 ])
    
    // user defined functor types
    let aFreeOfIdentityInt = Roll (Identity (Pure 1)) |> Free.bind (fun x -> Roll (Identity (Pure 42)))


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

    type FreeDSL<'a> =
        | Free   of DSL<FreeDSL<'a>>
        | Return of 'a

    let rec bindFree: ('a -> FreeDSL<'b>) -> (FreeDSL<'a>) -> FreeDSL<'b> =
        fun           f                   ->
           function
           | Return a   -> f a
           | Free   dsl -> Free (mapDSL (bindFree f) dsl)

    let ex1  = Set ("alma", "bela", (Get ("alma", id)))
    let exF1 = Free (Set ("alma", "bela", (Free (Get ("alma", (fun s -> Return s))))))

    type FreeDSLBuilder () =
        member this.Return     x = Return x
        member this.ReturnFrom x = x
        member this.Zero      () = Return ()
        member this.Bind (ma, f) = bindFree f ma

    let domain = FreeDSLBuilder ()

    let liftFree: DSL<'a> -> FreeDSL<'a> =
        fun       action  -> Free (mapDSL Return action)

    let get key       = liftFree (Get (key, id))
    let set key value = liftFree (Set (key, value, ()))

    let exF2 = domain.Bind(set "foo" "bar", (fun _ -> get "foo"))

    let exF3 = domain {
        let! value  = get "foo"
        do! set "bar" value
        get "bar" |> ignore
    }


    let rec interpreter: ('a -> unit) -> FreeDSL<'a> -> unit =
        fun              receiver        free        ->
            match free with
            | Free(Get(key,        nextF)) -> printfn "Get %s" key
                                              nextF (sprintf "'get.%s'" key) |> interpreter receiver
            | Free(Set(key, value, next )) -> printfn "Set %s = %s" key value
                                              next                           |> interpreter receiver
            | Return v                     -> printfn "return(%A)" v
                                              receiver v

    interpreter (printfn "Received: %A") exF1
    interpreter (printfn "Received: %A") exF2
    interpreter (printfn "Received: %A") exF3
