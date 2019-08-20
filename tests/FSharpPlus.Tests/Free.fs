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

