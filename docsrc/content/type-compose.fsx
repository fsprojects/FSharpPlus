(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

(**
Compose
=======

Allows to compose applicatives and functors.

It worth noting that:

 - A composition of 2 functors is a functor
 - A composition of 2 applicatives is an applicative
 - A composition of 2 monads is not always a monad

Examples
--------
*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open FSharpPlus
open FSharpPlus.Data

// First let's create some values

let (one : Async<Result<int, string>>) = async { return Ok 1 }
let (two : Async<Result<int, string>>) = async { return Ok 2 }

// Now we can combine then

let (Compose three) = Compose (async {return Ok (+)}) <*> Compose one <*> Compose two
// val three : Async<FSharpPlus.Result<int,string>>

// or shorter

let (Compose three') = (+) <!> Compose one <*> Compose two
// val three' : Async<FSharpPlus.Result<int,string>>