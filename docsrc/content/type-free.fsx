(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
(**
Free<'Functor<'T>, 'T>
======================

This type is an implementation of the [Free Monad](https://www.google.com/search?q=free+monad) which is generic to any [Functor](abstraction-functor.html).

The Free Monad is used typically to describe a pure program at high level and separately write different interpreters for it.

Related Types
-------------

 - [Coproduct](type-coproduct.html): A [Functor](abstraction-functor.html) used in conjunction with the Free Monad to combine different instruction sets.




Examples
--------

Free monad-interpreter in F# from [Mark Seemann's blog](https://blog.ploeh.dk/2017/07/17/a-pure-command-line-wizard) but encoded with Free.

*)



(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open System
open FSharpPlus
open FSharpPlus.Data


type CommandLineInstruction<'t> =
    | ReadLine  of (string -> 't)
    | WriteLine of  string  * 't
with static member Map (x, f) =
        match x with
        | ReadLine   g     -> ReadLine  (f << g)
        | WriteLine (s, g) -> WriteLine (s, f g)

let readLine    = Free.liftF (ReadLine id)
let writeLine s = Free.liftF (WriteLine (s, ()))


let rec interpretCommandLine = Free.run >> function
    | Pure x -> x
    | Roll (ReadLine      next)  -> Console.ReadLine () |> next |> interpretCommandLine
    | Roll (WriteLine (s, next)) ->
        Console.WriteLine s
        next |> interpretCommandLine

let rec readQuantity = monad {
    do! writeLine "Please enter number of diners:"
    let! l = readLine
    match tryParse l with
    | Some dinerCount -> return dinerCount
    | None ->
        do! writeLine "Not an integer."
        return! readQuantity }

let rec readDate = monad {
    do! writeLine "Please enter your desired date:"
    let! l = readLine
    match DateTimeOffset.TryParse l with
    | true, dt -> return dt
    | _ ->
        do! writeLine "Not a date."
        return! readDate }

let readName = monad {
    do! writeLine "Please enter your name:"
    return! readLine }
 
let readEmail = monad {
    do! writeLine "Please enter your email address:"
    return! readLine }


type Reservation = {
    Date : DateTimeOffset
    Name : string
    Email : string
    Quantity : int }
    with static member Create (Quantity, Date, Name, Email) = { Date = Date; Name = Name; Email = Email; Quantity = Quantity }

let readReservationRequest =
    curryN Reservation.Create
    <!> readQuantity
    <*> readDate
    <*> readName
    <*> readEmail



let mainFunc () =
    readReservationRequest
    >>= (writeLine << (sprintf "%A"))
    |> interpretCommandLine
    0


(**
More reading
------------

 - Highly recommended Matt Thornton's blog [Grokking Free monads](https://dev.to/choc13/grokking-free-monads-9jd) and [Interpreting Free Monads](https://dev.to/choc13/interpreting-free-monads-3l3e).
   It contains examples using F#+ and an explanation from scratch.

 - Mark Seemann's blog has an [article series](https://blog.ploeh.dk/2017/06/27/pure-times/) which ends 
   up describing Free Monads although he doesn't use F#+ and therefore either repeats boilerplate code or switches to Haskell.
   Anyways some code from those series (like the above fragment) can be found in [our test suite for Free](https://github.com/fsprojects/FSharpPlus/blob/master/tests/FSharpPlus.Tests/Free.fs) simplified using Free and Coproduct types.

 - Scott Wlaschin's [13 ways of looking at a turtle](https://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle) is also a series which ends up defining a Free Monad, without using F#+ but with boilerplate code instead.

*)