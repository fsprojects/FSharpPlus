(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

(**
Lens
====

Lens is an abstraction over function that allow to read and update parts of immutable data.

The abstraction name comes from the analogy of focusing on a specific part of the data structure.

Another analogy could be with pointers, but in this case data is treated as immutable which means that instead of modifying it returns a new copy.

In this [quick tour](tutorial.html#Lens) you can find some basic examples of operating with Lenses.

To allow lensing over your record types, lens (as functions) have to be written by hand for each field.

As a convention, all lens identifiers will start with an underscore `_`.

Here's an example usage of lenses with business objects:


*)
open System
// In order to use the Lens module of F#+ we import the following:
open FSharpPlus.Lens
// These modules contain other functions relevant for the examples:
open FSharpPlus
open FSharpPlus.Data

// From Mauricio Scheffer: https://gist.github.com/mausch/4260932
type Person = {
    Name: string
    DateOfBirth: DateTime
}
module Person=
    let inline _name f { Name = a; DateOfBirth = b } = f a <&> fun a' -> { Name = a'; DateOfBirth = b }
 type Book = {
    Title: string
    Author: Person
}
module Book =
    let inline _author f { Author = a; Title = b } = f a <&> fun a' -> { Author = a'; Title = b }
    let inline _authorName b = _author << Person._name <| b
let rayuela =
    { Book.Title = "Rayuela"
      Author = { Person.Name = "Julio Cortázar"
                 DateOfBirth = DateTime (1914, 8, 26) } }
// read book author name:
let authorName1 = view Book._authorName rayuela
//  you can also write the read operation as:
let authorName2 = rayuela ^. Book._authorName


(**

Note: 

The operator `<&>` is not available in F#+ v1.0 but since it's a flipped map, you can use `</flip map/>` instead.

However it's recommended to upgrade F#+ since you'll get better compile times with `<&>`.

Prism
=====

Also called a Partial Lens, they focus in parts of the data that could be there or not.

See the following example using the built-in `_Some` prism.

*)

type Team   = {name: string; victories: int}
let inline _name      f {name = a; victories = b} = f a <&> fun a' -> {name = a'; victories = b }
let inline _victories f {name = a; victories = b} = f b <&> fun b' -> {name = a ; victories = b'}

type Player = {team: Team; score: int}
let inline _team  f {team = a; score = b} = f a <&> fun a' -> {team = a'; score = b }
let inline _score f {team = a; score = b} = f b <&> fun b' -> {team = a ; score = b'}

type Result = {winner: Player option; started: bool}
let inline _winner   f {winner = a; started = b} = f a <&> fun a' -> {winner = a'; started = b }
let inline _started  f {winner = a; started = b} = f b <&> fun b' -> {winner = a ; started = b'}

type Match<'t>  = {players: 't; finished: bool}
let inline _players  f {players = a; finished = b} = f a <&> fun a' -> {players = a'; finished = b }
let inline _finished f {players = a; finished = b} = f b <&> fun b' -> {players = a ; finished = b'}

// Lens composed with Prism -> Prism
let inline _winner_team x = (_players << _winner << _Some << _team) x

// initial state
let match0 =
    {
        players = 
            {team = {name = "The A Team"; victories = 0}; score = 0},
            {team = {name = "The B Team"; victories = 0}; score = 0}
        finished = false
    }


// Team 1 scores
let match1 = over (_players << _1 << _score) ((+) 1) match0

// Team 2 scores
let match2 = over (_players << _2 << _score) ((+) 1) match1

// Produce Match<Result> from Match<Player * Player> 
// This is possible with these Lenses since they support polymorphic updates.
let matchResult0 = setl _players {winner = None; started = true} match2

// See if there is a winner by using a prism
let _noWinner = preview _winner_team matchResult0

// Team 1 scores
let match3 = over (_players << _1 << _score) ((+) 1) match2

// End of the match
let match4 = setl _finished true match3
let match5 = over (_players << _1 << _team << _victories) ((+) 1) match4
let matchResult1 = over _players (fun (x, _) -> {winner = Some x; started = true}) match5

// And the winner is ...
let winner = preview _winner_team matchResult1







(**
Traversal
=========

*)



let t1 = [|"Something"; ""; "Something Else"; ""|] |> setl (_all "") ("Nothing")
// val t1 : string [] = [|"Something"; "Nothing"; "Something Else"; "Nothing"|]

// we can preview it
let t2 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> preview (_all "Something")
// val t2 : string option = Some "Something"

// view all elements in a list
let t3 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> toListOf (_all "Something")
// val t3 : string list = ["Something"]

// also view it, since string is a monoid
let t4 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> view  (_all "Something")
// val t4 : string = "Something"

// Lens composed with a Traversal -> Traversal
let t5 = [((), "Something"); ((),""); ((), "Something Else"); ((),"")] |> preview  (_all ((),"Something") << _2)
// val t5 : Option<string> = Some "Something"




(**
Fold
====

*)


let f1 = over both length ("hello","world")
// val f1 : int * int = (5, 5)

let f2 = ("hello","world")^.both
// val f2 : string = "helloworld"

let f3 = anyOf both ((=)'x') ('x','y')
// val f3 : bool = true

let f4 = (1,2)^..both
// val f4 : int list = [1; 2]

let f5 = over items length ["hello";"world"]
// val f5 : int list = [5; 5]

let f6 = ["hello";"world"]^.items
// val f6 : string = "helloworld"

let f7 = anyOf items ((=)'x') ['x';'y']
// val f7 : bool = true

let f8 = [1;2]^..items
// val f8 : int list = [1; 2]

let f9 = foldMapOf (traverse << both << _Some) Mult [(Some 21, Some 21)]
// val f9 : Mult<int> = Mult 441

let f10 = foldOf (traverse << both << _Some) [(Some 21, Some 21)]
// val f10 : int = 42

let f11 = allOf both (fun x-> x >= 3) (4,5)
// val f11 : bool = true

(**
Iso
===

*)


let toOption (isSome, v) = if isSome then Some v else None
let fromOption = function Some (x:'t) -> (true, x) | None -> (false, Unchecked.defaultof<'t>)
let inline isoTupleOption x = x |> iso toOption fromOption


let i1 = view isoTupleOption (System.Int32.TryParse "42")
// val i1 : int option = Some 42

let i2 = view (from' isoTupleOption) (Some 42)
// val i2 : bool * int = (true, 42)

// Iso composed with a Lens -> Lens
let i3 = view (_1 << isoTupleOption) (System.Int32.TryParse "42", ())
// val i3 : int option = Some 42


(**
Maximum and minimum
===================

*)

let fv3 = maximumOf (traverse << both << _Some) [(Some 1, Some 2);(Some 3,Some 4)]
// val fv3 : int option = Some 4

let fv4 = minimumOf (traverse << both << _Some) [(Some 1, Some 2);(Some 3,Some 4)]
// val fv4 : int option = Some 1
