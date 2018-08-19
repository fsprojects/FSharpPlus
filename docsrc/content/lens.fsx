(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus
open FSharpPlus.Lens

(**
Lens
====

TODO Add some basic lens samples here

*)


(**
Prism
=====

*)

type Team   = {name: string; victories: int}
let inline _name      f {name = a; victories = b} = map (fun a' -> {name = a'; victories = b }) (f a)
let inline _victories f {name = a; victories = b} = map (fun b' -> {name = a ; victories = b'}) (f b)

type Player = {team: Team; score: int}
let inline _team  f {team = a; score = b} = map (fun a' -> {team = a'; score = b }) (f a)
let inline _score f {team = a; score = b} = map (fun b' -> {team = a ; score = b'}) (f b)

type Result = {winner: Player option; started: bool}
let inline _winner   f {winner = a; started = b} = map (fun a' -> {winner = a'; started = b }) (f a)
let inline _started  f {winner = a; started = b} = map (fun b' -> {winner = a ; started = b'}) (f b)

type Match<'t>  = {players: 't; finished: bool}
let inline _players  f {players = a; finished = b} = map (fun a' -> {players = a'; finished = b }) (f a)
let inline _finished f {players = a; finished = b} = map (fun b' -> {players = a ; finished = b'}) (f b)

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
Example usage of lenses with business objects
===

*)
open System
// From Mauricio Scheffer: https://gist.github.com/mausch/4260932
type Person = {
    Name: string
    DateOfBirth: DateTime
}
module Person=
    let inline name f { Name = a; DateOfBirth = b } = map (fun a' -> { Name = a'; DateOfBirth = b }) (f a)
 type Book = {
    Title: string
    Author: Person
}
module Book =
    let inline author f { Author = a; Title = b } = map (fun a' -> { Author = a'; Title = b }) (f a)
    let inline authorName b = author << Person.name <| b
let rayuela =
    { Book.Title = "Rayuela"
      Author = { Person.Name = "Julio Cortázar"
                 DateOfBirth = DateTime(1914, 8, 26) } }
// read book author name:
let authorName1 = view Book.authorName rayuela
//  you can also write the read operation as:
let authorName2 = rayuela ^. Book.authorName
