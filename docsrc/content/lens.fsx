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
open FSharpPlus
// In order to use the Lens module of F#+ we import the following:
open FSharpPlus.Lens

// From Mauricio Scheffer: https://gist.github.com/mausch/4260932
type Person = 
    { Name: string
      DateOfBirth: DateTime }

module Person =
    let inline _name f p =
        f p.Name <&> fun x -> { p with Name = x }

type Page =
    { Contents: string }

module Page =
    let inline _contents f p =
        f p.Contents <&> fun x -> {p with Contents = x}

type Book = 
    { Title: string
      Author: Person 
      Pages: Page list }

module Book =
    let inline _author f b =
        f b.Author <&> fun a -> { b with Author = a }

    let inline _authorName b = _author << Person._name <| b

    let inline _pages f b =
        f b.Pages <&> fun p -> { b with Pages = p }

    let inline _pageNumber i b =
        _pages << List._item i << _Some <| b

let rayuela =
    { Book.Title = "Rayuela"
      Author = { Person.Name = "Julio Cortázar"
                 DateOfBirth = DateTime(1914, 8, 26) } 
      Pages = [
        { Contents = "Once upon a time" }
        { Contents = "The End"} ] }
    
// read book author name:
let authorName1 = view Book._authorName rayuela
//  you can also write the read operation as:
let authorName2 = rayuela ^. Book._authorName

// write value through a lens
let book1 = setl Book._authorName "William Shakespear" rayuela
// update value
let book2 = over Book._authorName String.toUpper rayuela

(**

Note: 

The operator `<&>` is not available in F#+ v1.0 but since it's a flipped map, you can use `</flip map/>` instead.

However it's recommended to upgrade F#+ since you'll get better compile times with `<&>`.

Prism
=====

Also called a Partial Lens, they focus in parts of the data that could be there or not.

See the following example using the built-in `_Some` prism.

*)

type Team   = { Name: string; Victories: int }
let inline _name      f t = f t.Name      <&> fun n -> { t with Name      = n }
let inline _victories f t = f t.Victories <&> fun v -> { t with Victories = v }

type Player = { Team: Team; Score: int }
let inline _team  f p = f p.Team  <&> fun t -> { p with Team  = t }
let inline _score f p = f p.Score <&> fun s -> { p with Score = s }

type Result = { Winner: Player option; Started: bool}
let inline _winner   f r = f r.Winner  <&> fun w -> { r with Winner  = w }
let inline _started  f r = f r.Started <&> fun s -> { r with Started = s }

type Match<'t>  = { Players: 't; Finished: bool }
// For polymorphic updates to be possible, we can't use `with` expression on generic field lens.
let inline _players  f m = f m.Players  <&> fun p -> { Finished = m.Finished; Players  = p }
let inline _finished f m = f m.Finished <&> fun f -> { m with Finished = f }

// Lens composed with Prism -> Prism
let inline _winnerTeam x = (_players << _winner << _Some << _team) x

// initial state
let match0 =
    { Players = 
            { Team = { Name = "The A Team"; Victories = 0 }; Score = 0 },
            { Team = { Name = "The B Team"; Victories = 0 }; Score = 0 }
      Finished = false }


// Team 1 scores
let match1 = over (_players << _1 << _score) ((+) 1) match0

// Team 2 scores
let match2 = over (_players << _2 << _score) ((+) 1) match1

// Produce Match<Result> from Match<Player * Player> 
// This is possible with these Lenses since they support polymorphic updates.
let matchResult0 = setl _players { Winner = None; Started = true } match2

// See if there is a winner by using a prism
let _noWinner = preview _winnerTeam matchResult0

// Team 1 scores
let match3 = over (_players << _1 << _score) ((+) 1) match2

// End of the match
let match4 = setl _finished true match3
let match5 = over (_players << _1 << _team << _victories) ((+) 1) match4
let matchResult1 = over _players (fun (x, _) -> { Winner = Some x; Started = true }) match5

// And the winner is ...
let winner = preview _winnerTeam matchResult1







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
open FSharpPlus.Lens
open FSharpPlus // This module contain other functions relevant for the examples (length, traverse)
open FSharpPlus.Data // Mult

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
