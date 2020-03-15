(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
State
=====

The intention when using the State monad is to keep state in a purely functional manner without violating referential transparency of functions.

Related Tyes
------------

 - [Reader](type-reader.html): Similar but read-only.

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data
(**
From [Haskell Wiki on State monad](https://wiki.haskell.org/State_Monad)
*)

let rec playGame =
    function
    | []-> monad {
            let! (_, score) = State.get
            return score
        }
    | x::xs-> monad {
            let! (on, score) = State.get
            match x with
            | 'a' when on -> do! State.put (on, score + 1)
            | 'b' when on -> do! State.put (on, score - 1)
            | 'c'         -> do! State.put (not on, score)
            | _           -> do! State.put (on, score)
            return! playGame xs
        }

let startState = (false, 0)
let moves = toList "abcaaacbbcabbab"
State.eval (playGame moves) startState
