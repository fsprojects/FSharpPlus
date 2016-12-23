#nowarn "3186"
#r @"..\..\build\FsControl.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus

// from https://github.com/ekmett/lens/wiki/Examples

// First, open F#+ Lens
open FSharpPlus.Lens

// Now, you can read from lenses
let r1 = ("hello","world")^._2
// val it : string = "world"

// and you can write to lenses.
let r2 = set _2 42 ("hello","world")
// val it : string * int = ("hello", 42)

// Composing lenses for reading (or writing) goes in the order an imperative programmer would expect, and just uses (<<).
let r3 = ("hello",("world","!!!"))^.(_2 << _1)
// val it : string = "world"

let r4 = set (_2 << _1) 42 ("hello",("world","!!!"))             
// val it : string * (int * string) = ("hello", (42, "!!!"))

// You can make a Getter out of a pure function with to'.
let r5 = "hello"^.to' length
// val it : int = 5

// You can easily compose a Getter with a Lens just using (<<). No explicit coercion is necessary.
let r6 = ("hello",("world","!!!"))^. (_2 << _2 << to' length)
// val it : int = 3

// As we saw above, you can write to lenses and these writes can change the type of the container. (.->) is an infix alias for set.
let r7 = _1 .-> "hello" <| ((),"world")
// val it : string * string = ("hello", "world")

// It can be used in conjunction with (|>) for familiar von Neumann style assignment syntax:
let r8 = ((), "world") |> _1 .-> "hello"
// val it : string * string = ("hello", "world")

// Conversely view, can be used as an prefix alias for (^.).
let r9 = view _2 (10,20)
// val it : int = 20