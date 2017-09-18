#nowarn "3186"
#r @"../bin/Release/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Lens

let x = [|"Something"; ""; "Something Else"; "";|] |> set (_all "") ("Nothing")
// val it : string [] = c

// we can preview it
let r2 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> preview (_all "Something")
// val it : Option<string> = Some "Something"

// view all elements in a list
let r3 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> toListOf (_all "Something")
// val it : string list = ["Something"]

// also view it, since string is a monoid
let r4 = [|"Something"; "Nothing"; "Something Else"; "Nothing"|] |> view  (_all "Something")
// val it : string = "Something"

// Lens composed with a Traversal -> Traversal
let r5 = [((), "Something"); ((),""); ((), "Something Else"); ((),"")] |> preview  (_all ((),"Something") << _2)
// val r5 : Option<string> = Some "Something"