#nowarn "3186"
#r @"..\..\build\FsControl.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Lens

let x = set  (_all "") ("Nothing") [|"Something"; ""; "Something Else"; "";|]
// val it : string [] = c

// we can preview it
let r2 = preview (_all "Something") [|"Something"; "Nothing"; "Something Else"; "Nothing"|]
// val it : Option<string> = Some "Something"

// view all elements in a list
let r3 = toListOf (_all "Something") [|"Something"; "Nothing"; "Something Else"; "Nothing"|]
// val it : string list = ["Something"]

// also view it, since string is a monoid
let r4 = view  (_all "Something") [|"Something"; "Nothing"; "Something Else"; "Nothing"|]
// val it : string = "Something"

// Lens composed with a Traversal -> Traversal
let r5 = preview  (_all ((),"Something") << _2) [((), "Something"); ((),""); ((), "Something Else"); ((),"")]
// val r5 : Option<string> = Some "Something"