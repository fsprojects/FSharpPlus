#nowarn "3186"
#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Lens

let toOption (isSome, v) = if isSome then Some v else None
let fromOption = function Some (x:'t) -> (true, x) | None -> (false, Unchecked.defaultof<'t>)
let inline isoTupleOption x = x |> iso toOption fromOption


let r1 = view isoTupleOption (System.Int32.TryParse "42")
// val it : int option = Some 42

let r2 = view (from isoTupleOption) (Some 42)
// val it : bool * int = (true, 42)

// Iso composed with a Lens -> Lens
let r3 = view (_1 << isoTupleOption) (System.Int32.TryParse "42", ())
// val it : int option = Some 42