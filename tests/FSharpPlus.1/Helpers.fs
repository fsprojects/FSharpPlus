module FSharpPlus.One.Helpers

open NUnit.Framework
open System.Collections


let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)
let areStEqual x y = Assert.IsTrue( (x = y), sprintf "Expected %A to be structurally equal to %A" x y)
let areEquivalent (x:IEnumerable) (y:IEnumerable) = CollectionAssert.AreEquivalent (x, y)

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset () = effects.Clear ()
    let add x = effects.Add (x)
    let get () = effects |> Seq.toList