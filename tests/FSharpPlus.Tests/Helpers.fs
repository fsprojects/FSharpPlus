module FSharpPlus.Tests.Helpers

open Xunit
open System.Collections

let areEqual<'t> (x:'t) (y:'t) = Assert.Equal (x, y)
let areStEqual x y = Assert.True( (x = y), sprintf "Expected %A to be structurally equal to %A" x y)
let areEquivalent (x:IEnumerable) (y:IEnumerable) = Assert.Equal (x, y)
let require (x: bool) (msg: string) = Assert.True (x, msg)
module NUnit =
    module Framework =
        type CollectionAssert = class
            static member AreEqual<'t> (expected:'t seq, actual:'t seq) = Xunit.Assert.Equal<'t> (expected, actual) |> ignore
            static member AreNotEqual<'t> (expected:'t seq, actual:'t seq) = Xunit.Assert.NotEqual<'t> (expected, actual) |> ignore

        end
        type TestDelegate = System.Action

        type Assert() =
            static member AreEqual (expected:#obj, actual:#obj) = Xunit.Assert.Equal<obj> (box expected, box actual) |> ignore
            static member AreEqual (expected:#obj, actual:#obj, message:string) = Xunit.Assert.Equal<obj> (box expected, box actual) |> ignore
            static member Throws(exnT: System.Type, del:TestDelegate) = Xunit.Assert.Throws(exnT, del) |> ignore
            static member Throws<'t when 't :> exn>(del:TestDelegate) = Xunit.Assert.Throws<'t>(del) |> ignore
            static member IsInstanceOf<'t> (actual:'t) = Xunit.Assert.IsType<'t> ( actual) |> ignore
            static member IsTrue(actual:bool, message:string) = Xunit.Assert.True(actual, message)
            static member IsTrue(actual:bool) = Xunit.Assert.True(actual)
            static member True(actual:bool) = Xunit.Assert.True(actual)
            static member Ignore(message: string) = ()
            static member Pass() = ()

        type TestAttribute = FactAttribute
module SideEffects =
    let private effects = ResizeArray<string> []
    let reset () = effects.Clear ()
    let add x = effects.Add (x)
    let get () = effects |> Seq.toList
    let are lst = areEquivalent lst (get ())
