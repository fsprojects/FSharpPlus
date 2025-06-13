namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework
open Helpers

module Monads =
    
    [<Test>]
    let lazies () =
        let output = System.Text.StringBuilder ()
        let append (x: string) = output.Append x |> ignore

        let v5: Lazy<_> = lazy (append "5"; 5)
        Assert.AreEqual (0, output.Length)
        let fPlus10 x   = lazy (append " + 10"; x + 10)
        Assert.AreEqual (0, output.Length)
        let v5plus10    = v5 >>= fPlus10
        Assert.AreEqual (0, output.Length)
        let v15 = v5plus10.Force ()
        Assert.AreEqual ("5 + 10", string output)
        Assert.AreEqual (15, v15)

        output.Clear () |> ignore

        let v4ll: Lazy<_> = lazy (append "outer"; lazy (append "inner"; 4))
        Assert.AreEqual (0, output.Length)
        let v4l = join v4ll
        Assert.AreEqual (0, output.Length)
        let v4  = v4l.Force()
        Assert.AreEqual ("outerinner", string output)
        Assert.AreEqual (4, v4)

    [<Test>]
    let mapLikes () =
        let r1 = dict [ "a", 1; "b", 2 ; "c", 3 ] >>= fun x -> dict [ "a", x + 10; "c", x + 300; "d", x + 400 ]
        CollectionAssert.AreEqual (dict [ "a", 11; "c", 303 ], r1)

        let r2 = readOnlyDict [ "a", 1; "b", 2 ; "c", 3 ] >>= fun x -> readOnlyDict [ "a", x + 10; "c", x + 300; "d", x + 400 ]
        CollectionAssert.AreEqual (readOnlyDict [ "a", 11; "c", 303 ], r2)
