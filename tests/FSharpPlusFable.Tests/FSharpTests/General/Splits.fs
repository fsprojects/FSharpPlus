module General.Splits
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"
#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

type Sum<'a> = Sum of 'a with
    static member inline get_Zero () = Sum 0
    static member inline (+) (Sum (x:'n), Sum (y:'n)) = Sum (x + y)


let splits = testList "Splits" [

    #if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)
    testCase "splitArraysAndStrings" (fun () ->
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|]

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|]
        
        Assert.IsTrue ((toList a1 = ["this.isABa.t"; "est"]))
        Assert.IsTrue ((toList a2 = [[|116uy; 104uy; 105uy; 115uy; 46uy; 105uy; 115uy; 65uy; 66uy; 97uy; 46uy; 116uy|]; [|101uy; 115uy; 116uy|]]))
        Assert.IsTrue ((toList b1 = ["this"; "is"; "a"; "t"; ""; ""; "est"]))
        Assert.IsTrue ((toList b2 = [[|116uy; 104uy; 105uy; 115uy|]; [|105uy; 115uy|]; [|97uy|]; [|116uy|]; [||]; [||]; [|101uy; 115uy; 116uy|]]))

        #if !FABLE_COMPILER
        Assert.IsInstanceOf<Option<string []>> (Some a1)
        #endif
        )

    testCase "replaceArraysAndStrings" (fun () -> 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B

        Assert.IsTrue ((a1 = "this.isABa.tABCest"))
        Assert.IsTrue ((a2 = [|116uy; 104uy; 105uy; 115uy; 46uy; 105uy; 115uy; 65uy; 66uy; 97uy; 46uy; 116uy; 65uy; 66uy; 67uy; 101uy; 115uy; 116uy|]))
        Assert.IsTrue ((b1 = "this...is...a...t.........est"))
        Assert.IsTrue ((b2 = [|116uy; 104uy; 105uy; 115uy; 46uy; 46uy; 46uy; 105uy; 115uy; 46uy; 46uy; 46uy; 97uy; 46uy; 46uy; 46uy; 116uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 101uy; 115uy; 116uy|]))
        )

    testCase "intercalateArraysAndStrings" (fun () -> 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B

        //let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        // /Control/Monoid.fs(..): (..) error FABLE: Cannot resolve trait call + - Inline call from ./Collection.fs(..) < ../../../tests/FSharpPlusFable.Tests/FSharpTests/General.fs(..)
        #if !FABLE_COMPILER
        let _c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        let d  = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)
        let _e = intercalate 10 (seq [1; 2; 3])
        #endif

        Assert.IsTrue((a1 = "this is a test"))
        Assert.IsTrue((a2 = [|116uy; 104uy; 105uy; 115uy; 32uy; 105uy; 115uy; 32uy; 97uy; 32uy; 116uy; 101uy; 115uy; 116uy|]))
        //Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        #if !FABLE_COMPILER
        Assert.IsTrue((d = Sum 13))
        #endif
        )
    #endif

    ]