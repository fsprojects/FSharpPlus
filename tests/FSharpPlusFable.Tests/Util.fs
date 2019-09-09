module Testing


#if FABLE_COMPILER

    type TestKind =
    | TestList of string * TestKind seq
    | TestCase of (string*obj)

    open Fable.Core
    open Fable.Core.Testing

    let testList (name: string) (tests: TestKind seq) = TestList( name, tests )
    let testCase (msg: string) (test: unit->unit) = TestCase( msg, box test )


    //Assert.AreEqual does not provide value equality between two strings.
    //The following functions implement equality between sequences
    //This is adapted from Expecto

    let private firstDiff s1 s2 =
        let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
        let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
        Seq.mapi2 (fun i s p -> i,s,p) s1 s2
        |> Seq.find (function |_,Some s,Some p when s=p -> false |_-> true)

    let private sequenceEqual actual expected =
        match firstDiff actual expected with
        | _,None,None -> ()
        | i,Some a, Some e -> failwithf "Sequence does not match at position %i. Expected item: %A, but got %A. Actual %A : Expected %A" i e a actual expected
        | i,None, Some e -> failwithf "Sequence actual shorter than expected, at pos %i for expected item %A. Actual %A : Expected %A" i e actual expected
        | i,Some a, None -> failwithf "Sequence actual longer than expected, at pos %i found item %A. Actual %A : Expected %A" i a actual expected



    let equal expected actual: unit = Assert.AreEqual(actual, expected)
    let notEqual expected actual: unit = Assert.NotEqual(actual, expected)
    let equalSeq expected actual: unit = sequenceEqual expected actual

    let [<Global>] describe (name: string) (f: unit->unit) = jsNative
    let [<Global>] it (msg: string) (f: unit->unit) = jsNative


    let rec flattenTest (test:TestKind) : unit =
        match test with
        | TestList(name, tests) ->
            describe name (fun () ->
              for t in tests do
                flattenTest t)
        | TestCase (name, test) ->
            it name (unbox test)

#else 

    open Expecto

    let testCase (msg: string) test : Test = testCase msg test

    let testList (name: string) test : Test = testList name test

    let equal expected actual: unit = Expect.equal actual expected ""
    let equalSeq expected actual: unit = Expect.sequenceEqual actual expected ""
    let notEqual expected actual: unit = Expect.notEqual actual expected ""

#endif
