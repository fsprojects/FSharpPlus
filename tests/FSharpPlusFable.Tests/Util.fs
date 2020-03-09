module Testing


#if FABLE_COMPILER

    type TestKind =
    | TestList of string * TestKind seq
    | TestCase of (string * obj)

    open Fable.Core
    open Fable.Core.Testing
    open Fable.Core.JsInterop
    open Fable.Import
    open Fetch

    let testList (name: string) (tests: TestKind seq) = TestList( name, tests )
    let testCase (msg: string) (test: unit->unit) = TestCase( msg, box test )

    let equal expected actual: unit = Assert.AreEqual(actual, expected)
    let notEqual expected actual: unit = Assert.NotEqual(actual, expected)
    let deepEqual expected actual: unit = import "deepEqual" "./deepEqual.js"

    let equalSeq expected actual: unit = equal (Seq.toList expected) (Seq.toList actual)
    let equalMap expected actual: unit = equal (Map.toList expected) (Map.toList actual)


    //Code required to run the tests
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
    let notEqual expected actual: unit = Expect.notEqual actual expected ""

    let equalSeq expected actual: unit = Expect.sequenceEqual actual expected ""
    let equalMap expected actual: unit = equalSeq (Map.toSeq expected) (Map.toSeq actual)

#endif
