module Testing


    open Fuchu

    let testCase (msg: string) test : Test = testCase msg test
    let testList (name: string) (test:Test seq) : Test = testList name test

    let equal expected actual: unit = Assert.Equal ("",expected, actual)
    let notEqual expected actual: unit = Assert.NotEqual ("",expected,actual)

    let equalSeq (expected: _ seq) (actual: _ seq): unit = Assert.Equal ("", Seq.toList expected, Seq.toList actual)
    let equalMap expected actual: unit = equalSeq (Map.toSeq expected) (Map.toSeq actual)

    module SideEffects =
        let private effects = ResizeArray<string> []
        let reset () = effects.Clear ()
        let add x = effects.Add (x)
        let get () = effects |> Seq.toList
        let are lst = equalSeq lst (get ())

    type Assert=
        static member inline IsInstanceOf<'t> ( o:obj ) = if o :? 't then () else failwithf "Expected %O to be of type %O" o typeof<'t>
        static member AreEqual (expected:obj, actual:obj) = Assert.Equal ("",expected, actual)
        static member IsTrue (actual) = Assert.Equal ("", actual, true)
        static member IsEmpty (actual: _ seq) = if not <| Seq.isEmpty actual then failwithf "Expected sequence %A to be empty" actual