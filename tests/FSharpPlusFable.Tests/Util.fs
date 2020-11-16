module Testing


    open Fuchu

    let testCase (msg: string) test : Test = testCase msg test
    let testList (name: string) test : Test = testList name test

    let equal expected actual: unit = Assert.Equal ("",expected, actual) 
    let notEqual expected actual: unit = Assert.NotEqual ("",expected,actual)

    let equalSeq expected actual: unit = Assert.Equal ("",expected,actual)
    let equalMap expected actual: unit = equalSeq (Map.toSeq expected) (Map.toSeq actual)

