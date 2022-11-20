namespace FSharpPlus.Tests

#nowarn "44"

module Validation =

  open System
  open FsCheck
  open NUnit.Framework
  open FSharpPlus
  open FSharpPlus.Data
  open Validation
  open FSharpPlus.Tests.Helpers
    
  let private getSuccess =
    function
    | Success s -> s
    | Failure _ -> failwith "It's a failure"
  
  let private getFailure =
    function
    | Success _ -> failwith "It's a Success"
    | Failure f -> f   
  
  let fsCheck s x = Check.One({Config.QuickThrowOnFailure with Name = s}, x)
  module FunctorP =
    [<Test>]
    let ``map id = id `` () =
      fsCheck 
        "map id = id" 
        (fun (x :Validation<string list, int>) -> 
          (Validation.map id) x = id x)

    [<Test>]
    let ``map (f << g) = map f << map g `` () =
      fsCheck 
        "map (f << g) = map f << map g" 
        (fun (x :Validation<string list, int>) (f:string->int) (g:int->string) ->
          (Validation.map (f << g)) x = (Validation.map f << Validation.map g) x)

  module BifunctorP =
    [<Test>]
    let ``bimap f g = first f << second g`` () =
      fsCheck 
        "bimap f g = first f << second g"
        (fun (x :Validation<string, int>) (f:string->int) (g:int->string) ->
          (bimap f g) x = (first f << second g) x)

  module ApplicativeP =

    ///The identity law
    [<Test>]
    let ``result id <*> v = v`` () =
      fsCheck
        "result id <*> v = v"
        (fun (v :Validation<string list, int>) ->
          result id <*> v = v)

    [<Test>]
    let ``result (<<) <*> u <*> v <*> w = u <*> (v <*> w)`` () =
      fsCheck
        "result (<<) <*> u <*> v <*> w = u <*> (v <*> w)"
        (fun (v :Validation<string list, string->string>) (u :Validation<string list, string->string>) (w :Validation<string list, string>) ->
          (result (<<) <*> u <*> v <*> w) = (u <*> (v <*> w)))


    ///Homomorphism:
    [<Test>]
    let ``result f <*> result x = result (f x)`` () =
      fsCheck 
        "result f <*> result x = result (f x)"
        (fun (x :Validation<string list, int>) (f:Validation<string list, int> -> int) ->
          let y:Validation<string list, int> = result (f x)
          y = (result f <*> result x))

    /// Interchange
    /// in haskell: u <*> pure y = pure ($ y) <*> u
    [<Test>] 
    let ``u <*> result y = result ((|>) y) <*> u`` () =
      fsCheck 
        "u <*> result y = result ((|>) y) <*> u"
        (fun (u:Validation<string list, string->int>) (y:string) ->
          let right_side =result ((|>) y) <*> u
          let left_side = u <*> (result y)
          right_side = left_side)

  module AlternativeP =
    [<Test>]
    let ``empty <|> x = x`` () =
      fsCheck "empty <|> x = x" 
        (fun (x :Validation<string list, int>) ->
          getEmpty() <|> x = x)

    [<Test>]
    let ``x <|> empty = x`` () =
      fsCheck "x <|> empty = x" 
        (fun (x :Validation<string list, int>) ->
          x <|> getEmpty() = x)

    [<Test>]
    let ``(x <|> y) <|> z = x <|> (y <|> z)`` () =
      fsCheck "(x <|> y) <|> z = x <|> (y <|> z)" 
        (fun (x :Validation<string list, int>) (y :Validation<string list, int>) (z :Validation<string list, int>) ->
          ((x <|> y) <|> z) = (x <|> (y <|> z)))

    [<Test>]
    let ``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)`` () =
      fsCheck "f <!> (x <|> y) = (f <!> x) <|> (f <!> y)" 
        (fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:int->string)->
          (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y)))

    //Right Distribution: does not hold
    //[<Test()>]
    let ``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)`` () =
      fsCheck "(f <|> g) <*> x = (f <*> x) <|> (g <*> x)"
        (fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:Validation<string list,int->string>) (g:Validation<string list,int->string>) ->
          ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x)))

    // holds when f is a function (success)
    [<Test>]
    let ``empty <*> f = empty `` () =
      fsCheck "empty <*> f = empty" 
        (fun (f:string->int) ->
          let e: Validation<string list,_> = empty
          (e <*> Success f) = empty)

  module TraversableP =

    //let y_1 =traverse (fun x -> [0..x]) (Failure [1])
  (*
    [<Property>]
    let ``Result: t << traverse f = traverse (t << f) ``
      (x :Result<string list,string> ) (t :int->string list) (f:string list->int) =
      let t_f = (t << f)
      let right_side = x |> (Result.traverse (t << f))
      let left_side = x |> (t << Result.traverse f ) 
      left_side = right_side
  *)
  (*
    [<Property>]
    let ``t << traverse f = traverse (t << f) ``(x :Validation<string list, int>) (t :int->string) (f:string->int) =
      let right_side =((traverse (t << f) x))
      let left_side =(t << traverse f x)
      left_side = right_side
  *)
    [<Test>]
    let ``traverse Identity = Identity`` () =
      fsCheck "traverse Identity = Identity"
        (fun (x :Validation<int list, string>) ->
          Validation.traverse Identity x = Identity x)
  (*
    [<Property>]
    let ``traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f``(x :Validation<int list, string>) (g :int list->string) (f:string->int list) =
      let y_1 = traverse (Compose << map (g << f))
      let y_2 = Compose << map (traverse g) << traverse f
      y_1 x= y_2 x
  *)

  //( # ) :: AReview t b -> b -> t
  let seven = 7
  let three = 3
  let plusOne x = x + 1

  [<Test>]
  let testYY () =
    let subject: Validation<string,int> = Success plusOne <*> Success seven
    let expected = Success 8
    areStEqual expected subject

  [<Test>]
  let testNY () =
    let subject: Validation<string list,int> = Failure ["f1"] <*> Success seven
    let expected = Failure ["f1"]
    areStEqual expected subject

  [<Test>]
  let testYN () =
    let subject: Validation<string list,int> = Success plusOne <*> Failure ["f2"] 
    let expected = Failure ["f2"]
    areStEqual expected subject

  [<Test>]
  let testNN () =
    let subject: Validation<string list,int> = Failure ["f1"] <*> Failure ["f2"] 
    let expected = Failure ["f1";"f2"]
    areStEqual expected subject
  (*
  [<Fact>]
  let testValidationNel() =
    let subject = validation length (const' 0) $ validationNel (Error ())
    Assert.AreEqual(1, subject)
  *)

  [<Test>]
  let testEnsureLeftFalse () =
    let subject = ensure three (konst false) (Failure seven)
    areStEqual (Failure seven) subject

  [<Test>]
  let testEnsureLeftTrue () =
    let subject = ensure three (konst true) (Failure seven)
    areStEqual (Failure seven) subject

  [<Test>]
  let testEnsureRightFalse () =
    let subject = ensure three (konst false) (Success seven)
    areStEqual (Failure three) subject

  [<Test>]
  let testEnsureRightTrue () =
    let subject = ensure three (konst true ) (Success seven)
    areStEqual (Success seven) subject

  [<Test>]
  let testDefaultValueSuccess () =
    let v = Success seven
    let subject = Validation.defaultValue three v
    areStEqual seven subject

  [<Test>]
  let testDefaultValueFailure () =
    let v = Failure seven
    let subject = Validation.defaultValue three v
    areStEqual three subject

  //testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  //  testOrElseRight, testOrElseLeft
  //  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


  [<Test>]
  let testValidateTrue () =
    let subject = validate three (konst true) seven
    let expected = Success seven
    areStEqual expected subject

  [<Test>]
  let testValidateFalse () =
    let subject = validate three (konst false) seven
    let expected = Failure three
    areStEqual expected subject

  module Tests=
    //( # ) :: AReview t b -> b -> t
    let seven = 7
    let three = 3
    let plusOne x = x + 1
  
    [<Test>]
    let testYY() =
      let subject:Validation<string,int> = Success plusOne <*> Success seven
      let expected = Success 8
      areStEqual expected subject
    [<Test>]
    let testNY() =
      let subject:Validation<string list,int> = Failure ["f1"] <*> Success seven
      let expected = Failure ["f1"]
      areStEqual expected subject
    [<Test>]
    let testYN() =
      let subject:Validation<string list,int> = Success plusOne <*> Failure ["f2"] 
      let expected = Failure ["f2"]
      areStEqual expected subject
    [<Test>]
    let testNN() =
      let subject:Validation<string list,int> = Failure ["f1"] <*> Failure ["f2"] 
      let expected = Failure ["f1";"f2"]
      areStEqual expected subject
    (*
    [<Fact>]
    let testValidationNel() =
      let subject = validation length (const' 0) $ validationNel (Error ())
      Assert.AreEqual(1, subject)
    *)
    [<Test>]
    let testEnsureLeftFalse () =
      let subject = ensure three (konst false) (Failure seven)
      areStEqual (Failure seven) subject

    [<Test>]
    let testEnsureLeftTrue () =
      let subject = ensure three (konst true) (Failure seven)
      areStEqual (Failure seven) subject

    [<Test>]
    let testEnsureRightFalse () =
      let subject = ensure three (konst false) (Success seven)
      areStEqual (Failure three) subject

    [<Test>]
    let testEnsureRightTrue () =
      let subject = ensure three (konst true ) (Success seven)
      areStEqual (Success seven) subject

    [<Test>]
    let testOrElseRight () =
      let v = Success seven
      let subject = Validation.orElse v three
      areStEqual seven subject

    [<Test>]
    let testOrElseLeft () =
      let v = Failure seven
      let subject = Validation.orElse v three
      areStEqual three subject

    //testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
    //  testOrElseRight, testOrElseLeft
    //  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test
  
  
    [<Test>]
    let testValidateTrue () =
      let subject = validate three (konst true) seven
      let expected = Success seven
      areStEqual expected subject

    [<Test>]
    let testValidateFalse () =
      let subject = validate three (konst false) seven
      let expected = Failure three
      areStEqual expected subject

    [<Test>]
    let testValidateWithExceptions () = 
      let subject = 
          fun a b c d -> a + b + c + d
          <!> Failure (exn "Failure receiving first parameter") 
          <*> Success 4 
          <*> Failure (exn "Failure receiving third parameter") 
          <*> Failure (exn "Failure receiving last parameter")
      let expected : Validation<exn,int> = Failure ((new AggregateException ([exn "Failure receiving first parameter"; exn "Failure receiving third parameter"; exn "Failure receiving last parameter"])) :> exn)
      let f = function
          | Success _ -> failwith "unexpected"
          | Failure (e: exn) -> (e :?> AggregateException).InnerExceptions |> Seq.map (fun x -> x.Message) |> toList
      areStEqual (f subject) (f expected)

    [<Test>]
    let testValidateSequence () =
      let v: Validation<string, int Async> = Success (async {return 42})
      let r = Validation.sequence v
      let subject = Async.RunSynchronously r
      areStEqual subject (Success 42)

    [<Test>]
    let testValidateBisequence () =
      let v: Validation<string Async, int Async> = Success (async {return 42})
      let r = Validation.bisequence v
      let subject = Async.RunSynchronously r
      areStEqual subject (Success 42)
    
    [<Test>]
    [<TestCase("", false)>]
    [<TestCase("   ", false)>]
    [<TestCase(null, false)>]
    [<TestCase("NotEmpty", true)>]
    let testValidateRequireString (str, success) =
      let error = konst "Str"
      let r = RequiredValidation.string error str
      areStEqual (RequiredValidation.isSuccess r) success
      
      if not success then
        let failure = getFailure r
        areStEqual failure.Length 1
        areStEqual failure.[0] (error "")
      else
        ()
        
    [<Test>]
    [<TestCase(1, 0, true)>]
    [<TestCase(0, 0, false)>]
    [<TestCase(-1, 0, false)>]
    let testValidateRequireGreaterThan (value, limit, success) =
      let error = konst "Int"
      let r = RequiredValidation.greaterThan error limit value
      areStEqual (RequiredValidation.isSuccess r) success
      
      if not success then
        let failure = getFailure r
        areStEqual failure.Length 1
        areStEqual failure.[0] (error 1)
      else
        ()
        
    [<Test>]
    [<TestCase(1, 0, true)>]
    [<TestCase(0, 0, true)>]
    [<TestCase(-1, 0, false)>]
    let testValidateRequireGreaterOrEqualThan (value, limit, success) =
      let error = konst "Int"
      let r = RequiredValidation.greaterOrEqualThan error limit value
      areStEqual (RequiredValidation.isSuccess r) success
      
      if not success then
        let failure = getFailure r
        areStEqual failure.Length 1
        areStEqual failure.[0] (error 1)
      else
        ()