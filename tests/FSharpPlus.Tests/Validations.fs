module Validations

open System
open FSharpPlus
open FSharpPlus.Validations
open FsCheck
open NUnit.Framework
open FSharpPlus.Data

module FunctorP=
  [<Test>]
  let ``map id  =  id ``() =
    Check.Quick("map id = id",fun (x :AccValidation<string list, int>) ->
      (AccValidation.map id) x =  id x )

  [<Test>]
  let ``map (f << g) = map f << map g ``()=
    Check.Quick("map (f << g) = map f << map g", fun (x :AccValidation<string list, int>) (f:string->int) (g:int->string)->
      (AccValidation.map (f << g)) x = (AccValidation.map f << AccValidation.map g) x)

module BifunctorP=  
  [<Test>]
  let ``bimap f g = first f << second g``()=
     Check.Quick("bimap f g = first f << second g", fun (x :AccValidation<string, int>) (f:string->int) (g:int->string)->
       (bimap f g) x = (first f << second g) x)

module ApplicativeP=  
  ///The identity law
  [<Test>]
  let ``result id <*> v = v``() =
    Check.Quick("result id <*> v = v", fun (v :AccValidation<string list, int>)-> 
      result id <*> v = v)
  [<Test>]
  let ``result (<<) <*> u <*> v <*> w = u <*> (v <*> w)``()=
    Check.Quick("result (<<) <*> u <*> v <*> w = u <*> (v <*> w)", fun (v :AccValidation<string list, string->string>) (u :AccValidation<string list, string->string>) (w :AccValidation<string list, string>) ->
      (result (<<) <*> u <*> v <*> w) = (u <*> (v <*> w)))
  ///Homomorphism:
  [<Test>]
  let ``result f <*> result x = result (f x)``()=
    Check.Quick("result f <*> result x = result (f x)", fun (x :AccValidation<string list, int>) (f:AccValidation<string list, int> -> int) ->
      let y=(result (f x)):AccValidation<string list, int>
      y=(result f <*> result x) )
  /// Interchange
  /// in haskell: u <*> pure y = pure ($ y) <*> u
  [<Test>] 
  let ``u <*> result y = result ((|>) y) <*> u``()=
    Check.Quick("u <*> result y = result ((|>) y) <*> u", fun (u:AccValidation<string list, string->int>) (y:string) ->
      let right_side =result ((|>) y) <*> u
      let left_side = u <*> (result y)
      right_side=left_side)

module AlternativeP=  
  [<Test>]
  let ``empty <|> x = x``() =
    Check.Quick("empty <|> x = x", fun (x :AccValidation<string list, int>)-> 
      getEmpty() <|> x = x)
  
  [<Test>]
  let ``x <|> empty = x``() =
    Check.Quick("x <|> empty = x", fun (x :AccValidation<string list, int>)->
      x <|> getEmpty() = x)
  
  [<Test>]
  let ``(x <|> y) <|> z = x <|> (y <|> z)``()=
    Check.Quick("(x <|> y) <|> z = x <|> (y <|> z)",fun (x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (z :AccValidation<string list, int>)->
      ((x <|> y) <|> z) = (x <|> (y <|> z)))
  
  [<Test>]
  let ``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)``()=
    Check.Quick("f <!> (x <|> y) = (f <!> x) <|> (f <!> y)",fun (x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:int->string)->
      (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y)))
  
  //Right Distribution: does not hold
  //[<Test()>]
  let ``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``()=
    Check.Quick("(f <|> g) <*> x = (f <*> x) <|> (g <*> x)", fun (x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:AccValidation<string list,int->string>) (g:AccValidation<string list,int->string>)->
      ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x)))

  // holds when f is a function (success)
  [<Test>]
  let ``empty <*> f = empty ``(f:string->int)=
    let empty:AccValidation<string list,_>=getEmpty()
    (empty <*> (AccSuccess f))=getEmpty()

module TraversableP=

  //let y_1 =traverse (fun x -> [0..x]) (AccFailure [1])
(*
  [<Property>]
  let ``Result: t << traverse f = traverse (t << f) ``
    (x :Result<string list,string> ) (t :int->string list) (f:string list->int)=
    let t_f = (t << f)
    let right_side = x |> (Result.traverse (t << f))
    let left_side = x |> (t << Result.traverse f ) 
    left_side = right_side
*)
(*
  [<Property>]
  let ``t << traverse f = traverse (t << f) ``(x :AccValidation<string list, int>) (t :int->string) (f:string->int)=
    let right_side =((traverse (t << f) x))
    let left_side =(t << traverse f x)
    left_side = right_side
*)
  [<Test>]
  let ``traverse Identity = Identity``(x :AccValidation<int list, string>)=
    AccValidation.traverse (Identity) x = Identity x
(*
  [<Property>]
  let ``traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f``(x :AccValidation<int list, string>) (g :int list->string) (f:string->int list)=
    let y_1 = traverse (Compose << map (g << f))
    let y_2 = Compose << map (traverse g) << traverse f
    y_1 x= y_2 x
*)

//( # ) :: AReview t b -> b -> t
let seven = 7
let three = 3
let plusOne x = x + 1

[<Test>]
let testYY() =
  let subject:AccValidation<string,int>  = AccSuccess plusOne <*> AccSuccess seven
  let expected = AccSuccess 8
  Assert.AreEqual(expected, subject)
[<Test>]
let testNY() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccSuccess seven
  let expected = AccFailure ["f1"]
  Assert.AreEqual(expected, subject)
[<Test>]
let testYN() =
  let subject:AccValidation<string list,int>  = AccSuccess plusOne <*> AccFailure ["f2"] 
  let expected = AccFailure ["f2"]
  Assert.AreEqual(expected, subject)
[<Test>]
let testNN() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccFailure ["f2"] 
  let expected = AccFailure ["f1";"f2"]
  Assert.AreEqual(expected, subject)
(*
[<Fact>]
let testValidationNel() =
  let subject  = validation length (const' 0) $ validationNel (Error ())
  Assert.AreEqual(1, subject)
*)
let const' k _ = k
[<Test>]
let testEnsureLeftFalse () =
  let subject = ensure three (const' false) (AccFailure seven)
  Assert.AreEqual((AccFailure seven), subject)

[<Test>]
let testEnsureLeftTrue () =
  let subject = ensure three (const' true) (AccFailure seven)
  Assert.AreEqual((AccFailure seven), subject)

[<Test>]
let testEnsureRightFalse () =
  let subject = ensure three (const' false) (AccSuccess seven)
  Assert.AreEqual((AccFailure three), subject)

[<Test>]
let testEnsureRightTrue () =
  let subject = ensure three (const' true ) (AccSuccess seven)
  Assert.AreEqual((AccSuccess seven), subject)

[<Test>]
let testOrElseRight () =
  let v = AccSuccess  seven
  let subject = AccValidation.orElse v three
  Assert.AreEqual(seven, subject)

[<Test>]
let testOrElseLeft () =
  let v = AccFailure seven
  let subject = AccValidation.orElse v three
  Assert.AreEqual(three, subject)

//testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
//  testOrElseRight, testOrElseLeft
//  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


[<Test>]
let testValidateTrue ()=
  let subject = validate three (const' true) seven
  let expected = AccSuccess seven
  Assert.AreEqual(expected, subject)

[<Test>]
let testValidateFalse ()=
  let subject = validate three (const' false) seven
  let expected = AccFailure three
  Assert.AreEqual(expected, subject)

module Tests=
  //( # ) :: AReview t b -> b -> t
  let seven = 7
  let three = 3
  let plusOne x = x + 1

  [<Test>]
  let testYY() =
    let subject:AccValidation<string,int>  = AccSuccess plusOne <*> AccSuccess seven
    let expected = AccSuccess 8
    Assert.AreEqual(expected, subject)
  [<Test>]
  let testNY() =
    let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccSuccess seven
    let expected = AccFailure ["f1"]
    Assert.AreEqual(expected, subject)
  [<Test>]
  let testYN() =
    let subject:AccValidation<string list,int>  = AccSuccess plusOne <*> AccFailure ["f2"] 
    let expected = AccFailure ["f2"]
    Assert.AreEqual(expected, subject)
  [<Test>]
  let testNN() =
    let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccFailure ["f2"] 
    let expected = AccFailure ["f1";"f2"]
    Assert.AreEqual(expected, subject)
  (*
  [<Fact>]
  let testValidationNel() =
    let subject  = validation length (const' 0) $ validationNel (Error ())
    Assert.AreEqual(1, subject)
  *)
  [<Test>]
  let testEnsureLeftFalse () =
    let subject = ensure three (const' false) (AccFailure seven)
    Assert.AreEqual((AccFailure seven), subject)

  [<Test>]
  let testEnsureLeftTrue () =
    let subject = ensure three (const' true) (AccFailure seven)
    Assert.AreEqual((AccFailure seven), subject)

  [<Test>]
  let testEnsureRightFalse () =
    let subject = ensure three (const' false) (AccSuccess seven)
    Assert.AreEqual((AccFailure three), subject)

  [<Test>]
  let testEnsureRightTrue () =
    let subject = ensure three (const' true ) (AccSuccess seven)
    Assert.AreEqual((AccSuccess seven), subject)

  [<Test>]
  let testOrElseRight () =
    let v = AccSuccess  seven
    let subject = AccValidation.orElse v three
    Assert.AreEqual(seven, subject)

  [<Test>]
  let testOrElseLeft () =
    let v = AccFailure seven
    let subject = AccValidation.orElse v three
    Assert.AreEqual(three, subject)

  //testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  //  testOrElseRight, testOrElseLeft
  //  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


  [<Test>]
  let testValidateTrue ()=
    let subject = validate three (const' true) seven
    let expected = AccSuccess seven
    Assert.AreEqual(expected, subject)

  [<Test>]
  let testValidateFalse ()=
    let subject = validate three (const' false) seven
    let expected = AccFailure three
    Assert.AreEqual(expected, subject)

 