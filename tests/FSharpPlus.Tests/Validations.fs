module Validations

open System
open FSharpPlus
open FSharpPlus.Data
open FsCheck
open NUnit.Framework
open FSharpPlus.Data

open Validation
let areEqual x y = Assert.IsTrue( (x = y), sprintf "Expected %A to equal %A" x y)
module FunctorP=
  [<Test>]
  let ``map id  =  id ``() =
    Check.Quick("map id = id",fun (x :Validation<string list, int>) ->
      (Validation.map id) x =  id x )

  [<Test>]
  let ``map (f << g) = map f << map g ``()=
    Check.Quick("map (f << g) = map f << map g", fun (x :Validation<string list, int>) (f:string->int) (g:int->string)->
      (Validation.map (f << g)) x = (Validation.map f << Validation.map g) x)

module BifunctorP=  
  [<Test>]
  let ``bimap f g = first f << second g``()=
     Check.Quick("bimap f g = first f << second g", fun (x :Validation<string, int>) (f:string->int) (g:int->string)->
       (bimap f g) x = (first f << second g) x)

module ApplicativeP=  
  ///The identity law
  [<Test>]
  let ``result id <*> v = v``() =
    Check.Quick("result id <*> v = v", fun (v :Validation<string list, int>)-> 
      result id <*> v = v)
  [<Test>]
  let ``result (<<) <*> u <*> v <*> w = u <*> (v <*> w)``()=
    Check.Quick("result (<<) <*> u <*> v <*> w = u <*> (v <*> w)", fun (v :Validation<string list, string->string>) (u :Validation<string list, string->string>) (w :Validation<string list, string>) ->
      (result (<<) <*> u <*> v <*> w) = (u <*> (v <*> w)))
  ///Homomorphism:
  [<Test>]
  let ``result f <*> result x = result (f x)``()=
    Check.Quick("result f <*> result x = result (f x)", fun (x :Validation<string list, int>) (f:Validation<string list, int> -> int) ->
      let y=(result (f x)):Validation<string list, int>
      y=(result f <*> result x) )
  /// Interchange
  /// in haskell: u <*> pure y = pure ($ y) <*> u
  [<Test>] 
  let ``u <*> result y = result ((|>) y) <*> u``()=
    Check.Quick("u <*> result y = result ((|>) y) <*> u", fun (u:Validation<string list, string->int>) (y:string) ->
      let right_side =result ((|>) y) <*> u
      let left_side = u <*> (result y)
      right_side=left_side)

module AlternativeP=  
  [<Test>]
  let ``empty <|> x = x``() =
    Check.Quick("empty <|> x = x", fun (x :Validation<string list, int>)-> 
      getEmpty() <|> x = x)
  
  [<Test>]
  let ``x <|> empty = x``() =
    Check.Quick("x <|> empty = x", fun (x :Validation<string list, int>)->
      x <|> getEmpty() = x)
  
  [<Test>]
  let ``(x <|> y) <|> z = x <|> (y <|> z)``()=
    Check.Quick("(x <|> y) <|> z = x <|> (y <|> z)",fun (x :Validation<string list, int>) (y :Validation<string list, int>) (z :Validation<string list, int>)->
      ((x <|> y) <|> z) = (x <|> (y <|> z)))
  
  [<Test>]
  let ``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)``()=
    Check.Quick("f <!> (x <|> y) = (f <!> x) <|> (f <!> y)",fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:int->string)->
      (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y)))
  
  //Right Distribution: does not hold
  //[<Test()>]
  let ``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``()=
    Check.Quick("(f <|> g) <*> x = (f <*> x) <|> (g <*> x)", fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:Validation<string list,int->string>) (g:Validation<string list,int->string>)->
      ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x)))

  // holds when f is a function (success)
  [<Test>]
  let ``empty <*> f = empty ``()=
    Check.Quick("empty <*> f = empty", fun (f:string->int)->
      let empty:Validation<string list,_>=getEmpty()
      (empty <*> (Success f))=getEmpty())

module TraversableP=

  //let y_1 =traverse (fun x -> [0..x]) (Failure [1])
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
  let ``t << traverse f = traverse (t << f) ``(x :Validation<string list, int>) (t :int->string) (f:string->int)=
    let right_side =((traverse (t << f) x))
    let left_side =(t << traverse f x)
    left_side = right_side
*)
  [<Test>]
  let ``traverse Identity = Identity``()=
    Check.Quick("traverse Identity = Identity", fun (x :Validation<int list, string>)->
      Validation.traverse (Identity) x = Identity x)
(*
  [<Property>]
  let ``traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f``(x :Validation<int list, string>) (g :int list->string) (f:string->int list)=
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
  let subject:Validation<string,int>  = Success plusOne <*> Success seven
  let expected = Success 8
  areEqual expected subject
[<Test>]
let testNY() =
  let subject:Validation<string list,int>  = Failure ["f1"] <*> Success seven
  let expected = Failure ["f1"]
  areEqual expected subject
[<Test>]
let testYN() =
  let subject:Validation<string list,int>  = Success plusOne <*> Failure ["f2"] 
  let expected = Failure ["f2"]
  areEqual expected subject
[<Test>]
let testNN() =
  let subject:Validation<string list,int>  = Failure ["f1"] <*> Failure ["f2"] 
  let expected = Failure ["f1";"f2"]
  areEqual expected subject
(*
[<Fact>]
let testValidationNel() =
  let subject  = validation length (const' 0) $ validationNel (Error ())
  Assert.AreEqual(1, subject)
*)
let const' k _ = k
[<Test>]
let testEnsureLeftFalse () =
  let subject = ensure three (const' false) (Failure seven)
  areEqual (Failure seven) subject

[<Test>]
let testEnsureLeftTrue () =
  let subject = ensure three (const' true) (Failure seven)
  areEqual (Failure seven) subject

[<Test>]
let testEnsureRightFalse () =
  let subject = ensure three (const' false) (Success seven)
  areEqual (Failure three) subject

[<Test>]
let testEnsureRightTrue () =
  let subject = ensure three (const' true ) (Success seven)
  areEqual (Success seven) subject

[<Test>]
let testOrElseRight () =
  let v = Success  seven
  let subject = Validation.orElse v three
  areEqual seven subject

[<Test>]
let testOrElseLeft () =
  let v = Failure seven
  let subject = Validation.orElse v three
  areEqual three subject

//testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
//  testOrElseRight, testOrElseLeft
//  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


[<Test>]
let testValidateTrue ()=
  let subject = validate three (const' true) seven
  let expected = Success seven
  areEqual expected subject

[<Test>]
let testValidateFalse ()=
  let subject = validate three (const' false) seven
  let expected = Failure three
  areEqual expected subject

module Tests=
  //( # ) :: AReview t b -> b -> t
  let seven = 7
  let three = 3
  let plusOne x = x + 1

  [<Test>]
  let testYY() =
    let subject:Validation<string,int>  = Success plusOne <*> Success seven
    let expected = Success 8
    areEqual expected subject
  [<Test>]
  let testNY() =
    let subject:Validation<string list,int>  = Failure ["f1"] <*> Success seven
    let expected = Failure ["f1"]
    areEqual expected subject
  [<Test>]
  let testYN() =
    let subject:Validation<string list,int>  = Success plusOne <*> Failure ["f2"] 
    let expected = Failure ["f2"]
    areEqual expected subject
  [<Test>]
  let testNN() =
    let subject:Validation<string list,int>  = Failure ["f1"] <*> Failure ["f2"] 
    let expected = Failure ["f1";"f2"]
    areEqual expected subject
  (*
  [<Fact>]
  let testValidationNel() =
    let subject  = validation length (const' 0) $ validationNel (Error ())
    Assert.AreEqual(1, subject)
  *)
  [<Test>]
  let testEnsureLeftFalse () =
    let subject = ensure three (const' false) (Failure seven)
    areEqual (Failure seven) subject

  [<Test>]
  let testEnsureLeftTrue () =
    let subject = ensure three (const' true) (Failure seven)
    areEqual (Failure seven) subject

  [<Test>]
  let testEnsureRightFalse () =
    let subject = ensure three (const' false) (Success seven)
    areEqual (Failure three) subject

  [<Test>]
  let testEnsureRightTrue () =
    let subject = ensure three (const' true ) (Success seven)
    areEqual (Success seven) subject

  [<Test>]
  let testOrElseRight () =
    let v = Success  seven
    let subject = Validation.orElse v three
    areEqual seven subject

  [<Test>]
  let testOrElseLeft () =
    let v = Failure seven
    let subject = Validation.orElse v three
    areEqual three subject

  //testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  //  testOrElseRight, testOrElseLeft
  //  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


  [<Test>]
  let testValidateTrue ()=
    let subject = validate three (const' true) seven
    let expected = Success seven
    areEqual expected subject

  [<Test>]
  let testValidateFalse ()=
    let subject = validate three (const' false) seven
    let expected = Failure three
    areEqual expected subject

 