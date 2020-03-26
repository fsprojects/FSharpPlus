module FSharpPlus.One.Validations
open FSharpPlus.One.Usage
open System
open FSharpPlus
open FSharpPlus.Data
open FsCheck
open NUnit.Framework
open Validation
let areEqual x y = if x <> y then failwithf "Expected %A to equal %A" x y
[<AbstractClass>]
type FunctorP()=
  [<Test>]
  member __.``map id  =  id ``() =
    Check.Quick("map id = id",fun (x :Validation<string list, int>) ->
      (Validation.map id) x =  id x )

  [<Test>]
  member __.``map (f << g) = map f << map g ``()=
    Check.Quick("map (f << g) = map f << map g", fun (x :Validation<string list, int>) (f:string->int) (g:int->string)->
      (Validation.map (f << g)) x = (Validation.map f << Validation.map g) x)
[<AbstractClass>]
type BifunctorP()=  
  [<Test>]
  member __.``bimap f g = first f << second g``()=
     Check.Quick("bimap f g = first f << second g", fun (x :Validation<string, int>) (f:string->int) (g:int->string)->
       (bimap f g) x = (first f << second g) x)
[<AbstractClass>]
type ApplicativeP()=  
  ///The identity law
  [<Test>]
  member __.``result id <*> v = v``() =
    Check.Quick("result id <*> v = v", fun (v :Validation<string list, int>)-> 
      result id <*> v = v)
  [<Test>]
  member __.``result (<<) <*> u <*> v <*> w = u <*> (v <*> w)``()=
    Check.Quick("result (<<) <*> u <*> v <*> w = u <*> (v <*> w)", fun (v :Validation<string list, string->string>) (u :Validation<string list, string->string>) (w :Validation<string list, string>) ->
      (result (<<) <*> u <*> v <*> w) = (u <*> (v <*> w)))
  ///Homomorphism:
  [<Test>]
  member __.``result f <*> result x = result (f x)``()=
    Check.Quick("result f <*> result x = result (f x)", fun (x :Validation<string list, int>) (f:Validation<string list, int> -> int) ->
      let y=(result (f x)):Validation<string list, int>
      y=(result f <*> result x) )
  /// Interchange
  /// in haskell: u <*> pure y = pure ($ y) <*> u
  [<Test>] 
  member __.``u <*> result y = result ((|>) y) <*> u``()=
    Check.Quick("u <*> result y = result ((|>) y) <*> u", fun (u:Validation<string list, string->int>) (y:string) ->
      let right_side =result ((|>) y) <*> u
      let left_side = u <*> (result y)
      right_side=left_side)
[<AbstractClass>]
type AlternativeP()=  
  [<Test>]
  member __.``empty <|> x = x``() =
    Check.Quick("empty <|> x = x", fun (x :Validation<string list, int>)-> 
      getEmpty() <|> x = x)
  
  [<Test>]
  member __.``x <|> empty = x``() =
    Check.Quick("x <|> empty = x", fun (x :Validation<string list, int>)->
      x <|> getEmpty() = x)
  
  [<Test>]
  member __.``(x <|> y) <|> z = x <|> (y <|> z)``()=
    Check.Quick("(x <|> y) <|> z = x <|> (y <|> z)",fun (x :Validation<string list, int>) (y :Validation<string list, int>) (z :Validation<string list, int>)->
      ((x <|> y) <|> z) = (x <|> (y <|> z)))
  
  [<Test>]
  member __.``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)``()=
    Check.Quick("f <!> (x <|> y) = (f <!> x) <|> (f <!> y)",fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:int->string)->
      (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y)))
  
  //Right Distribution: does not hold
  //[<Test()>]
  member __.``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``()=
    Check.Quick("(f <|> g) <*> x = (f <*> x) <|> (g <*> x)", fun (x :Validation<string list, int>) (y :Validation<string list, int>) (f:Validation<string list,int->string>) (g:Validation<string list,int->string>)->
      ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x)))

  // holds when f is a function (success)
  [<Test>]
  member __.``empty <*> f = empty ``()=
    Check.Quick("empty <*> f = empty", fun (f:string->int)->
      let empty:Validation<string list,_>=getEmpty()
      (empty <*> (Success f))=getEmpty())
[<AbstractClass>]
type TraversableP()=

  [<Test>]
  member __.``traverse Identity = Identity``()=
    Check.Quick("traverse Identity = Identity", fun (x :Validation<int list, string>)->
      Validation.traverse (Identity) x = Identity x)
let const' k _ = k
[<AbstractClass>]
type Base()=
    //( # ) :: AReview t b -> b -> t
    let seven = 7
    let three = 3
    let plusOne x = x + 1

    [<Test>]
    member __.testYY() =
      let subject:Validation<string,int>  = Success plusOne <*> Success seven
      let expected = Success 8
      areEqual expected subject
    [<Test>]
    member __.testNY() =
      let subject:Validation<string list,int>  = Failure ["f1"] <*> Success seven
      let expected = Failure ["f1"]
      areEqual expected subject
    [<Test>]
    member __.testYN() =
      let subject:Validation<string list,int>  = Success plusOne <*> Failure ["f2"] 
      let expected = Failure ["f2"]
      areEqual expected subject
    [<Test>]
    member __.testNN() =
      let subject:Validation<string list,int>  = Failure ["f1"] <*> Failure ["f2"] 
      let expected = Failure ["f1";"f2"]
      areEqual expected subject
    [<Test>]
    member __.testEnsureLeftFalse () =
      let subject = ensure three (const' false) (Failure seven)
      areEqual (Failure seven) subject

    [<Test>]
    member __.testEnsureLeftTrue () =
      let subject = ensure three (const' true) (Failure seven)
      areEqual (Failure seven) subject

    [<Test>]
    member __.testEnsureRightFalse () =
      let subject = ensure three (const' false) (Success seven)
      areEqual (Failure three) subject

    [<Test>]
    member __.testEnsureRightTrue () =
      let subject = ensure three (const' true ) (Success seven)
      areEqual (Success seven) subject

    [<Test>]
    member __.testOrElseRight () =
      let v = Success  seven
      let subject = Validation.orElse v three
      areEqual seven subject

    [<Test>]
    member __.testOrElseLeft () =
      let v = Failure seven
      let subject = Validation.orElse v three
      areEqual three subject

    [<Test>]
    member __.testValidateTrue ()=
      let subject = validate three (const' true) seven
      let expected = Success seven
      areEqual expected subject

    [<Test>]
    member __.testValidateFalse ()=
      let subject = validate three (const' false) seven
      let expected = Failure three
      areEqual expected subject

[<AbstractClass>]
type Tests()=
  //( # ) :: AReview t b -> b -> t
  let seven = 7
  let three = 3
  let plusOne x = x + 1

  [<Test>]
  member __.testYY() =
    let subject:Validation<string,int>  = Success plusOne <*> Success seven
    let expected = Success 8
    areEqual expected subject
  [<Test>]
  member __.testNY() =
    let subject:Validation<string list,int>  = Failure ["f1"] <*> Success seven
    let expected = Failure ["f1"]
    areEqual expected subject
  [<Test>]
  member __.testYN() =
    let subject:Validation<string list,int>  = Success plusOne <*> Failure ["f2"] 
    let expected = Failure ["f2"]
    areEqual expected subject
  [<Test>]
  member __.testNN() =
    let subject:Validation<string list,int>  = Failure ["f1"] <*> Failure ["f2"] 
    let expected = Failure ["f1";"f2"]
    areEqual expected subject

  [<Test>]
  member __.testEnsureLeftFalse () =
    let subject = ensure three (const' false) (Failure seven)
    areEqual (Failure seven) subject

  [<Test>]
  member __.testEnsureLeftTrue () =
    let subject = ensure three (const' true) (Failure seven)
    areEqual (Failure seven) subject

  [<Test>]
  member __.testEnsureRightFalse () =
    let subject = ensure three (const' false) (Success seven)
    areEqual (Failure three) subject

  [<Test>]
  member __.testEnsureRightTrue () =
    let subject = ensure three (const' true ) (Success seven)
    areEqual (Success seven) subject

  [<Test>]
  member __.testOrElseRight () =
    let v = Success  seven
    let subject = Validation.orElse v three
    areEqual seven subject

  [<Test>]
  member __.testOrElseLeft () =
    let v = Failure seven
    let subject = Validation.orElse v three
    areEqual three subject

  //testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  //  testOrElseRight, testOrElseLeft
  //  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


  [<Test>]
  member __.testValidateTrue ()=
    let subject = validate three (const' true) seven
    let expected = Success seven
    areEqual expected subject

  [<Test>]
  member __.testValidateFalse ()=
    let subject = validate three (const' false) seven
    let expected = Failure three
    areEqual expected subject

 