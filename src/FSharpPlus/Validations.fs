module FSharpPlus.Validations

open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data

/// An 'AccValidation' is either a value of the type 'err or 'a, similar to 'Result'. However,
/// the 'Applicative' instance for 'AccValidation' /accumulates/ errors using a 'Semigroup' on 'err.
/// In contrast, the Applicative for 'Result' returns only the first error.
///
/// A consequence of this is that 'AccValidation' is not a monad. There is no F#+ 'Bind' method since
/// that would violate monad rules.
///
/// An example of typical usage can be found <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs here>.
///
type AccValidation<'err,'a> =
  | AccFailure of 'err
  | AccSuccess of 'a

module AccValidation=
  let inline map (f:'T->'U)=function
    |AccFailure e -> AccFailure e
    |AccSuccess a -> AccSuccess (f a) 
  let inline apply e1' e2' = 
    match e1',e2' with
    |AccFailure e1, AccFailure e2 -> AccFailure (plus e1 e2)
    |AccFailure e1, AccSuccess _  -> AccFailure e1
    |AccSuccess _, AccFailure e2 -> AccFailure e2
    |AccSuccess f, AccSuccess a -> AccSuccess (f a)
  let inline foldr f x = function 
    |AccSuccess a -> f a x
    |AccFailure _ -> x

  let inline traverse f = function 
    |AccSuccess a -> AccSuccess <!> f a
    |AccFailure e -> result (AccFailure e)

  let inline bimap f g = function
    |AccFailure e -> AccFailure (f e)
    |AccSuccess a -> AccSuccess (g a)

  let inline bifoldr f g x = function 
    |AccSuccess a -> g a x
    |AccFailure e -> f e x

  let inline bitraverse f g = function 
    | AccSuccess a -> AccSuccess <!> g a
    | AccFailure e -> AccFailure <!> f e

  /// 'bind' binds through an AccValidation, which is useful for
  /// composing AccValidations sequentially. Note that despite having a bind
  /// function of the correct type, AccValidation is not a monad.
  /// The reason is, this bind does not accumulate errors, so it does not
  /// agree with the Applicative instance.
  ///
  /// There is nothing wrong with using this function, it just does not make a
  /// valid Monad instance.
  let inline bind (f:'T->AccValidation<_,_>) x :AccValidation<_,_>=
      match x with 
      | AccFailure e -> AccFailure e
      | AccSuccess a -> f a

  /// orElse v a returns 'a when v is AccFailure, and the a in AccSuccess a.
  let inline orElse v (a:'a) = 
      match v with
      |AccFailure _ -> a
      |AccSuccess x -> x
  /// Return the 'a or run the given function over the 'e.
  let valueOr ea (v:AccValidation<'e,'a>) = 
    match v with
    |AccFailure e -> ea e
    |AccSuccess a -> a
  /// 'liftResult' is useful for converting a 'Result' to an 'AccValidation'
  /// when the 'Error' of the 'Result' needs to be lifted into a 'Semigroup'.
  let liftResult (f:('b -> 'e)) : (Result<'a,'b>->AccValidation<'e,'a>) = function | Error e-> AccFailure (f e) | Ok a-> AccSuccess a
  /// 'liftChoice' is useful for converting a 'Choice' to an 'AccValidation'
  /// when the 'Choice2Of2' of the 'Choice' needs to be lifted into a 'Semigroup'.
  let liftChoice (f:('b -> 'e)) : (Choice<'a,'b>->AccValidation<'e,'a>) = Choice.either (AccFailure << f) AccSuccess

  let appAccValidation (m:'err -> 'err -> 'err) (e1':AccValidation<'err,'a>) (e2':AccValidation<'err,'a>) =
    match e1',e2' with
    |AccFailure e1 , AccFailure e2 -> AccFailure (m e1 e2)
    |AccFailure _  , AccSuccess a2 -> AccSuccess a2
    |AccSuccess a1 , AccFailure _  -> AccSuccess a1
    |AccSuccess a1 , AccSuccess _  -> AccSuccess a1

  let toResult x :Result<_,_> = match x with AccSuccess a -> Ok a | AccFailure e -> Error e
  let fromResult (x :Result<_,_>) = match x with Ok a -> AccSuccess a | Error e -> AccFailure e
  let inline either f g         = function AccSuccess v      -> f v      | AccFailure e                 -> g e

type AccValidation<'err,'a> with

  // as Applicative
  static member Return            x = AccSuccess x
  static member inline (<*>)      (f:AccValidation<_,'T->'U>, x:AccValidation<_,'T>) : AccValidation<_,_> = 
        AccValidation.apply f x
  // as Alternative (inherits from Applicative)
  static member inline get_Empty () = AccFailure ( getEmpty() )
  static member inline Append (x:AccValidation<_,_>, y:AccValidation<_,_>) = AccValidation.appAccValidation Control.Append.Invoke x y

  // as Functor
  static member Map        (x : AccValidation<_,_>, f) = AccValidation.map f x
  // as Bifunctor
  static member Bimap (x:AccValidation<'T,'V>, f:'T->'U, g:'V->'W) :AccValidation<'U,'W> = AccValidation.bimap f g x
  // as Traversable
  static member inline Traverse (t:AccValidation<'err,'a>, f : 'a->'b) : 'c=AccValidation.traverse f t


/// Validate's the [a] with the given predicate, returning [e] if the predicate does not hold.
///
/// This can be thought of as having the less general type:
///
/// validate : 'e -> ('a -> bool) -> 'a -> AccValidation<'e, 'a>
///
let validate (e:'e) (p:('a -> bool)) (a:'a) : AccValidation<'e,'a> = if p a then AccSuccess a else AccFailure e

/// validationNel : Choice<'a,'e> -> AccValidation (NonEmptyList<'e>) a
/// This is 'liftError' specialized to 'NonEmptyList', since
/// they are a common semigroup to use.
let validationNel (x:Result<_,_>) : (AccValidation<NonEmptyList<'e>,'a>)= (AccValidation.liftResult result) x

/// Leaves the validation unchanged when the predicate holds, or
/// fails with [e] otherwise.
///
/// This can be thought of as having the less general type:
///
/// ensure : 'e -> ('a -> 'bool) -> AccValidation<'a,'e> -> AccValidation<'a,'e>
let inline ensure (e:'e) (p:'a-> bool) =
  function
  |AccFailure x -> AccFailure x
  |AccSuccess a -> validate e p a


let inline _Success    x = (prism AccSuccess    <| AccValidation.either Ok (Error << AccFailure)) x
let inline _Failure    x = (prism AccFailure <| AccValidation.either (Error << AccFailure) Ok ) x

let inline isoAccValidationResult x = x |> iso AccValidation.toResult AccValidation.fromResult
