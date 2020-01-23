namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open System.ComponentModel

// Validation is based on AccValidation from https://github.com/qfpl/validation

/// A 'Validation' is either a value of the type 'err or 'a, similar to 'Result'. However,
/// the 'Applicative' instance for 'Validation' /accumulates/ errors using a 'Semigroup' on 'err.
/// In contrast, the Applicative for 'Result' returns only the first error.
///
/// A consequence of this is that 'Validation' is not a monad. There is no F#+ 'Bind' method since
/// that would violate monad rules.
///
/// An example of typical usage can be found <a href="https://github.com/fsprojects/FSharpPlus/tree/master/src/FSharpPlus/Samples/Validations.fsx">here</a>.
///
type Validation<'err, 'a> =
  | Failure of 'err
  | Success of 'a

module Validation=

    let map (f: 'T->'U) = function
        | Failure e -> Failure e
        | Success a -> Success (f a) 

    let inline apply e1' e2' = 
        match e1', e2' with
        | Failure e1, Failure e2 -> Failure (plus e1 e2)
        | Failure e1, Success _  -> Failure e1
        | Success _ , Failure e2 -> Failure e2
        | Success f , Success a  -> Success (f a)

    let inline foldBack f state x =
        match state with
        | Success a -> f a x
        | Failure _ -> x

    #if !FABLE_COMPILER
    let inline traverse f = function 
        | Success a -> Success <!> f a
        | Failure e -> result (Failure e)
    #endif

    let bimap f g = function
        | Failure e -> Failure (f e)
        | Success a -> Success (g a)

    let bifoldBack f g x state =
        match x with
        | Success a -> g a state
        | Failure e -> f e state

    [<System.Obsolete("use bifoldBack from FSharpPlus.Operators (see: http://fsprojects.github.io/FSharpPlus/abstraction-bifoldable.html)")>]
    let biFoldBack f g state x =
        match state with
        | Success a -> g a x
        | Failure e -> f e x

    let inline bitraverse f g = function 
        | Success a -> Success <!> g a
        | Failure e -> Failure <!> f e

    /// Binds through a Validation, which is useful for
    /// composing Validations sequentially. Note that despite having a bind
    /// function of the correct type, Validation is not a monad.
    /// The reason is, this bind does not accumulate errors, so it does not
    /// agree with the Applicative instance.
    ///
    /// There is nothing wrong with using this function, it just does not make a
    /// valid Monad instance.
    let bind (f: 'T->Validation<_,_>) x : Validation<_,_> =
        match x with 
        | Failure e -> Failure e
        | Success a -> f a

    /// orElse v a returns 'a when v is Failure, and the a in Success a.
    [<System.Obsolete("Use Validation.defaultValue instead.")>]
    let orElse v (a: 'a) = match v with | Failure _ -> a | Success x -> x
    /// defaultValue value source returns value when source is Failure, and the v in Success v.
    let defaultValue (value:'a) (source:Validation<'err,'a>) :'a = match source with Success v -> v | _ -> value
    /// defaultWith returns either x when the source is Success x, otherwise applies the function f on e if the source is Failure e.
    let defaultWith (f:'err->'a) (source:Validation<'err,'a>) :'a = match source with | Success x -> x | Failure e -> f e

    /// Return the 'a or run the given function over the 'e.
    [<System.Obsolete("Use Validation.defaultWith instead.")>]
    let valueOr ea (v: Validation<'e,'a>) = match v with | Failure e -> ea e | Success a -> a

    /// 'liftResult' is useful for converting a 'Result' to an 'Validation'
    /// when the 'Error' of the 'Result' needs to be lifted into a 'Semigroup'.
    let liftResult (f: 'b -> 'e) : (Result<'a,'b> -> Validation<'e,'a>) = function | Error e-> Failure (f e) | Ok a-> Success a

    /// 'liftChoice' is useful for converting a 'Choice' to an 'Validation'
    /// when the 'Choice2Of2' of the 'Choice' needs to be lifted into a 'Semigroup'.
    let liftChoice (f: 'b -> 'e) : (Choice<'b,'a>->Validation<'e,'a>) = Choice.either (Failure << f) Success

    let appValidation (m: 'err -> 'err -> 'err) (e1': Validation<'err,'a>) (e2': Validation<'err,'a>) =
        match e1', e2' with
        | Failure e1 , Failure e2 -> Failure (m e1 e2)
        | Failure _  , Success a2 -> Success a2
        | Success a1 , Failure _  -> Success a1
        | Success a1 , Success _  -> Success a1

    let toResult x : Result<_,_>  = match x with Success a -> Ok a | Failure e -> Error e
    let ofResult (x :Result<_,_>) = match x with Ok a -> Success a | Error e -> Failure e
    let either f g                = function Success v -> f v      | Failure e     -> g e

    /// Validate's the [a] with the given predicate, returning [e] if the predicate does not hold.
    ///
    /// This can be thought of as having the less general type:
    ///
    /// validate : 'e -> ('a -> bool) -> 'a -> Validation<'e, 'a>
    ///
    [<System.Obsolete("This function will not be supported in future versions.")>]
    let validate (e: 'e) (p: 'a -> bool) (a: 'a) : Validation<'e,'a> = if p a then Success a else Failure e

    #if !FABLE_COMPILER
    /// validationNel : Result<'a,'e> -> Validation (NonEmptyList<'e>) a
    /// This is 'liftError' specialized to 'NonEmptyList', since
    /// they are a common semigroup to use.
    let validationNel (x: Result<_,_>) : (Validation<NonEmptyList<'e>,'a>) = (liftResult result) x
    #endif

    /// Leaves the validation unchanged when the predicate holds, or
    /// fails with [e] otherwise.
    ///
    /// This can be thought of as having the less general type:
    ///
    /// ensure : 'e -> ('a -> 'bool) -> Validation<'a,'e> -> Validation<'a,'e>
    [<System.Obsolete("This function will not be supported in future versions.")>]
    let ensure (e: 'e) (p: 'a-> bool) = function
        | Failure x -> Failure x
        | Success a -> validate e p a


    let inline _Success x = (prism Success <| either Ok (Error << Failure)) x
    let inline _Failure x = (prism Failure <| either (Error << Failure) Ok) x
    
    let inline isoValidationResult x = x |> iso toResult ofResult


type Validation<'err,'a> with

    // as Applicative
    static member Return x = Success x
    static member inline (<*>)  (f: Validation<_,'T->'U>, x: Validation<_,'T>) : Validation<_,_> = Validation.apply f x

    // as Alternative (inherits from Applicative)
    static member inline get_Empty () = Failure (getEmpty ())
    static member inline (<|>) (x: Validation<_,_>, y: Validation<_,_>) = Validation.appValidation Control.Append.Invoke x y

    // as Functor
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: Validation<_,_>, f) = Validation.map f x

    // as Bifunctor
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Bimap (x: Validation<'T,'V>, f: 'T->'U, g: 'V->'W) : Validation<'U,'W> = Validation.bimap f g x

    #if !FABLE_COMPILER
    // as Traversable
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Traverse (t: Validation<'err,'a>, f: 'a->'b) : 'c = Validation.traverse f t
    #endif

    // as Bifoldable
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline BifoldMap (t: Validation<'err,'a>, f: 'err->'b, g: 'a->'b) : 'b =
        match t with
        | Failure a -> f a
        | Success a -> g a
        
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline BifoldBack (t: Validation<'err,'a>, f: 'err->'b->'b, g: 'a->'b->'b, z: 'b) : 'b = Validation.bifoldBack f g t z
         
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Bifold (t: Validation<'err,'a>, f: 'b->'err->'b, g: 'b->'a->'b, z: 'b) : 'b =
        match t with
        | Failure a -> f z a
        | Success a -> g z a
        