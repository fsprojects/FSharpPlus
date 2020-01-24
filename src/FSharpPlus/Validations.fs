namespace FSharpPlus.Data

open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open System.ComponentModel

// Validation is based on https://github.com/qfpl/validation

/// A 'Validation' is either a value of the type 'err or 't, similar to 'Result'. However,
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

module Validation =

    let map (f: 'T->'U) (source: Validation<'Error,'T>) =
        match source with
        | Failure e -> Failure e
        | Success a -> Success (f a) 

    let inline apply f x : Validation<'Error,'U> = 
        match f, (x: Validation<'Error,'T>) with
        | Failure e1, Failure e2 -> Failure (plus e1 e2)
        | Failure e1, Success _  -> Failure e1
        | Success _ , Failure e2 -> Failure e2
        | Success f , Success a  -> Success (f a)

    let inline foldBack (folder: 'T->'State->'State) (source: Validation<'Error,'T>) (state: 'State) =
        match source with
        | Success a -> folder a state
        | Failure _ -> state

    #if !FABLE_COMPILER
    let inline traverse (f: 'T->'``Functor<'U>``) (source: Validation<'Error,'T>) : '``Functor<Validation<'Error,'U>>`` =
        match source with
        | Success a -> Validation<'Error,'U>.Success <!> f a
        | Failure e -> result (Validation<'Error,'U>.Failure e)
    #endif

    let bimap (f: 'T1->'U1) (g: 'T2->'U2) = function
        | Failure e -> Failure (f e)
        | Success a -> Success (g a)

    let bifoldBack f g (source: Validation<'Error,'T>) (state: 'State) : 'State =
        match source with
        | Success a -> g a state
        | Failure e -> f e state

    [<System.Obsolete("Use Validation.bifoldBack instead.")>]
    let biFoldBack f g state x =
        match state with
        | Success a -> g a x
        | Failure e -> f e x

    let inline bitraverse (f: 'T1->'``Functor<'U1>``) (g: 'T2->'``Functor<'U2>``) (source: Validation<'T1,'T2>) : '``Functor<Validation<'U1,'U2>>`` =
        match source with
        | Success a -> Validation<'U1,'U2>.Success <!> g a
        | Failure e -> Validation<'U1,'U2>.Failure <!> f e

    /// Binds through a Validation, which is useful for
    /// composing Validations sequentially. Note that despite having a bind
    /// function of the correct type, Validation is not a monad.
    /// The reason is, this bind does not accumulate errors, so it does not
    /// agree with the Applicative instance.
    ///
    /// There is nothing wrong with using this function, it just does not make a
    /// valid Monad instance.
    let bind (f: 'T->Validation<'Error,_>) x : Validation<_,'U> =
        match x with 
        | Failure e -> Failure e
        | Success a -> f a

    [<System.Obsolete("Use Validation.defaultValue instead.")>]
    let orElse v (a: 'a) = match v with | Failure _ -> a | Success x -> x

    /// Unwraps the Success or applies the compensation function to the Error.
    let valueOr compensation (source: Validation<'Error,'T>) = 
        match source with
        | Failure e -> compensation e
        | Success a -> a

    /// Converts a 'Result' to a 'Validation'
    /// when the 'Error' of the 'Result' needs to be lifted into a 'Semigroup'.
    let liftResult f : (Result<'T,'Error> -> Validation<'Semigroup,'T>) = function Error e -> Failure (f e) | Ok a -> Success a

    /// Converting a 'Choice' to a 'Validation'
    /// when the 'Choice2Of2' of the 'Choice' needs to be lifted into a 'Semigroup'.
    let liftChoice (f: 'b -> 'Semigroup) : (Choice<'b,'T>->Validation<'Semigroup,'T>) = Choice.either (Failure << f) Success

    let appValidation (m: 'err -> 'err -> 'err) (e1': Validation<'err,'a>) (e2': Validation<'err,'a>) =
        match e1', e2' with
        | Failure e1 , Failure e2 -> Failure (m e1 e2)
        | Failure _  , Success a2 -> Success a2
        | Success a1 , Failure _  -> Success a1
        | Success a1 , Success _  -> Success a1

    let toResult x : Result<'T,'Error>  = match x with Success a -> Ok a | Failure e -> Error e
    let ofResult (x: Result<'T,'Error>) = match x with Ok a -> Success a | Error e -> Failure e
    let toChoice x : Choice<'T,'Error>  = match x with Success a -> Choice1Of2 a | Failure e -> Choice2Of2 e
    let ofChoice (x: Choice<'T,'Error>) = match x with Choice1Of2 a -> Success a | Choice2Of2 e -> Failure e
    let either (f: 'T->'U) (g:'Error->'U) = function Success v -> f v    | Failure e     -> g e

    [<System.Obsolete("This function will not be supported in future versions.")>]
    let validate (e: 'e) (p: 'a -> bool) (a: 'a) : Validation<'e,'a> = if p a then Success a else Failure e

    #if !FABLE_COMPILER
    /// validationNel : Result<'a,'e> -> Validation (NonEmptyList<'e>) a
    /// This is 'liftError' specialized to 'NonEmptyList', since
    /// they are a common semigroup to use.
    let validationNel (x: Result<_,_>) : (Validation<NonEmptyList<'e>,'a>) = (liftResult result) x
    #endif

    [<System.Obsolete("This function will not be supported in future versions.")>]
    let ensure (e: 'e) (p: 'a-> bool) = function
        | Failure x -> Failure x
        | Success a -> validate e p a

    /// Creates a safe version of the supplied function, which returns a Validation<exn,'U> instead of throwing exceptions.
    let protect (f: 'T->'U) x =
        try
            Success (f x)
        with e -> Failure e


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
