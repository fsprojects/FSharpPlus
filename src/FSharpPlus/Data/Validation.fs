namespace FSharpPlus.Data

#nowarn "44"

open System.ComponentModel
open FSharpPlus

#if !FABLE_COMPILER || FABLE_COMPILER_3
open FSharpPlus.Lens
#endif

open FSharpPlus.Data


// Validation is based on https://github.com/qfpl/validation

/// A 'Validation' is either a value of the type 'error or 't, similar to 'Result'. However,
/// the 'Applicative' instance for 'Validation' accumulates errors using a 'Semigroup' on 'error.
/// In contrast, the Applicative for 'Result' returns only the first error.
///
/// A consequence of this is that 'Validation' is not a monad. There is no F#+ 'Bind' method since
/// that would violate monad rules.


type Validation<'error, 't> =
  | Failure of 'error
  | Success of 't

module Validation =
    open FSharp.Core.CompilerServices

    let map (f: 'T->'U) (source: Validation<'Error,'T>) =
        match source with
        | Failure e -> Failure e
        | Success a -> Success (f a) 

    /// Applies the wrapped value to the wrapped function when both are Success and returns a wrapped result or the Failure(s).
    /// <param name="f">The function wrapped in a Success or a Failure.</param>
    /// <param name="x">The value wrapped in a Success or a Failure.</param>
    /// <returns>A Success of the function applied to the value when both are Success, or the Failure(s) if more than one, combined with the Semigroup (++) operation of the Error type.</returns>
    let inline apply f x : Validation<'Error,'U> =
        match f, (x: Validation<'Error,'T>) with
        #if !FABLE_COMPILER
        | Failure e1, Failure e2 -> Failure (plus e1 e2)
        #else
        | Failure e1, Failure e2 -> Failure (e1 + e2)
        #endif
        | Failure e1, Success _  -> Failure e1
        | Success _ , Failure e2 -> Failure e2
        | Success f , Success a  -> Success (f a)

    let inline map2 f x y : Validation<'Error,'V> =
        match (x: Validation<'Error,'T>), (y: Validation<'Error,'U>) with
        #if !FABLE_COMPILER
        | Failure e1, Failure e2 -> Failure (plus e1 e2)
        #else
        | Failure e1, Failure e2 -> Failure (e1 + e2)
        #endif
        | Failure e1, Success _  -> Failure e1
        | Success _ , Failure e2 -> Failure e2
        | Success x , Success y  -> Success (f x y)

    let inline map3 f x y z : Validation<'Error,'W> =
        match (x: Validation<'Error,'T>), (y: Validation<'Error,'U>), (z: Validation<'Error,'V>) with
        | Success x , Success y, Success z -> Success (f x y z)

        #if !FABLE_COMPILER
        | Failure e1, Failure e2, Failure e3 -> Failure (e1 ++ e2 ++ e3)
        #else
        | Failure e1, Failure e2, Failure e3 -> Failure (e1 + e2 + e3)
        #endif

        | Failure e, Success _, Success _
        | Success _, Failure e, Success _
        | Success _, Success _, Failure e
            -> Failure e

        | Success _ , Failure e1, Failure e2
        | Failure e1, Success _ , Failure e2
        | Failure e1, Failure e2, Success _            
            #if !FABLE_COMPILER
            -> Failure (plus e1 e2)
            #else
            -> Failure (e1 + e2)
            #endif

    let inline foldBack (folder: 'T->'State->'State) (source: Validation<'Error,'T>) (state: 'State) =
        match source with
        | Success a -> folder a state
        | Failure _ -> state

    #if !FABLE_COMPILER || FABLE_COMPILER_3

    /// Traverse the Success case with the supplied function.
    let inline traverse (f: 'T->'``Functor<'U>``) (source: Validation<'Error,'T>) : '``Functor<Validation<'Error,'U>>`` =
        match source with
        | Success a -> Validation<'Error,'U>.Success <!> f a
        | Failure e -> result (Validation<'Error,'U>.Failure e)

    /// Traverse the Success case.
    let inline sequence (source: Validation<'Error,'``Functor<'T>``>) : '``Functor<Validation<'Error,'T>>`` = traverse id source

    #endif

    /// <summary>Maps both success and failure of a Validation.</summary>
    /// <param name="failureMapper">Function to be applied to source, if it contains a Failure value.</param>
    /// <param name="successMapper">Function to be applied to source, if it contains a Success value.</param>
    /// <param name="source">The source value, containing a Success or a Failure.</param>
    /// <returns>The result of applying the corresponding mapping function.</returns>
    let bimap (failureMapper: 'TError -> 'UError) (successMapper: 'T -> 'U) source =
        match source with
        | Failure e -> Failure (failureMapper e)
        | Success a -> Success (successMapper a)

    let bifoldBack f g (source: Validation<'Error,'T>) (state: 'State) : 'State =
        match source with
        | Success a -> g a state
        | Failure e -> f e state

    [<System.Obsolete("Use Validation.bifoldBack instead.")>]
    let biFoldBack f g state x =
        match state with
        | Success a -> g a x
        | Failure e -> f e x

    /// Like traverse but taking an additional function to traverse the Failure part as well.
    let inline bitraverse (f: 'TError -> '``Functor<'UError>``) (g: 'T -> '``Functor<'U>``) (source: Validation<'TError, 'T>) : '``Functor<Validation<'UError, 'U>>`` =
        match source with
        | Success a -> Validation<'Error2,'T2>.Success <!> g a
        | Failure e -> Validation<'Error2,'T2>.Failure <!> f e

    /// Like sequence but traversing the Failure part as well.
    let inline bisequence (source: Validation<'``Functor<'Error>``,'``Functor<'T>``>) : '``Functor<Validation<'Error,'T>>`` = bitraverse id id source

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
    
    /// Extracts the Success value or use the supplied default value when it's a Failure.
    let defaultValue (value: 'T) (source: Validation<'Error,'T>) : 'T = match source with Success v -> v | _ -> value
    
    /// Extracts the Success value or applies the compensation function over the Failure.
    let defaultWith (compensation: 'Error->'T) (source: Validation<'Error,'T>) : 'T = match source with | Success x -> x | Failure e -> compensation e

    [<System.Obsolete("Use Validation.defaultWith instead.")>]
    let valueOr ea (v: Validation<'e,'a>) = match v with | Failure e -> ea e | Success a -> a

    /// Converts a 'Result' to a 'Validation'
    /// when the 'Error' of the 'Result' needs to be lifted into a 'Semigroup'.
    let liftResult f : (Result<'T,'Error> -> Validation<'Semigroup,'T>) = function Error e -> Failure (f e) | Ok a -> Success a

    /// Converting a 'Choice' to a 'Validation'
    /// when the 'Choice2Of2' of the 'Choice' needs to be lifted into a 'Semigroup'.
    let liftChoice (f: 'b -> 'Semigroup) : (Choice<'b,'T>->Validation<'Semigroup,'T>) = Choice.either (Failure << f) Success

    /// Takes two Validations and returns the first Success.
    /// If both are Failures it returns both Failures combined with the supplied function.
    let appValidation (combine: 'err -> 'err -> 'err) (e1': Validation<'err,'a>) (e2': Validation<'err,'a>) =
        match e1', e2' with
        | Failure e1 , Failure e2 -> Failure (combine e1 e2)
        | Failure _  , Success a2 -> Success a2
        | Success a1 , Failure _  -> Success a1
        | Success a1 , Success _  -> Success a1

    /// Converts a Validation<'Error,'T> to a Result<'T,'Error>.
    let toResult x : Result<'T,'Error>  = match x with Success a -> Ok a | Failure e -> Error e

    /// Creates a Validation<'Error,'T> from a Result<'T,'Error>.
    let ofResult (x: Result<'T,'Error>) = match x with Ok a -> Success a | Error e -> Failure e

    /// Converts a Validation<'Error,'T> to a Choice<'T,'Error>.
    let toChoice x : Choice<'T,'Error>  = match x with Success a -> Choice1Of2 a | Failure e -> Choice2Of2 e

    /// Creates a Validation<'Error,'T> from a Choice<'T,'Error>.
    let ofChoice (x: Choice<'T,'Error>) = match x with Choice1Of2 a -> Success a | Choice2Of2 e -> Failure e

    /// <summary> Extracts a value from either side of a Validation.</summary>
    /// <param name="successMapper">Function to be applied to source, if it contains a Success value.</param>
    /// <param name="failureMapper">Function to be applied to source, if it contains a Failure value.</param>
    /// <param name="source">The source value, containing a Success or a Failure.</param>
    /// <returns>The result of applying either functions.</returns>
    let either (failureMapper: 'TError -> 'U) (successMapper: 'T -> 'U) source = match source with Success v -> successMapper v | Failure e -> failureMapper e

    [<System.Obsolete("This function will not be supported in future versions.")>]
    let validate (e: 'e) (p: 'a -> bool) (a: 'a) : Validation<'e,'a> = if p a then Success a else Failure e

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    /// validationNel : Result<'a,'e> -> Validation (NonEmptyList<'e>) a
    /// This is 'liftError' specialized to 'NonEmptyList', since
    /// they are a common semigroup to use.
    let validationNel (x: Result<'T, 'TError>) : (Validation<NonEmptyList<'TError>, 'T>) = (liftResult result) x
    #endif

    [<System.Obsolete("This function will not be supported in future versions.")>]
    let ensure (e: 'e) (p: 'a-> bool) = function
        | Failure x -> Failure x
        | Success a -> validate e p a

    /// Creates a safe version of the supplied function, which returns a Validation<exn, 'U> instead of throwing exceptions.
    let protect (unsafeFunction: 'T -> 'U) x =
        try
            Success (unsafeFunction x)
        with e -> Failure e

    #if !FABLE_COMPILER
    let inline _Success x = (prism Success <| either Ok (Error << Failure)) x
    let inline _Failure x = (prism Failure <| either (Error << Failure) Ok) x
    #endif
    #if !FABLE_COMPILER || FABLE_COMPILER_3
    let inline isoValidationResult x = x |> iso toResult ofResult
    #endif
    
    /// <summary>
    /// Creates two lists by classifying the values depending on whether they were wrapped with Success or Failure.
    /// </summary>
    /// <returns>
    /// A tuple with both resulting lists, Success are in the first list.
    /// </returns>
    let partition (source: list<Validation<'TErrors, 'T>>) =
    #if FABLE_COMPILER
        let rec loop ((acc1, acc2) as acc) = function
            | [] -> acc
            | x::xs ->
                match x with
                | Success x -> loop (x::acc1, acc2) xs
                | Failure x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)
    #else
        let mutable coll1 = new ListCollector<'T> ()
        let mutable coll2 = new ListCollector<'TErrors> ()
        List.iter (function Success e -> coll1.Add e | Failure e -> coll2.Add e) source
        coll1.Close (), coll2.Close ()
    #endif

type Validation<'err,'a> with

    // as Applicative
    static member Return x = Success x
    static member inline (<*>)  (f: Validation<_,'T->'U>, x: Validation<_,'T>) : Validation<_,_> = Validation.apply f x

    /// <summary>
    /// Sequences two Validations left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: Validation<'Error, 'T>, y: Validation<'Error, 'U>) : Validation<'Error, 'U> = ((fun (_: 'T) (k: 'U) -> k) </Validation.map/>  x : Validation<'Error, 'U->'U>) </Validation.apply/> y
    
    /// <summary>
    /// Sequences two Validations left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<*  ) (x: Validation<'Error, 'U>, y: Validation<'Error, 'T>): Validation<'Error, 'U> = ((fun (k: 'U) (_: 'T) -> k ) </Validation.map/> x : Validation<'Error, 'T->'U>) </Validation.apply/> y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f, x: Validation<_,'T>, y: Validation<_,'U>) : Validation<_,'V> = Validation.map2 f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f, x: Validation<_,'T>, y: Validation<_,'U>, z: Validation<_,'V>) : Validation<_,'W> = Validation.map3 f x y z

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    // as Alternative (inherits from Applicative)
    static member inline get_Empty () = Failure (getEmpty ())
    static member inline (<|>) (x: Validation<_,_>, y: Validation<_,_>) = Validation.appValidation Control.Append.Invoke x y
    #endif

    // as Functor
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: Validation<_,_>, f) = Validation.map f x
    
    /// <summary>Lifts a function into a Validator. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member (<!>) (f, x: Validation<_,_>) = Validation.map f x

    // as Bifunctor
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Bimap (x: Validation<'T,'V>, f: 'T->'U, g: 'V->'W) : Validation<'U,'W> = Validation.bimap f g x

    #if !FABLE_COMPILER || FABLE_COMPILER_3

    // as Traversable
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Traverse (t: Validation<'err,'a>, f: 'a->'b) : 'c = Validation.traverse f t

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Sequence (t: Validation<'err,'a>) : 'c = Validation.sequence t

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

    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Bitraverse (t: Validation<'err,'a>, f, g) = Validation.bitraverse f g t
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Bisequence (t: Validation<'err,'a>) = Validation.bisequence t
    
    
    /// Creates a list with either all Success values or the Failure ones.
    static member SequenceBiApply (t: list<Validation<'Error,'T>>) : Validation<list<'Error>,list<'T>> =
        Validation.partition t |> fun (x, y) -> if List.isEmpty y then Success x else Failure y

    /// Creates an array with either all Success values or the Failure ones.
    static member SequenceBiApply (t: Validation<'Error,'T> []) : Validation<'Error [],'T []> =
        Array.partitionMap Validation.toChoice t |> fun (x, y) -> if Array.isEmpty y then Success x else Failure y
