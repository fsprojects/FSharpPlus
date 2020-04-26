namespace FSharpPlus

open FSharpPlus.Control

/// Generic functions and operators
[<AutoOpenAttribute>]
module Operators =


    // Common combinators

    /// Creates a new function with first two arguments flipped.
    let inline flip f (x: 'T) (y: 'V) : 'Result = f y x
    
    /// <summary> Creates a constant function.</summary>
    /// <param name="k">The constant value.</param>
    /// <returns>The constant value function.</returns>
    let inline konst (k: 'T) = fun (_: 'Ignored) -> k

    /// Takes a function expecting a tuple of two elements and returns a function expecting two curried arguments.
    let inline curry f (x: 'T1) (y: 'T2) : 'Result = f (x, y)
    
    #if !FABLE_COMPILER
    /// Takes a function expecting a tuple of any N number of elements and returns a function expecting N curried arguments.
    let inline curryN (f: (^``T1 * ^T2 * ... * ^Tn``) -> 'Result) : 'T1 -> '``T2 -> ... -> 'Tn -> 'Result`` = fun t -> Curry.Invoke f t
    #endif

    /// Takes a function expecting two curried arguments and returns a function expecting a tuple of two elements. Same as (<||).
    let inline uncurry f (x: 'T1, y: 'T2) : 'Result = f x y
    
    #if !FABLE_COMPILER
    /// Takes a function expecting any N number of curried arguments and returns a function expecting a tuple of N elements.
    let inline uncurryN (f: 'T1 -> '``T2 -> ... -> 'Tn -> 'Result``) (t: (^``T1 * ^T2 * ... * ^Tn``)) = Uncurry.Invoke f t : 'Result
    #endif

    /// Used in conjunction with /> to make an ad-hoc binary operator out of a function (x </f/> y).
    let inline (</) x = (|>) x

    /// Used in conjunction with </ to make an ad-hoc binary operator out of a function (x </f/> y).
    let inline (/>) x = flip x
    
    /// Executes a side-effect function and returns the original input value.
    let tap (f: 'T -> unit) x = f x; x

    /// <summary> Extracts a value from either side of a Result.</summary>
    /// <param name="fOk">Function to be applied to source, if it contains an Ok value.</param>
    /// <param name="fError">Function to be applied to source, if it contains an Error value.</param>
    /// <param name="source">The source value, containing an Ok or an Error.</param>
    /// <returns>The result of applying either functions.</returns>
    let inline either fOk fError (source: Result<'T,'Error>) : 'U = match source with Ok x -> fOk x | Error x -> fError x

    /// Takes a function, a default value and a option value. If the option value is None, the function returns the default value.
    /// Otherwise, it applies the function to the value inside Some and returns the result.
    let inline option f n = function Some x -> f x | None -> n

    /// Tuple two arguments
    let inline tuple2 a b             = a,b
    /// Tuple three arguments
    let inline tuple3 a b c           = a,b,c
    /// Tuple four arguments
    let inline tuple4 a b c d         = a,b,c,d
    /// Tuple five arguments
    let inline tuple5 a b c d e       = a,b,c,d,e
    /// Tuple six arguments
    let inline tuple6 a b c d e f     = a,b,c,d,e,f
    /// Tuple seven arguments
    let inline tuple7 a b c d e f g   = a,b,c,d,e,f,g
    /// Tuple eight arguments
    let inline tuple8 a b c d e f g h = a,b,c,d,e,f,g,h


    #if !FABLE_COMPILER

    // Functor ----------------------------------------------------------------

    /// Lifts a function into a Functor.
    let inline map (f: 'T->'U) (x: '``Functor<'T>``) : '``Functor<'U>`` = Map.Invoke f x

    /// Lifts a function into a Functor. Same as map.
    /// To be used in Applicative Style expressions, combined with <*>
    let inline (<!>) (f: 'T->'U) (x: '``Functor<'T>``) : '``Functor<'U>`` = Map.Invoke f x

    /// Lifts a function into a Functor. Same as map.
    let inline (<<|) (f: 'T->'U) (x: '``Functor<'T>``) : '``Functor<'U>`` = Map.Invoke f x

    /// Lifts a function into a Functor. Same as map but with flipped arguments.
    /// To be used in pipe-forward style expressions
    let inline (|>>) (x: '``Functor<'T>``) (f: 'T->'U) : '``Functor<'U>`` = Map.Invoke f x

    /// Like map but ignoring the results.
    let inline iter (action: 'T->unit) (source: '``Functor<'T>``) : unit = Iterate.Invoke action source

    // Un-zips (un-tuple) two functors.
    let inline unzip (source: '``Functor<'T1 * 'T2>``) = Unzip.Invoke source : '``Functor<'T1>`` * '``Functor<'T2>``

    // Zips (tuple) two functors.
    let inline zip (source1: '``ZipFunctor<'T1>``) (source2: '``ZipFunctor<'T2>``) : '``ZipFunctor<'T1 * 'T2>`` = Zip.Invoke source1 source2
   

    // Applicative ------------------------------------------------------------

    

    /// Lifts a value into a Functor. Same as return in Computation Expressions.
    let inline result (x: 'T) : '``Functor<'T>`` = Return.Invoke x

    /// Apply a lifted argument to a lifted function: f <*> arg
    let inline (<*>) (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` = Apply.Invoke f x : '``Applicative<'U>``

    /// Applies 2 lifted arguments to a non-lifted function. Equivalent to map2 in non list-like types.
    let inline lift2 (f: 'T->'U->'V) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) : '``Applicative<'V>`` = Lift2.Invoke f x y

    [<System.Obsolete("Use lift2 instead.")>]
    /// Apply 2 lifted arguments to a non-lifted function.
    let inline liftA2 (f: 'T->'U->'V) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) : '``Applicative<'V>`` = lift2 f x y

    /// Sequences two applicatives left-to-right, discarding the value of the first argument.
    let inline ( *>) (x: '``Applicative<'T>``) (y: '``Applicative<'U>``) : '``Applicative<'U>`` = ((fun (_: 'T) (k: 'U) -> k) <!>  x : '``Applicative<'U->'U>``) <*> y
    
    /// Sequences two applicatives left-to-right, discarding the value of the second argument.
    let inline (<*  ) (x: '``Applicative<'U>``) (y: '``Applicative<'T>``): '``Applicative<'U>`` = ((fun (k: 'U) (_: 'T) -> k ) <!> x : '``Applicative<'T->'U>``) <*> y

    /// Apply a lifted argument to a lifted function (flipped): arg <**> f
    let inline (<**>) (x: '``Applicative<'T>``) : '``Applicative<'T -> 'U>``->'``Applicative<'U>`` = flip (<*>) x
    
    [<System.Obsolete("Use opt instead.")>]
    let inline optional v = Some <!> v </Append.Invoke/> result None

    /// Transforms an alternative value (which has the notion of success/failure) to an alternative
    /// that always succeed, wrapping the original value into an option to signify success/failure of the original alternative.
    let inline opt (v: '``Alternative<'T>``) : '``Alternative<option<'T>>`` = (Some : 'T -> _) <!> v </Append.Invoke/> result (None: option<'T>)



    // Monad -----------------------------------------------------------
    
    #endif

    /// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
    let inline bind (f:'T->'``Monad<'U>``) (x:'``Monad<'T>``) :'``Monad<'U>`` = Bind.Invoke x f
    
    /// Takes a monadic value and a function from a plain type to a monadic value, and returns a new monadic value.
    let inline (>>=) (x: '``Monad<'T>``) (f: 'T->'``Monad<'U>``) : '``Monad<'U>`` = Bind.Invoke x f

    /// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
    let inline (=<<) (f: 'T->'``Monad<'U>``) (x: '``Monad<'T>``) : '``Monad<'U>`` = Bind.Invoke x f

    /// Composes left-to-right two monadic functions (Kleisli composition).
    let inline (>=>) (f: 'T->'``Monad<'U>``) (g: 'U->'``Monad<'V>``) : 'T -> '``Monad<'V>`` = fun x -> Bind.Invoke (f x) g

    /// Composes right-to-left two monadic functions (Kleisli composition).
    let inline (<=<) (g: 'b->'``Monad<'V>``) (f: 'T->'``Monad<'U>``) : 'T -> '``Monad<'V>`` = fun x -> Bind.Invoke (f x) g

    /// Flattens two layers of monadic information into one.
    #if !FABLE_COMPILER
    let inline join (x: '``Monad<Monad<'T>>``) : '``Monad<'T>`` = Join.Invoke x
    #else
    let inline join (x: '``Monad<Monad<'T>>``) : '``Monad<'T>`` = Bind.Invoke x id
    #endif

    #if !FABLE_COMPILER

    /// Equivalent to map but only for Monads.
    let inline liftM (f: 'T->'U) (m1: '``Monad<'T>``) : '``Monad<'U>``= m1 >>= (result << f)


    // Monoid -----------------------------------------------------------------

    /// Gets a value that represents the 0 element of a Monoid.
    let inline getZero () : 'Monoid = Zero.Invoke ()

    /// A value that represents the 0 element of a Monoid.
    let inline zero< ^Monoid when (Zero or ^Monoid) : (static member Zero : ^Monoid * Zero -> ^Monoid) > : ^Monoid = Zero.Invoke ()

    /// Combines two monoids in one.
    let inline (++) (x: 'Monoid) (y: 'Monoid) : 'Monoid = Plus.Invoke x y

    /// Combines two monoids in one.
    let inline plus (x: 'Monoid) (y: 'Monoid) : 'Monoid = Plus.Invoke x y

    module Seq =
        /// Folds all values in the sequence using the monoidal addition.
        let inline sum (x: seq<'Monoid>) : 'Monoid = Sum.Invoke x


    // Alternative/Monadplus/Arrowplus ----------------------------------------

    /// Gets a functor representing the empty value.
    let inline getEmpty () : '``Functor<'T>`` = Empty.Invoke ()

    /// A functor representing the empty value.
    [<GeneralizableValue>]
    let inline empty< ^``Functor<'T>`` when (Empty or ^``Functor<'T>``) : (static member Empty : ^``Functor<'T>`` * Empty -> ^``Functor<'T>``) > : ^``Functor<'T>`` = Empty.Invoke ()

    /// Combines two Alternatives
    let inline (<|>) (x: '``Functor<'T>``) (y: '``Functor<'T>``) : '``Functor<'T>`` = Append.Invoke x y

    /// Conditional failure of Alternative computations.
    /// If true it lifts the unit value, else it returns empty.
    ///
    /// Common uses of guard include conditionally signaling an error in an error monad and conditionally rejecting the current choice in an Alternative-based parser.
    let inline guard x: '``MonadPlus<unit>`` = if x then Return.Invoke () else Empty.Invoke ()

   
    // Contravariant/Bifunctor/Profunctor/Invariant ---------------------------

    /// Maps over the input.
    let inline contramap (f: 'U->'T) (x: '``Contravariant<'T>``) : '``Contravariant<'U>`` = Contramap.Invoke f x

    /// Maps over both arguments of the Bifunctor at the same time.
    let inline bimap (f: 'T->'U) (g: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` = Bimap.Invoke f g source

    /// Maps covariantly over the first argument of the Bifunctor.
    let inline first (f: 'T->'V) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` = MapFirst.Invoke f source

    /// Maps covariantly over the second argument of the Bifunctor.
    let inline second (f: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'T,'W>`` = Map.Invoke f source

    /// Maps over both arguments at the same time of a Profunctor.
    let inline dimap (f: 'A->'B) (g: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` = Dimap.Invoke f g source

    /// Maps over the left part of a Profunctor.
    let inline lmap (f: 'A->'B) (source: ^``Profunctor<'B,'C>``) : '``Profunctor<'A,'C>`` = Contramap.Invoke f source

    /// Maps over the right part of a Profunctor.
    let inline rmap (f: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'B,'D>`` = Map.Invoke f source

    #endif

    /// Maps a pair of functions over an Invariant Functor
    let inline invmap (f: 'T -> 'U) (g: 'U -> 'T) (source: '``InvariantFunctor<'T>``) = Invmap.Invoke f g source : '``InvariantFunctor<'U>``


    #if !FABLE_COMPILER

    // Category ---------------------------------------------------------------

    /// Gets the identity morphism.
    let inline getCatId () = Id.Invoke () : '``Category<'T,'T>``

    /// The identity morphism.
    let inline catId< ^``Category<'T,'T>`` when (Id or ^``Category<'T,'T>``) : (static member Id : ^``Category<'T,'T>`` * Id -> ^``Category<'T,'T>``) > = Id.Invoke () : '``Category<'T,'T>``

    /// Right-to-left morphism composition.
    let inline catComp (f: '``Category<'U,'V>``) (g: '``Category<'T,'U>``) : '``Category<'T,'V>`` = Comp.Invoke f g

    
    // Arrow ------------------------------------------------------------------

    /// Lifts a function to an arrow.
    let inline arr (f: 'T -> 'U) : '``Arrow<'T,'U>`` = Arr.Invoke f

    /// Sends the first component of the input through the argument arrow, and copy the rest unchanged to the output.
    let inline arrFirst (f: '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` = ArrFirst.Invoke f

    /// Sends the second component of the input through the argument arrow, and copy the rest unchanged to the output.
    let inline arrSecond (f: '``Arrow<'T,'U>``) : '``Arrow<('V * 'T),('V * 'U)>`` = ArrSecond.Invoke f

    /// Splits the input between the two argument arrows and combine their output. Note that this is in general not a functor.
    let inline ( ***) (f: '``Arrow<'T1,'U1>``) (g: '``Arrow<'T2,'U2>``) : '``Arrow<('T1 * 'T2),('U1 * 'U2)>`` = ArrCombine.Invoke f g

    /// Sends the input to both argument arrows and combine their output. Also known as the (&&&) operator.
    let inline fanout (f: '``Arrow<'T,'U1>``) (g: '``Arrow<'T,'U2>``) : '``Arrow<'T,('U1 * 'U2)>`` = Fanout.Invoke f g


    // Arrow Choice------------------------------------------------------------

    /// Splits the input between the two argument arrows and merge their outputs. Also known as the (|||) operator.
    let inline fanin (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` = Fanin.Invoke f g

    /// Splits the input between both argument arrows, retagging and merging their outputs. Note that this is in general not a functor.
    let inline (+++) (f: '``ArrowChoice<'T1,'U1>``) (g: '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` = AcMerge.Invoke f g

    /// Feeds marked inputs through the left argument arrow, passing the rest through unchanged to the output.
    let inline left (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` = AcLeft.Invoke f

    /// Feeds marked inputs through the right argument arrow, passing the rest through unchanged to the output.
    let inline right (f: '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>`` = AcRight.Invoke f


    // Arrow Apply ------------------------------------------------------------

    /// Applies an arrow produced as the output of some previous computation to an input, producing its output as the output of app.
    let inline getApp () = App.Invoke () : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``

    /// Applies an arrow produced as the output of some previous computation to an input, producing its output as the output of app.
    let inline app< ^``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>`` when (App or ^``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``) : (static member App : ^``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>`` * App -> ^``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``) > =
        App.Invoke () : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``


    // Foldable

    /// <summary>Applies a function to each element of the foldable, starting from the end, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f i0 (...(f iN s))</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="foldable">The input foldable.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The state object after the folding function is applied to each element of the foldable.</returns>
    let inline foldBack (folder: 'T->'State->'State) (foldable: '``Foldable<'T>``) (state: 'State) : 'State = FoldBack.Invoke folder state foldable

    /// <summary>Applies a function to each element of the foldable, threading an accumulator argument
    /// through the computation. Take the second argument, and apply the function to it
    /// and the first element of the foldable. Then feed this result into the function along
    /// with the second element and so on. Return the final result.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
    /// computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="foldable">The input foldable.</param>
    /// <returns>The final state value.</returns>
    let inline fold (folder: 'State->'T->'State) (state: 'State) (foldable: '``Foldable<'T>``) : 'State = Fold.Invoke folder state foldable

    /// Folds by mapping all values to a Monoid
    let inline foldMap (f: 'T->'Monoid) (x: '``Foldable<'T>``) : 'Monoid = FoldMap.Invoke f x

    /// <summary>Builds a list from the given foldable.</summary>
    /// <param name="source">The input foldable.</param>
    /// <returns>The list of foldable elements.</returns>
    let inline toList value : 'T list = ToList.Invoke value

    /// <summary>Builds an array from the given foldable.</summary>
    /// <param name="source">The input foldable.</param>
    /// <returns>The array of foldable elements.</returns>
    let inline toArray value : 'T [] = ToArray.Invoke value

    /// <summary>Views the given foldable as a sequence.</summary>
    /// <param name="source">The input foldable.</param>
    /// <returns>The sequence of elements in the foldable.</returns>
    let inline toSeq (source: '``Foldable<'T>``) = ToSeq.Invoke source : seq<'T>

    /// <summary>Tests if any element of the list satisfies the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input foldable. If any application 
    /// returns true then the overall result is true and no further elements are tested. 
    /// Otherwise, false is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <returns>True if any element satisfies the predicate.</returns>
    let inline exists (predicate: 'T->bool) (source: '``Foldable<'T>``) = Exists.Invoke predicate source : bool

    /// <summary>Tests if all elements of the collection satisfy the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input foldable. If any application 
    /// returns false then the overall result is false and no further elements are tested. 
    /// Otherwise, true is returned.</remarks>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <returns>True if all of the elements satisfy the predicate.</returns>
    let inline forall (predicate: 'T->bool) (source: '``Foldable<'T>``) = ForAll.Invoke predicate source : bool

    /// <summary>Gets the first element for which the given function returns true.
    /// Raises <c>KeyNotFoundException</c> if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if the predicate evaluates to false for
    /// all the elements of the foldable.</exception>
    /// <returns>The first element that satisfies the predicate.</returns>
    let inline find (predicate: 'T->bool) (source: '``Foldable<'T>``) = Find.Invoke predicate source : 'T

    /// <summary>Gets the first element for which the given function returns true.
    /// Returns None if no such element exists.</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <returns>The first element for which the predicate returns true, or None if
    /// every element evaluates to false.</returns>
    let inline tryFind (predicate: 'T->bool) (source: '``Foldable<'T>``) = TryFind.Invoke predicate source : 'T option

    /// <summary>Applies the given function to successive elements, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If no such
    /// element exists then raise <c>System.Collections.Generic.KeyNotFoundException</c></summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when the foldable is empty.</exception>
    /// <returns>The first resulting value.</returns>
    let inline pick (chooser: 'T->'U option) (source: '``Foldable<'T>``) = Pick.Invoke chooser source : 'U

    /// <summary>Applies the given function to successive elements, returning <c>Some(x)</c> the first
    /// result where function returns <c>Some(x)</c> for some x. If no such element 
    /// exists then return <c>None</c>.</summary>
    /// <param name="chooser">The function to generate options from the elements.</param>
    /// <param name="source">The input foldable.</param>
    /// <returns>The first resulting value or None.</returns>
    let inline tryPick (chooser: 'T->'U option) (source: '``Foldable<'T>``) = TryPick.Invoke chooser source : 'U option

    /// Folds the source, inserting a separator between each element.
    let inline intercalate (sep: 'Monoid) (source: '``Foldable<'Monoid>``) = Intercalate.Invoke sep source : 'Monoid

    /// <summary>Gets the first element of the foldable.</summary>
    ///
    /// <param name="source">The input flodable.</param>
    /// <exception cref="System.ArgumentException">Thrown when the foldable is empty.</exception>
    /// <returns>The first element of the foldable.</returns>
    let inline head (source: '``Foldable<'T>``) = Head.Invoke source : 'T

    /// <summary>Gets the first element of the foldable, or
    /// <c>None</c> if the foldable is empty.</summary>
    /// <param name="source">The input foldable.</param>
    /// <returns>The first element of the foldable or None.</returns>
    let inline tryHead (source: '``Foldable<'T>``) = TryHead.Invoke source : 'T option

    /// <summary>Gets the number of elements in the foldable.</summary>
    /// <param name="list">The input foldable.</param>
    /// <returns>The length of the foldable.</returns>
    let inline length (source: '``Foldable<'T>``) : int = Length.Invoke source

    /// Gets the maximum value in the foldable
    let inline maximum (source: '``Foldable<'T>``) = Max.Invoke source : 'T when 'T : comparison

    /// Gets the minimum value in the foldable
    let inline minimum (source: '``Foldable<'T>``) = Min.Invoke source : 'T when 'T : comparison

    /// Gets the maximum value after projecting in the foldable
    let inline maxBy (projection: 'T->'U when 'U : comparison) (source: '``Foldable<'T>``) = MaxBy.Invoke projection source : 'T

    /// Gets the minimum value after projecting in the foldable
    let inline minBy (projection: 'T->'U when 'U : comparison) (source: '``Foldable<'T>``) = MinBy.Invoke projection source : 'T

    /// Gets the nth value in the foldable - i.e. at position 'n'
    let inline nth (n: int) (source: '``Foldable<'T>``) : 'T = Nth.Invoke n source


    // Reducible

    /// <summary>Applies a function to each element of the reducible, threading an accumulator argument
    /// through the computation. Apply the function to the first two elements of the reducible.
    /// Then feed this result into the function along with the third element and so on. 
    /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
    /// <param name="reduction">The function to reduce two reducible elements to a single element.</param>
    /// <param name="source">The input reducible.</param>
    /// <returns>The final reduced value.</returns>
    let inline reduce reduction (source: '``Reducible<'T>``) = Reduce.Invoke reduction source : 'T


    // Traversable

    /// Map each element of a structure to an action, evaluate these actions from left to right, and collect the results.
    let inline traverse (f: 'T->'``Functor<'U>``) (t: '``Traversable<'T>``) : '``Functor<'Traversable<'U>>`` = Traverse.Invoke f t

    /// Evaluate each action in the structure from left to right, and and collect the results.
    let inline sequence (t: '``Traversable<'Functor<'T>>``) : '``Functor<'Traversable<'T>>`` = Sequence.Invoke t


    // Bifoldable

    /// Combines the elements of a structure, given ways of mapping them to a common monoid.
    let inline bifoldMap (f: 'T1->'Monoid) (g: 'T2->'Monoid) (source: '``Bifoldable<'T1,'T2>``) = BifoldMap.Invoke f g source

    /// Combines the elements of a structure in a right associative manner.
    let inline bifold (leftFolder: 'State -> 'T1 -> 'State) (rightFolder: 'State -> 'T2 -> 'State) (state: 'State) (source: '``Bifoldable<'T1,'T2>``) = Bifold.Invoke leftFolder rightFolder state source
    
    /// Combines the elements of a structure in a left associative manner.
    let inline bifoldBack (leftFolder: 'T1 -> 'State -> 'State) (rightFolder: 'T2 -> 'State -> 'State) (source: '``Bifoldable<'T1,'T2>``) (state: 'State) : 'State = BifoldBack.Invoke leftFolder rightFolder state source

    /// Combines the elements of a structure using a monoid.
    let inline bisum (source: '``Bifoldable<'Monoid,'Monoid>``) : 'Monoid = Bisum.Invoke source

    
    // Bitraversable

    // Evaluates the relevant functions at each element in the structure, running the action, and builds a new structure with the same shape, using the results produced from sequencing the actions.
    let inline bitraverse (f: 'T1->'``Functor<'T2>``) (g: 'U1->'``Functor<'U2>``) (source: '``Bitraversable<'T1,'U1>``) : '``Functor<'Bitraversable<'T2,'U2>>`` = Bitraverse.Invoke f g source

    // Sequences all the actions in a structure, building a new structure with the same shape using the results of the actions.
    let inline bisequence (source: '``Bitraversable<'Functor<'T>,'Functor<'U>>``) : '``Functor<'Bitraversable<'T,'U>>`` = Bisequence.Invoke source


    // Indexable

    /// Gets an item from the given index.
    let inline item (n: 'K) (source: '``Indexed<'T>``) : 'T = Item.Invoke n source

    /// Tries to get an item from the given index.
    let inline tryItem (n: 'K) (source: '``Indexed<'T>``) : 'T option = TryItem.Invoke n source

    /// Maps with access to the index.
    let inline mapi (mapping: 'K->'T->'U) (source: '``FunctorWithIndex<'T>``) : '``FunctorWithIndex<'U>`` = MapIndexed.Invoke mapping source

    /// Maps an action with access to an index.
    let inline iteri (action: 'K->'T->unit) (source: '``FunctorWithIndex<'T>``) : unit = IterateIndexed.Invoke action source

    /// Left-associative fold of an indexed container with access to the index i.
    let inline foldi (folder: 'State->'K->'T->'State) (state: 'State) (source: '``FoldableWithIndex<'T>``) : 'State = FoldIndexed.Invoke folder state source

    /// Traverses an indexed container. Behaves exactly like a regular traverse except that the traversing function also has access to the key associated with a value.
    let inline traversei (f: 'K->'T->'``Applicative<'U>``) (t: '``Traversable<'T>>``) : '``Applicative<'Traversable<'U>>`` = TraverseIndexed.Invoke f t

    /// <summary>
    /// Gets the index of the first element in the source
    /// that satisfies the given predicate.
    /// </summary>
    /// <param name="predicate">
    /// The function to test the input elements.
    /// </param>
    /// <param name="source">The input collection.</param>
    /// <returns> 
    /// The index of the first element that satisfies the predicate.
    /// </returns>
    /// <exception cref="System.ArgumentException">
    /// Thrown if the predicate evaluates to false for all the elements of the source.
    /// </exception>
    let inline findIndex (predicate: 'T -> bool) (source: '``Indexable<'T>``) : 'Index = FindIndex.Invoke predicate source

    /// <summary>
    /// Gets the index of the first element in the source
    /// that satisfies the given predicate.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <param name="predicate">
    /// The function to test the input elements.
    /// </param>
    /// <param name="source">The input collection.</param>
    /// <returns> 
    /// The index of the first element that satisfies the predicate, or <c>None</c>.
    /// </returns>
    let inline tryFindIndex (predicate: 'T -> bool) (source: '``Indexable<'T>``) : 'Index option = TryFindIndex.Invoke predicate source

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// </summary>
    /// <param name="slice">The slice to be searched.</param>
    /// <param name="source">The input collection.</param>
    /// <exception cref="System.ArgumentException">
    /// Thrown when the slice was not found in the source.
    /// </exception>
    /// <returns>
    /// The index of the slice.
    /// </returns>
    let inline findSliceIndex (slice: '``Indexable<'T>``) (source: '``Indexable<'T>``) : 'Index = FindSliceIndex.Invoke slice source

    /// <summary>
    /// Gets the index of the first occurrence of the specified slice in the source.
    /// Returns <c>None</c> if not found.
    /// </summary>
    /// <param name="slice">The slice to be searched.</param>
    /// <param name="source">The input collection.</param>
    /// <returns>
    /// The index of the slice or <c>None</c>.
    /// </returns>
    let inline tryFindSliceIndex (slice: '``Indexable<'T>``) (source: '``Indexable<'T>``) : 'Index option = TryFindSliceIndex.Invoke slice source

    // Comonads

    #endif

    /// Extracts a value from a comonadic context.
    let inline extract (x: '``Comonad<'T>``) : 'T = Extract.Invoke x

    /// <summary> Extends a local context-dependent computation to a global computation. </summary>
    let inline extend (g: '``Comonad<'T>``->'U) (s: '``Comonad<'T>``) : '``Comonad<'U>`` = Extend.Invoke g s

    /// <summary> Extends a local context-dependent computation to a global computation.
    /// Same as <c>extend</c> but with flipped arguments. </summary>
    let inline (=>>) (s: '``Comonad<'T>``) (g: '``Comonad<'T>``->'U) : '``Comonad<'U>`` = Extend.Invoke g s

    /// Duplicates a comonadic context.
    #if !FABLE_COMPILER
    let inline duplicate (x: '``Comonad<'T>``) : '``Comonad<'Comonad<'T>>`` = Duplicate.Invoke x
    #else
    let inline duplicate (x: '``Comonad<'T>``) : '``Comonad<'Comonad<'T>>`` = Extend.Invoke id x
    #endif


    #if !FABLE_COMPILER

    // Monad Transformers

    /// Lifts a computation from the inner monad to the constructed monad.
    let inline lift (x: '``Monad<'T>``) : '``MonadTrans<'Monad<'T>>`` = Lift.Invoke x

    /// A specialized lift for Async<'T> which is able to bring an Async value from any depth of monad-layers.
    let inline liftAsync (x: Async<'T>) : '``MonadAsync<'T>`` = LiftAsync.Invoke x

    /// Calls a function with the current continuation as its argument (call-with-current-continuation).
    let inline callCC (f: ('T->'``MonadCont<'R,'U>``) -> '``MonadCont<'R,'T>``) : '``MonadCont<'R,'T>`` = CallCC.Invoke f
   
    /// The state from the internals of the monad.
    let inline get< ^``MonadState<'S * 'S>`` when ^``MonadState<'S * 'S>`` : (static member Get : ^``MonadState<'S * 'S>``)> = (^``MonadState<'S * 'S>`` : (static member Get : _) ())

    /// Gets a value which depends on the current state.
    let inline gets (f: 'S->'T) : '``MonadState<'T * 'S>`` = get |> if FSharpPlus.Internals.Prelude.opaqueId false then liftM f else Map.InvokeOnInstance f

    /// Replaces the state inside the monad.
    let inline put (x: 'S) : '``MonadState<unit * 'S>`` = Put.Invoke x

    /// Modifies the state inside the monad by applying a function.
    let inline modify (f: 'S->'S) : '``MonadState<unit * ('S->'S)>`` = get >>= (Put.Invoke << f)

    /// The environment from the monad.
    let inline ask< ^``MonadReader<'R,'T>`` when ^``MonadReader<'R,'T>`` : (static member Ask : ^``MonadReader<'R,'T>``)> = (^``MonadReader<'R,'T>`` : (static member Ask : _) ())
   
    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let inline local (f: 'R1->'R2) (m: '``MonadReader<'R2,'T>``) : '``MonadReader<'R1,'T>`` = Local.Invoke f m

    /// Embeds a simple writer action.
    let inline tell (w: 'Monoid) : '``MonadWriter<'Monoid,unit>`` = Tell.Invoke w

    /// <summary> Executes the action <paramref name="m"/> and adds its output to the value of the computation. </summary>
    /// <param name="m">The action to be executed.</param>
    let inline listen (m: '``MonadWriter<'Monoid,'T>``) : '``MonadWriter<'Monoid,('T * 'Monoid)>`` = Listen.Invoke m

    /// Executes the action <paramref name="m"/>, which returns a value and a function, and returns the value, applying the function to the output.
    let inline pass (m: '``MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>``) : '``MonadWriter<'Monoid,'T>`` = Pass.Invoke m

    /// Throws an error value inside the Error monad.
    let inline throw (error: 'E) : '``'MonadError<'E,'T>`` = Throw.Invoke error

    /// <summary> Executes a handler when the value contained in the Error monad represents an error. </summary>
    let inline catch (value: '``'MonadError<'E1,'T>``) (handler: 'E1->'``'MonadError<'E2,'T>``) : '``'MonadError<'E2,'T>`` = Catch.Invoke value handler


    // Collection

    /// Converts to a Collection from a list.
    let inline ofList (source: list<'T>) = OfList.Invoke source : 'Collection

    /// Converts to a Collection from a seq.
    let inline ofSeq (source: seq<'T> ) = OfSeq.Invoke source : 'Collection

    /// <summary>Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true"</summary>
    /// <param name="predicate">The function to test the input elements.</param>
    /// <param name="source">The input collection.</param>
    /// <returns>A collection containing only the elements that satisfy the predicate.</returns>
    let inline filter (predicate: _->bool) (source: 'Collection) : 'Collection = Filter.Invoke predicate source

    /// <summary>Returns a collection that skips N elements of the original collection and then yields the
    /// remaining elements of the collection.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// when count exceeds the number of elements in the collection. <c>drop</c>
    /// returns an empty collection instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to skip.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the collection.</exception>
    let inline skip (count: int) (source: '``Collection<'T>``) : '``Collection<'T>`` = Skip.Invoke count source

    /// <summary>Gets the first N elements of the collection.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the collection. <c>limit</c>
    /// returns as many items as the collection contains instead of throwing an exception.</remarks>
    ///
    /// <param name="count">The number of items to take.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input collection is empty.</exception>
    /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the collection.</exception>
    let inline take (count: int) (source: '``Collection<'T>``) : '``Collection<'T>`` = Take.Invoke count source

    /// <summary>Returns a collection that drops N elements of the original collection and then yields the
    /// remaining elements of the collection.</summary>
    /// <remarks>When count exceeds the number of elements in the collection it
    /// returns an empty collection instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    let inline drop (count: int) (source: '``Collection<'T>``) : '``Collection<'T>`` = Drop.Invoke count source

    /// <summary>Returns a collection with at most N elements.</summary>
    ///
    /// <param name="count">The maximum number of items to return.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline limit (count: int) (source: '``Collection<'T>``) : '``Collection<'T>`` = Limit.Invoke count source

    /// <summary>Applies a key-generating function to each element of a collection and yields a collection of 
    /// unique keys. Each unique key contains a collection of all elements that match 
    /// to this key.</summary>
    /// 
    /// <remarks>This function returns a collection that digests the whole initial collection as soon as 
    /// that collection is iterated. As a result this function should not be used with 
    /// large or infinite collections. The function makes no assumption on the ordering of the original 
    /// collection.</remarks>
    ///
    /// <param name="projection">A function that transforms an element of the collection into a comparable key.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    let inline groupBy (projection: 'T->'Key when 'Key : equality) (source: '``Collection<'T>``) : '``Collection<'Key * 'Collection<'T>>`` = GroupBy.Invoke projection source

    /// <summary>Applies a key-generating function to each element of a collection and yields a collection of 
    /// keys tupled with values. Each key contains a collection of all adjacent elements that match 
    /// to this key, therefore keys are not unique but they can't be adjacent
    /// as each time the key changes, a new group is yield.</summary>
    /// 
    /// <remarks>The ordering of the original collection is respected.</remarks>
    ///
    /// <param name="projection">A function that transforms an element of the collection into a comparable key.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    let inline chunkBy (projection: 'T->'Key when 'Key : equality) (source: '``Collection<'T>``) : '``Collection<'Key * 'Collection<'T>>`` = ChunkBy.Invoke projection source

    /// <summary>Returns a collection that contains all elements of the original collection while the
    /// given predicate returns true, and then returns no further elements.</summary>
    ///
    /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline takeWhile (predicate: 'T->bool) (source: '``Collection<'T>``) : '``Collection<'T>`` = TakeWhile.Invoke predicate source

    /// <summary>Bypasses elements in a collection while the given predicate returns true, and then returns
    /// the remaining elements of the collection.</summary>
    ///
    /// <param name="predicate">A function that evaluates to false when no more items should be skipped.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline skipWhile (predicate: 'T->bool) (source: '``Collection<'T>``) : '``Collection<'T>`` = SkipWhile.Invoke predicate source

    /// <summary>
    /// Generic 'choose' for any collection.
    /// 
    /// A combination of map and filter, `choose` enables you to transform
    /// and select elements at the same time.
    /// </summary>
    ///
    /// <param name="chooser">
    /// A function that is applied to each element in the
    /// collection and returns an option value. When the result is a Some then
    /// the unwrapped value is included in the result collection, otherwise
    /// it is discarded.
    /// </param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.InvalidOperationException">Thrown when the input collection is an Id.</exception>
    let inline choose (chooser: 'T->'U option) (source: '``Collection<'T>``) : '``Collection<'U>`` = Choose.Invoke chooser source

    /// <summary>Returns a collection that contains no duplicate entries according to generic hash and
    /// equality comparisons on the entries.
    /// If an element occurs multiple times in the collection then the later occurrences are discarded.</summary>
    ///
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline distinct (source: '``Collection<'T> when 'T : equality``) : '``Collection<'T> when 'T : equality`` = Distinct.Invoke source

    /// <summary>Returns a collection that contains no duplicate entries according to the 
    /// generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the collection then the later occurrences are discarded.</summary>
    ///
    /// <param name="projection">A function transforming the collection items into comparable keys.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline distinctBy (projection: 'T->'Key when 'Key : equality) (source: '``Collection<'T>``) : '``Collection<'T>`` = DistinctBy.Invoke projection source

    /// Inserts a separator between each element.
    let inline intersperse (sep: 'T) (source: '``Collection<'T>``) : '``Collection<'T>`` = Intersperse.Invoke sep source
    
    /// <summary>Replaces part of the collection with a new part</summary>
    ///
    /// <param name="oldValue">A collection that if part of the source collection
    /// should be replaced with newValue.</param>
    /// <param name="newValue">The collection to replace oldValue with.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The resulting collection with oldValue replaced with newValue.</returns>
    let inline replace (oldValue: 'Collection) (newValue: 'Collection) (source: 'Collection) = Replace.Invoke oldValue newValue source : 'Collection

    /// <summary>Returns a new collection with the elements in reverse order.</summary>
    /// <param name="source">The input collection.</param>
    /// <returns>The reversed collection.</returns>
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline rev (source: '``Collection<'T>``) = Rev.Invoke source : '``Collection<'T>``

    /// <summary>Like fold, but computes on-demand and returns the collection of intermediary and final results.</summary>
    ///
    /// <param name="folder">A function that updates the state with each element from the collection.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The resulting collection of computed states.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline scan (folder: 'State->'T->'State) state (source: '``Collection<'T>``) = Scan.Invoke folder (state: 'State) source : '``Collection<'State>``

    /// <summary>Returns a collection ordered by keys.</summary>
    /// 
    /// <remarks>This function makes no assumption on the ordering of the original collection.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline sort (source: '``Collection<'T> when 'T : comparison``) : '``Collection<'T> when 'T : comparison`` = Sort.Invoke source

    /// <summary>Applies a key-generating function to each element of a collection and returns a collection ordered
    /// by keys. The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
    /// 
    /// <remarks>This function makes no assumption on the ordering of the original collection.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="projection">A function to transform items of the input collection into comparable keys.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline sortBy (projection: 'T->'Key when 'Key : comparison) (source: '``Collection<'T>``) : '``Collection<'T>`` = SortBy.Invoke projection source

    /// <summary>Yields a collection ordered descending by keys.</summary>
    /// 
    /// <remarks>This function makes no assumption on the ordering of the original collection.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    let inline sortByDescending (projection: 'T->'Key when 'Key : comparison) (source: '``Collection<'T>``) : '``Collection<'T>`` = SortByDescending.Invoke projection source

    /// Splits a given ordered collection at each of the given sub-ordered collections
    ///
    /// <example>
    /// <code>
    /// > "asdf" |> split ["s"];;
    /// val it : string list = ["a"; "df"]
    ///
    /// > [1;2;3;4;5;6;7] |> split [ [2;3]; [5] ];;
    /// val it : int list list = [[1]; [4]; [6; 7]]
    /// </code>
    /// </example>
    let inline split (sep: '``'Collection<'OrderedCollection>``) (source: 'OrderedCollection) = Split.Invoke sep source : '``'Collection<'OrderedCollection>``


    #endif

    // Tuple
    
    /// Gets the value of the first component of a tuple.
    let inline item1 tuple = Item1.Invoke tuple

    /// Gets the value of the second component of a tuple.
    let inline item2 tuple = Item2.Invoke tuple

    /// Gets the value of the third component of a tuple.
    let inline item3 tuple = Item3.Invoke tuple

    /// Gets the value of the fourth component of a tuple.
    let inline item4 tuple = Item4.Invoke tuple

    /// Gets the value of the fifth component of a tuple.
    let inline item5 tuple = Item5.Invoke tuple

    /// Maps the first value of a tuple.
    let inline mapItem1 (mapping: 'T -> 'U) (tuple: '``('T * ..)``) = MapItem1.Invoke mapping tuple : '``('U * ..)``

    /// Maps the second value of a tuple.
    let inline mapItem2 (mapping: 'T -> 'U) (tuple: '``('A * 'T * ..)``) = MapItem2.Invoke mapping tuple : '``('A * 'U * ..)``

    /// Maps the third value of a tuple.
    let inline mapItem3 (mapping: 'T -> 'U) (tuple: '``('A * 'B * 'T * ..)``) = MapItem3.Invoke mapping tuple : '``('A * 'B * 'U * ..)``

    /// Maps the fourth value of a tuple.
    let inline mapItem4 (mapping: 'T -> 'U) (tuple: '``('A * 'B * 'C * 'T * ..)``) = MapItem4.Invoke mapping tuple : '``('A * 'B * 'C * 'U * ..)``

    /// Maps the fifth value of a tuple.
    let inline mapItem5 (mapping: 'T -> 'U) (tuple: '``('A * 'B * 'C * 'D * 'T * ..)``) = MapItem5.Invoke mapping tuple : '``('A * 'B * 'C * 'D * 'U * ..)``
    
    
    #if !FABLE_COMPILER
    
    // Converter

    /// Converts using the explicit operator.
    let inline explicit (value: 'T) : 'U = Explicit.Invoke value

    /// Convert from a byte array value, given options of little-endian, and startIndex
    let inline ofBytesWithOptions (isLtEndian: bool) (startIndex: int) (value: byte[]) = OfBytes.Invoke isLtEndian startIndex value

    /// Convert from a byte array value, assuming little-endian
    let inline ofBytes (value: byte[]) = OfBytes.Invoke true 0 value

    /// Convert from a byte array value, assuming big-endian
    let inline ofBytesBE (value: byte[]) = OfBytes.Invoke false 0 value

    /// Convert to a byte array value, assuming little endian
    let inline toBytes value : byte[] = ToBytes.Invoke true value

    /// Convert to a byte array value, assuming big endian
    let inline toBytesBE value : byte[] = ToBytes.Invoke false value
     
    /// Converts to a value from its string representation.
    let inline parse (value: string) = Parse.Invoke value

    /// Converts to a value from its string representation. Returns None if the convertion doesn't succeed.
    let inline tryParse (value: string) = TryParse.Invoke value


    // Numerics

    /// Gets a value that represents the number 1 (one).
    let inline getOne () = One.Invoke ()

    /// A value that represents the 1 element.
    let inline one< ^Num when (One or ^Num) : (static member One : ^Num * One -> ^Num) > : ^Num = One.Invoke ()

    /// Divides one number by another, returns a tuple with the result and the remainder.
    let inline divRem (D: 'T) (d: 'T) : 'T*'T = DivRem.Invoke D d

    /// Gets the smallest possible value.
    let inline getMinValue () = MinValue.Invoke ()

    /// The smallest possible value.
    let inline minValue< ^Num when (MinValue or ^Num) : (static member MinValue : ^Num * MinValue -> ^Num) > : ^Num = MinValue.Invoke ()

    /// Gets the largest possible value.
    let inline getMaxValue () = MaxValue.Invoke ()

    /// The largest possible value.
    let inline maxValue< ^Num when (MaxValue or ^Num) : (static member MaxValue : ^Num * MaxValue -> ^Num) > : ^Num = MaxValue.Invoke ()

    /// Converts from BigInteger to the inferred destination type.
    let inline fromBigInt (x: bigint) : 'Num = FromBigInt.Invoke x

    /// Converts to BigInteger.
    let inline toBigInt (x: 'Integral) : bigint = ToBigInt.Invoke x

    /// Gets the pi number.
    let inline getPi () : 'Floating = Pi.Invoke ()

    /// The pi number.
    let inline pi< ^Num when (Pi or ^Num) : (static member Pi : ^Num * Pi -> ^Num) > : ^Num = Pi.Invoke ()

    /// Additive inverse of the number.
    let inline negate (x: 'Num) : 'Num = x |> TryNegate.Invoke |> function Ok x -> x | Error e -> raise e

    /// Additive inverse of the number.
    /// Works also for unsigned types (Throws an exception if there is no inverse).
    let inline negate' (x: 'Num) : 'Num = x |> TryNegate'.Invoke |> function Ok x -> x | Error e -> raise e

    /// Additive inverse of the number.
    /// Works also for unsigned types (Returns none if there is no inverse).
    let inline tryNegate' (x: 'Num) : 'Num option = TryNegate'.Invoke x |> function Ok x -> Some x | Error _ -> None

    /// Subtraction between two numbers. Throws an error if the result is negative on unsigned types.
    let inline subtract (x: 'Num) (y: 'Num) : 'Num = Subtract.Invoke x y

    /// Subtraction between two numbers. Returns None if the result is negative on unsigned types.
    let inline trySubtract (x: 'Num) (y: 'Num) : 'Num option = y |> TrySubtract.Invoke x |> function Ok x -> Some x | Error _ -> None

    /// Division between two numbers. If the numbers are not divisible throws an error.
    let inline div (dividend: 'Num) (divisor: 'Num) : 'Num = Divide.Invoke dividend divisor
    
    /// Division between two numbers. Returns None if the numbers are not divisible.
    let inline tryDiv (dividend: 'Num) (divisor: 'Num) : 'Num option = divisor |> TryDivide.Invoke dividend |> function Ok x -> Some x | Error _ -> None

    /// Square root of a number of any type. Throws an exception if there is no square root.
    let inline sqrt x = x |> Sqrt.Invoke

    /// Square root of a number of any type. Returns None if there is no square root.
    let inline trySqrt x = x |> TrySqrt.Invoke |> function Ok x -> Some x | Error _ -> None

    /// Square root of an integral number.
    let inline isqrt (x: 'Integral) : 'Integral = x |> TrySqrtRem.Invoke |> function Ok (x, _) -> x | Error e -> raise e

    /// Square root of an integral number.
    let inline sqrtRem (x: 'Integral) : 'Integral*'Integral = x |> TrySqrtRem.Invoke |> function Ok x -> x | Error e -> raise e

    /// <summary>Sign of the given number
    /// <para/>   Rule: signum x * abs x = x </summary>
    /// <param name="value">The input value.</param>
    /// <returns>-1, 0, or 1 depending on the sign of the input.</returns>
    let inline signum (value: 'Num) : 'Num = Signum.Invoke value

    /// <summary>Sign of the given number
    ///           Works also for unsigned types. 
    /// <para/>   Rule: signum x * abs x = x </summary>
    /// <param name="value">The input value.</param>
    /// <returns>-1, 0, or 1 depending on the sign of the input.</returns>
    let inline signum' (value: 'Num) : 'Num = Signum'.Invoke value

    /// <summary> Gets the absolute value of the given number.
    /// <para/>   Rule: signum x * abs x = x </summary>
    /// <param name="value">The input value.</param>
    /// <returns>The absolute value of the input.</returns>
    let inline abs (value: 'Num) : 'Num = Abs.Invoke value

    /// <summary> Gets the absolute value of the given number.
    ///           Works also for unsigned types. 
    /// <para/>   Rule: signum x * abs x = x </summary>
    /// <param name="value">The input value.</param>
    /// <returns>The absolute value of the input.</returns>
    let inline abs' (value: 'Num) : 'Num = Abs'.Invoke value



    // Additional functions

    /// Reduces using alternative operator `<|>`.
    let inline choice (x: '``Foldable<'Alternative<'T>>``) = Choice.Invoke x : '``Alternative<'T>>``

    /// Generic filter operation for MonadZero. It returns all values satisfying the predicate, if the predicate returns false will use the empty value.
    let inline mfilter (predicate: 'T->bool) (m: '``MonadZero<'T>``) : '``MonadZero<'T>`` = m >>= fun a -> if predicate a then result a else Empty.Invoke ()

    /// Folds the sum of all monoid elements in the Foldable.
    let inline sum (x: '``Foldable<'Monoid>``) : 'Monoid = fold (++) (getZero () : 'Monoid) x

    /// Converts using the implicit operator. 
    let inline implicit (x: ^T) = ((^R or ^T) : (static member op_Implicit : ^T -> ^R) x) : ^R

    /// An active recognizer for a generic value parser.
    let inline (|Parse|_|) str : 'T option = tryParse str

    #endif

    /// Safely dispose a resource (includes null-checking).
    let dispose (resource: System.IDisposable) = match resource with null -> () | x -> x.Dispose ()

    #if !FABLE_COMPILER

    /// <summary>Additional operators for Arrows related functions which shadows some F# operators for bitwise functions.</summary>
    module Arrows =
    
        /// Right-to-left morphism composition.
        let inline (<<<) f g = catComp f g
        
        /// Left-to-right morphism composition.
        let inline (>>>) f g = catComp g f
        
        /// Sends the input to both argument arrows and combine their output. Also known as fanout.
        let inline (&&&) (f: '``Arrow<'T,'U1>``) (g: '``Arrow<'T,'U2>``) : '``Arrow<'T,('U1 * 'U2)>`` = Fanout.Invoke f g

        /// Splits the input between the two argument arrows and merge their outputs. Also known as fanin.
        let inline (|||) (f: '``ArrowChoice<'T,'V>``) (g: '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` = Fanin.Invoke f g

    #endif