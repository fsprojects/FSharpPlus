namespace FSharpPlus
    
    /// Generic functions and operators
    module Operators =
        
        /// <summary>Creates a new function with first two arguments flipped.</summary>
        /// <category index="0">Common Combinators</category>
        val inline flip: f: ('V -> 'T -> 'Result) -> x: 'T -> y: 'V -> 'Result
        
        /// <summary> Creates a constant function.</summary>
        /// <param name="k">The constant value.</param>
        /// <returns>The constant value function.</returns>
        /// <category index="0">Common Combinators</category>
        val inline konst: k: 'T -> 'Ignored -> 'T
        
        /// <summary>Takes a function expecting a tuple of two elements and returns a function expecting two curried arguments.</summary>
        /// <category index="0">Common Combinators</category>
        val inline curry:
          f: ('T1 * 'T2 -> 'Result) -> x: 'T1 -> y: 'T2 -> 'Result
        
        /// <summary>
        /// Takes a function expecting two curried arguments and returns a function expecting a tuple of two elements. Same as (&lt;||).
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline uncurry:
          f: ('T1 -> 'T2 -> 'Result) -> x: 'T1 * y: 'T2 -> 'Result
        
        /// <summary>
        /// Takes a function expecting a tuple of any N number of elements and returns a function expecting N curried arguments.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline curryN:
          f: ( ^T1 * ^T2 * ... * ^Tn -> 'Result) -> t: 'T1
            -> 'T2 -> ... -> 'Tn -> 'Result
            when (Control.Curry or  ^T1 * ^T2 * ... * ^Tn) :
                   (static member Curry:
                       ^T1 * ^T2 * ... * ^Tn * Control.Curry
                        -> (( ^T1 * ^T2 * ... * ^Tn -> 'Result) -> 'T1
                              -> 'T2 -> ... -> 'Tn -> 'Result))
        
        /// <summary>
        /// Takes a function expecting any N number of curried arguments and returns a function expecting a tuple of N elements.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline uncurryN:
          f: ('T1 -> 'T2 -> ... -> 'Tn -> 'Result) -> t:  ^T1 * ^T2 * ... * ^Tn
            -> 'Result
            when (Control.Uncurry or  ^T1 * ^T2 * ... * ^Tn) :
                   (static member Uncurry:
                       ^T1 * ^T2 * ... * ^Tn * Control.Uncurry
                        -> (('T1 -> 'T2 -> ... -> 'Tn -> 'Result) -> 'Result))
        
        /// <summary>
        /// Turn a function into something that looks like an operator. See <a href="../operators-common.html#Functions-as-operators">Functions as operators</a>.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline (</) : x: 'a -> (('a -> 'b) -> 'b)
        
        /// <summary>
        /// Turn a function into something that looks like an operator. See <a href="../operators-common.html#Functions-as-operators">Functions as operators</a>.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline (/>) : x: ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
        
        /// <summary>
        /// Executes a side-effect function and returns the original input value.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val tap: f: ('T -> unit) -> x: 'T -> 'T
        
        /// <summary>Extracts a value from either side of a Result.</summary>
        /// <category index="0">Common Combinators</category>
        /// <param name="fOk">Function to be applied to source, if it contains an Ok value.</param>
        /// <param name="fError">Function to be applied to source, if it contains an Error value.</param>
        /// <param name="source">The source value, containing an Ok or an Error.</param>
        /// <returns>The result of applying either functions.</returns>
        val inline either:
          fOk: ('T -> 'U) -> fError: ('Error -> 'U) -> source: Result<'T,'Error>
            -> 'U
        
        /// <summary>
        /// Takes a function, a default value and a option value. If the option value is None, the function returns the default value.
        /// Otherwise, it applies the function to the value inside Some and returns the result.
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline option: f: ('a -> 'b) -> n: 'b -> _arg1: 'a option -> 'b
        
        /// <summary>
        /// Tuple two arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple2: t1: 'T1 -> t2: 'T2 -> 'T1 * 'T2
        
        /// <summary>
        /// Tuple three arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple3: t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> 'T1 * 'T2 * 'T3
        
        /// <summary>
        /// Tuple four arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple4:
          t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> t4: 'T4 -> 'T1 * 'T2 * 'T3 * 'T4
        
        /// <summary>
        /// Tuple five arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple5:
          t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> t4: 'T4 -> t5: 'T5
            -> 'T1 * 'T2 * 'T3 * 'T4 * 'T5
        
        /// Tuple six arguments
        /// <category index="0">Common Combinators</category>
        val inline tuple6:
          t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> t4: 'T4 -> t5: 'T5 -> t6: 'T6
            -> 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6
        
        /// <summary>
        /// Tuple seven arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple7:
          t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> t4: 'T4 -> t5: 'T5 -> t6: 'T6
          -> t7: 'T7 -> 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7
        
        /// <summary>
        /// Tuple eight arguments
        /// </summary>
        /// <category index="0">Common Combinators</category>
        val inline tuple8:
          t1: 'T1 -> t2: 'T2 -> t3: 'T3 -> t4: 'T4 -> t5: 'T5 -> t6: 'T6
          -> t7: 'T7 -> t8: 'T8 -> 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8
        
        /// <summary>Lifts a function into a Functor.</summary>
        /// <category index="1">Functor</category>
        val inline map:
          f: ('T -> 'U) -> x:  ^Functor<'T> ->  ^Functor<'U>
            when (Control.Map or  ^Functor<'T> or  ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> 'U)) * Control.Map
                        ->  ^Functor<'U>)
        
        /// <summary>Lifts a function into a Functor. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        val inline (<!>) :
          f: ('T -> 'U) -> x:  ^Functor<'T> ->  ^Functor<'U>
            when (Control.Map or  ^Functor<'T> or  ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> 'U)) * Control.Map
                        ->  ^Functor<'U>)
        
        /// <summary>
        /// Lifts a function into a Functor. Same as map.
        /// </summary>
        /// <category index="1">Functor</category>
        val inline (<<|) :
          f: ('T -> 'U) -> x:  ^Functor<'T> ->  ^Functor<'U>
            when (Control.Map or  ^Functor<'T> or  ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> 'U)) * Control.Map
                        ->  ^Functor<'U>)
        
        /// <summary>
        /// Lifts a function into a Functor. Same as map but with flipped arguments.
        /// To be used in pipe-forward style expressions
        /// </summary>
        /// <category index="1">Functor</category>
        val inline (|>>) :
          x:  ^Functor<'T> -> f: ('T -> 'U) ->  ^Functor<'U>
            when (Control.Map or  ^Functor<'T> or  ^Functor<'U>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> 'U)) * Control.Map
                        ->  ^Functor<'U>)
        
        /// <summary>
        /// Like map but ignoring the results.
        /// </summary>
        /// <category index="1">Functor</category>
        val inline iter:
          action: ('T -> unit) -> source:  ^Functor<'T> -> unit
            when (Control.Iterate or  ^Functor<'T>) :
                   (static member Iterate:  ^Functor<'T> * ('T -> unit) -> unit)
        
        /// <summary>
        /// Un-zips (un-tuple) two functors.
        /// </summary>
        /// <category index="1">Functor</category>
        val inline unzip:
          source:  ^Functor<'T1 * 'T2> -> 'Functor<'T1> * 'Functor<'T2>
            when (Control.Unzip or  ^Functor<'T1 * 'T2> or
                  ('Functor<'T1> * 'Functor<'T2>)) :
                   (static member Unzip:
                      ( ^Functor<'T1 * 'T2> * ('Functor<'T1> * 'Functor<'T2>)) *
                      Control.Unzip -> 'Functor<'T1> * 'Functor<'T2>)
        
        /// <summary>
        /// Zips (tuple) two functors.
        /// </summary>
        /// <remarks>
        /// For collections, if one collection is shorter, excess elements are discarded from the right end of the longer collection. 
        /// </remarks>
        /// <category index="1">Functor</category>
        val inline zip:
          source1:  ^ZipFunctor<'T1> -> source2:  ^ZipFunctor<'T2>
            ->  ^ZipFunctor<'T1 * 'T2>
            when (Control.Zip or  ^ZipFunctor<'T1> or  ^ZipFunctor<'T2> or
                   ^ZipFunctor<'T1 * 'T2>) :
                   (static member Zip:
                      ( ^ZipFunctor<'T1> *  ^ZipFunctor<'T2> *
                        ^ZipFunctor<'T1 * 'T2>) * Control.Zip
                        ->  ^ZipFunctor<'T1 * 'T2>)
        
        /// <summary>
        /// Lifts a value into a Functor. Same as return in Computation Expressions.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline result:
          x: 'T ->  ^Functor<'T>
            when (Control.Return or  ^Functor<'T>) :
                   (static member Return:
                       ^Functor<'T> * Control.Return -> ('T ->  ^Functor<'T>))
        
        /// <summary>
        /// Apply a lifted argument to a lifted function: f &lt;*&gt; arg
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline (<*>) :
          f:  ^Applicative<'T -> 'U> -> x:  ^Applicative<'T>
            ->  ^Applicative<'U>
            when (Control.Apply or  ^Applicative<'T -> 'U> or  ^Applicative<'T> or
                   ^Applicative<'U>) :
                   (static member ``<*>`` :
                       ^Applicative<'T -> 'U> *  ^Applicative<'T> *
                       ^Applicative<'U> * Control.Apply ->  ^Applicative<'U>)
        
        /// <summary>
        /// Applies 2 lifted arguments to a non-lifted function. Equivalent to map2 in non list-like types.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline lift2:
          f: ('T -> 'U -> 'V) -> x:  ^Applicative<'T> -> y:  ^Applicative<'U>
            ->  ^Applicative<'V>
            when (Control.Lift2 or  ^Applicative<'T> or  ^Applicative<'U> or
                   ^Applicative<'V>) :
                   (static member Lift2:
                      ('T -> 'U -> 'V) * ( ^Applicative<'T> *  ^Applicative<'U>) *
                      Control.Lift2 ->  ^Applicative<'V>)
        
        [<System.Obsolete ("Use lift2 instead.")>]
        val inline liftA2:
          f: ('T -> 'U -> 'V) -> x:  ^Applicative<'T> -> y:  ^Applicative<'U>
            ->  ^Applicative<'V>
            when (Control.Lift2 or  ^Applicative<'T> or  ^Applicative<'U> or
                   ^Applicative<'V>) :
                   (static member Lift2:
                      ('T -> 'U -> 'V) * ( ^Applicative<'T> *  ^Applicative<'U>) *
                      Control.Lift2 ->  ^Applicative<'V>)
        
        /// <summary>
        /// Applies 3 lifted arguments to a non-lifted function. Equivalent to map3 in non list-like types.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline lift3:
          f: ('T -> 'U -> 'V -> 'W) -> x:  ^Applicative<'T>
          -> y:  ^Applicative<'U> -> z:  ^Applicative<'V> ->  ^Applicative<'W>
            when (Control.Lift3 or  ^Applicative<'T> or  ^Applicative<'U> or
                   ^Applicative<'V> or  ^Applicative<'W>) :
                   (static member Lift3:
                      ('T -> 'U -> 'V -> 'W) *
                      ( ^Applicative<'T> *  ^Applicative<'U> *  ^Applicative<'V>) *
                      Control.Lift3 ->  ^Applicative<'W>)
        
        /// <summary>
        /// Sequences two applicatives left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline ( *> ) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'U> ->  ^Applicative<'U>
            when (Control.Map or  ^Applicative<'T> or  ^Applicative<'U->'U>) :
                   (static member Map:
                      ( ^Applicative<'T> * ('T -> 'U -> 'U)) * Control.Map
                        ->  ^Applicative<'U->'U>) and
                 (Control.Apply or  ^Applicative<'U->'U> or  ^Applicative<'U>) :
                   (static member ``<*>`` :
                       ^Applicative<'U->'U> *  ^Applicative<'U> *
                       ^Applicative<'U> * Control.Apply ->  ^Applicative<'U>)
        
        /// <summary>
        /// Sequences two applicatives left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline ( <* ) :
          x:  ^Applicative<'U> -> y:  ^Applicative<'T> ->  ^Applicative<'U>
            when (Control.Apply or  ^Applicative<'T->'U> or  ^Applicative<'T> or
                   ^Applicative<'U>) :
                   (static member ``<*>`` :
                       ^Applicative<'T->'U> *  ^Applicative<'T> *
                       ^Applicative<'U> * Control.Apply ->  ^Applicative<'U>) and
                 (Control.Map or  ^Applicative<'U> or  ^Applicative<'T->'U>) :
                   (static member Map:
                      ( ^Applicative<'U> * ('U -> 'T -> 'U)) * Control.Map
                        ->  ^Applicative<'T->'U>)
        
        /// <summary>
        /// Apply a lifted argument to a lifted function (flipped): arg &lt;**&gt; f
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline (<**>) :
          x:  ^Applicative<'T> -> ( ^Applicative<'T -> 'U> ->  ^Applicative<'U>)
            when (Control.Apply or  ^Applicative<'T -> 'U> or  ^Applicative<'T> or
                   ^Applicative<'U>) :
                   (static member ``<*>`` :
                       ^Applicative<'T -> 'U> *  ^Applicative<'T> *
                       ^Applicative<'U> * Control.Apply ->  ^Applicative<'U>)
        
        [<System.Obsolete ("Use opt instead.")>]
        val inline optional:
          v:  ^a ->  ^b
            when (Control.Map or  ^a or  ^b) :
                   (static member Map:
                      ( ^a * ('d -> 'd option)) * Control.Map ->  ^b) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c option ->  ^b)) and
                 (Control.Append or  ^b) :
                   (static member ``<|>`` :  ^b *  ^b * Control.Append ->  ^b)
        
        /// <summary>
        /// Transforms an alternative value (which has the notion of success/failure) to an alternative
        /// that always succeed, wrapping the original value into an option to signify success/failure of the original alternative.
        /// </summary>
        /// <category index="2">Applicative</category>
        val inline opt:
          v:  ^Alternative<'T> ->  ^Alternative<option<'T>>
            when (Control.Map or  ^Alternative<'T> or  ^Alternative<option<'T>>) :
                   (static member Map:
                      ( ^Alternative<'T> * ('T -> 'T option)) * Control.Map
                        ->  ^Alternative<option<'T>>) and
                 (Control.Append or  ^Alternative<option<'T>>) :
                   (static member ``<|>`` :
                       ^Alternative<option<'T>> *  ^Alternative<option<'T>> *
                      Control.Append ->  ^Alternative<option<'T>>) and
                 (Control.Return or  ^Alternative<option<'T>>) :
                   (static member Return:
                       ^Alternative<option<'T>> * Control.Return
                        -> ('T option ->  ^Alternative<option<'T>>))
        
        /// <summary>
        /// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
        /// </summary>
        /// <category index="3">Monad</category>
        val inline bind:
          f: ('T ->  ^Monad<'U>) -> x:  ^Monad<'T> ->  ^Monad<'U>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'U>) ->  ^Monad<'U>)
        
        /// <summary>
        /// Takes a monadic value and a function from a plain type to a monadic value, and returns a new monadic value.
        /// </summary>
        /// <category index="3">Monad</category>
        val inline (>>=) :
          x:  ^Monad<'T> -> f: ('T ->  ^Monad<'U>) ->  ^Monad<'U>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'U>) ->  ^Monad<'U>)
        
        /// <summary>
        /// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
        /// </summary>
        /// <category index="3">Monad</category>
        val inline (=<<) :
          f: ('T ->  ^Monad<'U>) -> x:  ^Monad<'T> ->  ^Monad<'U>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'U>) ->  ^Monad<'U>)
        
        /// <summary>
        /// Composes left-to-right two monadic functions (Kleisli composition).
        /// </summary>
        /// <category index="3">Monad</category>
        val inline (>=>) :
          f: ('T ->  ^Monad<'U>) -> g: ('U ->  ^Monad<'V>) -> x: 'T
            ->  ^Monad<'V>
            when (Control.Bind or  ^Monad<'U> or  ^Monad<'V>) :
                   (static member (>>=) :
                       ^Monad<'U> * ('U ->  ^Monad<'V>) ->  ^Monad<'V>)
        
        /// <summary>
        /// Composes right-to-left two monadic functions (Kleisli composition).
        /// </summary>
        /// <category index="3">Monad</category>
        val inline (<=<) :
          g: ('b ->  ^Monad<'V>) -> f: ('T ->  ^Monad<'U>) -> x: 'T
            ->  ^Monad<'V>
            when (Control.Bind or  ^Monad<'U> or  ^Monad<'V>) :
                   (static member (>>=) :
                       ^Monad<'U> * ('b ->  ^Monad<'V>) ->  ^Monad<'V>)
        
        /// <summary>
        /// Flattens two layers of monadic information into one.
        /// </summary>
        /// <category index="3">Monad</category>
        val inline join:
          x:  ^Monad<Monad<'T>> ->  ^Monad<'T>
            when (Control.Join or  ^Monad<Monad<'T>> or  ^Monad<'T>) :
                   (static member Join:
                       ^Monad<Monad<'T>> *  ^Monad<'T> * Control.Join
                        ->  ^Monad<'T>)
        
        /// <summary>
        /// Equivalent to map but only for Monads.
        /// </summary>
        /// <category index="3">Monad</category>
        val inline liftM:
          f: ('T -> 'U) -> m1:  ^Monad<'T> ->  ^Monad<'U>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'U>) ->  ^Monad<'U>) and
                 (Control.Return or  ^Monad<'U>) :
                   (static member Return:
                       ^Monad<'U> * Control.Return -> ('U ->  ^Monad<'U>))
        
        /// <summary>
        /// Gets a value that represents the 0 element of a Monoid.
        /// </summary>
        /// <category index="4">Monoid</category>
        val inline getZero:
          unit ->  ^Monoid
            when (Control.Zero or  ^Monoid) :
                   (static member Zero:  ^Monoid * Control.Zero ->  ^Monoid)
        
        /// <summary>
        /// A value that represents the 0 element of a Monoid.
        /// </summary>
        /// <category index="4">Monoid</category>
        val inline zero< ^Monoid
                          when (Control.Zero or  ^Monoid) :
                                 (static member Zero:
                                     ^Monoid * Control.Zero ->  ^Monoid)> :
           ^Monoid
            when (Control.Zero or  ^Monoid) :
                   (static member Zero:  ^Monoid * Control.Zero ->  ^Monoid)
        
        /// <summary>
        /// Combines two monoids in one.
        /// </summary>
        /// <category index="4">Monoid</category>
        val inline (++) :
          x:  ^Monoid -> y:  ^Monoid ->  ^Monoid
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        /// <summary>
        /// Combines two monoids in one.
        /// </summary>
        /// <category index="4">Monoid</category>
        val inline plus:
          x:  ^Monoid -> y:  ^Monoid ->  ^Monoid
            when (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid)
        
        module Seq =
            
            /// <summary>
            /// Folds all values in the sequence using the monoidal addition.
            /// </summary>
            /// <category index="4">Monoid</category>
            val inline sum:
              x: seq< ^Monoid> ->  ^Monoid
                when (Control.Sum or seq< ^Monoid> or  ^Monoid) :
                       (static member Sum:
                          seq< ^Monoid> *  ^Monoid * Control.Sum ->  ^Monoid)
        
        /// <summary>
        /// Gets a functor representing the empty value.
        /// </summary>
        /// <category index="5">Alternative/Monadplus/Arrowplus</category>
        val inline getEmpty:
          unit ->  ^Functor<'T>
            when (Control.Empty or  ^Functor<'T>) :
                   (static member Empty:
                       ^Functor<'T> * Control.Empty ->  ^Functor<'T>)
        
        /// <summary>
        /// A functor representing the empty value.
        /// </summary>
        /// <category index="5">Alternative/Monadplus/Arrowplus</category>
        [<GeneralizableValue>]
        val inline empty< ^Functor<'T>
                           when (Control.Empty or  ^Functor<'T>) :
                                  (static member Empty:
                                      ^Functor<'T> * Control.Empty
                                       ->  ^Functor<'T>)> :
           ^Functor<'T>
            when (Control.Empty or  ^Functor<'T>) :
                   (static member Empty:
                       ^Functor<'T> * Control.Empty ->  ^Functor<'T>)
        
        /// <summary>
        /// Combines two Alternatives
        /// </summary>
        /// <category index="5">Alternative/Monadplus/Arrowplus</category>
        val inline (<|>) :
          x:  ^Functor<'T> -> y:  ^Functor<'T> ->  ^Functor<'T>
            when (Control.Append or  ^Functor<'T>) :
                   (static member ``<|>`` :
                       ^Functor<'T> *  ^Functor<'T> * Control.Append
                        ->  ^Functor<'T>)
        
        /// <summary>
        /// Conditional failure of Alternative computations.
        /// If true it lifts the unit value, else it returns empty.
        ///
        /// Common uses of guard include conditionally signaling an error in an error monad and conditionally rejecting the current choice in an Alternative-based parser.
        /// </summary>
        /// <category index="5">Alternative/Monadplus/Arrowplus</category>
        val inline guard:
          x: bool ->  ^MonadPlus<unit>
            when (Control.Return or  ^MonadPlus<unit>) :
                   (static member Return:
                       ^MonadPlus<unit> * Control.Return
                        -> (unit ->  ^MonadPlus<unit>)) and
                 (Control.Empty or  ^MonadPlus<unit>) :
                   (static member Empty:
                       ^MonadPlus<unit> * Control.Empty ->  ^MonadPlus<unit>)
        
        /// <summary>
        /// Maps over the input.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline contramap:
          f: ('U -> 'T) -> x:  ^Contravariant<'T> ->  ^Contravariant<'U>
            when (Control.Contramap or  ^Contravariant<'T> or
                   ^Contravariant<'U>) :
                   (static member Contramap:
                       ^Contravariant<'T> * ('U -> 'T) * Control.Contramap
                        ->  ^Contravariant<'U>)
        
        /// <summary>
        /// Maps over both arguments of the Bifunctor at the same time.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline bimap:
          f: ('T -> 'U) -> g: ('V -> 'W) -> source:  ^Bifunctor<'T,'V>
            ->  ^Bifunctor<'U,'W>
            when (Control.Bimap or  ^Bifunctor<'T,'V> or  ^Bifunctor<'U,'W>) :
                   (static member Bimap:
                       ^Bifunctor<'T,'V> * ('T -> 'U) * ('V -> 'W) *
                      Control.Bimap ->  ^Bifunctor<'U,'W>)
        
        /// <summary>
        /// Maps covariantly over the first argument of the Bifunctor.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline first:
          f: ('T -> 'V) -> source:  ^Bifunctor<'T,'V> ->  ^Bifunctor<'U,'V>
            when (Control.MapFirst or  ^Bifunctor<'T,'V> or  ^Bifunctor<'U,'V>) :
                   (static member First:
                       ^Bifunctor<'T,'V> * ('T -> 'V) * Control.MapFirst
                        ->  ^Bifunctor<'U,'V>)
        
        /// <summary>
        /// Maps covariantly over the second argument of the Bifunctor.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline second:
          f: ('V -> 'W) -> source:  ^Bifunctor<'T,'V> ->  ^Bifunctor<'T,'W>
            when (Control.Map or  ^Bifunctor<'T,'V> or  ^Bifunctor<'T,'W>) :
                   (static member Map:
                      ( ^Bifunctor<'T,'V> * ('V -> 'W)) * Control.Map
                        ->  ^Bifunctor<'T,'W>)
        
        /// <summary>
        /// Maps over both arguments at the same time of a Profunctor.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline dimap:
          f: ('A -> 'B) -> g: ('C -> 'D) -> source:  ^Profunctor<'B,'C>
            ->  ^Profunctor<'A,'D>
            when (Control.Dimap or  ^Profunctor<'B,'C> or  ^Profunctor<'A,'D>) :
                   (static member Dimap:
                       ^Profunctor<'B,'C> * ('A -> 'B) * ('C -> 'D) *
                      Control.Dimap ->  ^Profunctor<'A,'D>)
        
        /// <summary>
        /// Maps over the left part of a Profunctor.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline lmap:
          f: ('A -> 'B) -> source:  ^Profunctor<'B,'C> ->  ^Profunctor<'A,'C>
            when (Control.Contramap or  ^Profunctor<'B,'C> or
                   ^Profunctor<'A,'C>) :
                   (static member Contramap:
                       ^Profunctor<'B,'C> * ('A -> 'B) * Control.Contramap
                        ->  ^Profunctor<'A,'C>)
        
        /// <summary>
        /// Maps over the right part of a Profunctor.
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline rmap:
          f: ('C -> 'D) -> source:  ^Profunctor<'B,'C> ->  ^Profunctor<'B,'D>
            when (Control.Map or  ^Profunctor<'B,'C> or  ^Profunctor<'B,'D>) :
                   (static member Map:
                      ( ^Profunctor<'B,'C> * ('C -> 'D)) * Control.Map
                        ->  ^Profunctor<'B,'D>)
        
        /// <summary>
        /// Maps a pair of functions over an Invariant Functor
        /// </summary>
        /// <category index="6">Contravariant/Bifunctor/Profunctor/Invariant</category>
        val inline invmap:
          f: ('T -> 'U) -> g: ('U -> 'T) -> source:  ^InvariantFunctor<'T>
            -> 'InvariantFunctor<'U>
            when  ^InvariantFunctor<'T> :
                   (static member Invmap:
                       ^InvariantFunctor<'T> * ('T -> 'U) * ('U -> 'T)
                        -> 'InvariantFunctor<'U>)
        
        /// <summary>
        /// Gets the identity morphism.
        /// </summary>
        /// <category index="7">Category</category>
        val inline getCatId:
          unit ->  ^Category<'T,'T>
            when (Control.Id or  ^Category<'T,'T>) :
                   (static member Id:
                       ^Category<'T,'T> * Control.Id ->  ^Category<'T,'T>)
        
        /// <summary>
        /// The identity morphism.
        /// </summary>
        /// <category index="7">Category</category>
        val inline catId< ^Category<'T,'T>
                           when (Control.Id or  ^Category<'T,'T>) :
                                  (static member Id:
                                      ^Category<'T,'T> * Control.Id
                                       ->  ^Category<'T,'T>)> :
           ^Category<'T,'T>
            when (Control.Id or  ^Category<'T,'T>) :
                   (static member Id:
                       ^Category<'T,'T> * Control.Id ->  ^Category<'T,'T>)
        
        /// <summary>
        /// Right-to-left morphism composition.
        /// </summary>
        /// <category index="7">Category</category>
        val inline catComp:
          f:  ^Category<'U,'V> -> g: 'Category<'T,'U> -> 'Category<'T,'V>
            when (Control.Comp or  ^Category<'U,'V>) :
                   (static member ``<<<`` :
                       ^Category<'U,'V> * 'Category<'T,'U> * Control.Comp *
                      Control.Comp -> 'Category<'T,'V>)
        
        /// <summary>
        /// Lifts a function to an arrow.
        /// </summary>
        /// <category index="8">Arrow</category>
        val inline arr:
          f: ('T -> 'U) ->  ^Arrow<'T,'U>
            when (Control.Arr or  ^Arrow<'T,'U>) :
                   (static member Arr:
                      ('T -> 'U) *  ^Arrow<'T,'U> * Control.Arr
                        ->  ^Arrow<'T,'U>)
        
        /// <summary>
        /// Sends the first component of the input through the argument arrow, and copy the rest unchanged to the output.
        /// </summary>
        /// <category index="8">Arrow</category>
        val inline arrFirst:
          f:  ^Arrow<'T,'U> ->  ^Arrow<('T * 'V),('U * 'V)>
            when (Control.ArrFirst or  ^Arrow<'T,'U> or
                   ^Arrow<('T * 'V),('U * 'V)>) :
                   (static member First:
                       ^Arrow<'T,'U> *  ^Arrow<('T * 'V),('U * 'V)> *
                      Control.ArrFirst ->  ^Arrow<('T * 'V),('U * 'V)>)
        
        /// <summary>
        /// Sends the second component of the input through the argument arrow, and copy the rest unchanged to the output.
        /// </summary>
        /// <category index="8">Arrow</category>
        val inline arrSecond:
          f:  ^Arrow<'T,'U> ->  ^Arrow<('V * 'T),('V * 'U)>
            when (Control.ArrSecond or  ^Arrow<'T,'U> or
                   ^Arrow<('V * 'T),('V * 'U)>) :
                   (static member Second:
                       ^Arrow<'T,'U> *  ^Arrow<('V * 'T),('V * 'U)> *
                      Control.ArrSecond ->  ^Arrow<('V * 'T),('V * 'U)>)
        
        /// <summary>
        /// Splits the input between the two argument arrows and combine their output. Note that this is in general not a functor.
        /// </summary>
        /// <category index="8">Arrow</category>
        val inline ( *** ) :
          f: 'Arrow<'T1,'U1> -> g: 'Arrow<'T2,'U2>
            ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>
            when (Control.ArrCombine or  ^Arrow<('T1 * 'T2),('U1 * 'U2)>) :
                   (static member ``***`` :
                      'Arrow<'T1,'U1> * 'Arrow<'T2,'U2> *
                       ^Arrow<('T1 * 'T2),('U1 * 'U2)> * Control.ArrCombine
                        ->  ^Arrow<('T1 * 'T2),('U1 * 'U2)>)
        
        /// <summary>
        /// Sends the input to both argument arrows and combine their output. Also known as the (&amp;&amp;&amp;) operator.
        /// </summary>
        /// <category index="8">Arrow</category>
        val inline fanout:
          f: 'Arrow<'T,'U1> -> g: 'Arrow<'T,'U2> ->  ^Arrow<'T,('U1 * 'U2)>
            when (Control.Fanout or  ^Arrow<'T,('U1 * 'U2)>) :
                   (static member ``&&&`` :
                      'Arrow<'T,'U1> * 'Arrow<'T,'U2> *  ^Arrow<'T,('U1 * 'U2)> *
                      Control.Fanout ->  ^Arrow<'T,('U1 * 'U2)>)
        
        /// <summary>
        /// Splits the input between the two argument arrows and merge their outputs. Also known as the (|||) operator.
        /// </summary>
        /// <category index="9">Arrow Choice</category>
        val inline fanin:
          f: 'ArrowChoice<'T,'V> -> g: 'ArrowChoice<'U,'V>
            ->  ^ArrowChoice<Choice<'U,'T>,'V>
            when (Control.Fanin or  ^ArrowChoice<Choice<'U,'T>,'V>) :
                   (static member ``|||`` :
                      'ArrowChoice<'T,'V> * 'ArrowChoice<'U,'V> *
                       ^ArrowChoice<Choice<'U,'T>,'V> * Control.Fanin
                        ->  ^ArrowChoice<Choice<'U,'T>,'V>)
        
        /// <summary>
        /// Splits the input between both argument arrows, retagging and merging their outputs. Note that this is in general not a functor.
        /// </summary>
        /// <category index="9">Arrow Choice</category>
        val inline (+++) :
          f: 'ArrowChoice<'T1,'U1> -> g: 'ArrowChoice<'T2,'U2>
            ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>
            when (Control.AcMerge or
                   ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>) :
                   (static member ``+++`` :
                      'ArrowChoice<'T1,'U1> * 'ArrowChoice<'T2,'U2> *
                       ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>> *
                      Control.AcMerge
                        ->  ^ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>)
        
        /// <summary>
        /// Feeds marked inputs through the left argument arrow, passing the rest through unchanged to the output.
        /// </summary>
        /// <category index="9">Arrow Choice</category>
        val inline left:
          f:  ^ArrowChoice<'T,'U> ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>
            when (Control.AcLeft or  ^ArrowChoice<'T,'U> or
                   ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>) :
                   (static member Left:
                       ^ArrowChoice<'T,'U> *
                       ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>> *
                      Control.AcLeft
                        ->  ^ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>)
        
        /// <summary>
        /// Feeds marked inputs through the right argument arrow, passing the rest through unchanged to the output.
        /// </summary>
        /// <category index="9">Arrow Choice</category>
        val inline right:
          f:  ^ArrowChoice<'T,'U> ->  ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>
            when (Control.AcRight or  ^ArrowChoice<'T,'U> or
                   ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>) :
                   (static member Right:
                       ^ArrowChoice<'T,'U> *
                       ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>> *
                      Control.AcRight
                        ->  ^ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>)
        
        /// <summary>
        /// Applies an arrow produced as the output of some previous computation to an input, producing its output as the output of app.
        /// </summary>
        /// <category index="10">Arrow Apply</category>
        val inline getApp:
          unit ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
            when (Control.App or  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>) :
                   (static member App:
                       ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> * Control.App
                        ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)
        
        /// <summary>
        /// Applies an arrow produced as the output of some previous computation to an input, producing its output as the output of app.
        /// </summary>
        /// <category index="10">Arrow Apply</category>
        val inline app< ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
                         when (Control.App or
                                ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>) :
                                (static member App:
                                    ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> *
                                   Control.App
                                     ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)> :
           ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>
            when (Control.App or  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>) :
                   (static member App:
                       ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)> * Control.App
                        ->  ^ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>)
        
        /// Foldable
        /// <summary>Applies a function to each element of the foldable, starting from the end, threading an accumulator argument
        /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
        /// computes <c>f i0 (...(f iN s))</c>.</summary>
        /// <category index="11">Foldable</category>
        /// <param name="folder">The function to update the state given the input elements.</param>
        /// <param name="foldable">The input foldable.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The state object after the folding function is applied to each element of the foldable.</returns>
        val inline foldBack:
          folder: ('T -> 'State -> 'State) -> foldable:  ^Foldable<'T>
          -> state: 'State -> 'State
            when (Control.FoldBack or  ^Foldable<'T>) :
                   (static member FoldBack:
                       ^Foldable<'T> * ('T -> 'State -> 'State) * 'State *
                      Control.FoldBack -> 'State)
        
        /// <summary>Applies a function to each element of the foldable, threading an accumulator argument
        /// through the computation. Take the second argument, and apply the function to it
        /// and the first element of the foldable. Then feed this result into the function along
        /// with the second element and so on. Return the final result.
        /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then 
        /// computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
        /// <category index="11">Foldable</category>
        /// <param name="folder">The function to update the state given the input elements.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="foldable">The input foldable.</param>
        /// <returns>The final state value.</returns>
        val inline fold:
          folder: ('State -> 'T -> 'State) -> state: 'State
          -> foldable:  ^Foldable<'T> -> 'State
            when (Control.Fold or  ^Foldable<'T>) :
                   (static member Fold:
                       ^Foldable<'T> * ('State -> 'T -> 'State) * 'State *
                      Control.Fold -> 'State)
        
        /// <summary>
        /// Folds by mapping all values to a Monoid
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline foldMap:
          f: ('T -> 'Monoid) -> x:  ^Foldable<'T> -> 'Monoid
            when (Control.FoldMap or  ^Foldable<'T>) :
                   (static member FoldMap:
                       ^Foldable<'T> * ('T -> 'Monoid) * Control.FoldMap
                        -> 'Monoid)
        
        /// <summary>Builds a list from the given foldable.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The list of foldable elements.</returns>
        val inline toList:
          source:  ^a -> 'T list
            when (Control.ToList or  ^a) :
                   (static member ToList:  ^a * Control.ToList -> 'T list)
        
        /// <summary>Builds an array from the given foldable.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The array of foldable elements.</returns>
        val inline toArray:
          source:  ^a -> 'T[]
            when (Control.ToArray or  ^a) :
                   (static member ToArray:  ^a * Control.ToArray -> 'T[])
        
        /// <summary>Views the given foldable as a sequence.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The sequence of elements in the foldable.</returns>
        val inline toSeq:
          source:  ^Foldable<'T> -> seq<'T>
            when (Control.ToSeq or  ^Foldable<'T>) :
                   (static member ToSeq:
                       ^Foldable<'T> * Control.ToSeq -> seq<'T>)
        
        /// <summary>Tests if any element of the list satisfies the given predicate.</summary>
        /// <category index="11">Foldable</category>
        ///
        /// <remarks>The predicate is applied to the elements of the input foldable. If any application 
        /// returns true then the overall result is true and no further elements are tested. 
        /// Otherwise, false is returned.</remarks>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <returns>True if any element satisfies the predicate.</returns>
        val inline exists:
          predicate: ('T -> bool) -> source:  ^Foldable<'T> -> bool
            when (Control.Exists or  ^Foldable<'T>) :
                   (static member Exists:
                       ^Foldable<'T> * ('T -> bool) * Control.Exists -> bool)
        
        /// <summary>Tests if all elements of the collection satisfy the given predicate.</summary>
        /// <category index="11">Foldable</category>
        ///
        /// <remarks>The predicate is applied to the elements of the input foldable. If any application 
        /// returns false then the overall result is false and no further elements are tested. 
        /// Otherwise, true is returned.</remarks>
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <returns>True if all of the elements satisfy the predicate.</returns>
        val inline forall:
          predicate: ('T -> bool) -> source:  ^Foldable<'T> -> bool
            when (Control.ForAll or  ^Foldable<'T>) :
                   (static member ForAll:
                       ^Foldable<'T> * ('T -> bool) * Control.ForAll -> bool)
        
        /// <summary>Gets the first element for which the given function returns true.
        /// Raises <c>KeyNotFoundException</c> if no such element exists.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if the predicate evaluates to false for
        /// all the elements of the foldable.</exception>
        /// <returns>The first element that satisfies the predicate.</returns>
        val inline find:
          predicate: ('T -> bool) -> source:  ^Foldable<'T> -> 'T
            when (Control.Find or  ^Foldable<'T>) :
                   (static member Find:
                       ^Foldable<'T> * ('T -> bool) * Control.Find -> 'T)
        
        /// <summary>Gets the first element for which the given function returns true.
        /// Returns None if no such element exists.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <returns>The first element for which the predicate returns true, or None if
        /// every element evaluates to false.</returns>
        val inline tryFind:
          predicate: ('T -> bool) -> source:  ^Foldable<'T> -> 'T option
            when (Control.TryFind or  ^Foldable<'T>) :
                   (static member TryFind:
                       ^Foldable<'T> * ('T -> bool) * Control.TryFind
                        -> 'T option)
        
        /// <summary>Applies the given function to successive elements, returning the first
        /// result where function returns <c>Some(x)</c> for some x. If no such
        /// element exists then raise <c>System.Collections.Generic.KeyNotFoundException</c></summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when the foldable is empty.</exception>
        /// <returns>The first resulting value.</returns>
        val inline pick:
          chooser: ('T -> 'U option) -> source:  ^Foldable<'T> -> 'U
            when (Control.Pick or  ^Foldable<'T>) :
                   (static member Pick:
                       ^Foldable<'T> * ('T -> 'U option) * Control.Pick -> 'U)
        
        /// <summary>Applies the given function to successive elements, returning <c>Some(x)</c> the first
        /// result where function returns <c>Some(x)</c> for some x. If no such element 
        /// exists then return <c>None</c>.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="chooser">The function to generate options from the elements.</param>
        /// <param name="source">The input foldable.</param>
        /// <returns>The first resulting value or None.</returns>
        val inline tryPick:
          chooser: ('T -> 'U option) -> source:  ^Foldable<'T> -> 'U option
            when (Control.TryPick or  ^Foldable<'T>) :
                   (static member TryPick:
                       ^Foldable<'T> * ('T -> 'U option) * Control.TryPick
                        -> 'U option)
        
        /// <summary>
        /// Folds the source, inserting a separator between each element.
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline intercalate:
          sep:  ^Monoid -> source:  ^Foldable<'Monoid> ->  ^Monoid
            when (Control.Intercalate or  ^Monoid) :
                   (static member Intercalate:
                       ^Foldable<'Monoid> *  ^Monoid * Control.Intercalate
                        ->  ^Monoid)
        
        /// <summary>Gets the first element of the foldable.</summary>
        /// <category index="11">Foldable</category>
        ///
        /// <param name="source">The input flodable.</param>
        /// <exception cref="System.ArgumentException">Thrown when the foldable is empty.</exception>
        /// <returns>The first element of the foldable.</returns>
        val inline head:
          source:  ^Foldable<'T> -> 'T
            when (Control.Head or  ^Foldable<'T>) :
                   (static member Head:  ^Foldable<'T> * Control.Head -> 'T)
        
        /// <summary>Gets the first element of the foldable, or
        /// <c>None</c> if the foldable is empty.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The first element of the foldable or None.</returns>
        val inline tryHead:
          source:  ^Foldable<'T> -> 'T option
            when (Control.TryHead or  ^Foldable<'T>) :
                   (static member TryHead:
                       ^Foldable<'T> * Control.TryHead -> 'T option)
        
        /// <summary>Gets the last element of the foldable, or
        /// <c>None</c> if the foldable is empty.</summary>
        /// <category index="11">Foldable</category>
        /// <remarks>Unsafe for infinite sequence input.</remarks>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The last element of the foldable or None.</returns>
        val inline tryLast:
          source:  ^Foldable<'T> -> 'T option
            when (Control.TryLast or  ^Foldable<'T>) :
                   (static member TryLast:
                       ^Foldable<'T> * Control.TryLast -> 'T option)
        
        /// <summary>Gets the number of elements in the foldable.</summary>
        /// <category index="11">Foldable</category>
        /// 
        /// <param name="source">The input foldable.</param>
        /// <returns>The length of the foldable.</returns>
        val inline length:
          source:  ^Foldable<'T> -> int
            when (Control.Length or  ^Foldable<'T>) :
                   (static member Length:  ^Foldable<'T> * Control.Length -> int)
        
        /// <summary>
        /// Gets the maximum value in the foldable
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline maximum:
          source:  ^Foldable<'T> -> 'T
            when (Control.Max or  ^Foldable<'T>) :
                   (static member Max:  ^Foldable<'T> * Control.Max -> 'T) and
                 'T: comparison
        
        /// <summary>
        /// Gets the minimum value in the foldable
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline minimum:
          source:  ^Foldable<'T> -> 'T
            when (Control.Min or  ^Foldable<'T>) :
                   (static member Min:  ^Foldable<'T> * Control.Min -> 'T) and
                 'T: comparison
        
        /// <summary>
        /// Gets the maximum value after projecting in the foldable
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline maxBy:
          projection: ('T -> 'U) -> source:  ^Foldable<'T> -> 'T
            when 'U: comparison and
                 (Control.MaxBy or  ^Foldable<'T>) :
                   (static member MaxBy:
                       ^Foldable<'T> * ('T -> 'U) * Control.MaxBy -> 'T)
        
        /// <summary>
        /// Gets the minimum value after projecting in the foldable
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline minBy:
          projection: ('T -> 'U) -> source:  ^Foldable<'T> -> 'T
            when 'U: comparison and
                 (Control.MinBy or  ^Foldable<'T>) :
                   (static member MinBy:
                       ^Foldable<'T> * ('T -> 'U) * Control.MinBy -> 'T)
        
        /// <summary>
        /// Gets the nth value in the foldable - i.e. at position 'n'
        /// </summary>
        /// <category index="11">Foldable</category>
        val inline nth:
          n: int -> source:  ^Foldable<'T> -> 'T
            when (Control.Nth or  ^Foldable<'T>) :
                   (static member Nth:  ^Foldable<'T> * int * Control.Nth -> 'T)
        
        /// <summary>Applies a function to each element of the reducible, threading an accumulator argument
        /// through the computation. Apply the function to the first two elements of the reducible.
        /// Then feed this result into the function along with the third element and so on. 
        /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
        /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
        /// <category index="12">Reducible</category>
        /// 
        /// <param name="reduction">The function to reduce two reducible elements to a single element.</param>
        /// <param name="source">The input reducible.</param>
        /// <returns>The final reduced value.</returns>
        val inline reduce:
          reduction: ('T -> 'T -> 'T) -> source:  ^Reducible<'T> -> 'T
            when  ^Reducible<'T> :
                   (static member Reduce:
                       ^Reducible<'T> * ('T -> 'T -> 'T) -> 'T)
        
        /// <summary>
        /// Map each element of a structure to an action, evaluate these actions from left to right, and collect the results.
        /// </summary>
        /// <category index="13">Traversable</category>
        val inline traverse:
          f: ('T -> 'Functor<'U>) -> t:  ^Traversable<'T>
            ->  ^Functor<'Traversable<'U>>
            when (Control.Traverse or  ^Traversable<'T> or
                   ^Functor<'Traversable<'U>>) :
                   (static member Traverse:
                       ^Traversable<'T> * ('T -> 'Functor<'U>) *
                       ^Functor<'Traversable<'U>> * Control.Traverse
                        ->  ^Functor<'Traversable<'U>>)
        
        /// <summary>
        /// Evaluate each action in the structure from left to right, and collect the results.
        /// </summary>
        /// <category index="13">Traversable</category>
        val inline sequence:
          t:  ^Traversable<'Functor<'T>> ->  ^Functor<'Traversable<'T>>
            when (Control.Sequence or  ^Traversable<'Functor<'T>> or
                   ^Functor<'Traversable<'T>>) :
                   (static member Sequence:
                       ^Traversable<'Functor<'T>> *  ^Functor<'Traversable<'T>> *
                      Control.Sequence ->  ^Functor<'Traversable<'T>>)
        
        /// <summary>
        /// Combines the elements of a structure, given ways of mapping them to a Common Combinators monoid.
        /// </summary>
        /// <category index="14">Bifoldable</category>
        val inline bifoldMap:
          f: ('T1 -> 'Monoid) -> g: ('T2 -> 'Monoid)
          -> source:  ^Bifoldable<'T1,'T2> -> 'Monoid
            when (Control.BifoldMap or  ^Bifoldable<'T1,'T2>) :
                   (static member BifoldMap:
                       ^Bifoldable<'T1,'T2> * ('T1 -> 'Monoid) *
                      ('T2 -> 'Monoid) * Control.BifoldMap -> 'Monoid)
        
        /// <summary>
        /// Combines the elements of a structure in a right associative manner.
        /// </summary>
        /// <category index="14">Bifoldable</category>
        val inline bifold:
          leftFolder: ('State -> 'T1 -> 'State)
          -> rightFolder: ('State -> 'T2 -> 'State) -> state: 'State
          -> source:  ^Bifoldable<'T1,'T2> -> 'State
            when (Control.Bifold or  ^Bifoldable<'T1,'T2>) :
                   (static member Bifold:
                       ^Bifoldable<'T1,'T2> * ('State -> 'T1 -> 'State) *
                      ('State -> 'T2 -> 'State) * 'State * Control.Bifold
                        -> 'State)
        
        /// <summary>
        /// Combines the elements of a structure in a left associative manner.
        /// </summary>
        /// <category index="14">Bifoldable</category>
        val inline bifoldBack:
          leftFolder: ('T1 -> 'State -> 'State)
          -> rightFolder: ('T2 -> 'State -> 'State)
          -> source:  ^Bifoldable<'T1,'T2> -> state: 'State -> 'State
            when (Control.BifoldBack or  ^Bifoldable<'T1,'T2>) :
                   (static member BifoldBack:
                       ^Bifoldable<'T1,'T2> * ('T1 -> 'State -> 'State) *
                      ('T2 -> 'State -> 'State) * 'State * Control.BifoldBack
                        -> 'State)
        
        /// <summary>
        /// Combines the elements of a structure using a monoid.
        /// </summary>
        /// <category index="14">Bifoldable</category>
        val inline bisum:
          source:  ^Bifoldable<'Monoid,'Monoid> -> 'Monoid
            when (Control.Bisum or  ^Bifoldable<'Monoid,'Monoid>) :
                   (static member Bisum:
                       ^Bifoldable<'Monoid,'Monoid> * Control.Bisum -> 'Monoid)
        
        /// <summary>
        /// Evaluates the relevant functions at each element in the structure, running the action, and builds a new structure with the same shape, using the results produced from sequencing the actions.
        /// </summary>
        /// <category index="15">Bitraversable</category>
        val inline bitraverse:
          f: ('T1 -> 'Functor<'T2>) -> g: ('U1 -> 'Functor<'U2>)
          -> source:  ^Bitraversable<'T1,'U1>
            ->  ^Functor<'Bitraversable<'T2,'U2>>
            when (Control.Bitraverse or  ^Bitraversable<'T1,'U1> or
                   ^Functor<'Bitraversable<'T2,'U2>>) :
                   (static member Bitraverse:
                       ^Bitraversable<'T1,'U1> * ('T1 -> 'Functor<'T2>) *
                      ('U1 -> 'Functor<'U2>) * Control.Bitraverse
                        ->  ^Functor<'Bitraversable<'T2,'U2>>)
        
        /// <summary>
        /// Sequences all the actions in a structure, building a new structure with the same shape using the results of the actions.
        /// </summary>
        /// <category index="15">Bitraversable</category>
        val inline bisequence:
          source:  ^Bitraversable<'Functor<'T>,'Functor<'U>>
            ->  ^Functor<'Bitraversable<'T,'U>>
            when (Control.Bisequence or
                   ^Bitraversable<'Functor<'T>,'Functor<'U>> or
                   ^Functor<'Bitraversable<'T,'U>>) :
                   (static member Bisequence:
                       ^Bitraversable<'Functor<'T>,'Functor<'U>> *
                      Control.Bisequence ->  ^Functor<'Bitraversable<'T,'U>>)
        
        /// <summary>
        /// Gets an item from the given index.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline item:
          n: 'K -> source:  ^Indexed<'T> -> 'T
            when (Control.Item or  ^Indexed<'T>) :
                   (static member Item:  ^Indexed<'T> * 'K * Control.Item -> 'T)
        
        /// <summary>
        /// Tries to get an item from the given index.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline tryItem:
          n: 'K -> source:  ^Indexed<'T> -> 'T option
            when (Control.TryItem or  ^Indexed<'T>) :
                   (static member TryItem:
                       ^Indexed<'T> * 'K * Control.TryItem -> 'T option)
        
        /// <summary>
        /// Maps with access to the index.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline mapi:
          mapping: ('K -> 'T -> 'U) -> source:  ^FunctorWithIndex<'T>
            -> 'FunctorWithIndex<'U>
            when (Control.MapIndexed or  ^FunctorWithIndex<'T>) :
                   (static member MapIndexed:
                       ^FunctorWithIndex<'T> * ('K -> 'T -> 'U) *
                      Control.MapIndexed -> 'FunctorWithIndex<'U>)
        
        /// <summary>
        /// Choose with access to the index.
        /// </summary>
        /// <param name="mapping">The mapping function, taking index and element as parameters.</param>
        /// <param name="source">The input collection.</param>
        /// <category index="16">Indexable</category>
        val inline choosei:
          mapping: ('K -> 'T -> 'U option) -> source:  ^FunctorWithIndex<'T>
            -> 'FunctorWithIndex<'U>
            when (Control.ChooseIndexed or  ^FunctorWithIndex<'T>) :
                   (static member ChooseIndexed:
                       ^FunctorWithIndex<'T> * ('K -> 'T -> 'U option) *
                      Control.ChooseIndexed -> 'FunctorWithIndex<'U>)
        
        /// <summary>
        /// Maps an action with access to an index.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline iteri:
          action: ('K -> 'T -> unit) -> source:  ^FunctorWithIndex<'T> -> unit
            when (Control.IterateIndexed or  ^FunctorWithIndex<'T>) :
                   (static member IterateIndexed:
                       ^FunctorWithIndex<'T> * ('K -> 'T -> unit) *
                      Control.IterateIndexed -> unit)
        
        /// <summary>
        /// Left-associative fold of an indexed container with access to the index i.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline foldi:
          folder: ('State -> 'K -> 'T -> 'State) -> state: 'State
          -> source:  ^FoldableWithIndex<'T> -> 'State
            when (Control.FoldIndexed or  ^FoldableWithIndex<'T>) :
                   (static member FoldIndexed:
                       ^FoldableWithIndex<'T> * ('State -> 'K -> 'T -> 'State) *
                      'State * Control.FoldIndexed -> 'State)
        
        /// <summary>
        /// Traverses an indexed container. Behaves exactly like a regular traverse except that the traversing function also has access to the key associated with a value.
        /// </summary>
        /// <category index="16">Indexable</category>
        val inline traversei:
          f: ('K -> 'T -> 'Applicative<'U>) -> t:  ^Traversable<'T>>
            ->  ^Applicative<'Traversable<'U>>
            when (Control.TraverseIndexed or  ^Traversable<'T>> or
                   ^Applicative<'Traversable<'U>>) :
                   (static member TraverseIndexed:
                       ^Traversable<'T>> * ('K -> 'T -> 'Applicative<'U>) *
                       ^Applicative<'Traversable<'U>> * Control.TraverseIndexed
                        ->  ^Applicative<'Traversable<'U>>)
        
        /// <summary>
        /// Gets the index of the first element in the source
        /// that satisfies the given predicate.
        /// </summary>
        /// <category index="16">Indexable</category>
        /// 
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
        val inline findIndex:
          predicate: ('T -> bool) -> source:  ^Indexable<'T> -> 'Index
            when (Control.FindIndex or  ^Indexable<'T>) :
                   (static member FindIndex:
                       ^Indexable<'T> * ('T -> bool) * Control.FindIndex
                        -> 'Index)
        
        /// <summary>
        /// Gets the index of the first element in the source
        /// that satisfies the given predicate.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <category index="16">Indexable</category>
        /// 
        /// <param name="predicate">
        /// The function to test the input elements.
        /// </param>
        /// <param name="source">The input collection.</param>
        /// <returns> 
        /// The index of the first element that satisfies the predicate, or <c>None</c>.
        /// </returns>
        val inline tryFindIndex:
          predicate: ('T -> bool) -> source:  ^Indexable<'T> -> 'Index option
            when (Control.TryFindIndex or  ^Indexable<'T>) :
                   (static member TryFindIndex:
                       ^Indexable<'T> * ('T -> bool) * Control.TryFindIndex
                        -> 'Index option)
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// </summary>
        /// <category index="16">Indexable</category>
        /// 
        /// <param name="slice">The slice to be searched.</param>
        /// <param name="source">The input collection.</param>
        /// <exception cref="System.ArgumentException">
        /// Thrown when the slice was not found in the source.
        /// </exception>
        /// <returns>
        /// The index of the slice.
        /// </returns>
        val inline findSliceIndex:
          slice:  ^Indexable<'T> -> source:  ^Indexable<'T> -> 'Index
            when (Control.FindSliceIndex or  ^Indexable<'T>) :
                   (static member FindSliceIndex:
                       ^Indexable<'T> *  ^Indexable<'T> * Control.FindSliceIndex
                        -> 'Index)
        
        /// <summary>
        /// Gets the index of the first occurrence of the specified slice in the source.
        /// Returns <c>None</c> if not found.
        /// </summary>
        /// <category index="16">Indexable</category>
        /// 
        /// <param name="slice">The slice to be searched.</param>
        /// <param name="source">The input collection.</param>
        /// <returns>
        /// The index of the slice or <c>None</c>.
        /// </returns>
        val inline tryFindSliceIndex:
          slice:  ^Indexable<'T> -> source:  ^Indexable<'T> -> 'Index option
            when (Control.TryFindSliceIndex or  ^Indexable<'T>) :
                   (static member TryFindSliceIndex:
                       ^Indexable<'T> *  ^Indexable<'T> *
                      Control.TryFindSliceIndex -> 'Index option)
        
        /// <summary>
        /// Extracts a value from a comonadic context.
        /// </summary>
        /// <category index="17">Comonads</category>
        val inline extract:
          x:  ^Comonad<'T> -> 'T
            when (Control.Extract or  ^Comonad<'T>) :
                   (static member Extract:  ^Comonad<'T> -> 'T)
        
        /// <summary> Extends a local context-dependent computation to a global computation. </summary>
        /// <category index="17">Comonads</category>
        val inline extend:
          g: ( ^Comonad<'T> -> 'U) -> s:  ^Comonad<'T> ->  ^Comonad<'U>
            when (Control.Extend or  ^Comonad<'T> or  ^Comonad<'U>) :
                   (static member (=>>) :
                       ^Comonad<'T> * ( ^Comonad<'T> -> 'U) ->  ^Comonad<'U>)
        
        /// <summary> Extends a local context-dependent computation to a global computation.
        /// Same as <c>extend</c> but with flipped arguments. </summary>
        /// <category index="17">Comonads</category>
        val inline (=>>) :
          s:  ^Comonad<'T> -> g: ( ^Comonad<'T> -> 'U) ->  ^Comonad<'U>
            when (Control.Extend or  ^Comonad<'T> or  ^Comonad<'U>) :
                   (static member (=>>) :
                       ^Comonad<'T> * ( ^Comonad<'T> -> 'U) ->  ^Comonad<'U>)
        
        /// <summary>
        /// Duplicates a comonadic context.
        /// </summary>
        /// <category index="17">Comonads</category>
        val inline duplicate:
          x:  ^Comonad<'T> ->  ^Comonad<'Comonad<'T>>
            when (Control.Duplicate or  ^Comonad<'T> or  ^Comonad<'Comonad<'T>>) :
                   (static member Duplicate:
                       ^Comonad<'T> * Control.Duplicate
                        ->  ^Comonad<'Comonad<'T>>)
        
        /// <summary>
        /// Lifts a computation from the inner monad to the constructed monad.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline lift:
          x: 'Monad<'T> ->  ^MonadTrans<'Monad<'T>>
            when  ^MonadTrans<'Monad<'T>> :
                   (static member Lift: 'Monad<'T> ->  ^MonadTrans<'Monad<'T>>)
        
        /// <summary>
        /// A specialized lift for Async&lt;'T&gt; which is able to bring an Async value from any depth of monad-layers.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline liftAsync:
          x: Async<'T> ->  ^MonadAsync<'T>
            when (Control.LiftAsync or  ^MonadAsync<'T>) :
                   (static member LiftAsync:
                       ^MonadAsync<'T> -> (Async<'T> ->  ^MonadAsync<'T>))
        
        /// <summary>
        /// Calls a function with the current continuation as its argument (call-with-current-continuation).
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline callCC:
          f: (('T -> 'MonadCont<'R,'U>) ->  ^MonadCont<'R,'T>)
            ->  ^MonadCont<'R,'T>
            when  ^MonadCont<'R,'T> :
                   (static member CallCC:
                      (('T -> 'MonadCont<'R,'U>) ->  ^MonadCont<'R,'T>)
                        ->  ^MonadCont<'R,'T>)
        
        /// <summary>
        /// The state from the internals of the monad.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline get< ^MonadState<'S, 'S>
                         when  ^MonadState<'S, 'S> :
                                (static member get_Get: ->  ^MonadState<'S, 'S>)> :
           ^MonadState<'S, 'S>
            when  ^MonadState<'S, 'S> :
                   (static member get_Get: ->  ^MonadState<'S, 'S>)
        
        /// <summary>
        /// Gets a value which depends on the current state.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline gets:
          f: ('S -> 'T) ->  ^MonadState<'S, 'T>
            when (Control.Bind or  ^a or  ^MonadState<'S, 'T>) :
                   (static member (>>=) :
                       ^a * ('S ->  ^MonadState<'S, 'T>) ->  ^MonadState<'S, 'T>) and
                 (Control.Return or  ^MonadState<'S, 'T>) :
                   (static member Return:
                       ^MonadState<'S, 'T> * Control.Return
                        -> ('T ->  ^MonadState<'S, 'T>)) and
                  ^a:
                   (static member Map:  ^a * ('S -> 'T) ->  ^MonadState<'S, 'T>) and
                  ^a: (static member get_Get: ->  ^a)
        
        /// <summary>
        /// Replaces the state inside the monad.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline put:
          x: 'S ->  ^MonadState<'S, unit>
            when  ^MonadState<'S, unit> :
                   (static member Put: 'S ->  ^MonadState<'S, unit>)
        
        /// <summary>
        /// Modifies the state inside the monad by applying a function.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline modify:
          f: ('S -> 'S) ->  ^MonadState<'S, unit>
            when (Control.Bind or  ^a or  ^MonadState<'S, unit>) :
                   (static member (>>=) :
                       ^a * ('S ->  ^MonadState<'S, unit>)
                        ->  ^MonadState<'S, unit>) and
                  ^MonadState<'S, unit> :
                   (static member Put: 'S ->  ^MonadState<'S, unit>) and
                  ^a: (static member get_Get: ->  ^a)
        
        /// <summary>The environment from the monad.</summary>
        /// <category index="18">Monad Transformers</category>
        val inline ask< ^MonadReader<'R,'T>
                         when  ^MonadReader<'R,'T> :
                                (static member get_Ask: ->  ^MonadReader<'R,'T>)> :
           ^MonadReader<'R,'T>
            when  ^MonadReader<'R,'T> :
                   (static member get_Ask: ->  ^MonadReader<'R,'T>)
        
        /// <summary> Executes a computation in a modified environment. </summary>
        /// <category index="18">Monad Transformers</category>
        /// 
        /// <param name="f"> The function to modify the environment.    </param>
        /// <param name="m"> Reader to run in the modified environment. </param>
        val inline local:
          f: ('R1 -> 'R2) -> m:  ^MonadReader<'R2,'T> ->  ^MonadReader<'R1,'T>
            when  ^MonadReader<'R1,'T> :
                   (static member Local:
                       ^MonadReader<'R2,'T> * ('R1 -> 'R2)
                        ->  ^MonadReader<'R1,'T>)
        
        /// <summary>
        /// Embeds a simple writer action.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline tell:
          w: 'Monoid ->  ^MonadWriter<'Monoid,unit>
            when  ^MonadWriter<'Monoid,unit> :
                   (static member Tell: 'Monoid ->  ^MonadWriter<'Monoid,unit>)
        
        /// <summary> Executes the action <paramref name="m"/> and adds its output to the value of the computation. </summary>
        /// <category index="18">Monad Transformers</category>
        ///
        /// <param name="m">The action to be executed.</param>
        val inline listen:
          m: 'MonadWriter<'Monoid,'T> ->  ^MonadWriter<'Monoid,('T * 'Monoid)>
            when  ^MonadWriter<'Monoid,('T * 'Monoid)> :
                   (static member Listen:
                      'MonadWriter<'Monoid,'T>
                        ->  ^MonadWriter<'Monoid,('T * 'Monoid)>)
        
        /// <summary>
        /// Executes the action <paramref name="m"/>, which returns a value and a function, and returns the value, applying the function to the output.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline pass:
          m: 'MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>
            ->  ^MonadWriter<'Monoid,'T>
            when  ^MonadWriter<'Monoid,'T> :
                   (static member Pass:
                      'MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>
                        ->  ^MonadWriter<'Monoid,'T>)
        
        /// <summary>
        /// Throws an error value inside the Error monad.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline throw:
          error: 'E ->  ^'MonadError<'E,'T>
            when (Control.Throw or  ^'MonadError<'E,'T>) :
                   (static member Throw:
                       ^'MonadError<'E,'T> * 'E ->  ^'MonadError<'E,'T>)
        
        /// <summary>
        /// Executes a handler when the value contained in the Error monad represents an error.
        /// This is bindError flipped, which makes it useful when used as an operator.
        /// </summary>
        /// <example>
        /// <code>
        ///    let doSomeOperation x = ResultT &lt;| async {
        ///        if x &lt; 10 then return Ok 10
        ///        else return Error "failure" }
        ///
        ///    doSomeOperation &lt;/catch/&gt; (fun s -> throw ("The error was: " + s))
        /// </code>
        /// </example>
        /// <category index="18">Monad Transformers</category>
        val inline catch:
          value:  ^'MonadError<'E1,'T>
          -> handler: ('E1 ->  ^'MonadError<'E2,'T>) ->  ^'MonadError<'E2,'T>
            when (Control.Catch or  ^'MonadError<'E1,'T> or
                   ^'MonadError<'E2,'T>) :
                   (static member Catch:
                       ^'MonadError<'E1,'T> * ('E1 ->  ^'MonadError<'E2,'T>)
                        ->  ^'MonadError<'E2,'T>)
        
        /// <summary>
        /// Executes a handler when the value contained in the Error monad represents an error.
        /// </summary>
        /// <category index="18">Monad Transformers</category>
        val inline bindError:
          handler: ('E1 ->  ^'MonadError<'E2,'T>)
          -> value:  ^'MonadError<'E1,'T> ->  ^'MonadError<'E2,'T>
            when (Control.Catch or  ^'MonadError<'E1,'T> or
                   ^'MonadError<'E2,'T>) :
                   (static member Catch:
                       ^'MonadError<'E1,'T> * ('E1 ->  ^'MonadError<'E2,'T>)
                        ->  ^'MonadError<'E2,'T>)
        
        /// <summary>
        /// Converts to a Collection from a list.
        /// </summary>
        /// <category index="19">Collection</category>
        val inline ofList:
          source: 'T list ->  ^Collection
            when (Control.OfList or  ^Collection) :
                   (static member OfList:
                      ('T list *  ^Collection) * Control.OfList ->  ^Collection)
        
        /// <summary>
        /// Converts to a Collection from a seq.
        /// </summary>
        /// <category index="19">Collection</category>
        val inline ofSeq:
          source: seq<'T> ->  ^Collection
            when (Control.OfSeq or  ^Collection) :
                   (static member OfSeq:
                      (seq<'T> *  ^Collection) * Control.OfSeq ->  ^Collection)
        
        /// <summary>Returns a new collection containing only the elements of the collection
        /// for which the given predicate returns "true"</summary>
        /// <category index="19">Collection</category>
        /// 
        /// <param name="predicate">The function to test the input elements.</param>
        /// <param name="source">The input collection.</param>
        /// <returns>A collection containing only the elements that satisfy the predicate.</returns>
        val inline filter:
          predicate: ('a -> bool) -> source:  ^Collection ->  ^Collection
            when (Control.Filter or  ^Collection) :
                   (static member Filter:
                       ^Collection * ('a -> bool) * Control.Filter
                        ->  ^Collection)
        
        /// <summary>Returns a collection that skips N elements of the original collection and then yields the
        /// remaining elements of the collection.</summary>
        /// <category index="19">Collection</category>
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
        val inline skip:
          count: int -> source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.Skip or  ^Collection<'T>) :
                   (static member Skip:
                       ^Collection<'T> * int * Control.Skip ->  ^Collection<'T>)
        
        /// <summary>Gets the first N elements of the collection.</summary>
        /// <category index="19">Collection</category>
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
        val inline take:
          count: int -> source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.Take or  ^Collection<'T>) :
                   (static member Take:
                       ^Collection<'T> * int * Control.Take ->  ^Collection<'T>)
        
        /// <summary>Returns a collection that drops N elements of the original collection and then yields the
        /// remaining elements of the collection.</summary>
        /// <category index="19">Collection</category>
        /// <remarks>When count exceeds the number of elements in the collection it
        /// returns an empty collection instead of throwing an exception.</remarks>
        /// <param name="count">The number of items to drop.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        val inline drop:
          count: int -> source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.Drop or  ^Collection<'T>) :
                   (static member Drop:
                       ^Collection<'T> * int * Control.Drop ->  ^Collection<'T>)
        
        /// <summary>Returns a collection with at most N elements.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="count">The maximum number of items to return.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
        val inline limit:
          count: int -> source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.Limit or  ^Collection<'T>) :
                   (static member Limit:
                       ^Collection<'T> * int * Control.Limit ->  ^Collection<'T>)
        
        /// <summary>Applies a key-generating function to each element of a collection and yields a collection of 
        /// unique keys. Each unique key contains a collection of all elements that match 
        /// to this key.</summary>
        /// <category index="19">Collection</category>
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
        val inline groupBy:
          projection: ('T -> 'Key) -> source:  ^Collection<'T>
            ->  ^Collection<'Key * 'Collection<'T>>
            when 'Key: equality and
                 (Control.GroupBy or  ^Collection<'T> or
                   ^Collection<'Key * 'Collection<'T>>) :
                   (static member GroupBy:
                       ^Collection<'T> * ('T -> 'Key) *
                       ^Collection<'Key * 'Collection<'T>> * Control.GroupBy
                        ->  ^Collection<'Key * 'Collection<'T>>)
        
        /// <summary>Applies a key-generating function to each element of a collection and yields a collection of 
        /// keys tupled with values. Each key contains a collection of all adjacent elements that match 
        /// to this key, therefore keys are not unique but they can't be adjacent
        /// as each time the key changes, a new group is yield.</summary>
        /// <category index="19">Collection</category>
        /// 
        /// <remarks>The ordering of the original collection is respected.</remarks>
        ///
        /// <param name="projection">A function that transforms an element of the collection into a comparable key.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        val inline chunkBy:
          projection: ('T -> 'Key) -> source:  ^Collection<'T>
            ->  ^Collection<'Key * 'Collection<'T>>
            when 'Key: equality and
                 (Control.ChunkBy or  ^Collection<'T> or
                   ^Collection<'Key * 'Collection<'T>>) :
                   (static member ChunkBy:
                       ^Collection<'T> * ('T -> 'Key) *
                       ^Collection<'Key * 'Collection<'T>> * Control.ChunkBy
                        ->  ^Collection<'Key * 'Collection<'T>>)
        
        /// <summary>Returns a collection that contains all elements of the original collection while the
        /// given predicate returns true, and then returns no further elements.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline takeWhile:
          predicate: ('T -> bool) -> source:  ^Collection<'T>
            ->  ^Collection<'T>
            when (Control.TakeWhile or  ^Collection<'T>) :
                   (static member TakeWhile:
                       ^Collection<'T> * ('T -> bool) * Control.TakeWhile
                        ->  ^Collection<'T>)
        
        /// <summary>Bypasses elements in a collection while the given predicate returns true, and then returns
        /// the remaining elements of the collection.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="predicate">A function that evaluates to false when no more items should be skipped.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline skipWhile:
          predicate: ('T -> bool) -> source:  ^Collection<'T>
            ->  ^Collection<'T>
            when (Control.SkipWhile or  ^Collection<'T>) :
                   (static member SkipWhile:
                       ^Collection<'T> * ('T -> bool) * Control.SkipWhile
                        ->  ^Collection<'T>)
        
        /// <summary>
        /// Generic 'choose' for any collection.
        /// 
        /// A combination of map and filter, `choose` enables you to transform
        /// and select elements at the same time.
        /// </summary>
        /// <category index="19">Collection</category>
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
        val inline choose:
          chooser: ('T -> 'U option) -> source:  ^Collection<'T>
            ->  ^Collection<'U>
            when (Control.Choose or  ^Collection<'T> or  ^Collection<'U>) :
                   (static member Choose:
                       ^Collection<'T> * ('T -> 'U option) * Control.Choose
                        ->  ^Collection<'U>)
        
        /// <summary>Returns a collection that contains no duplicate entries according to generic hash and
        /// equality comparisons on the entries.
        /// If an element occurs multiple times in the collection then the later occurrences are discarded.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline distinct:
          source:  ^Collection<'T> when 'T : equality
            ->  ^Collection<'T> when 'T : equality
            when (Control.Distinct or  ^Collection<'T> when 'T : equality) :
                   (static member Distinct:
                       ^Collection<'T> when 'T : equality * Control.Distinct
                        ->  ^Collection<'T> when 'T : equality)
        
        /// <summary>Returns a collection that contains no duplicate entries according to the 
        /// generic hash and equality comparisons on the keys returned by the given key-generating function.
        /// If an element occurs multiple times in the collection then the later occurrences are discarded.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="projection">A function transforming the collection items into comparable keys.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The result collection.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline distinctBy:
          projection: ('T -> 'Key) -> source:  ^Collection<'T>
            ->  ^Collection<'T>
            when 'Key: equality and
                 (Control.DistinctBy or  ^Collection<'T>) :
                   (static member DistinctBy:
                       ^Collection<'T> * ('T -> 'Key) * Control.DistinctBy
                        ->  ^Collection<'T>)
        
        /// <summary>Inserts a separator between each element.</summary>
        /// <category index="19">Collection</category>
        val inline intersperse:
          sep: 'T -> source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.OfSeq or  ^Collection<'T>) :
                   (static member OfSeq:
                      (seq<'T> *  ^Collection<'T>) * Control.OfSeq
                        ->  ^Collection<'T>) and
                 (Control.ToSeq or  ^Collection<'T>) :
                   (static member ToSeq:
                       ^Collection<'T> * Control.ToSeq -> seq<'T>)
        
        /// <summary>Replaces part of the collection with a new part</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="oldValue">A collection that if part of the source collection
        /// should be replaced with newValue.</param>
        /// <param name="newValue">The collection to replace oldValue with.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The resulting collection with oldValue replaced with newValue.</returns>
        val inline replace:
          oldValue:  ^Collection -> newValue:  ^Collection
          -> source:  ^Collection ->  ^Collection
            when (Control.Replace or  ^Collection) :
                   (static member Replace:
                       ^Collection *  ^Collection *  ^Collection *
                      Control.Replace ->  ^Collection)
        
        /// <summary>Returns a new collection with the elements in reverse order.</summary>
        /// <category index="19">Collection</category>
        /// 
        /// <param name="source">The input collection.</param>
        /// <returns>The reversed collection.</returns>
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline rev:
          source:  ^Collection<'T> ->  ^Collection<'T>
            when (Control.Rev or  ^Collection<'T>) :
                   (static member Rev:
                       ^Collection<'T> * Control.Rev ->  ^Collection<'T>)
        
        /// <summary>Like fold, but computes on-demand and returns the collection of intermediary and final results.</summary>
        /// <category index="19">Collection</category>
        ///
        /// <param name="folder">A function that updates the state with each element from the collection.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="source">The input collection.</param>
        ///
        /// <returns>The resulting collection of computed states.</returns>
        ///
        /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
        val inline scan:
          folder: ('State -> 'T -> 'State) -> state: 'State
          -> source:  ^Collection<'T> ->  ^Collection<'State>
            when (Control.Scan or  ^Collection<'T> or  ^Collection<'State>) :
                   (static member Scan:
                       ^Collection<'T> * ('State -> 'T -> 'State) * 'State *
                       ^Collection<'State> * Control.Scan
                        ->  ^Collection<'State>)
        
        /// <summary>Returns a collection ordered by keys.</summary>
        /// <category index="19">Collection</category>
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
        val inline sort:
          source:  ^Collection<'T> when 'T : comparison
            ->  ^Collection<'T> when 'T : comparison
            when (Control.Sort or  ^Collection<'T> when 'T : comparison) :
                   (static member Sort:
                       ^Collection<'T> when 'T : comparison * Control.Sort
                        ->  ^Collection<'T> when 'T : comparison)
        
        /// <summary>Applies a key-generating function to each element of a collection and returns a collection ordered
        /// by keys. The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary> 
        /// <category index="19">Collection</category>
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
        val inline sortBy:
          projection: ('T -> 'Key) -> source:  ^Collection<'T>
            ->  ^Collection<'T>
            when 'Key: comparison and
                 (Control.SortBy or  ^Collection<'T>) :
                   (static member SortBy:
                       ^Collection<'T> * ('T -> 'Key) * Control.SortBy
                        ->  ^Collection<'T>)
        
        /// <summary>Yields a collection ordered descending by keys.</summary>
        /// <category index="19">Collection</category>
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
        val inline sortByDescending:
          projection: ('T -> 'Key) -> source:  ^Collection<'T>
            ->  ^Collection<'T>
            when 'Key: comparison and
                 (Control.SortByDescending or  ^Collection<'T>) :
                   (static member SortByDescending:
                       ^Collection<'T> * ('T -> 'Key) * Control.SortByDescending
                        ->  ^Collection<'T>)
        
        /// <summary>Splits a given ordered collection at each of the given sub-ordered collections</summary>
        /// <category index="19">Collection</category>
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
        val inline split:
          sep: ''Collection<'OrderedCollection> -> source:  ^OrderedCollection
            -> ''Collection<'OrderedCollection>
            when (Control.Split or  ^OrderedCollection) :
                   (static member Split:
                      (''Collection<'OrderedCollection> *  ^OrderedCollection) *
                      Control.Split -> ''Collection<'OrderedCollection>)
        
        /// <summary>
        /// Gets the value of the first component of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline item1:
          tuple:  ^a -> 'b when  ^a: (member get_Item1:  ^a -> 'b)
        
        /// <summary>
        /// Gets the value of the second component of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline item2:
          tuple:  ^a -> 'b when  ^a: (member get_Item2:  ^a -> 'b)
        
        /// <summary>
        /// Gets the value of the third component of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline item3:
          tuple:  ^a -> 'b when  ^a: (member get_Item3:  ^a -> 'b)
        
        /// <summary>
        /// Gets the value of the fourth component of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline item4:
          tuple:  ^a -> 'b when  ^a: (member get_Item4:  ^a -> 'b)
        
        /// <summary>
        /// Gets the value of the fifth component of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline item5:
          tuple:  ^a -> 'b when  ^a: (member get_Item5:  ^a -> 'b)
        
        /// <summary>
        /// Maps the first value of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline mapItem1:
          mapping: ('T -> 'U) -> tuple:  ^('T * ..) -> '('U * ..)
            when (Control.MapItem1 or  ^('T * ..)) :
                   (static member MapItem1:
                       ^('T * ..) * ('T -> 'U) -> '('U * ..))
        
        /// <summary>
        /// Maps the second value of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline mapItem2:
          mapping: ('T -> 'U) -> tuple:  ^('A * 'T * ..) -> '('A * 'U * ..)
            when (Control.MapItem2 or  ^('A * 'T * ..)) :
                   (static member MapItem2:
                       ^('A * 'T * ..) * ('T -> 'U) -> '('A * 'U * ..))
        
        /// <summary>
        /// Maps the third value of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline mapItem3:
          mapping: ('T -> 'U) -> tuple:  ^('A * 'B * 'T * ..)
            -> '('A * 'B * 'U * ..)
            when (Control.MapItem3 or  ^('A * 'B * 'T * ..)) :
                   (static member MapItem3:
                       ^('A * 'B * 'T * ..) * ('T -> 'U) -> '('A * 'B * 'U * ..))
        
        /// <summary>
        /// Maps the fourth value of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline mapItem4:
          mapping: ('T -> 'U) -> tuple:  ^('A * 'B * 'C * 'T * ..)
            -> '('A * 'B * 'C * 'U * ..)
            when (Control.MapItem4 or  ^('A * 'B * 'C * 'T * ..)) :
                   (static member MapItem4:
                       ^('A * 'B * 'C * 'T * ..) * ('T -> 'U)
                        -> '('A * 'B * 'C * 'U * ..))
        
        /// <summary>
        /// Maps the fifth value of a tuple.
        /// </summary>
        /// <category index="20">Tuple</category>
        val inline mapItem5:
          mapping: ('T -> 'U) -> tuple:  ^('A * 'B * 'C * 'D * 'T * ..)
            -> '('A * 'B * 'C * 'D * 'U * ..)
            when (Control.MapItem5 or  ^('A * 'B * 'C * 'D * 'T * ..)) :
                   (static member MapItem5:
                       ^('A * 'B * 'C * 'D * 'T * ..) * ('T -> 'U)
                        -> '('A * 'B * 'C * 'D * 'U * ..))
        
        /// <summary>
        /// Converts using the explicit operator.
        /// </summary>
        /// <category index="21">Converter</category>
        val inline explicit:
          value:  ^T ->  ^U
            when (Control.Explicit or  ^U or  ^T) :
                   (static member Explicit:
                       ^U * Control.Explicit -> ( ^T ->  ^U))
        
        /// <summary>
        /// Convert from a byte array value, given options of little-endian, and startIndex
        /// </summary>
        /// <category index="21">Converter</category>
        val inline ofBytesWithOptions:
          isLtEndian: bool -> startIndex: int -> value: byte[] ->  ^a
            when (Control.OfBytes or  ^a) :
                   (static member OfBytes:
                       ^a * Control.OfBytes -> (byte[] * int * bool ->  ^a))
        
        /// <summary>
        /// Convert from a byte array value, assuming little-endian
        /// </summary>
        /// <category index="21">Converter</category>
        val inline ofBytes:
          value: byte[] ->  ^a
            when (Control.OfBytes or  ^a) :
                   (static member OfBytes:
                       ^a * Control.OfBytes -> (byte[] * int * bool ->  ^a))
        
        /// <summary>
        /// Convert from a byte array value, assuming big-endian
        /// </summary>
        /// <category index="21">Converter</category>
        val inline ofBytesBE:
          value: byte[] ->  ^a
            when (Control.OfBytes or  ^a) :
                   (static member OfBytes:
                       ^a * Control.OfBytes -> (byte[] * int * bool ->  ^a))
        
        /// <summary>
        /// Convert to a byte array value, assuming little endian
        /// </summary>
        /// <category index="21">Converter</category>
        val inline toBytes:
          value:  ^a -> byte[]
            when (Control.ToBytes or  ^a) :
                   (static member ToBytes:
                       ^a * bool * Control.ToBytes -> byte[])
        
        /// <summary>
        /// Convert to a byte array value, assuming big endian
        /// </summary>
        /// <category index="21">Converter</category>
        val inline toBytesBE:
          value:  ^a -> byte[]
            when (Control.ToBytes or  ^a) :
                   (static member ToBytes:
                       ^a * bool * Control.ToBytes -> byte[])
        
        /// <summary>
        /// Converts to a value from its string representation.
        /// </summary>
        /// <category index="21">Converter</category>
        val inline parse:
          value: string ->  ^a
            when (Control.Parse or  ^a) :
                   (static member Parse:  ^a * Control.Parse -> (string ->  ^a))
        
        /// <summary>
        /// Converts to a value from its string representation. Returns None if the convertion doesn't succeed.
        /// </summary>
        /// <category index="21">Converter</category>
        val inline tryParse:
          value: string ->  ^a option
            when (Control.TryParse or  ^a) :
                   (static member TryParse:
                       ^a * Control.TryParse -> (string ->  ^a option))
        
        /// <summary>Gets a value that represents the number 1 (one).</summary>
        /// <category index="22">Numerics</category>
        val inline getOne:
          unit ->  ^a
            when (Control.One or  ^a) :
                   (static member One:  ^a * Control.One ->  ^a)
        
        /// <summary>A value that represents the 1 element.</summary>
        /// <category index="22">Numerics</category>
        val inline one< ^Num
                         when (Control.One or  ^Num) :
                                (static member One:  ^Num * Control.One ->  ^Num)> :
           ^Num
            when (Control.One or  ^Num) :
                   (static member One:  ^Num * Control.One ->  ^Num)
        
        /// <summary>Divides one number by another, returns a tuple with the result and the remainder.</summary>
        /// <category index="22">Numerics</category>
        val inline divRem:
          dividend:  ^Num -> divisor:  ^Num ->  ^Num *  ^Num
            when (Control.DivRem or  ^Num) :
                   (static member DivRem:
                       ^Num *  ^Num * Control.DivRem ->  ^Num *  ^Num)
        
        /// <summary>Gets the smallest possible value.</summary>
        /// <category index="22">Numerics</category>
        val inline getMinValue:
          unit ->  ^a
            when (Control.MinValue or  ^a) :
                   (static member MinValue:  ^a * Control.MinValue ->  ^a)
        
        /// <summary>The smallest possible value.</summary>
        /// <category index="22">Numerics</category>
        val inline minValue< ^Num
                              when (Control.MinValue or  ^Num) :
                                     (static member MinValue:
                                         ^Num * Control.MinValue ->  ^Num)> :
           ^Num
            when (Control.MinValue or  ^Num) :
                   (static member MinValue:  ^Num * Control.MinValue ->  ^Num)
        
        /// <summary>Gets the largest possible value.</summary>
        /// <category index="22">Numerics</category>
        val inline getMaxValue:
          unit ->  ^a
            when (Control.MaxValue or  ^a) :
                   (static member MaxValue:  ^a * Control.MaxValue ->  ^a)
        
        /// <summary>The largest possible value.</summary>
        /// <category index="22">Numerics</category>
        val inline maxValue< ^Num
                              when (Control.MaxValue or  ^Num) :
                                     (static member MaxValue:
                                         ^Num * Control.MaxValue ->  ^Num)> :
           ^Num
            when (Control.MaxValue or  ^Num) :
                   (static member MaxValue:  ^Num * Control.MaxValue ->  ^Num)
        
        /// <summary>Converts from BigInteger to the inferred destination type.</summary>
        /// <category index="22">Numerics</category>
        val inline fromBigInt:
          x: bigint ->  ^Num
            when (Control.FromBigInt or  ^Num) :
                   (static member FromBigInt:
                       ^Num * Control.FromBigInt -> (bigint ->  ^Num))
        
        /// <summary>Converts to BigInteger.</summary>
        /// <category index="22">Numerics</category>
        val inline toBigInt:
          x:  ^Integral -> bigint
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint)
        
        /// <summary>Gets the pi number.</summary>
        /// <category index="22">Numerics</category>
        val inline getPi:
          unit ->  ^Floating
            when (Control.Pi or  ^Floating) :
                   (static member Pi:  ^Floating * Control.Pi ->  ^Floating)
        
        /// <summary>The pi number.</summary>
        /// <category index="22">Numerics</category>
        val inline pi< ^Num
                        when (Control.Pi or  ^Num) :
                               (static member Pi:  ^Num * Control.Pi ->  ^Num)> :
           ^Num
            when (Control.Pi or  ^Num) :
                   (static member Pi:  ^Num * Control.Pi ->  ^Num)
        
        /// <summary>Additive inverse of the number.</summary>
        /// <category index="22">Numerics</category>
        val inline negate:
          x:  ^Num ->  ^Num
            when (Control.TryNegate or  ^Num) :
                   (static member TryNegate:  ^Num -> Result< ^Num,exn>)
        
        /// <summary>Additive inverse of the number.</summary>
        /// <category index="22">Numerics</category>
        /// Works also for unsigned types (Throws an exception if there is no inverse).
        val inline negate':
          x:  ^Num ->  ^Num
            when (Control.TryNegate' or  ^Num) :
                   (static member TryNegate:  ^Num -> Result< ^Num,exn>)
        
        /// <summary>
        /// Additive inverse of the number.
        /// Works also for unsigned types (Returns none if there is no inverse).
        /// </summary>
        /// <category index="22">Numerics</category>
        val inline tryNegate':
          x:  ^Num ->  ^Num option
            when (Control.TryNegate' or  ^Num) :
                   (static member TryNegate:  ^Num -> Result< ^Num,exn>)
        
        /// <summary>Subtraction between two numbers. Throws an error if the result is negative on unsigned types.</summary>
        /// <category index="22">Numerics</category>
        val inline subtract:
          x:  ^Num -> y:  ^Num ->  ^Num
            when (Control.Subtract or  ^Num) :
                   (static member Subtract:  ^Num *  ^Num ->  ^Num)
        
        /// <summary>Subtraction between two numbers. Returns None if the result is negative on unsigned types.</summary>
        /// <category index="22">Numerics</category>
        val inline trySubtract:
          x:  ^Num -> y:  ^Num ->  ^Num option
            when (Control.TrySubtract or  ^Num) :
                   (static member TrySubtract:
                       ^Num *  ^Num -> Result< ^Num,exn>)
        
        /// <summary>Division between two numbers. If the numbers are not divisible throws an error.</summary>
        /// <category index="22">Numerics</category>
        val inline div:
          dividend:  ^Num -> divisor:  ^Num ->  ^Num
            when (Control.Divide or  ^Num) :
                   (static member Divide:  ^Num *  ^Num ->  ^Num)
        
        /// <summary>Division between two numbers. Returns None if the numbers are not divisible.</summary>
        /// <category index="22">Numerics</category>
        val inline tryDiv:
          dividend:  ^Num -> divisor:  ^Num ->  ^Num option
            when (Control.TryDivide or  ^Num) :
                   (static member TryDivide:  ^Num *  ^Num -> Result< ^Num,exn>)
        
        /// <summary>Square root of a number of any type. Throws an exception if there is no square root.</summary>
        /// <category index="22">Numerics</category>
        val inline sqrt:
          x:  ^a ->  ^a
            when (Control.Sqrt or  ^a) :
                   (static member Sqrt:  ^a * Control.Sqrt ->  ^a)
        
        /// <summary>Square root of a number of any type. Returns None if there is no square root.</summary>
        /// <category index="22">Numerics</category>
        val inline trySqrt:
          x:  ^a ->  ^a option
            when (Control.TrySqrt or  ^a) :
                   (static member TrySqrt:  ^a -> Result< ^a,exn>)
        
        /// <summary>Square root of an integral number.</summary>
        /// <category index="22">Numerics</category>
        val inline isqrt:
          x:  ^Integral ->  ^Integral
            when (Control.TrySqrtRem or  ^Integral) :
                   (static member TrySqrtRem:
                       ^Integral -> Result<( ^Integral *  ^Integral),exn>)
        
        /// <summary>Square root of an integral number.</summary>
        /// <category index="22">Numerics</category>
        val inline sqrtRem:
          x:  ^Integral ->  ^Integral *  ^Integral
            when (Control.TrySqrtRem or  ^Integral) :
                   (static member TrySqrtRem:
                       ^Integral -> Result<( ^Integral *  ^Integral),exn>)
        
        /// <summary>Sign of the given number
        /// <para/>   Rule: signum x * abs x = x </summary>
        /// <category index="22">Numerics</category>
        /// 
        /// <param name="value">The input value.</param>
        /// <returns>-1, 0, or 1 depending on the sign of the input.</returns>
        val inline signum:
          value:  ^Num ->  ^Num
            when (Control.Signum or  ^Num) :
                   (static member Signum:  ^Num * Control.Signum ->  ^Num)
        
        /// <summary>Sign of the given number
        ///           Works also for unsigned types. 
        /// <para/>   Rule: signum x * abs x = x </summary>
        /// <category index="22">Numerics</category>
        /// 
        /// <param name="value">The input value.</param>
        /// <returns>-1, 0, or 1 depending on the sign of the input.</returns>
        val inline signum':
          value:  ^Num ->  ^Num
            when (Control.Signum' or  ^Num) :
                   (static member Signum:  ^Num * Control.Signum' ->  ^Num)
        
        /// <summary> Gets the absolute value of the given number.
        /// <para/>   Rule: signum x * abs x = x </summary>
        /// <category index="22">Numerics</category>
        /// 
        /// <param name="value">The input value.</param>
        /// <returns>The absolute value of the input.</returns>
        val inline abs:
          value:  ^Num ->  ^Num
            when (Control.Abs or  ^Num) :
                   (static member Abs:  ^Num * Control.Abs ->  ^Num)
        
        /// <summary> Gets the absolute value of the given number.
        ///           Works also for unsigned types. 
        /// <para/>   Rule: signum x * abs x = x </summary>
        /// <category index="22">Numerics</category>
        /// 
        /// <param name="value">The input value.</param>
        /// <returns>The absolute value of the input.</returns>
        val inline abs':
          value:  ^Num ->  ^Num
            when (Control.Abs' or  ^Num) :
                   (static member Abs:  ^Num * Control.Abs' ->  ^Num)
        
        /// <summary>
        /// Reduces using alternative operator `&lt;|&gt;`.
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val inline choice:
          x:  ^Foldable<'Alternative<'T>> -> 'Alternative<'T>>
            when (Control.Choice or  ^Foldable<'Alternative<'T>>) :
                   (static member Choice:
                       ^Foldable<'Alternative<'T>> ref * Control.Choice
                        -> 'Alternative<'T>>)
        
        /// <summary>
        /// Generic filter operation for MonadZero. It returns all values satisfying the predicate, if the predicate returns false will use the empty value.
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val inline mfilter:
          predicate: ('T -> bool) -> m:  ^MonadZero<'T> ->  ^MonadZero<'T>
            when (Control.Bind or  ^MonadZero<'T>) :
                   (static member (>>=) :
                       ^MonadZero<'T> * ('T ->  ^MonadZero<'T>)
                        ->  ^MonadZero<'T>) and
                 (Control.Return or  ^MonadZero<'T>) :
                   (static member Return:
                       ^MonadZero<'T> * Control.Return
                        -> ('T ->  ^MonadZero<'T>)) and
                 (Control.Empty or  ^MonadZero<'T>) :
                   (static member Empty:
                       ^MonadZero<'T> * Control.Empty ->  ^MonadZero<'T>)
        
        /// <summary>
        /// Folds the sum of all monoid elements in the Foldable.
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val inline sum:
          x:  ^Foldable<'Monoid> ->  ^Monoid
            when (Control.Fold or  ^Foldable<'Monoid>) :
                   (static member Fold:
                       ^Foldable<'Monoid> * ( ^Monoid ->  ^Monoid ->  ^Monoid) *
                       ^Monoid * Control.Fold ->  ^Monoid) and
                 (Control.Plus or  ^Monoid) :
                   (static member ``+`` :
                       ^Monoid *  ^Monoid * Control.Plus ->  ^Monoid) and
                 (Control.Zero or  ^Monoid) :
                   (static member Zero:  ^Monoid * Control.Zero ->  ^Monoid)
        
        /// <summary>
        /// Converts using the implicit operator. 
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val inline implicit:
          x:  ^T ->  ^R
            when ( ^R or  ^T) : (static member op_Implicit:  ^T ->  ^R)
        
        [<System.Obsolete ("Use Parsed instead.")>]
        val inline (|Parse|_|) :
          str: string ->  ^T option
            when (Control.TryParse or  ^T) :
                   (static member TryParse:
                       ^T * Control.TryParse -> (string ->  ^T option))
        
        /// <summary>
        /// An active recognizer for a generic value parser.
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val inline (|Parsed|_|) :
          str: string ->  ^T option
            when (Control.TryParse or  ^T) :
                   (static member TryParse:
                       ^T * Control.TryParse -> (string ->  ^T option))
        
        /// <summary>
        /// Safely dispose a resource (includes null-checking).
        /// </summary>
        /// <category index="23">Additional Functions</category>
        val dispose: resource: System.IDisposable -> unit
        
        /// <summary>Additional operators for Arrows related functions which shadows some F# operators for bitwise functions.</summary>
        module Arrows =
            
            /// <summary>Right-to-left morphism composition.</summary>
            /// <category index="24">Arrow Functions</category>
            val inline (<<<) :
              f:  ^a -> g: 'b -> 'c
                when (Control.Comp or  ^a) :
                       (static member ``<<<`` :
                           ^a * 'b * Control.Comp * Control.Comp -> 'c)
            
            /// <summary>Left-to-right morphism composition.</summary>
            /// <category index="24">Arrow Functions</category>
            val inline (>>>) :
              f: 'a -> g:  ^b -> 'c
                when (Control.Comp or  ^b) :
                       (static member ``<<<`` :
                           ^b * 'a * Control.Comp * Control.Comp -> 'c)
            
            /// <summary>Sends the input to both argument arrows and combine their output. Also known as fanout.</summary>
            /// <category index="24">Arrow Functions</category>
            val inline (&&&) :
              f: 'Arrow<'T,'U1> -> g: 'Arrow<'T,'U2> ->  ^Arrow<'T,('U1 * 'U2)>
                when (Control.Fanout or  ^Arrow<'T,('U1 * 'U2)>) :
                       (static member ``&&&`` :
                          'Arrow<'T,'U1> * 'Arrow<'T,'U2> *
                           ^Arrow<'T,('U1 * 'U2)> * Control.Fanout
                            ->  ^Arrow<'T,('U1 * 'U2)>)
            
            /// <summary>Splits the input between the two argument arrows and merge their outputs. Also known as fanin.</summary>
            /// <category index="24">Arrow Functions</category>
            val inline (|||) :
              f: 'ArrowChoice<'T,'V> -> g: 'ArrowChoice<'U,'V>
                ->  ^ArrowChoice<Choice<'U,'T>,'V>
                when (Control.Fanin or  ^ArrowChoice<Choice<'U,'T>,'V>) :
                       (static member ``|||`` :
                          'ArrowChoice<'T,'V> * 'ArrowChoice<'U,'V> *
                           ^ArrowChoice<Choice<'U,'T>,'V> * Control.Fanin
                            ->  ^ArrowChoice<Choice<'U,'T>,'V>)

