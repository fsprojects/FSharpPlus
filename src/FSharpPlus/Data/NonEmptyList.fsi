namespace FSharpPlus.Data
    
    /// A type-safe list that contains at least one element.
    type NonEmptyList<'t> =
        {
          Head: 't
          Tail: 't list
        }
        interface NonEmptySeq<'t>
        interface System.Collections.Generic.IReadOnlyList<'t>
        interface System.Collections.Generic.IReadOnlyCollection<'t>
        interface System.Collections.IEnumerable
        interface System.Collections.Generic.IEnumerable<'t>
        
        static member
          (+) : NonEmptyList<'a> * x: NonEmptyList<'a> -> NonEmptyList<'a>
        
        static member
          (<*>) : f: NonEmptyList<('T -> 'U)> * x: NonEmptyList<'T>
                    -> NonEmptyList<'U>
        
        static member
          (=>>) : s: NonEmptyList<'a> * g: (NonEmptyList<'a> -> 'b)
                    -> NonEmptyList<'b>
        
        static member
          (>>=) : NonEmptyList<'a> * f: ('a -> NonEmptyList<'b>)
                    -> NonEmptyList<'b>
        
        static member
          inline Choice: source: NonEmptyList< ^Alt<'T>> ->  ^Alt<'T>
                           when (Control.IsAltLeftZero or  ^Alt<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Alt<'T> ref * Control.IsAltLeftZero
                                       -> bool) and
                                (Control.Append or  ^Alt<'T>) :
                                  (static member ``<|>`` :
                                      ^Alt<'T> *  ^Alt<'T> * Control.Append
                                       ->  ^Alt<'T>)
        
        static member
          Duplicate: s: NonEmptyList<'a> * _impl: Control.Duplicate
                       -> NonEmptyList<NonEmptyList<'a>>
        
        static member Extract: NonEmptyList<'t> -> 't
        
        static member Fold: NonEmptyList<'a> * f: ('b -> 'a -> 'b) * z: 'b -> 'b
        
        static member
          FoldBack: NonEmptyList<'a> * f: ('a -> 'b -> 'b) * z: 'b -> 'b
        
        static member
          Lift2: f: ('T -> 'U -> 'V) * x: NonEmptyList<'T> * y: NonEmptyList<'U>
                   -> NonEmptyList<'V>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'W) * x: NonEmptyList<'V> *
                 y: NonEmptyList<'T> * z: NonEmptyList<'U> -> NonEmptyList<'W>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: NonEmptyList<'a> * f: ('a -> 'b) -> NonEmptyList<'b>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          MapIndexed: x: NonEmptyList<'a> * f: (int -> 'a -> 'b)
                        -> NonEmptyList<'b>
        
        static member
          Reduce: NonEmptyList<'T> * reduction: ('T -> 'T -> 'T) -> 'T
        
        static member
          Replace: source: NonEmptyList<'T> * oldValue: NonEmptyList<'T> *
                   newValue: NonEmptyList<'T> * _impl: Control.Replace
                     -> NonEmptyList<'T> when 'T: equality
        
        static member Return: x: 'a -> NonEmptyList<'a>
        
        static member Sum: source: seq<NonEmptyList<'T>> -> NonEmptyList<'T>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToList: s: NonEmptyList<'a> * _impl: Control.ToList -> 'a list
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToSeq: s: NonEmptyList<'a> * _impl: Control.ToSeq -> seq<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Traverse: s: NonEmptyList<'T> * f: ('T ->  ^Functor<'U>)
                             ->  ^Functor<NonEmptyList<'U>>
                             when (Control.Map or  ^Functor<'U> or  ^a) :
                                    (static member Map:
                                       ( ^Functor<'U> *
                                        ('b -> 'b list -> 'b list)) *
                                       Control.Map ->  ^a) and
                                  (Control.Apply or  ^a or  ^c) :
                                    (static member ``<*>`` :
                                        ^a *  ^c *  ^c * Control.Apply ->  ^c) and
                                  (Control.Traverse or 'T list or  ^c) :
                                    (static member Traverse:
                                       'T list * ('T ->  ^Functor<'U>) *  ^c *
                                       Control.Traverse ->  ^c) and
                                  (Control.Return or  ^c) :
                                    (static member Return:
                                        ^c * Control.Return -> ('d list ->  ^c)) and
                                  (Control.Map or  ^c or
                                    ^Functor<NonEmptyList<'U>>) :
                                    (static member Map:
                                       ( ^c * ('e list -> NonEmptyList<'e>)) *
                                       Control.Map
                                         ->  ^Functor<NonEmptyList<'U>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Unzip: s: NonEmptyList<'a * 'b> -> NonEmptyList<'a> * NonEmptyList<'b>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Zip: x: NonEmptyList<'a> * y: NonEmptyList<'b>
                 -> NonEmptyList<'a * 'b>
        
        member GetSlice: (int option * int option -> NonEmptyList<'t>)
        
        member Item: (int -> 't)
        
        member Length: int
        
        [<System.Obsolete ("Use Head instead.")>]
        member head: 't
        
        [<System.Obsolete ("Use Tail instead.")>]
        member tail: 't list
    
    /// A type alias for NonEmptyList<'t>
    type nelist<'t> = NonEmptyList<'t>
    
    /// Basic operations on NonEmptyList
    module NonEmptyList =
        
        /// <summary>Builds a non empty list.</summary>
        val create: x: 'a -> xs: 'a list -> NonEmptyList<'a>
        
        /// <summary>Builds a non empty list with a single element.</summary>
        val singleton: x: 'a -> NonEmptyList<'a>
        
        /// <summary>Builds a list from the given non empty list.</summary>
        val toList: NonEmptyList<'a> -> 'a list
        
        /// <summary>Builds a sequence from the given non empty list.</summary>
        val toSeq: NonEmptyList<'a> -> seq<'a>
        
        /// <summary>Builds an array from the given non empty list.</summary>
        val toArray: nel: NonEmptyList<'a> -> 'a[]
        
        /// <summary>Builds a non empty list from the given array.</summary>
        /// <param name="array">The input array.</param>
        /// <returns>Non empty list containing the elements of the array.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
        /// <remarks>Throws exception for empty array</remarks>
        val ofArray: array: 'a array -> NonEmptyList<'a>
        
        /// <summary>Builds a non empty list from the given list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Non empty list containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty list</remarks>
        val ofList: list: 'a list -> NonEmptyList<'a>
        
        /// <summary>Builds a non empty list from the given sequence.</summary>
        /// <param name="seq">The input list.</param>
        /// <returns>Non empty list containing the elements of the list.</returns>
        /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
        /// <remarks>Throws exception for empty list</remarks>
        val ofSeq: seq: seq<'a> -> NonEmptyList<'a>
        
        /// Returns the length of a non empty list. You can also use property nel.Length.
        val length: nel: NonEmptyList<'a> -> int
        
        /// <summary>Build a new non empty list whose elements are the results of applying the given function
        /// to each of the elements of the non empty list.</summary>
        val map: f: ('a -> 'b) -> NonEmptyList<'a> -> NonEmptyList<'b>
        
        /// <summary>Safely build a new non empty list whose elements are the results of applying the given function
        /// to each of the elements of the two non empty list pairwise.</summary>
        /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
        val map2Shortest:
          f: ('a -> 'b -> 'c) -> l1: NonEmptyList<'a> -> l2: NonEmptyList<'b>
            -> NonEmptyList<'c>
        
        /// <summary>Build a new non empty list whose elements are the results of applying the given function with index
        /// to each of the elements of the non empty list.</summary>
        val mapi: f: (int -> 'a -> 'b) -> NonEmptyList<'a> -> NonEmptyList<'b>
        
        /// <summary>Splits a list of pairs into two lists.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>Two lists of split elements.</returns>
        val unzip:
          list: NonEmptyList<'T1 * 'T2> -> NonEmptyList<'T1> * NonEmptyList<'T2>
        
        /// <summary>Combines the two lists into a list of pairs. The two lists must have equal lengths.</summary>
        /// <param name="list1">The first input list.</param>
        /// <param name="list2">The second input list.</param>
        /// <returns>A single list containing pairs of matching elements from the input lists.</returns>
        val zip:
          list1: NonEmptyList<'T> -> list2: NonEmptyList<'U>
            -> NonEmptyList<'T * 'U>
        
        /// <summary>
        /// Zip safely two lists. If one list is shorter, excess elements are discarded from the right end of the longer list. 
        /// </summary>
        /// <param name="list1">First input list.</param>
        /// <param name="list2">Second input list.</param>
        /// <returns>List with corresponding pairs of input lists.</returns>
        val zipShortest:
          list1: NonEmptyList<'T> -> list2: NonEmptyList<'U>
            -> NonEmptyList<'T * 'U>
        
        /// Returns a new NonEmptyList with the element added to the beginning.
        val cons: e: 'a -> NonEmptyList<'a> -> NonEmptyList<'a>
        
        /// Returns the first element of a new non empty list. You can also use property nel.Head.
        val head: NonEmptyList<'a> -> 'a
        
        /// <summary>Returns a new NonEmptyList of the elements trailing the first element.</summary>
        /// <exception cref="System.ArgumentException">Thrown when the tail is empty.</exception>
        /// <remarks>Throws exception for empty tail</remarks>
        val tail: NonEmptyList<'a> -> NonEmptyList<'a>
        
        val tails: s: NonEmptyList<'a> -> NonEmptyList<NonEmptyList<'a>>
        
        val inline traverse:
          f: ('T ->  ^Functor<'U>) -> s: NonEmptyList<'T>
            ->  ^Functor<NonEmptyList<'U>>
            when (Control.Map or  ^Functor<'U> or  ^a) :
                   (static member Map:
                      ( ^Functor<'U> * ('b -> 'b list -> 'b list)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Functor<'List<'U>>) :
                   (static member ``<*>`` :
                       ^a *  ^Functor<'List<'U>> *  ^Functor<'List<'U>> *
                      Control.Apply ->  ^Functor<'List<'U>>) and
                 (Control.Traverse or 'T list or  ^Functor<'List<'U>>) :
                   (static member Traverse:
                      'T list * ('T ->  ^Functor<'U>) *  ^Functor<'List<'U>> *
                      Control.Traverse ->  ^Functor<'List<'U>>) and
                 (Control.Return or  ^Functor<'List<'U>>) :
                   (static member Return:
                       ^Functor<'List<'U>> * Control.Return
                        -> ('c list ->  ^Functor<'List<'U>>)) and
                 (Control.Map or  ^Functor<'List<'U>> or
                   ^Functor<NonEmptyList<'U>>) :
                   (static member Map:
                      ( ^Functor<'List<'U>> * ('d list -> NonEmptyList<'d>)) *
                      Control.Map ->  ^Functor<NonEmptyList<'U>>)
        
        /// <summary>Returns the average of the elements in the list.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The resulting average.</returns>
        val inline average:
          list: NonEmptyList< ^T> ->  ^T
            when  ^T: (static member get_Zero: ->  ^T) and
                  ^T: (static member DivideByInt:  ^T * int ->  ^T) and
                  ^T: (static member (+) :  ^T *  ^T ->  ^T)
        
        /// <summary>Returns the average of the elements generated by applying the function to each element of the list.</summary>
        /// <param name="projection">The function to transform the list elements into the type to be averaged.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The resulting average.</returns>
        val inline averageBy:
          projection: ('T ->  ^U) -> list: NonEmptyList<'T> ->  ^U
            when  ^U: (static member (+) :  ^U *  ^U ->  ^U) and
                  ^U: (static member DivideByInt:  ^U * int ->  ^U) and
                  ^U: (static member get_Zero: ->  ^U)
        
        /// <summary>Applies a function to each element of the list, threading an accumulator argument
        /// through the computation. Apply the function to the first two elements of the list.
        /// Then feed this result into the function along with the third element and so on. 
        /// Return the final result. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
        /// <c>f (... (f i0 i1) i2 ...) iN</c>.</summary>
        /// <param name="reduction">The function to reduce two list elements to a single element.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The final reduced value.</returns>
        val reduce: reduction: ('T -> 'T -> 'T) -> list: NonEmptyList<'T> -> 'T
        
        /// <summary>Applies a function to each element of the list, starting from the end, threading an accumulator argument
        /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
        /// <c>f i0 (...(f iN-1 iN))</c>.</summary>
        /// <param name="reduction">A function that takes in the next-to-last element of the list and the
        /// current accumulated result to produce the next accumulated result.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The final result of the reductions.</returns>
        val reduceBack:
          reduction: ('T -> 'T -> 'T) -> list: NonEmptyList<'T> -> 'T
        
        /// <summary>Returns the greatest of all elements of the list, compared via Operators.max.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The maximum element.</returns>
        val max: list: NonEmptyList<'T> -> 'T when 'T: comparison
        
        /// <summary>Returns the greatest of all elements of the list, compared via Operators.max on the function result.</summary>
        /// <param name="projection">The function to transform the list elements into the type to be compared.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The maximum element.</returns>
        val maxBy:
          projection: ('T -> 'U) -> list: NonEmptyList<'T> -> 'T
            when 'U: comparison
        
        /// <summary>Returns the lowest of all elements of the list, compared via Operators.min.</summary>
        /// <param name="list">The input list.</param>
        /// <returns>The minimum value.</returns>
        val min: list: NonEmptyList<'T> -> 'T when 'T: comparison
        
        /// <summary>Returns the lowest of all elements of the list, compared via Operators.min on the function result</summary>
        /// <param name="projection">The function to transform list elements into the type to be compared.</param>
        /// <param name="list">The input list.</param>
        /// <returns>The minimum value.</returns>
        val minBy:
          projection: ('T -> 'U) -> list: NonEmptyList<'T> -> 'T
            when 'U: comparison
        
        /// Equivalent to [start..stop] on regular lists.
        val inline range:
          start:  ^T -> stop:  ^T -> NonEmptyList< ^T>
            when  ^T: (static member (+) :  ^T *  ^T ->  ^T) and
                  ^T: (static member get_One: ->  ^T) and  ^T: comparison
        
        /// Reduces using alternative operator `<|>`.
        val inline choice:
          list: NonEmptyList< ^Alt<'T>> ->  ^Alt<'T>
            when (Control.Append or  ^Alt<'T>) :
                   (static member ``<|>`` :
                       ^Alt<'T> *  ^Alt<'T> * Control.Append ->  ^Alt<'T>)
        
        /// Transforms a list to a NonEmptyList, returning an option to signal when the original list was empty.
        val tryOfList: s: 'a list -> NonEmptyList<'a> option
        
        val ofNonEmptySeq: s: NonEmptySeq<'a> -> NonEmptyList<'a>
        
        val toNonEmptySeq: list: NonEmptyList<'a> -> NonEmptySeq<'a>
    
    module NonEmptyListBuilder =
        
        type NelBuilder =
            
            new: unit -> NelBuilder
            
            member Combine: a: 'T * NonEmptyList<'T> -> NonEmptyList<'T>
            
            member Delay: expr: (unit -> 'b) -> 'b
            
            member Run: x: NonEmptyList<'a> -> NonEmptyList<'a>
            
            member Yield: x: 'c -> 'c
            
            [<CompilerMessage
              ("A NonEmptyList doesn't support the Zero operation.", 708)>]
            member Zero: unit -> 'd
        
        [<System.Obsolete ("Use nelist instead.")>]
        val nel: NelBuilder
        
        val nelist: NelBuilder
    
    module NonEmptyListBuilderExtensions =
        type NelBuilder with
            
            member Combine: a: 'T * b: 'T -> NonEmptyList<'T>
        type NelBuilder with
            
            member Run: x: 'a -> NonEmptyList<'a>

