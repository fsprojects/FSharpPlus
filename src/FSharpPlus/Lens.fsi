namespace FSharpPlus
    
    /// Lens functions and operators
    module Lens =
        
        /// [omit]
        module Internals =
            
            val dimap':
              ab: ('a -> 'b) -> cd: ('c -> 'd) -> p: ('b -> 'c) -> ('a -> 'd)
            
            val getAny: Data.Any -> bool
            
            val getAll: Data.All -> bool
            
            [<Struct>]
            type Exchange<'A,'B,'S,'T> =
                | Exchange of ('S -> 'A) * ('B -> 'T)
                
                static member
                  Dimap: Exchange<'a,'b,'c,'d> * f: ('e -> 'c) * g: ('d -> 'f)
                           -> Exchange<'a,'b,'e,'f>
        
        /// <summary>Write to a lens.</summary>
        /// <param name="optic">The lens.</param>
        /// <param name="value">The value we want to write in the part targeted by the lens.</param>
        /// <param name="source">The original object.</param>
        /// <returns>The new object with the value modified.</returns>
        val setl:
          optic: (('a -> Data.Identity<'b>) -> 's -> Data.Identity<'t>)
          -> value: 'b -> source: 's -> 't
        
        /// <summary>Update a value in a lens.</summary>
        /// <param name="optic">The lens.</param>
        /// <param name="updater">A function that converts the value we want to write in the part targeted by the lens.</param>
        /// <param name="source">The original object.</param>
        /// <returns>The new object with the value modified.</returns>
        val over:
          optic: (('a -> Data.Identity<'b>) -> 's -> Data.Identity<'t>)
          -> updater: ('a -> 'b) -> source: 's -> 't
        
        /// <summary>Read from a lens.</summary>
        /// <param name="optic">The lens.</param>
        /// <param name="source">The object.</param>
        /// <returns>The part the lens is targeting.</returns>
        val view:
          optic: (('a -> Data.Const<'a,'b>) -> 's -> Data.Const<'a,'t>)
          -> source: 's -> 'a
        
        /// <summary>Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). See also (^?).</summary>
        /// <param name="optic">The prism.</param>
        /// <param name="source">The object.</param>
        /// <returns>The value (if any) the prism is targeting.</returns>
        val preview:
          optic: (('a -> Data.Const<Data.First<'a>,'b>) -> 's
                    -> Data.Const<Data.First<'a>,'t>) -> source: 's -> 'a option
        
        /// <summary>Build a 'Lens' from a getter and a setter.</summary>
        /// <remarks>The lens should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
        /// <param name="getter">The getter function.</param>
        /// <param name="setter">The setter function, having as first parameter the object and second the value to set.</param>
        /// <param name="f">The free parameter.</param>
        /// <param name="s"></param>
        /// <returns>The lens.</returns>
        val inline lens:
          getter: ('s -> 'a) -> setter: ('s -> 'b -> 't) -> f: ('a ->  ^F<'b>)
          -> s: 's -> 'F<'t>
            when  ^F<'b> : (static member Map:  ^F<'b> * ('b -> 't) -> 'F<'t>)
        
        /// <summary>Build a 'Prism' from a constructor and a getter.</summary>
        /// <remarks>The prism should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
        /// <remarks>Using Result instead of Option to permit the types of 's and 't to differ.</remarks>
        /// <param name="constructor">The constructor function.</param>
        /// <param name="getter">The getter function, having as first parameter the object and second the value to set.</param>
        /// <param name="f">The free parameter.</param>
        /// <returns>The prism.</returns>
        val inline prism:
          constructor: ('b -> 't) -> getter: ('s -> Result<'a,'t>)
          -> f: ('a ->  ^F<'b>) -> ('s ->  ^F<'t>)
            when  ^F<'b> : (static member Map:  ^F<'b> * ('b -> 't) ->  ^F<'t>) and
                  ^F<'t> : (static member Return: 't ->  ^F<'t>)
        
        /// <summary>Build a 'Prism' from a constructor and a getter.</summary>
        /// <remarks>The prism should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
        /// <remarks>Using Option which makes 's and 't the same type.</remarks>
        /// <param name="constructor">The constructor function.</param>
        /// <param name="getter">The getter function, having as first parameter the object and second the value to set.</param>
        /// <param name="f">The free parameter.</param>
        /// <returns>The prism.</returns>
        val inline prism':
          constructor: ('b -> 's) -> getter: ('s -> Option<'a>)
          -> f: ('a ->  ^F<'b>) -> ('s ->  ^F<'t>)
            when  ^F<'b> : (static member Map:  ^F<'b> * ('b -> 's) ->  ^F<'t>) and
                  ^F<'t> : (static member Return: 's ->  ^F<'t>)
        
        /// <summary>Build an 'Iso' from a pair of inverse functions.</summary>
        /// <param name="func">The transform function.</param>
        /// <param name="inv">The inverse of the transform function.</param>
        /// <returns>The iso.</returns>
        val inline iso:
          func: ('s -> 'a) -> inv: ('b -> 't) -> ( ^a0 ->  ^b1)
            when (Control.Dimap or  ^a0 or  ^b1) :
                   (static member Dimap:
                       ^a0 * ('s -> 'a) * ( ^c -> 'd) * Control.Dimap ->  ^b1) and
                  ^c: (static member Map:  ^c * ('b -> 't) -> 'd)
        
        /// Merge two lenses, getters, setters, folds or traversals.
        /// <param name="optic1">The first optic.</param>
        /// <param name="optic2">The second optic.</param>
        /// <param name="f">The free parameter.</param>
        /// <returns>An optic for a Result which uses the first optic for the Ok and the second for the Error.</returns>
        val inline choosing<'s1,'t1,'s2,'t2,'a,'b, ^F<'t1>, ^F<'t2>,'F<'b>,
                            'F<Result<'t2,'t1>>
                              when  ^F<'t1> :
                                     (static member Map:
                                         ^F<'t1> * ('t1 -> Result<'t2,'t1>)
                                          -> 'F<Result<'t2,'t1>>) and
                                    ^F<'t2> :
                                     (static member Map:
                                         ^F<'t2> * ('t2 -> Result<'t2,'t1>)
                                          -> 'F<Result<'t2,'t1>>)> :
          optic1: (('a -> 'F<'b>) -> 's1 ->  ^F<'t1>)
          -> optic2: (('a -> 'F<'b>) -> 's2 ->  ^F<'t2>) -> f: ('a -> 'F<'b>)
          -> _arg1: Result<'s2,'s1> -> 'F<Result<'t2,'t1>>
            when  ^F<'t1> :
                   (static member Map:
                       ^F<'t1> * ('t1 -> Result<'t2,'t1>) -> 'F<Result<'t2,'t1>>) and
                  ^F<'t2> :
                   (static member Map:
                       ^F<'t2> * ('t2 -> Result<'t2,'t1>) -> 'F<Result<'t2,'t1>>)
        
        /// Lens for the first element of a tuple
        val inline _1:
          f: ('a ->  ^b) -> t:  ^f -> 'e
            when  ^b: (static member Map:  ^b * ('c -> 'd) -> 'e) and
                 (Control.MapItem1 or  ^f) :
                   (static member MapItem1:  ^f * ('g -> 'c) -> 'd) and
                  ^f: (member get_Item1:  ^f -> 'a)
        
        /// Lens for the second element of a tuple
        val inline _2:
          f: ('a ->  ^b) -> t:  ^f -> 'e
            when  ^b: (static member Map:  ^b * ('c -> 'd) -> 'e) and
                 (Control.MapItem2 or  ^f) :
                   (static member MapItem2:  ^f * ('g -> 'c) -> 'd) and
                  ^f: (member get_Item2:  ^f -> 'a)
        
        /// Lens for the third element of a tuple
        val inline _3:
          f: ('a ->  ^b) -> t:  ^f -> 'e
            when  ^b: (static member Map:  ^b * ('c -> 'd) -> 'e) and
                 (Control.MapItem3 or  ^f) :
                   (static member MapItem3:  ^f * ('g -> 'c) -> 'd) and
                  ^f: (member get_Item3:  ^f -> 'a)
        
        /// Lens for the fourth element of a tuple
        val inline _4:
          f: ('a ->  ^b) -> t:  ^f -> 'e
            when  ^b: (static member Map:  ^b * ('c -> 'd) -> 'e) and
                 (Control.MapItem4 or  ^f) :
                   (static member MapItem4:  ^f * ('g -> 'c) -> 'd) and
                  ^f: (member get_Item4:  ^f -> 'a)
        
        /// Lens for the fifth element of a tuple
        val inline _5:
          f: ('a ->  ^b) -> t:  ^f -> 'e
            when  ^b: (static member Map:  ^b * ('c -> 'd) -> 'e) and
                 (Control.MapItem5 or  ^f) :
                   (static member MapItem5:  ^f * ('g -> 'c) -> 'd) and
                  ^f: (member get_Item5:  ^f -> 'a)
        
        module List =
            
            /// Given a specific key, produces a Lens from a List<value> to an Option<value>. When setting,
            /// a Some(value) will insert or replace the value into the list at the given index. Setting a value of
            /// None will delete the value at the specified index.  Works well together with non.
            val inline _item:
              i: int -> f: ('a option ->  ^b) -> t: 'a list -> 'c
                when  ^b:
                       (static member Map:  ^b * ('a option -> 'a list) -> 'c)
        
        module Array =
            
            /// Given a specific key, produces a Lens from a Array<value> to an Option<value>.
            val inline _item:
              i: int -> f: ('a option ->  ^b) -> t: 'a[] -> 'c
                when  ^b: (static member Map:  ^b * ('a -> 'a[]) -> 'c)
        
        module Set =
            
            val inline _contains:
              i: 'a -> f: (bool ->  ^b) -> t: Set<'a> -> 'c
                when 'a: comparison and
                      ^b: (static member Map:  ^b * (bool -> Set<'a>) -> 'c)
        
        module Map =
            
            /// Given a specific key, produces a Lens from a Map<key, value> to an Option<value>.  When setting,
            /// a Some(value) will insert or replace the value into the map at the given key.  Setting a value of
            /// None will delete the value at the specified key.  Works well together with non.
            val inline _item:
              i: 'a -> f: ('b option ->  ^c) -> t: Map<'a,'b> -> 'd
                when 'a: comparison and
                      ^c:
                       (static member Map:  ^c * ('b option -> Map<'a,'b>) -> 'd)
        
        module IReadOnlyDictionary =
            
            /// Given a specific key, produces a Lens from a IReadOnlyDictionary<key, value> to an Option<value>.  When setting,
            /// a Some(value) will insert or replace the value into the dictionary at the given key.  Setting a value of
            /// None will delete the value at the specified key.  Works well together with non.
            val inline _item:
              i: 'a -> f: ('b option ->  ^c)
              -> t: System.Collections.Generic.IReadOnlyDictionary<'a,'b> -> 'd
                when 'a: comparison and
                      ^c:
                       (static member Map:
                           ^c *
                          ('b option
                             -> System.Collections.Generic.IReadOnlyDictionary<'a,
                                                                               'b>)
                            -> 'd)
        
        /// Lens for the value inside an Option or the given default value if the Option is None.  Works well when combined with Map._item
        val inline non:
          def: 'a -> f: ('a ->  ^b) -> ma: 'a option -> 'c
            when 'a: equality and
                  ^b: (static member Map:  ^b * ('a -> 'a option) -> 'c)
        
        /// Prism providing a Traversal for targeting the 'Ok' part of a Result<'T,'Error>
        val inline _Ok:
          x: ('a ->  ^b) -> (Result<'a,'d> ->  ^e)
            when  ^b: (static member Map:  ^b * ('c -> Result<'c,'d>) ->  ^e) and
                  ^e: (static member Return: Result<'c,'d> ->  ^e)
        
        /// Prism providing a Traversal for targeting the 'Error' part of a Result<'T,'Error>
        val inline _Error:
          x: ('a ->  ^b) -> (Result<'d,'a> ->  ^e)
            when  ^b: (static member Map:  ^b * ('c -> Result<'d,'c>) ->  ^e) and
                  ^e: (static member Return: Result<'d,'c> ->  ^e)
        
        /// Prism providing a Traversal for targeting the 'Some' part of an Option<'T>
        val inline _Some:
          x: ('a ->  ^b) -> ('a option ->  ^d)
            when  ^b: (static member Map:  ^b * ('c -> 'c option) ->  ^d) and
                  ^d: (static member Return: 'c option ->  ^d)
        
        /// Prism providing a Traversal for targeting the 'None' part of an Option<'T>
        val inline _None:
          x: (unit ->  ^a) -> ('c option ->  ^d)
            when  ^a: (static member Map:  ^a * ('b -> 'c option) ->  ^d) and
                  ^d: (static member Return: 'c option ->  ^d)
        
        val inline _all:
          ref: 'a -> f: ('a ->  ^b) -> s:  ^c ->  ^d
            when 'a: equality and  ^b: (static member Return: 'a ->  ^b) and
                 (Control.Traverse or  ^c or  ^d) :
                   (static member Traverse:
                       ^c * ('a ->  ^b) *  ^d * Control.Traverse ->  ^d)
        
        val inline to':
          k: ('a -> 'b) -> ( ^c ->  ^d)
            when (Control.Dimap or  ^c or  ^d) :
                   (static member Dimap:
                       ^c * ('a -> 'b) * ( ^e -> 'f) * Control.Dimap ->  ^d) and
                  ^e: (static member Contramap:  ^e * ('a -> 'b) -> 'f)
        
        val foldMapOf:
          l: (('a -> Data.Const<'b,'c>) -> 'd -> Data.Const<'e,'f>)
          -> f: ('a -> 'b) -> ('d -> 'e)
        
        val foldOf:
          l: (('a -> Data.Const<'a,'b>) -> 'c -> Data.Const<'d,'e>)
            -> ('c -> 'd)
        
        val foldrOf:
          l: (('a -> Data.Const<Data.Endo<'b>,'c>) -> 'd
                -> Data.Const<Data.Endo<'e>,'f>) -> f: ('a -> 'b -> 'b) -> z: 'e
            -> ('d -> 'e)
        
        val foldlOf:
          l: (('a -> Data.Const<Data.Dual<Data.Endo<'b>>,'c>) -> 'd
                -> Data.Const<Data.Dual<Data.Endo<'e>>,'f>)
          -> f: ('b -> 'a -> 'b) -> z: 'e -> ('d -> 'e)
        
        /// Extract a list of the targets of a Fold. See also (^..).
        val toListOf:
          l: (('a -> Data.Const<Data.Endo<'a list>,'b>) -> 'c
                -> Data.Const<Data.Endo<'d list>,'e>) -> ('c -> 'd list)
        
        /// Get the largest target of a Fold.
        val maximumOf:
          l: (('a -> Data.Const<Data.Dual<Data.Endo<'a option>>,'b>) -> 'c
                -> Data.Const<Data.Dual<Data.Endo<'d option>>,'e>)
            -> ('c -> 'd option) when 'a: comparison
        
        /// Get the smallest target of a Fold.
        val minimumOf:
          l: (('a -> Data.Const<Data.Dual<Data.Endo<'a option>>,'b>) -> 'c
                -> Data.Const<Data.Dual<Data.Endo<'d option>>,'e>)
            -> ('c -> 'd option) when 'a: comparison
        
        val anyOf:
          l: (('a -> Data.Const<Data.Any,'b>) -> 'c -> Data.Const<Data.Any,'d>)
          -> f: ('a -> bool) -> ('c -> bool)
        
        val allOf:
          l: (('a -> Data.Const<Data.All,'b>) -> 'c -> Data.Const<Data.All,'d>)
          -> f: ('a -> bool) -> ('c -> bool)
        
        val elemOf:
          l: (('a -> Data.Const<Data.Any,'b>) -> 'c -> Data.Const<Data.Any,'d>)
            -> ('a -> 'c -> bool) when 'a: equality
        
        val inline items:
          x: ('a -> 'b) -> ( ^c ->  ^d)
            when (Control.Traverse or  ^c or  ^d) :
                   (static member Traverse:
                       ^c * ('a -> 'b) *  ^d * Control.Traverse ->  ^d)
        
        val inline filtered:
          p: ('a -> bool) -> f: ('a ->  ^b) -> s: 'a ->  ^b
            when  ^b: (static member Return: 'a ->  ^b)
        
        val inline choosed:
          p: ('a -> 'b option) -> f: ('b ->  ^c) -> s: 'a ->  ^c
            when  ^c: (static member Return: 'a ->  ^c)
        
        val inline both:
          f: ('a ->  ^b) -> a: 'a * b: 'a ->  ^f
            when  ^b: (static member Map:  ^b * ('c -> 'd -> 'c * 'd) ->  ^e) and
                 ( ^e or  ^b or  ^f) : (static member (<*>) :  ^e *  ^b ->  ^f)
        
        val inline withIso:
          ai: (Internals.Exchange<'a,'b,'a,Data.Identity<'b>>
                 -> Internals.Exchange<'c,'d,'e,Data.Identity<'f>>)
          -> k: (('e -> 'c) -> ('d -> 'f) -> 'g) -> 'g
        
        val inline from':
          l: (Internals.Exchange<'a,'b,'a,Data.Identity<'b>>
                -> Internals.Exchange<'c,'d,'e,Data.Identity<'f>>)
            -> ( ^g ->  ^h)
            when (Control.Dimap or  ^g or  ^h) :
                   (static member Dimap:
                       ^g * ('d -> 'f) * ( ^i -> 'j) * Control.Dimap ->  ^h) and
                  ^i: (static member Map:  ^i * ('e -> 'c) -> 'j)
        
        val inline mapping:
          k: (Internals.Exchange<'a,'b,'a,Data.Identity<'b>>
                -> Internals.Exchange<'c,'d,'e,Data.Identity<'f>>)
            -> ( ^g ->  ^h)
            when (Control.Dimap or  ^g or  ^h) :
                   (static member Dimap:
                       ^g * ( ^i -> 'j) * ( ^k -> 'n) * Control.Dimap ->  ^h) and
                  ^i: (static member Map:  ^i * ('e -> 'c) -> 'j) and
                  ^k: (static member Map:  ^k * ( ^l -> 'm) -> 'n) and
                  ^l: (static member Map:  ^l * ('d -> 'f) -> 'm)
        
        /// <summary>Read from a lens. Same as ``view`` but with the arguments flipped.</summary>
        /// <param name="lens">The lens.</param>
        /// <param name="source">The object.</param>
        /// <returns>The part the lens is targeting.</returns>
        val (^.) :
          source: 's
          -> lens: (('a -> Data.Const<'a,'b>) -> 's -> Data.Const<'a,'t>) -> 'a
        
        /// <summary>Write to a lens. Same as ``setl``.</summary>
        /// <param name="lens">The lens.</param>
        /// <param name="value">The value we want to write in the part targeted by the lens.</param>
        /// <returns>The new object with the value modified.</returns>
        val (.->) :
          lens: (('a -> Data.Identity<'b>) -> 's -> Data.Identity<'t>)
          -> value: 'b -> ('s -> 't)
        
        /// <summary>Update a value in a lens. Same as ``over``.</summary>
        /// <param name="lens">The lens.</param>
        /// <param name="updater">A function that converts the value we want to write in the part targeted by the lens.</param>
        /// <returns>The new object with the value modified.</returns>
        val (%->) :
          lens: (('a -> Data.Identity<'b>) -> 's -> Data.Identity<'t>)
          -> updater: ('a -> 'b) -> ('s -> 't)
        
        /// <summary>Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). Same as ``preview`` but with the arguments flipped.</summary>
        /// <param name="prism">The prism.</param>
        /// <param name="source">The object.</param>
        /// <returns>The value (if any) the prism is targeting.</returns>
        val (^?) :
          source: 's
          -> prism: (('a -> Data.Const<Data.First<'a>,'b>) -> 's
                       -> Data.Const<Data.First<'a>,'t>) -> 'a option
        
        /// Extract a list of the targets of a Fold. Same as ``toListOf`` but with the arguments flipped.
        val (^..) :
          s: 'a
          -> l: (('b -> Data.Const<Data.Endo<'b list>,'c>) -> 'a
                   -> Data.Const<Data.Endo<'d list>,'e>) -> 'd list
        
        /// <summary>An infix flipped map, restricted to non-primitive types.</summary>
        /// <param name="x">The functor.</param>
        /// <param name="f">The mapper function.</param>
        /// <returns>The mapped Functor.</returns>
        val inline (<&>) :
          x:  ^F<'t> -> f: ('t -> 'u) -> 'F<'u>
            when  ^F<'t> : (static member Map:  ^F<'t> * ('t -> 'u) -> 'F<'u>)

