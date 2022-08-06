namespace FSharpPlus.Control
    
    [<Class>]
    type Sequence =
        inherit Internals.Default1
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline ForInfiniteSequences: t: seq< ^a> * isFailure: ( ^a -> bool) *
                                       conversion: ('g list -> 'h) ->  ^f
                                         when (Apply or  ^b or  ^a or  ^c) :
                                                (static member ``<*>`` :
                                                    ^b *  ^a *  ^c * Apply
                                                     ->  ^c) and
                                              (Map or  ^c or  ^b) :
                                                (static member Map:
                                                   ( ^c *
                                                    ('e list -> 'e -> 'e list)) *
                                                   Map ->  ^b) and
                                              (Return or  ^c) :
                                                (static member Return:
                                                    ^c * Return
                                                     -> ('d list ->  ^c)) and
                                              (Map or  ^c or  ^f) :
                                                (static member Map:
                                                   ( ^c * ('g list -> 'h)) * Map
                                                     ->  ^f)
        
        static member
          inline Invoke: t:  ^Traversable<'Applicative<'T>>
                           ->  ^Applicative<'Traversable<'T>>
                           when (Sequence or  ^Traversable<'Applicative<'T>> or
                                  ^Applicative<'Traversable<'T>>) :
                                  (static member Sequence:
                                      ^Traversable<'Applicative<'T>> *
                                      ^Applicative<'Traversable<'T>> * Sequence
                                       ->  ^Applicative<'Traversable<'T>>)
        
        static member
          inline InvokeOnInstance: t:  ^Traversable<Functor<'T>>
                                     -> 'Functor<'Traversable<'T>>
                                     when  ^Traversable<Functor<'T>> :
                                            (static member Sequence:
                                                ^Traversable<Functor<'T>>
                                                 -> 'Functor<'Traversable<'T>>)
        
        static member
          inline Sequence: t: ResizeArray< ^Functor<'T>> *
                           _output:  ^Functor<ResizeArray<'T>> * _impl: Sequence
                             ->  ^Functor<ResizeArray<'T>>
                             when (Apply or  ^a or  ^Functor<'T> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Functor<'T> *  ^b * Apply ->  ^b) and
                                  (IsLeftZero or  ^Functor<'T>) :
                                    (static member IsLeftZero:
                                        ^Functor<'T> ref * IsLeftZero -> bool) and
                                  (Apply or  ^g or  ^Functor<'T> or  ^f) :
                                    (static member ``<*>`` :
                                        ^g *  ^Functor<'T> *  ^f * Apply ->  ^f) and
                                  (Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) * Map
                                         ->  ^a) and
                                  (Return or  ^b) :
                                    (static member Return:
                                        ^b * Return -> ('c list ->  ^b)) and
                                  (Map or  ^b or  ^Functor<ResizeArray<'T>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> seq<'e>)) * Map
                                         ->  ^Functor<ResizeArray<'T>>) and
                                  (Traverse or ResizeArray< ^Functor<'T>> or
                                    ^Functor<ResizeArray<'T>>) :
                                    (static member Traverse:
                                       ResizeArray< ^Functor<'T>> *
                                       ( ^Functor<'T> ->  ^Functor<'T>) *
                                        ^Functor<ResizeArray<'T>> * Traverse
                                         ->  ^Functor<ResizeArray<'T>>) and
                                  (Map or  ^f or  ^Functor<ResizeArray<'T>>) :
                                    (static member Map:
                                       ( ^f * ('j list -> seq<'j>)) * Map
                                         ->  ^Functor<ResizeArray<'T>>) and
                                  (Return or  ^f) :
                                    (static member Return:
                                        ^f * Return -> ('i list ->  ^f)) and
                                  (Map or  ^f or  ^g) :
                                    (static member Map:
                                       ( ^f * ('h list -> 'h -> 'h list)) * Map
                                         ->  ^g)
        
        static member
          inline Sequence: t: Internals.Id< ^Functor<'T>> *
                           _output:  ^Functor<Id<'T>> * _impl: Sequence
                             ->  ^Functor<Id<'T>>
                             when (Map or  ^Functor<'T> or  ^Functor<Id<'T>>) :
                                    (static member Map:
                                       ( ^Functor<'T> * ('a -> Internals.Id<'a>)) *
                                       Map ->  ^Functor<Id<'T>>) and
                                  (Traverse or Internals.Id< ^Functor<'T>> or
                                    ^Functor<Id<'T>>) :
                                    (static member Traverse:
                                       Internals.Id< ^Functor<'T>> *
                                       ( ^Functor<'T> ->  ^Functor<'T>) *
                                        ^Functor<Id<'T>> * Traverse
                                         ->  ^Functor<Id<'T>>)
        
        static member
          inline Sequence: t:  ^a[] * _output:  ^R * _impl: Sequence ->  ^R
                             when (Apply or  ^b or  ^a or  ^c) :
                                    (static member ``<*>`` :
                                        ^b *  ^a *  ^c * Apply ->  ^c) and
                                  (IsLeftZero or  ^a) :
                                    (static member IsLeftZero:
                                        ^a ref * IsLeftZero -> bool) and
                                  (Map or  ^c or  ^b) :
                                    (static member Map:
                                       ( ^c * ('e list -> 'e -> 'e list)) * Map
                                         ->  ^b) and
                                  (Return or  ^c) :
                                    (static member Return:
                                        ^c * Return -> ('d list ->  ^c)) and
                                  (Map or  ^c or  ^R) :
                                    (static member Map:
                                       ( ^c * ('f list -> 'f[])) * Map ->  ^R)
        
        static member
          inline Sequence: t: Choice< ^Functor<'T>,'Error> *
                           _output:  ^Functor<Choice<'T,'Error>> *
                           _impl: Sequence ->  ^Functor<Choice<'T,'Error>>
                             when (Map or  ^Functor<'T> or
                                    ^Functor<Choice<'T,'Error>>) :
                                    (static member Map:
                                       ( ^Functor<'T> *
                                        ('T -> Choice<'T,'Error>)) * Map
                                         ->  ^Functor<Choice<'T,'Error>>) and
                                  (Return or  ^Functor<Choice<'T,'Error>>) :
                                    (static member Return:
                                        ^Functor<Choice<'T,'Error>> * Return
                                         -> (Choice<'T,'Error>
                                               ->  ^Functor<Choice<'T,'Error>>))
        
        static member
          inline Sequence: t: Result< ^Functor<'T>,'Error> *
                           _output:  ^Functor<Result<'T,'Error>> *
                           _impl: Sequence ->  ^Functor<Result<'T,'Error>>
                             when (Map or  ^Functor<'T> or
                                    ^Functor<Result<'T,'Error>>) :
                                    (static member Map:
                                       ( ^Functor<'T> *
                                        ('T -> Result<'T,'Error>)) * Map
                                         ->  ^Functor<Result<'T,'Error>>) and
                                  (Return or  ^Functor<Result<'T,'Error>>) :
                                    (static member Return:
                                        ^Functor<Result<'T,'Error>> * Return
                                         -> (Result<'T,'Error>
                                               ->  ^Functor<Result<'T,'Error>>))
        
        static member
          inline Sequence: t: Map<'a, ^b> * _output:  ^R * _impl: Sequence
                             ->  ^R
                             when 'a: comparison and
                                  (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('d -> Map<'a,'d> -> Map<'a,'d>)) *
                                       Map ->  ^c) and
                                  (Apply or  ^c or  ^R) :
                                    (static member ``<*>`` :
                                        ^c *  ^R *  ^R * Apply ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> (Map<'e,'f> ->  ^R)) and
                                  'e: comparison
        
        static member
          inline Sequence: t:  ^a list * _output:  ^R * _impl: Sequence ->  ^R
                             when (Apply or  ^b or  ^a or  ^c) :
                                    (static member ``<*>`` :
                                        ^b *  ^a *  ^c * Apply ->  ^c) and
                                  (IsLeftZero or  ^a) :
                                    (static member IsLeftZero:
                                        ^a ref * IsLeftZero -> bool) and
                                  (Map or  ^c or  ^b) :
                                    (static member Map:
                                       ( ^c * ('e list -> 'e -> 'e list)) * Map
                                         ->  ^b) and
                                  (Return or  ^c) :
                                    (static member Return:
                                        ^c * Return -> ('d list ->  ^c)) and
                                  (Map or  ^c or  ^R) :
                                    (static member Map:
                                       ( ^c * ('f list -> 'f list)) * Map ->  ^R)
        
        static member
          inline Sequence: t:  ^a option * _output:  ^R * _impl: Sequence ->  ^R
                             when (Map or  ^a or  ^R) :
                                    (static member Map:
                                       ( ^a * ('b -> 'b option)) * Map ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> ('c option ->  ^R))
        
        static member
          inline Sequence: t:  ^a * _output: 'R * _impl: Internals.Default1
                             -> 'R when  ^a: (static member Sequence:  ^a -> 'R)
        
        static member
          inline Sequence: t:  ^a * _output: 'R * _impl: Internals.Default2
                             -> 'R
                             when  ^a:
                                    (static member Traverse:
                                        ^a * ('a0 -> 'a0) -> 'R)
        
        static member
          Sequence: t: Data.NonEmptySeq<Async<'t>> *
                    _output: Async<Data.NonEmptySeq<'t>> *
                    _impl: Internals.Default3 -> Async<Data.NonEmptySeq<'t>>
        
        static member
          Sequence: t: Data.NonEmptySeq<'t[]> * _output: Data.NonEmptySeq<'t>[] *
                    _impl: Internals.Default3 -> Data.NonEmptySeq<'t>[]
        
        static member
          Sequence: t: Data.NonEmptySeq<'t list> *
                    _output: Data.NonEmptySeq<'t> list *
                    _impl: Internals.Default3 -> Data.NonEmptySeq<'t> list
        
        static member
          Sequence: t: Data.NonEmptySeq<Choice<'t,'e>> *
                    _output: Choice<Data.NonEmptySeq<'t>,'e> *
                    _impl: Internals.Default3 -> Choice<Data.NonEmptySeq<'t>,'e>
        
        static member
          Sequence: t: Data.NonEmptySeq<Result<'t,'e>> *
                    _output: Result<Data.NonEmptySeq<'t>,'e> *
                    _impl: Internals.Default3 -> Result<Data.NonEmptySeq<'t>,'e>
        
        static member
          Sequence: t: Data.NonEmptySeq<'t option> *
                    _output: Data.NonEmptySeq<'t> option *
                    _impl: Internals.Default3 -> Data.NonEmptySeq<'t> option
        
        static member
          inline Sequence: t: Data.NonEmptySeq< ^Applicative<'T>> *
                           _output:  ^Applicative<NonEmptySeq<'T>> *
                           _impl: Internals.Default4
                             ->  ^Applicative<NonEmptySeq<'T>>
                             when (Apply or  ^a or  ^Applicative<'T> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Applicative<'T> *  ^b * Apply
                                         ->  ^b) and
                                  (IsLeftZero or  ^Applicative<'T>) :
                                    (static member IsLeftZero:
                                        ^Applicative<'T> ref * IsLeftZero
                                         -> bool) and
                                  (Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) * Map
                                         ->  ^a) and
                                  (Return or  ^b) :
                                    (static member Return:
                                        ^b * Return -> ('c list ->  ^b)) and
                                  (Map or  ^b or  ^Applicative<NonEmptySeq<'T>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> Data.NonEmptySeq<'e>)) *
                                       Map ->  ^Applicative<NonEmptySeq<'T>>)
        
        static member
          Sequence: t: seq<Async<'t>> * _output: Async<seq<'t>> *
                    _impl: Internals.Default3 -> Async<seq<'t>>
        
        static member
          Sequence: t: seq<'t[]> * _output: seq<'t>[] *
                    _impl: Internals.Default3 -> seq<'t>[]
        
        static member
          Sequence: t: seq<'t list> * _output: seq<'t> list *
                    _impl: Internals.Default3 -> seq<'t> list
        
        static member
          Sequence: t: seq<Choice<'t,'e>> * _output: Choice<seq<'t>,'e> *
                    _impl: Internals.Default3 -> Choice<seq<'t>,'e>
        
        static member
          Sequence: t: seq<Result<'t,'e>> * _output: Result<seq<'t>,'e> *
                    _impl: Internals.Default3 -> Result<seq<'t>,'e>
        
        static member
          Sequence: t: seq<'t option> * _output: seq<'t> option *
                    _impl: Internals.Default3 -> seq<'t> option
        
        static member
          inline Sequence: t: seq< ^Applicative<'T>> *
                           _output:  ^Applicative<seq<'T>> *
                           _impl: Internals.Default4 ->  ^Applicative<seq<'T>>
                             when (Apply or  ^a or  ^Applicative<'T> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Applicative<'T> *  ^b * Apply
                                         ->  ^b) and
                                  (IsLeftZero or  ^Applicative<'T>) :
                                    (static member IsLeftZero:
                                        ^Applicative<'T> ref * IsLeftZero
                                         -> bool) and
                                  (Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) * Map
                                         ->  ^a) and
                                  (Return or  ^b) :
                                    (static member Return:
                                        ^b * Return -> ('c list ->  ^b)) and
                                  (Map or  ^b or  ^Applicative<seq<'T>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> seq<'e>)) * Map
                                         ->  ^Applicative<seq<'T>>)
        
        static member
          inline Sequence: t: seq< ^a> * _output:  ^R *
                           _impl: Internals.Default5 ->  ^R
                             when (Map or  ^a or  ^b) :
                                    (static member Map:
                                       ( ^a * ('a0 -> seq<'a0> -> seq<'a0>)) *
                                       Map ->  ^b) and
                                  (Apply or  ^b or  ^R) :
                                    (static member ``<*>`` :
                                        ^b *  ^R *  ^R * Apply ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> (seq<'c> ->  ^R))
    
    [<Class>]
    type Traverse =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: ('T -> 'Functor<'U>) -> t:  ^Traversable<'T>
                           ->  ^Functor<'Traversable<'U>>
                           when (Traverse or  ^Traversable<'T> or
                                  ^Functor<'Traversable<'U>>) :
                                  (static member Traverse:
                                      ^Traversable<'T> * ('T -> 'Functor<'U>) *
                                      ^Functor<'Traversable<'U>> * Traverse
                                       ->  ^Functor<'Traversable<'U>>)
        
        static member
          inline InvokeOnInstance: f: 'a -> t:  ^a0 -> 'R
                                     when  ^a0:
                                            (static member Traverse:
                                                ^a0 * 'a -> 'R)
        
        static member
          inline Traverse: t: 'a[] * f: ('a ->  ^b) * _output:  ^R *
                           _impl: Traverse ->  ^R
                             when (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('d -> 'd[] -> 'd[])) * Map ->  ^c) and
                                  (Apply or  ^c or  ^R) :
                                    (static member ``<*>`` :
                                        ^c *  ^R *  ^R * Apply ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> ('e[] ->  ^R))
        
        static member
          inline Traverse: t: 'a list * f: ('a ->  ^b) * _output:  ^R *
                           _impl: Traverse ->  ^R
                             when (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('d -> 'd list -> 'd list)) * Map
                                         ->  ^c) and
                                  (Apply or  ^c or  ^R) :
                                    (static member ``<*>`` :
                                        ^c *  ^R *  ^R * Apply ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> ('e list ->  ^R))
        
        static member
          inline Traverse: t: Choice<'T,'Error> * f: ('T ->  ^Functor<'U>) *
                           _output:  ^Functor<Choice<'U,'Error>> *
                           _impl: Traverse ->  ^Functor<Choice<'U,'Error>>
                             when (Map or  ^Functor<'U> or
                                    ^Functor<Choice<'U,'Error>>) :
                                    (static member Map:
                                       ( ^Functor<'U> *
                                        ('U -> Choice<'U,'Error>)) * Map
                                         ->  ^Functor<Choice<'U,'Error>>) and
                                  (Return or  ^Functor<Choice<'U,'Error>>) :
                                    (static member Return:
                                        ^Functor<Choice<'U,'Error>> * Return
                                         -> (Choice<'U,'Error>
                                               ->  ^Functor<Choice<'U,'Error>>))
        
        static member
          inline Traverse: t: Result<'T,'Error> * f: ('T ->  ^Functor<'U>) *
                           _output:  ^Functor<Result<'U,'Error>> *
                           _impl: Traverse ->  ^Functor<Result<'U,'Error>>
                             when (Map or  ^Functor<'U> or
                                    ^Functor<Result<'U,'Error>>) :
                                    (static member Map:
                                       ( ^Functor<'U> *
                                        ('U -> Result<'U,'Error>)) * Map
                                         ->  ^Functor<Result<'U,'Error>>) and
                                  (Return or  ^Functor<Result<'U,'Error>>) :
                                    (static member Return:
                                        ^Functor<Result<'U,'Error>> * Return
                                         -> (Result<'U,'Error>
                                               ->  ^Functor<Result<'U,'Error>>))
        
        static member
          inline Traverse: t: Map<'a,'b> * f: ('b ->  ^c) * _output:  ^R *
                           _impl: Traverse ->  ^R
                             when 'a: comparison and
                                  (Map or  ^c or  ^d) :
                                    (static member Map:
                                       ( ^c * ('e -> Map<'a,'e> -> Map<'a,'e>)) *
                                       Map ->  ^d) and
                                  (Apply or  ^d or  ^R) :
                                    (static member ``<*>`` :
                                        ^d *  ^R *  ^R * Apply ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> (Map<'f,'g> ->  ^R)) and
                                  'f: comparison
        
        static member
          inline Traverse: t: 'a option * f: ('a ->  ^b) * _output:  ^R *
                           _impl: Traverse ->  ^R
                             when (Map or  ^b or  ^R) :
                                    (static member Map:
                                       ( ^b * ('c -> 'c option)) * Map ->  ^R) and
                                  (Return or  ^R) :
                                    (static member Return:
                                        ^R * Return -> ('d option ->  ^R))
        
        static member
          Traverse: t: Internals.Id<'t> * f: ('t -> 'u option) *
                    _output: Internals.Id<'u> option * _impl: Traverse
                      -> Internals.Id<'u> option
        
        static member
          Traverse: t: Data.NonEmptySeq<'t> * f: ('t -> Async<'u>) *
                    _output: Async<Data.NonEmptySeq<'u>> * _impl: Traverse
                      -> Async<Data.NonEmptySeq<'u>>
        
        static member
          Traverse: t: seq<'t> * f: ('t -> Async<'u>) * _output: Async<seq<'u>> *
                    _impl: Traverse -> Async<seq<'u>>
        
        static member
          inline Traverse:  ^a * 'a0 * 'R * _impl: Internals.Default1
                             -> ('b -> 'b) when  ^a: null and  ^a: struct
        
        static member
          inline Traverse: t:  ^a * f: 'a0 * _output: 'R *
                           _impl: Internals.Default1 -> 'R
                             when  ^a: (static member Traverse:  ^a * 'a0 -> 'R)
        
        static member
          inline Traverse: t: Data.NonEmptySeq<'T> * f: ('T ->  ^Functor<'U>) *
                           _output:  ^Functor<NonEmptySeq<'U>> *
                           _impl: Internals.Default2
                             ->  ^Functor<NonEmptySeq<'U>>
                             when (Apply or  ^a or  ^Functor<'U> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Functor<'U> *  ^b * Apply ->  ^b) and
                                  (IsLeftZero or  ^Functor<'U>) :
                                    (static member IsLeftZero:
                                        ^Functor<'U> ref * IsLeftZero -> bool) and
                                  (Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) * Map
                                         ->  ^a) and
                                  (Return or  ^b) :
                                    (static member Return:
                                        ^b * Return -> ('c list ->  ^b)) and
                                  (Map or  ^b or  ^Functor<NonEmptySeq<'U>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> Data.NonEmptySeq<'e>)) *
                                       Map ->  ^Functor<NonEmptySeq<'U>>)
        
        static member
          inline Traverse: t: seq<'T> * f: ('T ->  ^Functor<'U>) *
                           _output:  ^Functor<seq<'U>> *
                           _impl: Internals.Default2 ->  ^Functor<seq<'U>>
                             when (Apply or  ^a or  ^Functor<'U> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Functor<'U> *  ^b * Apply ->  ^b) and
                                  (IsLeftZero or  ^Functor<'U>) :
                                    (static member IsLeftZero:
                                        ^Functor<'U> ref * IsLeftZero -> bool) and
                                  (Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) * Map
                                         ->  ^a) and
                                  (Return or  ^b) :
                                    (static member Return:
                                        ^b * Return -> ('c list ->  ^b)) and
                                  (Map or  ^b or  ^Functor<seq<'U>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> seq<'e>)) * Map
                                         ->  ^Functor<seq<'U>>)
        
        static member
          inline Traverse: t: Data.NonEmptySeq<'a> * f: ('a ->  ^b) *
                           _output: 'R * _impl: Internals.Default3 ->  ^f
                             when (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('a0 -> seq<'a0> -> seq<'a0>)) *
                                       Map ->  ^c) and
                                  (Apply or  ^c or  ^d) :
                                    (static member ``<*>`` :
                                        ^c *  ^d *  ^d * Apply ->  ^d) and
                                  (Return or  ^d) :
                                    (static member Return:
                                        ^d * Return -> (seq<'e> ->  ^d)) and
                                  (Map or  ^d or  ^f) :
                                    (static member Map:
                                       ( ^d * ('g -> Data.NonEmptySeq<'h>)) *
                                       Map ->  ^f) and 'g :> seq<'h>
        
        static member
          inline Traverse: t: seq<'a> * f: ('a ->  ^b) * _output: 'R *
                           _impl: Internals.Default3 ->  ^d
                             when (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('a0 -> seq<'a0> -> seq<'a0>)) *
                                       Map ->  ^c) and
                                  (Apply or  ^c or  ^d) :
                                    (static member ``<*>`` :
                                        ^c *  ^d *  ^d * Apply ->  ^d) and
                                  (Return or  ^d) :
                                    (static member Return:
                                        ^d * Return -> (seq<'e> ->  ^d))
        
        static member
          inline Traverse: t: Internals.Id<'a> * f: ('a ->  ^b) * _output: 'R *
                           _impl: Internals.Default3 ->  ^c
                             when (Map or  ^b or  ^c) :
                                    (static member Map:
                                       ( ^b * ('d -> Internals.Id<'d>)) * Map
                                         ->  ^c)
        
        static member
          inline Traverse: t:  ^Traversable<'T> * f: ('T -> 'Functor<'U>) *
                           _output: 'Functor<'Traversable<'U>> *
                           _impl: Internals.Default4
                             -> 'Functor<'Traversable<'U>>
                             when (Map or  ^Traversable<'T> or
                                    ^Traversable<'Functor<'U>>) :
                                    (static member Map:
                                       ( ^Traversable<'T> * ('T -> 'Functor<'U>)) *
                                       Map ->  ^Traversable<'Functor<'U>>) and
                                   ^Traversable<'T> :
                                    (static member Sequence:
                                        ^Traversable<'Functor<'U>>
                                         -> 'Functor<'Traversable<'U>>)

