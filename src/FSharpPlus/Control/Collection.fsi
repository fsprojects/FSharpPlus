namespace FSharpPlus.Control
    
    [<Class>]
    type OfSeq =
        inherit Internals.Default1
        
        static member
          inline Invoke: value: seq<'t> ->  ^a
                           when (OfSeq or  ^a) :
                                  (static member OfSeq:
                                     (seq<'t> *  ^a) * OfSeq ->  ^a)
        
        static member
          OfSeq: (seq<'t> * System.Collections.Generic.Stack<'t>) * OfSeq
                   -> System.Collections.Generic.Stack<'t>
        
        static member
          OfSeq: (seq<char> * System.Text.StringBuilder) * OfSeq
                   -> System.Text.StringBuilder
        
        static member OfSeq: (seq<char> * string) * OfSeq -> string
        
        static member OfSeq: (#seq<'t> * 't list) * OfSeq -> 't list
        
        static member OfSeq: (#seq<'t> * 't[]) * OfSeq -> 't[]
        
        static member
          inline OfSeq: ('a * 't) * OfSeq -> ('b -> 'b)
                          when 't: null and 't: struct
        
        static member
          inline OfSeq: (seq<'t> *  ^UserType) * OfSeq ->  ^UserType
                          when  ^UserType:
                                 (static member OfSeq: seq<'t> ->  ^UserType)
        
        static member
          OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> * 'T) *
                 OfSeq -> 'T
                   when 'T :> System.Collections.Generic.IDictionary<'k,'v> and
                        'T: (new: unit -> 'T)
        
        static member
          OfSeq: (seq<'k * 'v> * 'T) * OfSeq -> 'T
                   when 'T :> System.Collections.Generic.IDictionary<'k,'v> and
                        'T: (new: unit -> 'T)
        
        static member
          OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> * 'T) *
                 Internals.Default1 -> 'T
                   when 'T :> System.Collections.IDictionary and
                        'T: (new: unit -> 'T)
        
        static member
          OfSeq: (seq<'k * 'v> * 'T) * Internals.Default1 -> 'T
                   when 'T :> System.Collections.IDictionary and
                        'T: (new: unit -> 'T)
        
        static member
          OfSeq: (seq<'t> * 'T) * Internals.Default1 -> 'T
                   when 'T :> System.Collections.Generic.ICollection<'t> and
                        'T: (new: unit -> 'T)
        
        static member
          inline OfSeq: (seq<'t> *  ^F) * Internals.Default2 ->  ^F
                          when  ^F: (new: unit ->  ^F) and
                                ^F: (member Add:  ^F * 't ->  ^R)
        
        static member
          OfSeq: (seq<'t> * System.Collections.Generic.IReadOnlyCollection<'t>) *
                 Internals.Default3
                   -> System.Collections.Generic.IReadOnlyCollection<'t>
        
        static member
          OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> *
                  System.Collections.IDictionary) * Internals.Default3
                   -> System.Collections.IDictionary
        
        static member
          OfSeq: (seq<'k * 'v> * System.Collections.IDictionary) *
                 Internals.Default3 -> System.Collections.IDictionary
        
        static member
          OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> *
                  System.Collections.Generic.IDictionary<'k,'v>) *
                 Internals.Default3
                   -> System.Collections.Generic.IDictionary<'k,'v>
                   when 'k: equality
        
        static member
          OfSeq: (seq<'k * 'v> * System.Collections.Generic.IDictionary<'k,'v>) *
                 Internals.Default3
                   -> System.Collections.Generic.IDictionary<'k,'v>
                   when 'k: equality
        
        static member
          OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> *
                  System.Collections.Generic.IReadOnlyDictionary<'k,'v>) *
                 Internals.Default3
                   -> System.Collections.Generic.IReadOnlyDictionary<'k,'v>
                   when 'k: equality
        
        static member
          OfSeq: (seq<'k * 'v> *
                  System.Collections.Generic.IReadOnlyDictionary<'k,'v>) *
                 Internals.Default3
                   -> System.Collections.Generic.IReadOnlyDictionary<'k,'v>
                   when 'k: equality
        
        static member
          OfSeq: (seq<'t> * System.Collections.IList) * Internals.Default3
                   -> System.Collections.IList
        
        static member
          OfSeq: (seq<'t> * System.Collections.Generic.IList<'t>) *
                 Internals.Default3 -> System.Collections.Generic.IList<'t>
        
        static member
          OfSeq: (seq<'t> * System.Collections.Generic.ICollection<'t>) *
                 Internals.Default3
                   -> System.Collections.Generic.ICollection<'t>
        
        static member OfSeq: (seq<'t> * seq<'t>) * Internals.Default3 -> seq<'t>
        
        static member
          inline OfSeq: (seq<'t> *  ^Foldable'<T>) * Internals.Default4
                          ->  ^Foldable'<T>
                          when (Sum or seq< ^Foldable'<T>> or  ^Foldable'<T>) :
                                 (static member Sum:
                                    seq< ^Foldable'<T>> *  ^Foldable'<T> * Sum
                                      ->  ^Foldable'<T>) and
                               (Return or  ^Foldable'<T>) :
                                 (static member Return:
                                     ^Foldable'<T> * Return
                                      -> ('t ->  ^Foldable'<T>))
        
        static member
          inline OfSeq: (seq<System.Collections.Generic.KeyValuePair<'k,'v>> *
                          ^R) * Internals.Default5 ->  ^R
                          when  ^R: (``.ctor`` : seq<'k * 'v> ->  ^R)
        
        static member
          inline OfSeq: (seq<'t> *  ^R) * Internals.Default5 ->  ^R
                          when  ^R: (``.ctor`` : seq<'t> ->  ^R)
    
    [<Class>]
    type OfList =
        inherit Internals.Default1
        
        static member
          inline Invoke: value: 't list ->  ^a
                           when (OfList or  ^a) :
                                  (static member OfList:
                                     ('t list *  ^a) * OfList ->  ^a)
        
        static member
          OfList: ('t list * System.Collections.Generic.Stack<'t>) * OfList
                    -> System.Collections.Generic.Stack<'t>
        
        static member
          OfList: (char list * System.Text.StringBuilder) * OfList
                    -> System.Text.StringBuilder
        
        static member OfList: (char list * string) * OfList -> System.String
        
        static member OfList: ('a * 't list) * OfList -> 'a
        
        static member OfList: ('t list * 't[]) * OfList -> 't[]
        
        static member
          inline OfList: ('a * 't) * OfList -> ('b -> 'b)
                           when 't: null and 't: struct
        
        static member
          inline OfList: ('t list *  ^UserType) * OfList ->  ^UserType
                           when  ^UserType:
                                  (static member OfList: 't list ->  ^UserType)
        
        static member
          OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list * 'T) *
                  OfList -> 'T
                    when 'T :> System.Collections.Generic.IDictionary<'k,'v> and
                         'T: (new: unit -> 'T)
        
        static member
          OfList: (('k * 'v) list * 'T) * OfList -> 'T
                    when 'T :> System.Collections.Generic.IDictionary<'k,'v> and
                         'T: (new: unit -> 'T)
        
        static member
          OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list * 'T) *
                  Internals.Default1 -> 'T
                    when 'T :> System.Collections.IDictionary and
                         'T: (new: unit -> 'T)
        
        static member
          OfList: (('k * 'v) list * 'T) * Internals.Default1 -> 'T
                    when 'T :> System.Collections.IDictionary and
                         'T: (new: unit -> 'T)
        
        static member
          OfList: ('t list * 'T) * Internals.Default1 -> 'T
                    when 'T :> System.Collections.Generic.ICollection<'t> and
                         'T: (new: unit -> 'T)
        
        static member
          inline OfList: ('t list *  ^F) * Internals.Default2 ->  ^F
                           when  ^F: (new: unit ->  ^F) and
                                 ^F: (member Add:  ^F * 't ->  ^R)
        
        static member
          OfList: ('t list * System.Collections.Generic.IReadOnlyCollection<'t>) *
                  Internals.Default4
                    -> System.Collections.Generic.IReadOnlyCollection<'t>
        
        static member
          OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list *
                   System.Collections.IDictionary) * Internals.Default4
                    -> System.Collections.IDictionary
        
        static member
          OfList: (('k * 'v) list * System.Collections.IDictionary) *
                  Internals.Default4 -> System.Collections.IDictionary
        
        static member
          OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list *
                   System.Collections.Generic.IDictionary<'k,'v>) *
                  Internals.Default4
                    -> System.Collections.Generic.IDictionary<'k,'v>
                    when 'k: equality
        
        static member
          OfList: (('k * 'v) list *
                   System.Collections.Generic.IDictionary<'k,'v>) *
                  Internals.Default4
                    -> System.Collections.Generic.IDictionary<'k,'v>
                    when 'k: equality
        
        static member
          OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list *
                   System.Collections.Generic.IReadOnlyDictionary<'k,'v>) *
                  Internals.Default4
                    -> System.Collections.Generic.IReadOnlyDictionary<'k,'v>
                    when 'k: equality
        
        static member
          OfList: (('k * 'v) list *
                   System.Collections.Generic.IReadOnlyDictionary<'k,'v>) *
                  Internals.Default4
                    -> System.Collections.Generic.IReadOnlyDictionary<'k,'v>
                    when 'k: equality
        
        static member
          OfList: ('t list * System.Collections.IList) * Internals.Default4
                    -> System.Collections.IList
        
        static member
          OfList: ('t list * System.Collections.Generic.IList<'t>) *
                  Internals.Default4 -> System.Collections.Generic.IList<'t>
        
        static member
          OfList: ('t list * System.Collections.Generic.ICollection<'t>) *
                  Internals.Default4
                    -> System.Collections.Generic.ICollection<'t>
        
        static member
          OfList: ('t list * seq<'t>) * Internals.Default4 -> seq<'t>
        
        static member
          inline OfList: ('t list *  ^Foldable'<T>) * Internals.Default5
                           ->  ^Foldable'<T>
                           when (Sum or seq< ^Foldable'<T>> or  ^Foldable'<T>) :
                                  (static member Sum:
                                     seq< ^Foldable'<T>> *  ^Foldable'<T> * Sum
                                       ->  ^Foldable'<T>) and
                                (Return or  ^Foldable'<T>) :
                                  (static member Return:
                                      ^Foldable'<T> * Return
                                       -> ('t ->  ^Foldable'<T>))
        
        static member
          inline OfList: (System.Collections.Generic.KeyValuePair<'k,'v> list *
                           ^R) * Internals.Default6 ->  ^R
                           when  ^R: (``.ctor`` : seq<'k * 'v> ->  ^R)
        
        static member
          inline OfList: ('t list *  ^R) * Internals.Default6 ->  ^R
                           when  ^R: (``.ctor`` : seq<'t> ->  ^R)
    
    [<Class>]
    type Filter =
        inherit Internals.Default1
        
        static member
          inline Filter:  ^t * 'a * Internals.Default1 -> unit
                           when  ^t: null and  ^t: struct
        
        static member
          inline Filter: x:  ^Collection'<T> * p: ('a -> bool) *
                         _impl: Internals.Default1 ->  ^Collection'<T>
                           when  ^Collection'<T> :
                                  (static member Filter:
                                      ^Collection'<T> * ('a -> bool)
                                       ->  ^Collection'<T>)
        
        static member
          inline Filter: x:  ^Collection'<T> * p: ('a -> bool) *
                         _impl: Internals.Default2 ->  ^Collection'<T>
                           when (ToSeq or  ^Collection'<T>) :
                                  (static member ToSeq:
                                      ^Collection'<T> * ToSeq -> seq<'a>) and
                                (OfSeq or  ^Collection'<T>) :
                                  (static member OfSeq:
                                     (seq<'a> *  ^Collection'<T>) * OfSeq
                                       ->  ^Collection'<T>)
        
        static member
          Filter: x: seq<'t> * p: ('t -> bool) * _impl: Internals.Default3
                    -> seq<'t>
        
        static member
          Filter: x: ResizeArray<'t> * p: ('t -> bool) * _impl: Filter
                    -> ResizeArray<'t>
        
        static member
          Filter: x: System.IObservable<'t> * p: ('t -> bool) * _impl: Filter
                    -> System.IObservable<'t>
        
        static member Filter: x: 't[] * p: ('t -> bool) * _impl: Filter -> 't[]
        
        static member
          Filter: x: 't list * p: ('t -> bool) * _impl: Filter -> 't list
        
        static member
          Filter: x: 't option * p: ('t -> bool) * _impl: Filter -> 't option
        
        static member
          Filter: x: Set<'t> * p: ('t -> bool) * _impl: Filter -> Set<'t>
                    when 't: comparison
        
        static member
          inline Invoke: predicate: ('T -> bool) -> x:  ^Collection<'T>
                           ->  ^Collection<'T>
                           when (Filter or  ^Collection<'T>) :
                                  (static member Filter:
                                      ^Collection<'T> * ('T -> bool) * Filter
                                       ->  ^Collection<'T>)
        
        static member
          inline InvokeOnInstance: predicate: ('T -> bool)
                                   -> source:  ^Collection<'T>
                                     ->  ^Collection<'T>
                                     when  ^Collection<'T> :
                                            (static member Filter:
                                                ^Collection<'T> * ('T -> bool)
                                                 ->  ^Collection<'T>)
    
    [<Class>]
    type Skip =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: int -> source:  ^Collection<'T> ->  ^Collection<'T>
                           when (Skip or  ^Collection<'T>) :
                                  (static member Skip:
                                      ^Collection<'T> * int * Skip
                                       ->  ^Collection<'T>)
        
        static member
          Skip: x: Internals.Id<'a> * 'a0 * _impl: Skip -> Internals.Id<'a>
        
        static member Skip: x: 'a list * n: int * _impl: Skip -> 'a list
        
        static member
          Skip: x: ResizeArray<'a> * n: int * _impl: Skip -> ResizeArray<'a>
        
        static member Skip: x: 'a[] * n: int * _impl: Skip -> 'a[]
        
        static member
          Skip: x: System.Text.StringBuilder * n: int * _impl: Skip
                  -> System.Text.StringBuilder
        
        static member Skip: x: string * n: int * _impl: Skip -> string
        
        static member
          inline Skip: x:  ^Foldable<'T> * n: int * _impl: Internals.Default1
                         ->  ^Foldable<'T>
                         when (ToSeq or  ^Foldable<'T>) :
                                (static member ToSeq:
                                    ^Foldable<'T> * ToSeq -> seq<'a>) and
                              (OfSeq or  ^Foldable<'T>) :
                                (static member OfSeq:
                                   (seq<'a> *  ^Foldable<'T>) * OfSeq
                                     ->  ^Foldable<'T>)
    
    [<Class>]
    type Take =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: int -> source:  ^Collection<'T> ->  ^Collection<'T>
                           when (Take or  ^Collection<'T>) :
                                  (static member Take:
                                      ^Collection<'T> * int * Take
                                       ->  ^Collection<'T>)
        
        static member
          Take: x: Internals.Id<'a> * 'a0 * _impl: Take -> Internals.Id<'a>
        
        static member Take: x: 'a list * n: int * _impl: Take -> 'a list
        
        static member
          Take: x: ResizeArray<'a> * n: int * _impl: Take -> ResizeArray<'a>
        
        static member Take: x: 'a[] * n: int * _impl: Take -> 'a[]
        
        static member
          Take: x: System.Text.StringBuilder * n: int * _impl: Take
                  -> System.Text.StringBuilder
        
        static member Take: x: string * n: int * _impl: Take -> string
        
        static member
          inline Take: x:  ^Foldable<'T> * n: int * _impl: Internals.Default1
                         ->  ^Foldable<'T>
                         when (ToSeq or  ^Foldable<'T>) :
                                (static member ToSeq:
                                    ^Foldable<'T> * ToSeq -> seq<'a>) and
                              (OfSeq or  ^Foldable<'T>) :
                                (static member OfSeq:
                                   (seq<'a> *  ^Foldable<'T>) * OfSeq
                                     ->  ^Foldable<'T>)
    
    [<Class>]
    type TakeWhile =
        inherit Internals.Default1
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Collection<'T>
                           ->  ^Collection<'T>
                           when (TakeWhile or  ^Collection<'T>) :
                                  (static member TakeWhile:
                                      ^Collection<'T> * ('T -> bool) * TakeWhile
                                       ->  ^Collection<'T>)
        
        static member
          TakeWhile: x: Internals.Id<'a> * 'a0 * _impl: TakeWhile
                       -> Internals.Id<'a>
        
        static member
          TakeWhile: x: 'a list * p: ('a -> bool) * _impl: TakeWhile -> 'a list
        
        static member
          TakeWhile: x: ResizeArray<'a> * p: ('a -> bool) * _impl: TakeWhile
                       -> ResizeArray<'a>
        
        static member
          TakeWhile: x: 'a[] * p: ('a -> bool) * _impl: TakeWhile -> 'a[]
        
        static member
          TakeWhile: x: string * p: (char -> bool) * _impl: TakeWhile -> string
        
        static member
          inline TakeWhile: x:  ^Foldable<'T> * p: ('a -> bool) *
                            _impl: Internals.Default1 ->  ^Foldable<'T>
                              when (ToSeq or  ^Foldable<'T>) :
                                     (static member ToSeq:
                                         ^Foldable<'T> * ToSeq -> seq<'a>) and
                                   (OfSeq or  ^Foldable<'T>) :
                                     (static member OfSeq:
                                        (seq<'a> *  ^Foldable<'T>) * OfSeq
                                          ->  ^Foldable<'T>)
    
    [<Class>]
    type SkipWhile =
        inherit Internals.Default1
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Collection<'T>
                           ->  ^Collection<'T>
                           when (SkipWhile or  ^Collection<'T>) :
                                  (static member SkipWhile:
                                      ^Collection<'T> * ('T -> bool) * SkipWhile
                                       ->  ^Collection<'T>)
        
        static member
          SkipWhile: x: Internals.Id<'a> * 'a0 * _impl: SkipWhile
                       -> Internals.Id<'a>
        
        static member
          SkipWhile: x: 'a list * p: ('a -> bool) * _impl: SkipWhile -> 'a list
        
        static member
          SkipWhile: x: ResizeArray<'a> * p: ('a -> bool) * _impl: SkipWhile
                       -> ResizeArray<'a>
        
        static member
          SkipWhile: x: 'a[] * p: ('a -> bool) * _impl: SkipWhile -> 'a[]
        
        static member
          SkipWhile: x: string * p: (char -> bool) * _impl: SkipWhile -> string
        
        static member
          inline SkipWhile: x:  ^Foldable<'T> * p: ('a -> bool) *
                            _impl: Internals.Default1 ->  ^Foldable<'T>
                              when (ToSeq or  ^Foldable<'T>) :
                                     (static member ToSeq:
                                         ^Foldable<'T> * ToSeq -> seq<'a>) and
                                   (OfSeq or  ^Foldable<'T>) :
                                     (static member OfSeq:
                                        (seq<'a> *  ^Foldable<'T>) * OfSeq
                                          ->  ^Foldable<'T>)
    
    [<Class>]
    type Drop =
        inherit Internals.Default1
        
        static member
          Drop: x: Internals.Id<'a> * 'a0 * _impl: Drop -> Internals.Id<'a>
        
        static member Drop: x: 'a list * n: int * _impl: Drop -> 'a list
        
        static member
          Drop: x: ResizeArray<'a> * n: int * _impl: Drop -> ResizeArray<'a>
        
        static member Drop: x: 'a[] * n: int * _impl: Drop -> 'a[]
        
        static member
          Drop: x: System.Text.StringBuilder * n: int * _impl: Drop
                  -> System.Text.StringBuilder
        
        static member Drop: x: string * n: int * _impl: Drop -> string
        
        static member
          inline Drop: x:  ^Foldable<'T> * n: int * _impl: Internals.Default1
                         ->  ^Foldable<'T>
                         when (ToSeq or  ^Foldable<'T>) :
                                (static member ToSeq:
                                    ^Foldable<'T> * ToSeq -> seq<'a>) and
                              (OfSeq or  ^Foldable<'T>) :
                                (static member OfSeq:
                                   (seq<'a> *  ^Foldable<'T>) * OfSeq
                                     ->  ^Foldable<'T>)
        
        static member
          inline Invoke: n: int -> source:  ^Collection<'T> ->  ^Collection<'T>
                           when (Drop or  ^Collection<'T>) :
                                  (static member Drop:
                                      ^Collection<'T> * int * Drop
                                       ->  ^Collection<'T>)
    
    [<Class>]
    type Limit =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: int -> source:  ^Collection<'T> ->  ^Collection<'T>
                           when (Limit or  ^Collection<'T>) :
                                  (static member Limit:
                                      ^Collection<'T> * int * Limit
                                       ->  ^Collection<'T>)
        
        static member
          Limit: x: Internals.Id<'a> * 'a0 * _impl: Limit -> Internals.Id<'a>
        
        static member Limit: x: 'a list * n: int * _impl: Limit -> 'a list
        
        static member
          Limit: x: ResizeArray<'a> * n: int * _impl: Limit -> ResizeArray<'a>
        
        static member Limit: x: 'a[] * n: int * _impl: Limit -> 'a[]
        
        static member
          Limit: x: System.Text.StringBuilder * n: int * _impl: Limit
                   -> System.Text.StringBuilder
        
        static member Limit: x: string * n: int * _impl: Limit -> string
        
        static member
          inline Limit: x:  ^Foldable<'T> * n: int * _impl: Internals.Default1
                          ->  ^Foldable<'T>
                          when (ToSeq or  ^Foldable<'T>) :
                                 (static member ToSeq:
                                     ^Foldable<'T> * ToSeq -> seq<'a>) and
                               (OfSeq or  ^Foldable<'T>) :
                                 (static member OfSeq:
                                    (seq<'a> *  ^Foldable<'T>) * OfSeq
                                      ->  ^Foldable<'T>)
    
    [<Class>]
    type Choose =
        
        static member
          Choose: x: Map<'K,'V> * f: ('V -> 'U option) * _impl: Choose
                    -> Map<'K,'U> when 'K: comparison
        
        static member
          Choose: x: System.Collections.Generic.IDictionary<'K,'V> *
                  f: ('V -> 'U option) * _impl: Choose
                    -> System.Collections.Generic.IDictionary<'K,'U>
                    when 'K: equality
        
        static member
          Choose: x: System.Collections.Generic.IReadOnlyDictionary<'K,'V> *
                  f: ('V -> 'U option) * _impl: Choose
                    -> System.Collections.Generic.IReadOnlyDictionary<'K,'U>
                    when 'K: equality
        
        static member
          Choose: x: System.Collections.Generic.Dictionary<'K,'V> *
                  f: ('V -> 'U option) * _impl: Choose
                    -> System.Collections.Generic.Dictionary<'K,'U>
                    when 'K: equality
        
        static member
          Choose: x: 'T[] * f: ('T -> 'U option) * _impl: Choose -> 'U[]
        
        static member
          Choose: x: 'T list * f: ('T -> 'U option) * _impl: Choose -> 'U list
        
        static member
          Choose: x: seq<'T> * f: ('T -> 'U option) * _impl: Choose -> seq<'U>
        
        static member
          Choose: Internals.Id<'T> * ('a -> 'U option) * _impl: Choose
                    -> Internals.Id<'U>
        
        static member
          inline Invoke: chooser: ('T -> 'U option) -> source:  ^Collection<'T>
                           ->  ^Collection'U
                           when (Choose or  ^Collection<'T> or  ^Collection'U) :
                                  (static member Choose:
                                      ^Collection<'T> * ('T -> 'U option) *
                                     Choose ->  ^Collection'U)
    
    [<Class>]
    type Distinct =
        inherit Internals.Default1
        
        static member
          inline Distinct:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                             when  ^t: null and  ^t: struct
        
        static member
          inline Distinct: x:  ^Collection<'T> * _impl: Internals.Default1
                             ->  ^Collection<'T>
                             when  ^Collection<'T> :
                                    (static member Distinct:
                                        ^Collection<'T> ->  ^Collection<'T>)
        
        static member
          inline Distinct: x:  ^Collection<'T> * _impl: Internals.Default2
                             ->  ^Collection<'T>
                             when (ToSeq or  ^Collection<'T>) :
                                    (static member ToSeq:
                                        ^Collection<'T> * ToSeq -> seq<'a>) and
                                  (OfSeq or  ^Collection<'T>) :
                                    (static member OfSeq:
                                       (seq<'a> *  ^Collection<'T>) * OfSeq
                                         ->  ^Collection<'T>) and 'a: equality
        
        static member
          Distinct: x: 'a[] * _impl: Distinct -> 'a[] when 'a: equality
        
        static member
          Distinct: x: 'a list * _impl: Distinct -> 'a list when 'a: equality
        
        static member
          inline Invoke: source:  ^C<'T> ->  ^C<'T>
                           when (Distinct or  ^C<'T>) :
                                  (static member Distinct:
                                      ^C<'T> * Distinct ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: source:  ^C<'T> ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member Distinct:
                                                ^C<'T> ->  ^C<'T>)
    
    [<Class>]
    type DistinctBy =
        inherit Internals.Default1
        
        static member
          inline DistinctBy:  ^t * ('T -> 'U) * _mthd: Internals.Default1
                               -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline DistinctBy: x:  ^Collection<'T> * f: 'a *
                             _impl: Internals.Default1 ->  ^Collection<'T>
                               when  ^Collection<'T> :
                                      (static member DistinctBy:
                                         'a *  ^Collection<'T>
                                           ->  ^Collection<'T>)
        
        static member
          inline DistinctBy: x:  ^Collection<'T> * f: ('a -> 'b) *
                             _impl: Internals.Default2 ->  ^Collection<'T>
                               when (ToSeq or  ^Collection<'T>) :
                                      (static member ToSeq:
                                          ^Collection<'T> * ToSeq -> seq<'a>) and
                                    (OfSeq or  ^Collection<'T>) :
                                      (static member OfSeq:
                                         (seq<'a> *  ^Collection<'T>) * OfSeq
                                           ->  ^Collection<'T>) and 'b: equality
        
        static member
          DistinctBy: x: 'a[] * f: ('a -> 'a0) * _impl: DistinctBy -> 'a[]
                        when 'a0: equality
        
        static member
          DistinctBy: x: 'a list * f: ('a -> 'a0) * _impl: DistinctBy -> 'a list
                        when 'a0: equality
        
        static member
          inline Invoke: projection: ('T -> 'Key) -> source:  ^C<'T> ->  ^C<'T>
                           when (DistinctBy or  ^C<'T>) :
                                  (static member DistinctBy:
                                      ^C<'T> * ('T -> 'Key) * DistinctBy
                                       ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: projection: ('T -> 'Key) -> source:  ^C<'T>
                                     ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member DistinctBy:
                                                ^C<'T> * ('T -> 'Key) ->  ^C<'T>)
    
    [<Class>]
    type GroupBy =
        
        static member
          GroupBy: x: 'T[] * f: ('T -> 'Key) * ('Key * 'T[])[] * _impl: GroupBy
                     -> ('Key * 'T[])[] when 'Key: equality
        
        static member
          GroupBy: x: 'T list * f: ('T -> 'Key) * ('Key * 'T list) list *
                   _impl: GroupBy -> ('Key * 'T list) list when 'Key: equality
        
        static member
          GroupBy: x: seq<'T> * f: ('T -> 'Key) * seq<'Key * seq<'T>> *
                   _impl: GroupBy -> seq<'Key * seq<'T>> when 'Key: equality
        
        static member
          GroupBy: x: Internals.Id<'T> * f: ('T -> 'Key) *
                   Internals.Id<'Key * Internals.Id<'T>> * _impl: GroupBy
                     -> Internals.Id<'Key * Internals.Id<'T>>
        
        static member
          inline Invoke: projection: ('T -> 'Key) -> source:  ^C<'T>
                           ->  ^C<'Key * 'C<'T>>
                           when (GroupBy or  ^C<'T> or  ^C<'Key * 'C<'T>>) :
                                  (static member GroupBy:
                                      ^C<'T> * ('T -> 'Key) *  ^C<'Key * 'C<'T>> *
                                     GroupBy ->  ^C<'Key * 'C<'T>>)
    
    [<Class>]
    type ChunkBy =
        
        static member
          ChunkBy: x: 'T[] * f: ('T -> 'Key) * ('Key * 'T[])[] * _impl: ChunkBy
                     -> ('Key * 'T[])[] when 'Key: equality
        
        static member
          ChunkBy: x: 'T list * f: ('T -> 'Key) * ('Key * 'T list) list *
                   _impl: ChunkBy -> ('Key * 'T list) list when 'Key: equality
        
        static member
          ChunkBy: x: seq<'T> * f: ('T -> 'Key) * seq<'Key * seq<'T>> *
                   _impl: ChunkBy -> seq<'Key * seq<'T>> when 'Key: equality
        
        static member
          ChunkBy: x: Internals.Id<'T> * f: ('T -> 'Key) *
                   Internals.Id<'Key * Internals.Id<'T>> * _impl: ChunkBy
                     -> Internals.Id<'Key * Internals.Id<'T>>
        
        static member
          inline Invoke: projection: ('T -> 'Key) -> source:  ^Collection<'T>
                           ->  ^Collection<'Key * 'Collection<'T>>
                           when (ChunkBy or  ^Collection<'T> or
                                  ^Collection<'Key * 'Collection<'T>>) :
                                  (static member ChunkBy:
                                      ^Collection<'T> * ('T -> 'Key) *
                                      ^Collection<'Key * 'Collection<'T>> *
                                     ChunkBy
                                       ->  ^Collection<'Key * 'Collection<'T>>)
    
    [<Class>]
    type Replace =
        inherit Internals.Default1
        
        static member
          inline Invoke: o:  ^Collection -> n:  ^Collection
                         -> source:  ^Collection ->  ^Collection
                           when (Replace or  ^Collection) :
                                  (static member Replace:
                                      ^Collection *  ^Collection *  ^Collection *
                                     Replace ->  ^Collection)
        
        static member
          Replace: x: System.Text.StringBuilder * o: System.Text.StringBuilder *
                   n: System.Text.StringBuilder * _impl: Replace
                     -> System.Text.StringBuilder
        
        static member
          Replace: x: string * o: string * n: string * _impl: Replace -> string
        
        static member
          Replace: x: 'T[] * o: 'T[] * n: 'T[] * _impl: Replace -> 'T[]
                     when 'T: equality
        
        static member
          Replace: x: 'T list * o: 'T list * n: 'T list * _impl: Replace
                     -> 'T list when 'T: equality
        
        static member
          Replace: x: Internals.Id<'T> * o: Internals.Id<'T> *
                   n: Internals.Id<'T> * _impl: Internals.Default1
                     -> Internals.Id<'T>
        
        static member
          inline Replace: x:  ^Collection * o:  ^Collection * n:  ^Collection *
                          _impl: Internals.Default1 ->  ^Collection
                            when (ToSeq or  ^Collection) :
                                   (static member ToSeq:
                                       ^Collection * ToSeq -> seq<'a>) and
                                 (OfSeq or  ^Collection) :
                                   (static member OfSeq:
                                      (seq<'a> *  ^Collection) * OfSeq
                                        ->  ^Collection) and 'a: equality
    
    [<Class>]
    type Rev =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^C<'T> ->  ^C<'T>
                           when (Rev or  ^C<'T>) :
                                  (static member Rev:  ^C<'T> * Rev ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: source:  ^C<'T> ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member Rev:
                                                ^C<'T> ->  ^C<'T>)
        
        static member
          inline Rev:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                        when  ^t: null and  ^t: struct
        
        static member
          inline Rev: x:  ^Collection<'T> * _impl: Internals.Default1
                        ->  ^Collection<'T>
                        when  ^Collection<'T> :
                               (static member Rev:
                                   ^Collection<'T> ->  ^Collection<'T>)
        
        static member
          inline Rev: x:  ^Collection<'T> * _impl: Internals.Default2
                        ->  ^Collection<'T>
                        when (ToSeq or  ^Collection<'T>) :
                               (static member ToSeq:
                                   ^Collection<'T> * ToSeq -> seq<'a>) and
                             (OfSeq or  ^Collection<'T>) :
                               (static member OfSeq:
                                  (seq<'a> *  ^Collection<'T>) * OfSeq
                                    ->  ^Collection<'T>)
        
        static member Rev: x: 'a[] * _impl: Rev -> 'a[]
        
        static member Rev: x: 'a list * _impl: Rev -> 'a list
    
    [<Class>]
    type Scan =
        
        static member
          inline Invoke: folder: ('State -> 'T -> 'State) -> state: 'State
                         -> source:  ^Collection<'T> ->  ^Collection<'State>
                           when (Scan or  ^Collection<'T> or
                                  ^Collection<'State>) :
                                  (static member Scan:
                                      ^Collection<'T> * ('State -> 'T -> 'State) *
                                     'State *  ^Collection<'State> * Scan
                                       ->  ^Collection<'State>)
        
        static member
          Scan: x: 'T[] * f: ('S -> 'T -> 'S) * z: 'S * _output: 'S[] *
                _impl: Scan -> 'S[]
        
        static member
          Scan: x: 'T list * f: ('S -> 'T -> 'S) * z: 'S * _output: 'S list *
                _impl: Scan -> 'S list
        
        static member
          Scan: x: seq<'T> * f: ('S -> 'T -> 'S) * z: 'S * _output: seq<'S> *
                _impl: Scan -> seq<'S>
        
        static member
          Scan: x: Internals.Id<'T> * f: ('S -> 'T -> 'a) * z: 'S *
                _output: Internals.Id<'S> * _impl: Scan -> Internals.Id<'a>
    
    [<Class>]
    type Sort =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^C<'T> ->  ^C<'T>
                           when (Sort or  ^C<'T>) :
                                  (static member Sort:  ^C<'T> * Sort ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: source:  ^C<'T> ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member Sort:
                                                ^C<'T> ->  ^C<'T>)
        
        static member
          inline Sort:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                         when  ^t: null and  ^t: struct
        
        static member
          inline Sort: x:  ^Collection<'T> * _impl: Internals.Default1
                         ->  ^Collection<'T>
                         when  ^Collection<'T> :
                                (static member Sort:
                                    ^Collection<'T> ->  ^Collection<'T>)
        
        static member
          inline Sort: x:  ^Collection<'T> * _impl: Internals.Default2
                         ->  ^Collection<'T>
                         when (ToSeq or  ^Collection<'T>) :
                                (static member ToSeq:
                                    ^Collection<'T> * ToSeq -> seq<'a>) and
                              (OfSeq or  ^Collection<'T>) :
                                (static member OfSeq:
                                   (seq<'a> *  ^Collection<'T>) * OfSeq
                                     ->  ^Collection<'T>) and 'a: comparison
        
        static member Sort: x: 'a[] * _impl: Sort -> 'a[] when 'a: comparison
        
        static member
          Sort: x: 'a list * _impl: Sort -> 'a list when 'a: comparison
    
    [<Class>]
    type SortBy =
        inherit Internals.Default1
        
        static member
          inline Invoke: projection: ('T -> 'Key) -> source:  ^C<'T> ->  ^C<'T>
                           when (SortBy or  ^C<'T>) :
                                  (static member SortBy:
                                      ^C<'T> * ('T -> 'Key) * SortBy ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: projection: ('T -> 'Key) -> source:  ^C<'T>
                                     ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member SortBy:
                                                ^C<'T> * ('T -> 'Key) ->  ^C<'T>)
        
        static member
          inline SortBy:  ^t * ('T -> 'U) * _mthd: Internals.Default1
                           -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline SortBy: x:  ^Collection<'T> * f: 'a * _impl: Internals.Default1
                           ->  ^Collection<'T>
                           when  ^Collection<'T> :
                                  (static member SortBy:
                                     'a *  ^Collection<'T> ->  ^Collection<'T>)
        
        static member
          inline SortBy: x:  ^Collection<'T> * f: ('a -> 'b) *
                         _impl: Internals.Default2 ->  ^Collection<'T>
                           when (ToSeq or  ^Collection<'T>) :
                                  (static member ToSeq:
                                      ^Collection<'T> * ToSeq -> seq<'a>) and
                                (OfSeq or  ^Collection<'T>) :
                                  (static member OfSeq:
                                     (seq<'a> *  ^Collection<'T>) * OfSeq
                                       ->  ^Collection<'T>) and 'b: comparison
        
        static member
          SortBy: x: 'a[] * f: ('a -> 'a0) * _impl: SortBy -> 'a[]
                    when 'a0: comparison
        
        static member
          SortBy: x: 'a list * f: ('a -> 'a0) * _impl: SortBy -> 'a list
                    when 'a0: comparison
    
    [<Class>]
    type SortByDescending =
        inherit Internals.Default1
        
        static member
          inline Invoke: projection: ('T -> 'Key) -> source:  ^C<'T> ->  ^C<'T>
                           when (SortByDescending or  ^C<'T>) :
                                  (static member SortByDescending:
                                      ^C<'T> * ('T -> 'Key) * SortByDescending
                                       ->  ^C<'T>)
        
        static member
          inline InvokeOnInstance: projection: ('T -> 'Key) -> source:  ^C<'T>
                                     ->  ^C<'T>
                                     when  ^C<'T> :
                                            (static member SortByDescending:
                                                ^C<'T> * ('T -> 'Key) ->  ^C<'T>)
        
        static member
          inline SortByDescending:  ^t * ('T -> 'U) * _mthd: Internals.Default1
                                     -> ('a -> 'a)
                                     when  ^t: null and  ^t: struct
        
        static member
          inline SortByDescending: x:  ^Collection<'T> * f: 'a *
                                   _impl: Internals.Default1 ->  ^Collection<'T>
                                     when  ^Collection<'T> :
                                            (static member SortByDescending:
                                               'a *  ^Collection<'T>
                                                 ->  ^Collection<'T>)
        
        static member
          inline SortByDescending: x:  ^Collection<'T> * f: ('a -> 'b) *
                                   _impl: Internals.Default2 ->  ^Collection<'T>
                                     when (ToSeq or  ^Collection<'T>) :
                                            (static member ToSeq:
                                                ^Collection<'T> * ToSeq
                                                 -> seq<'a>) and
                                          (OfSeq or  ^Collection<'T>) :
                                            (static member OfSeq:
                                               (seq<'a> *  ^Collection<'T>) *
                                               OfSeq ->  ^Collection<'T>) and
                                          'b: comparison
        
        static member
          SortByDescending: x: 'a[] * f: ('a -> 'a0) * _impl: SortBy -> 'a[]
                              when 'a0: comparison
        
        static member
          SortByDescending: x: 'a list * f: ('a -> 'a0) * _impl: SortBy
                              -> 'a list when 'a0: comparison
    
    [<Class>]
    type Split =
        inherit Internals.Default1
        
        static member
          inline Invoke: sep: ''Collection<'OrderedCollection>
                         -> source:  ^OrderedCollection
                           -> ''Collection<'OrderedCollection>
                           when (Split or  ^OrderedCollection) :
                                  (static member Split:
                                     (''Collection<'OrderedCollection> *
                                       ^OrderedCollection) * Split
                                       -> ''Collection<'OrderedCollection>)
        
        static member
          inline Split: ( ^t * 'a) * _mthd: Internals.Default1 -> ('b -> 'b)
                          when  ^t: null and  ^t: struct
        
        static member
          inline Split: (''Collection<'OrderedCollection> *  ^'OrderedCollection) *
                        _impl: Internals.Default1
                          -> ''Collection<'OrderedCollection>
                          when  ^'OrderedCollection:
                                 (static member Split:
                                    ''Collection<'OrderedCollection> *
                                     ^'OrderedCollection
                                      -> ''Collection<'OrderedCollection>)
        
        static member
          inline Split: ( ^'Collection<'OrderedCollection> *
                          ^'OrderedCollection) * _impl: Internals.Default2
                          ->  ^'Collection<'OrderedCollection>
                          when (ToSeq or  ^'Collection<'OrderedCollection>) :
                                 (static member ToSeq:
                                     ^'Collection<'OrderedCollection> * ToSeq
                                      -> seq<'a>) and
                               (OfSeq or  ^'Collection<'OrderedCollection>) :
                                 (static member OfSeq:
                                    (seq< ^c> *
                                      ^'Collection<'OrderedCollection>) * OfSeq
                                      ->  ^'Collection<'OrderedCollection>) and
                               'a :> seq<'b> and 'b: equality and
                               (OfSeq or  ^c) :
                                 (static member OfSeq:
                                    (seq<'b> *  ^c) * OfSeq ->  ^c) and
                               (ToSeq or  ^'OrderedCollection) :
                                 (static member ToSeq:
                                     ^'OrderedCollection * ToSeq -> seq<'b>)
        
        static member
          Split: (System.Text.StringBuilder list * System.Text.StringBuilder) *
                 _impl: Split -> System.Text.StringBuilder list
        
        static member
          Split: (System.Text.StringBuilder[] * System.Text.StringBuilder) *
                 _impl: Split -> System.Text.StringBuilder[]
        
        static member
          Split: (seq<System.Text.StringBuilder> * System.Text.StringBuilder) *
                 _impl: Split -> seq<System.Text.StringBuilder>
        
        static member
          Split: (string list * string) * _impl: Split -> string list
        
        static member Split: (string array * string) * _impl: Split -> string[]
        
        static member
          Split: (seq<string> * string) * _impl: Split -> seq<string>
        
        static member
          Split: ('T[] list * 'T[]) * _impl: Split -> 'T[] list
                   when 'T: equality
        
        static member
          Split: ('T[] array * 'T[]) * _impl: Split -> 'T[][] when 'T: equality
        
        static member
          Split: (seq<'T[]> * 'T[]) * _impl: Split -> seq<'T[]>
                   when 'T: equality
        
        static member
          Split: ('T list list * 'T list) * _impl: Split -> 'T list list
                   when 'T: equality
        
        static member
          Split: ('T list array * 'T list) * _impl: Split -> 'T list[]
                   when 'T: equality
        
        static member
          Split: (seq<'T list> * 'T list) * _impl: Split -> seq<'T list>
                   when 'T: equality
        
        static member
          Split: (seq<'T> list * seq<'T>) * _impl: Split -> seq<'T> list
                   when 'T: equality
        
        static member
          Split: (seq<'T> array * seq<'T>) * _impl: Split -> seq<'T>[]
                   when 'T: equality
        
        static member
          Split: (seq<seq<'T>> * seq<'T>) * _impl: Split -> seq<seq<'T>>
                   when 'T: equality
    
    [<Class>]
    type Intersperse =
        inherit Internals.Default1
        
        static member Intersperse: x: 'T[] * e: 'T * _impl: Intersperse -> 'T[]
        
        static member
          Intersperse: x: 'T list * e: 'T * _impl: Intersperse -> 'T list
        
        static member
          inline Intersperse: x:  ^Collection<'T> * e: 'T *
                              _impl: Internals.Default1 ->  ^Collection<'T>
                                when (ToSeq or  ^Collection<'T>) :
                                       (static member ToSeq:
                                           ^Collection<'T> * ToSeq -> seq<'T>) and
                                     (OfSeq or  ^Collection<'T>) :
                                       (static member OfSeq:
                                          (seq<'T> *  ^Collection<'T>) * OfSeq
                                            ->  ^Collection<'T>)
        
        static member
          inline Invoke: sep: 'T -> source:  ^Collection<'T> ->  ^Collection<'T>
                           when (Intersperse or  ^Collection<'T>) :
                                  (static member Intersperse:
                                      ^Collection<'T> * 'T * Intersperse
                                       ->  ^Collection<'T>) and
                                (ToSeq or  ^Collection<'T>) :
                                  (static member ToSeq:
                                      ^Collection<'T> * ToSeq -> seq<'T>) and
                                (OfSeq or  ^Collection<'T>) :
                                  (static member OfSeq:
                                     (seq<'T> *  ^Collection<'T>) * OfSeq
                                       ->  ^Collection<'T>)
    
    [<Class>]
    type Intercalate =
        inherit Internals.Default1
        
        static member
          Intercalate: x: seq<System.Text.StringBuilder> *
                       e: System.Text.StringBuilder * _impl: Intercalate
                         -> System.Text.StringBuilder
        
        static member
          Intercalate: x: seq<string> * e: string * _impl: Intercalate -> string
        
        static member
          Intercalate: x: seq<'T[]> * e: 'T[] * _impl: Intercalate -> 'T[]
        
        static member
          Intercalate: x: seq<'T list> * e: 'T list * _impl: Intercalate
                         -> 'T list
        
        static member
          inline Intercalate: seq<'Foldable<'T>> *  ^t * Internals.Default1
                                -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline Intercalate: x:  ^Foldable<'Monoid> * e:  ^Monoid *
                              _impl: Internals.Default1 ->  ^Monoid
                                when (Fold or  ^Foldable<'Monoid>) :
                                       (static member Fold:
                                           ^Foldable<'Monoid> *
                                          (bool *  ^Monoid ->  ^Monoid
                                             -> bool *  ^Monoid) *
                                          (bool *  ^Monoid) * Fold
                                            -> bool *  ^Monoid) and
                                     (Plus or  ^Monoid) :
                                       (static member ``+`` :
                                           ^Monoid *  ^Monoid * Plus ->  ^Monoid) and
                                     (Zero or  ^Monoid) :
                                       (static member Zero:
                                           ^Monoid * Zero ->  ^Monoid)
        
        static member
          inline Invoke: sep:  ^Monoid -> source:  ^Foldable<'Monoid>
                           ->  ^Monoid
                           when (Intercalate or  ^Monoid) :
                                  (static member Intercalate:
                                      ^Foldable<'Monoid> *  ^Monoid *
                                     Intercalate ->  ^Monoid)

