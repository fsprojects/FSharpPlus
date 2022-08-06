namespace FSharpPlus.Control
    
    [<Class>]
    type Item =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: 'K -> source:  ^Indexed<'T> -> 'T
                           when (Item or  ^Indexed<'T>) :
                                  (static member Item:
                                      ^Indexed<'T> * 'K * Item -> 'T)
        
        static member Item: x: 'T list * n: int * _impl: Item -> 'T
        
        static member
          Item: x: 'T[,,,] * (int * int * int * int) * _impl: Item -> 'T
        
        static member Item: x: 'T[,,] * (int * int * int) * _impl: Item -> 'T
        
        static member Item: x: 'T[,] * (int * int) * _impl: Item -> 'T
        
        static member Item: x: 'T[] * n: int * _impl: Item -> 'T
        
        static member
          Item: x: System.Text.StringBuilder * n: int * _impl: Item -> char
        
        static member Item: x: string * n: int * _impl: Item -> char
        
        static member
          inline Item: 'T * 'a * _impl: Internals.Default1 -> unit
                         when 'T: null and 'T: struct
        
        static member
          inline Item: x:  ^Indexable<'T> * k: 'a * _impl: Internals.Default1
                         -> 'T
                         when  ^Indexable<'T> :
                                (member get_Item:  ^Indexable<'T> * 'a -> 'T)
    
    [<Class>]
    type TryItem =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: 'K -> source:  ^Indexed<'T> -> 'T option
                           when (TryItem or  ^Indexed<'T>) :
                                  (static member TryItem:
                                      ^Indexed<'T> * 'K * TryItem -> 'T option)
        
        static member
          TryItem: x: Map<'K,'T> * k: 'K * _impl: TryItem -> 'T option
                     when 'K: comparison
        
        static member
          TryItem: x: System.Collections.Generic.IReadOnlyList<'a> * n: int *
                   _impl: Internals.Default3 -> 'a option
        
        static member
          TryItem: x: System.Collections.Generic.IList<'a> * n: int *
                   _impl: Internals.Default2 -> 'a option
        
        static member TryItem: x: 'a list * n: int * _impl: TryItem -> 'a option
        
        static member
          TryItem: x: ResizeArray<'a> * n: int * _impl: TryItem -> 'a option
        
        static member
          TryItem: x: 'a[,,,] * (int * int * int * int) * _impl: TryItem
                     -> 'a option
        
        static member
          TryItem: x: 'a[,,] * (int * int * int) * _impl: TryItem -> 'a option
        
        static member
          TryItem: x: 'a[,] * (int * int) * _impl: TryItem -> 'a option
        
        static member TryItem: x: 'a[] * n: int * _impl: TryItem -> 'a option
        
        static member
          TryItem: x: System.Text.StringBuilder * n: int * _impl: TryItem
                     -> char option
        
        static member
          TryItem: x: string * n: int * _impl: TryItem -> char option
        
        static member
          inline TryItem: 'T * 'a * _impl: Internals.Default1 -> unit
                            when 'T: null and 'T: struct
        
        static member
          inline TryItem: x:  ^Indexable<'T> * k: 'a * _impl: Internals.Default1
                            -> 'T option
                            when  ^Indexable<'T> :
                                   (static member TryItem:
                                      'a *  ^Indexable<'T> -> 'T option)
        
        static member
          inline TryItem: x:  ^Indexable<'T> * k: 'a * _impl: Internals.Default2
                            ->  ^R option
                            when  ^Indexable<'T> :
                                   (member TryGetValue:
                                       ^Indexable<'T> * 'a * byref< ^R> -> bool)
    
    [<Class>]
    type MapIndexed =
        inherit Internals.Default1
        
        static member
          inline Invoke: mapping: ('K -> 'T -> 'U) -> source:  ^Indexable<'T>
                           -> 'a
                           when (MapIndexed or  ^Indexable<'T>) :
                                  (static member MapIndexed:
                                      ^Indexable<'T> * ('K -> 'T -> 'U) *
                                     MapIndexed -> 'a)
        
        static member
          inline InvokeOnInstance: mapping: ('K -> 'T -> 'Key)
                                   -> source:  ^Indexable<'T> -> 'Indexable<'U>
                                     when  ^Indexable<'T> :
                                            (static member MapIndexed:
                                                ^Indexable<'T> *
                                               ('K -> 'T -> 'Key)
                                                 -> 'Indexable<'U>)
        
        static member
          inline MapIndexed:  ^t * ('K -> 'T -> 'U) * _mthd: Internals.Default1
                               -> unit when  ^t: null and  ^t: struct
        
        static member
          inline MapIndexed: x:  ^I<'T> * f: ('K -> 'T -> 'U) *
                             _impl: Internals.Default1 -> 'I<'U>
                               when  ^I<'T> :
                                      (static member MapIndexed:
                                          ^I<'T> * ('K -> 'T -> 'U) -> 'I<'U>)
        
        static member
          inline MapIndexed: x: seq<'T> * f: (int -> 'T -> 'U) *
                             _impl: Internals.Default2 -> seq<'U>
        
        static member
          MapIndexed: x: Map<'K,'T> * f: ('K -> 'T -> 'U) * _impl: MapIndexed
                        -> Map<'K,'U> when 'K: comparison
        
        static member
          MapIndexed: g: ('K -> 'T) * f: ('K -> 'T -> 'U) * _impl: MapIndexed
                        -> ('K -> 'U)
        
        static member
          MapIndexed: ('K * 'T) * f: ('K -> 'T -> 'U) * _impl: MapIndexed
                        -> 'K * 'U
        
        static member
          MapIndexed: x: 'T[] * f: (int -> 'T -> 'a) * _impl: MapIndexed -> 'a[]
        
        static member
          MapIndexed: x: 'T list * f: (int -> 'T -> 'a) * _impl: MapIndexed
                        -> 'a list
    
    [<Class>]
    type ChooseIndexed =
        inherit Internals.Default1
        
        static member
          inline ChooseIndexed:  ^t * ('K -> 'T -> 'U option) *
                                _mthd: Internals.Default1 -> unit
                                  when  ^t: null and  ^t: struct
        
        static member
          inline ChooseIndexed: x:  ^I<'T> * f: ('K -> 'T -> 'U option) *
                                _impl: Internals.Default1 -> 'I<'U>
                                  when  ^I<'T> :
                                         (static member ChooseIndexed:
                                             ^I<'T> * ('K -> 'T -> 'U option)
                                              -> 'I<'U>)
        
        static member
          inline ChooseIndexed: x: seq<'T> * f: (int -> 'T -> 'U option) *
                                _impl: Internals.Default2 -> seq<'U>
        
        static member
          ChooseIndexed: x: Map<'K,'T> * f: ('K -> 'T -> 'U option) *
                         _impl: ChooseIndexed -> Map<'K,'U> when 'K: comparison
        
        static member
          ChooseIndexed: g: ('K -> 'T) * f: ('K -> 'T -> 'U option) *
                         _impl: ChooseIndexed -> ('K -> 'U option)
        
        static member
          ChooseIndexed: ('K * 'T) * f: ('K -> 'T -> 'U) * _impl: ChooseIndexed
                           -> 'K * 'U
        
        static member
          ChooseIndexed: x: 'T[] * f: (int -> 'T -> 'a option) *
                         _impl: ChooseIndexed -> 'a[]
        
        static member
          ChooseIndexed: x: 'T list * f: (int -> 'T -> 'a option) *
                         _impl: ChooseIndexed -> 'a list
        
        static member
          inline Invoke: mapping: ('K -> 'T -> 'U option)
                         -> source:  ^Indexable<'T> -> 'a
                           when (ChooseIndexed or  ^Indexable<'T>) :
                                  (static member ChooseIndexed:
                                      ^Indexable<'T> * ('K -> 'T -> 'U option) *
                                     ChooseIndexed -> 'a)
        
        static member
          inline InvokeOnInstance: mapping: ('K -> 'T -> 'U option)
                                   -> source:  ^Indexable<'T> -> 'Indexable<'U>
                                     when  ^Indexable<'T> :
                                            (static member ChooseIndexed:
                                                ^Indexable<'T> *
                                               ('K -> 'T -> 'U option)
                                                 -> 'Indexable<'U>)
    
    [<Class>]
    type IterateIndexed =
        inherit Internals.Default1
        
        static member
          inline Invoke: action: ('K -> 'T -> unit) -> source:  ^Indexable<'T>
                           -> unit
                           when (IterateIndexed or  ^Indexable<'T>) :
                                  (static member IterateIndexed:
                                      ^Indexable<'T> * ('K -> 'T -> unit) *
                                     IterateIndexed -> unit)
        
        static member
          inline InvokeOnInstance: action: ('K -> 'T -> unit)
                                   -> source:  ^Indexable<'T> -> unit
                                     when  ^Indexable<'T> :
                                            (static member IterateIndexed:
                                                ^Indexable<'T> *
                                               ('K -> 'T -> unit) -> unit)
        
        static member
          inline IterateIndexed:  ^t * ('K -> 'T -> 'U) *
                                 _mthd: Internals.Default1 -> unit
                                   when  ^t: null and  ^t: struct
        
        static member
          inline IterateIndexed: x:  ^I<'T> * f: ('K -> 'T -> unit) *
                                 _impl: Internals.Default1 -> unit
                                   when  ^I<'T> :
                                          (static member IterateIndexed:
                                              ^I<'T> * ('K -> 'T -> unit)
                                               -> unit)
        
        static member
          inline IterateIndexed: x: seq<'T> * f: (int -> 'T -> unit) *
                                 _impl: Internals.Default2 -> unit
        
        static member
          IterateIndexed: x: Map<'K,'T> * f: ('K -> 'T -> unit) *
                          _impl: IterateIndexed -> unit when 'K: comparison
        
        static member
          IterateIndexed: x: 'T[] * f: (int -> 'T -> unit) *
                          _impl: IterateIndexed -> unit
        
        static member
          IterateIndexed: x: 'T list * f: (int -> 'T -> unit) *
                          _impl: IterateIndexed -> unit
    
    [<Class>]
    type FoldIndexed =
        inherit Internals.Default1
        
        static member
          inline FoldIndexed:  ^t * ('State -> 'Key -> 'T -> 'State) * 'S *
                              _mthd: Internals.Default1 -> unit
                                when  ^t: null and  ^t: struct
        
        static member
          inline FoldIndexed: x:  ^I<'T> * f: ('State -> 'Key -> 'T -> 'State) *
                              z: 'State * _impl: Internals.Default1 -> 'State
                                when  ^I<'T> :
                                       (static member FoldIndexed:
                                           ^I<'T> *
                                          ('State -> 'Key -> 'T -> 'State) *
                                          'State -> 'State)
        
        static member
          inline FoldIndexed: x: seq<'T> * f: ('State -> int -> 'T -> 'State) *
                              z: 'State * _impl: Internals.Default2 -> 'State
        
        static member
          FoldIndexed: Map<'k,'t> * f: ('a -> 'b -> 'c -> 'a) * z: 'a *
                       _impl: FoldIndexed -> (Map<'b,'c> -> 'a)
                         when 'k: comparison and 'b: comparison
        
        static member
          FoldIndexed: x: 'a[] * f: ('b -> int -> 'a -> 'b) * z: 'b *
                       _impl: FoldIndexed -> 'b
        
        static member
          FoldIndexed: x: 'a list * f: ('b -> int -> 'a -> 'b) * z: 'b *
                       _impl: FoldIndexed -> 'b
        
        static member
          inline Invoke: folder: ('State -> 'Key -> 'T -> 'State)
                         -> state: 'State -> foldable:  ^Foldable<'T> -> 'State
                           when (FoldIndexed or  ^Foldable<'T>) :
                                  (static member FoldIndexed:
                                      ^Foldable<'T> *
                                     ('State -> 'Key -> 'T -> 'State) * 'State *
                                     FoldIndexed -> 'State)
        
        static member
          inline InvokeOnInstance: folder: ('State -> 'Key -> 'T -> 'State)
                                   -> state: 'State -> source:  ^Indexable<'T>
                                     -> 'State
                                     when  ^Indexable<'T> :
                                            (static member FoldIndexed:
                                                ^Indexable<'T> *
                                               ('State -> 'Key -> 'T -> 'State) *
                                               'State -> 'State)
    
    [<Class>]
    type TraverseIndexed =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: ('Key -> 'T -> 'Functor<'U>) -> t:  ^Indexable<'T>
                           ->  ^Functor<'Indexable<'U>>
                           when (TraverseIndexed or  ^Indexable<'T> or
                                  ^Functor<'Indexable<'U>>) :
                                  (static member TraverseIndexed:
                                      ^Indexable<'T> *
                                     ('Key -> 'T -> 'Functor<'U>) *
                                      ^Functor<'Indexable<'U>> * TraverseIndexed
                                       ->  ^Functor<'Indexable<'U>>)
        
        static member
          inline InvokeOnInstance: f: ('Key -> 'T -> 'Functor<'U>)
                                   -> t:  ^Indexable<'T>
                                     -> 'Functor<'Indexable<'U>>
                                     when  ^Indexable<'T> :
                                            (static member TraverseIndexed:
                                                ^Indexable<'T> *
                                               ('Key -> 'T -> 'Functor<'U>)
                                                 -> 'Functor<'Indexable<'U>>)
        
        static member
          inline TraverseIndexed:  ^t * ('Index -> 'T -> 'Functor<'U>) * 'R *
                                  _mthd: Internals.Default1 -> ('a -> 'a)
                                    when  ^t: null and  ^t: struct
        
        static member
          inline TraverseIndexed: t:  ^I<'T> * f: ('Index -> 'T -> 'Functor<'U>) *
                                  _output: 'Functor<'I<'U>> *
                                  _impl: Internals.Default1 -> 'Functor<'I<'U>>
                                    when  ^I<'T> :
                                           (static member TraverseIndexed:
                                               ^I<'T> *
                                              ('Index -> 'T -> 'Functor<'U>)
                                                -> 'Functor<'I<'U>>)
        
        static member
          inline TraverseIndexed: t: seq<'T> * f: (int -> 'T ->  ^Functor<'U>) *
                                  _output:  ^Functor<seq<'U>> *
                                  _impl: Internals.Default2
                                    ->  ^Functor<seq<'U>>
                                    when (Apply or  ^a or  ^Functor<'U> or  ^b) :
                                           (static member ``<*>`` :
                                               ^a *  ^Functor<'U> *  ^b * Apply
                                                ->  ^b) and
                                         (IsLeftZero or  ^Functor<'U>) :
                                           (static member IsLeftZero:
                                               ^Functor<'U> ref * IsLeftZero
                                                -> bool) and
                                         (Apply or  ^g or  ^Functor<'U> or  ^f) :
                                           (static member ``<*>`` :
                                               ^g *  ^Functor<'U> *  ^f * Apply
                                                ->  ^f) and
                                         (Map or  ^b or  ^a) :
                                           (static member Map:
                                              ( ^b * ('d list -> 'd -> 'd list)) *
                                              Map ->  ^a) and
                                         (Return or  ^b) :
                                           (static member Return:
                                               ^b * Return -> ('c list ->  ^b)) and
                                         (Map or  ^b or  ^Functor<seq<'U>>) :
                                           (static member Map:
                                              ( ^b * ('e list -> seq<'e>)) * Map
                                                ->  ^Functor<seq<'U>>) and
                                         (Traverse or seq<int * 'T> or
                                           ^Functor<seq<'U>>) :
                                           (static member Traverse:
                                              seq<int * 'T> *
                                              (int * 'T ->  ^Functor<'U>) *
                                               ^Functor<seq<'U>> * Traverse
                                                ->  ^Functor<seq<'U>>) and
                                         (Map or  ^f or  ^Functor<seq<'U>>) :
                                           (static member Map:
                                              ( ^f * ('j list -> seq<'j>)) * Map
                                                ->  ^Functor<seq<'U>>) and
                                         (Return or  ^f) :
                                           (static member Return:
                                               ^f * Return -> ('i list ->  ^f)) and
                                         (Map or  ^f or  ^g) :
                                           (static member Map:
                                              ( ^f * ('h list -> 'h -> 'h list)) *
                                              Map ->  ^g)
        
        static member
          inline TraverseIndexed: t: Map<'a,'b> * f: ('a -> 'b ->  ^c) *
                                  _output:  ^R * _impl: TraverseIndexed ->  ^R
                                    when 'a: comparison and
                                         (Map or  ^c or  ^d) :
                                           (static member Map:
                                              ( ^c *
                                               ('e -> Map<'a,'e> -> Map<'a,'e>)) *
                                              Map ->  ^d) and
                                         (Apply or  ^d or  ^R) :
                                           (static member ``<*>`` :
                                               ^d *  ^R *  ^R * Apply ->  ^R) and
                                         (Return or  ^R) :
                                           (static member Return:
                                               ^R * Return
                                                -> (Map<'f,'g> ->  ^R)) and
                                         'f: comparison
        
        static member
          inline TraverseIndexed: a: System.Tuple<'a> * f: (unit -> 'a ->  ^b) *
                                  _output:  ^R * _impl: TraverseIndexed ->  ^R
                                    when (Map or  ^b or  ^R) :
                                           (static member Map:
                                              ( ^b * ('c -> System.Tuple<'c>)) *
                                              Map ->  ^R)
        
        static member
          inline TraverseIndexed: ('K * 'T) * f: ('K -> 'T ->  ^a) *
                                  _output:  ^R * _impl: TraverseIndexed ->  ^R
                                    when (Map or  ^a or  ^R) :
                                           (static member Map:
                                              ( ^a * ('b -> 'K * 'b)) * Map
                                                ->  ^R)
        
        static member
          inline TraverseIndexed: t: 'a[] * f: (int -> 'a ->  ^b) * _output:  ^R *
                                  _impl: TraverseIndexed ->  ^R
                                    when (Map or  ^b or  ^c) :
                                           (static member Map:
                                              ( ^b * ('d -> 'd[] -> 'd[])) * Map
                                                ->  ^c) and
                                         (Map or  ^b or  ^f) :
                                           (static member Map:
                                              ( ^b * ('g -> 'g[] -> 'g[])) * Map
                                                ->  ^f) and
                                         (Apply or  ^c or  ^R) :
                                           (static member ``<*>`` :
                                               ^c *  ^R *  ^R * Apply ->  ^R) and
                                         (Traverse or (int * 'a)[] or  ^R) :
                                           (static member Traverse:
                                              (int * 'a)[] * (int * 'a ->  ^b) *
                                               ^R * Traverse ->  ^R) and
                                         (Return or  ^R) :
                                           (static member Return:
                                               ^R * Return -> ('e[] ->  ^R)) and
                                         (Apply or  ^f or  ^R) :
                                           (static member ``<*>`` :
                                               ^f *  ^R *  ^R * Apply ->  ^R)
        
        static member
          inline TraverseIndexed: t: 'a list * f: (int -> 'a ->  ^b) *
                                  _output:  ^R * _impl: TraverseIndexed ->  ^R
                                    when (Map or  ^b or  ^c) :
                                           (static member Map:
                                              ( ^b * ('d -> 'd list -> 'd list)) *
                                              Map ->  ^c) and
                                         (Map or  ^b or  ^f) :
                                           (static member Map:
                                              ( ^b * ('g -> 'g list -> 'g list)) *
                                              Map ->  ^f) and
                                         (Apply or  ^c or  ^R) :
                                           (static member ``<*>`` :
                                               ^c *  ^R *  ^R * Apply ->  ^R) and
                                         (Traverse or (int * 'a) list or  ^R) :
                                           (static member Traverse:
                                              (int * 'a) list *
                                              (int * 'a ->  ^b) *  ^R * Traverse
                                                ->  ^R) and
                                         (Return or  ^R) :
                                           (static member Return:
                                               ^R * Return -> ('e list ->  ^R)) and
                                         (Apply or  ^f or  ^R) :
                                           (static member ``<*>`` :
                                               ^f *  ^R *  ^R * Apply ->  ^R)
    
    [<Class>]
    type FindIndex =
        inherit Internals.Default1
        
        static member
          inline FindIndex:  ^t * ('T -> 'bool) * _impl: Internals.Default1
                              -> unit when  ^t: null and  ^t: struct
        
        static member
          inline FindIndex: x:  ^C<T> * p: ('T -> bool) *
                            _impl: Internals.Default1 -> 'Index
                              when  ^C<T> :
                                     (static member FindIndex:
                                         ^C<T> * ('T -> bool) -> 'Index)
        
        static member
          FindIndex: x: seq<'a> * p: ('a -> bool) * _impl: Internals.Default2
                       -> int
        
        static member
          FindIndex: x: Internals.Id<'a> * p: ('a -> bool) * _impl: FindIndex
                       -> int
        
        static member
          FindIndex: x: 'a list * p: ('a -> bool) * _impl: FindIndex -> int
        
        static member
          FindIndex: x: ResizeArray<'a> * p: ('a -> bool) * _impl: FindIndex
                       -> int
        
        static member
          FindIndex: x: 'a[] * p: ('a -> bool) * _impl: FindIndex -> int
        
        static member
          FindIndex: x: string * p: (char -> bool) * _impl: FindIndex -> int
        
        static member
          inline Invoke: p: ('T -> bool) -> source:  ^Collection<'T> -> 'Index
                           when (FindIndex or  ^Collection<'T>) :
                                  (static member FindIndex:
                                      ^Collection<'T> * ('T -> bool) * FindIndex
                                       -> 'Index)
        
        static member
          inline InvokeOnInstance: p: ('T -> bool) -> source:  ^Collection<'T>
                                     -> 'Index
                                     when  ^Collection<'T> :
                                            (static member FindIndex:
                                                ^Collection<'T> * ('T -> bool)
                                                 -> 'Index)
    
    [<Class>]
    type TryFindIndex =
        inherit Internals.Default1
        
        static member
          inline Invoke: p: ('T -> bool) -> source:  ^Collection<'T>
                           -> 'Index option
                           when (TryFindIndex or  ^Collection<'T>) :
                                  (static member TryFindIndex:
                                      ^Collection<'T> * ('T -> bool) *
                                     TryFindIndex -> 'Index option)
        
        static member
          inline InvokeOnInstance: p: ('T -> bool) -> source:  ^Collection<'T>
                                     -> 'Index option
                                     when  ^Collection<'T> :
                                            (static member TryFindIndex:
                                                ^Collection<'T> * ('T -> bool)
                                                 -> 'Index option)
        
        static member
          inline TryFindIndex:  ^t * ('T -> bool) * _impl: Internals.Default1
                                 -> unit when  ^t: null and  ^t: struct
        
        static member
          inline TryFindIndex: x:  ^C<T> * p: ('T -> bool) *
                               _impl: Internals.Default1 -> 'Index option
                                 when  ^C<T> :
                                        (static member TryFindIndex:
                                            ^C<T> * ('T -> bool)
                                             -> 'Index option)
        
        static member
          TryFindIndex: x: seq<'a> * p: ('a -> bool) * _impl: Internals.Default2
                          -> int option
        
        static member
          TryFindIndex: x: Internals.Id<'a> * p: ('a -> bool) *
                        _impl: TryFindIndex -> int option
        
        static member
          TryFindIndex: x: 'a list * p: ('a -> bool) * _impl: TryFindIndex
                          -> int option
        
        static member
          TryFindIndex: x: ResizeArray<'a> * p: ('a -> bool) *
                        _impl: TryFindIndex -> int option
        
        static member
          TryFindIndex: x: 'a[] * p: ('a -> bool) * _impl: TryFindIndex
                          -> int option
        
        static member
          TryFindIndex: x: string * p: (char -> bool) * _impl: TryFindIndex
                          -> int option
    
    [<Class>]
    type FindSliceIndex =
        inherit Internals.Default1
        
        static member
          inline FindSliceIndex:  ^t * 'a * _impl: Internals.Default1 -> unit
                                   when  ^t: null and  ^t: struct
        
        static member
          inline FindSliceIndex: x:  ^C<'T> * e:  ^C<'T> *
                                 _impl: Internals.Default1 -> 'Index
                                   when  ^C<'T> :
                                          (static member FindSliceIndex:
                                              ^C<'T> *  ^C<'T> -> 'Index)
        
        static member
          FindSliceIndex: x: seq<'a> * e: seq<'a> * _impl: Internals.Default2
                            -> int
        
        static member
          FindSliceIndex: x: Internals.Id<'a> * e: Internals.Id<'a> *
                          _impl: FindSliceIndex -> int
        
        static member
          FindSliceIndex: x: 'a list * e: 'a list * _impl: FindSliceIndex -> int
        
        static member
          FindSliceIndex: x: 'a[] * e: 'a[] * _impl: FindSliceIndex -> int
        
        static member
          FindSliceIndex: x: ResizeArray<'a> * e: ResizeArray<'a> *
                          _impl: FindSliceIndex -> int
        
        static member
          FindSliceIndex: x: string * e: string * _impl: FindSliceIndex -> int
        
        static member
          inline Invoke: slice:  ^Collection<'T> -> source:  ^Collection<'T>
                           -> 'Index
                           when (FindSliceIndex or  ^Collection<'T>) :
                                  (static member FindSliceIndex:
                                      ^Collection<'T> *  ^Collection<'T> *
                                     FindSliceIndex -> 'Index)
        
        static member
          inline InvokeOnInstance: slice:  ^Collection<'T>
                                   -> source:  ^Collection<'T> -> 'Index
                                     when  ^Collection<'T> :
                                            (static member FindSliceIndex:
                                                ^Collection<'T> *
                                                ^Collection<'T> -> 'Index)
    
    [<Class>]
    type TryFindSliceIndex =
        inherit Internals.Default1
        
        static member
          inline Invoke: slice:  ^Collection<'T> -> source:  ^Collection<'T>
                           -> 'Index option
                           when (TryFindSliceIndex or  ^Collection<'T>) :
                                  (static member TryFindSliceIndex:
                                      ^Collection<'T> *  ^Collection<'T> *
                                     TryFindSliceIndex -> 'Index option)
        
        static member
          inline InvokeOnInstance: slice:  ^Collection<'T>
                                   -> source:  ^Collection<'T> -> 'Index option
                                     when  ^Collection<'T> :
                                            (static member TryFindSliceIndex:
                                                ^Collection<'T> *
                                                ^Collection<'T> -> 'Index option)
        
        static member
          inline TryFindSliceIndex:  ^t * 'a * _impl: Internals.Default1 -> unit
                                      when  ^t: null and  ^t: struct
        
        static member
          inline TryFindSliceIndex: x:  ^C<'T> * e:  ^C<'T> *
                                    _impl: Internals.Default1 -> 'Index option
                                      when  ^C<'T> :
                                             (static member TryFindSliceIndex:
                                                 ^C<'T> *  ^C<'T>
                                                  -> 'Index option)
        
        static member
          TryFindSliceIndex: x: seq<'a> * e: seq<'a> * _impl: Internals.Default2
                               -> int option
        
        static member
          TryFindSliceIndex: x: Internals.Id<'a> * e: Internals.Id<'a> *
                             _impl: TryFindSliceIndex -> int option
        
        static member
          TryFindSliceIndex: x: 'a list * e: 'a list * _impl: TryFindSliceIndex
                               -> int option
        
        static member
          TryFindSliceIndex: x: 'a[] * e: 'a[] * _impl: TryFindSliceIndex
                               -> int option
        
        static member
          TryFindSliceIndex: x: string * e: string * _impl: TryFindSliceIndex
                               -> int option
        
        static member
          TryFindSliceIndex: x: ResizeArray<'a> * e: ResizeArray<'a> *
                             _impl: TryFindSliceIndex -> int option

