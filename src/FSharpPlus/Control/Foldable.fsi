namespace FSharpPlus.Internals
    
    [<Struct>]
    type _Dual<'T> =
        
        new: value: 'T -> _Dual<'T>
        
        val Value: 'T
        
        static member
          inline (+) : x: _Dual< ^m> * y: _Dual< ^m> -> _Dual< ^m>
                         when (Control.Plus or  ^m) :
                                (static member ``+`` :
                                    ^m *  ^m * Control.Plus ->  ^m)
        
        static member
          inline get_Zero: unit -> _Dual< ^m>
                             when (Control.Zero or  ^m) :
                                    (static member Zero:
                                        ^m * Control.Zero ->  ^m)
    
    [<Struct>]
    type _Endo<'T> =
        
        new: value: ('T -> 'T) -> _Endo<'T>
        
        val Value: 'T -> 'T
        
        static member (+) : f: _Endo<'m> * g: _Endo<'m> -> _Endo<'m>
        
        static member get_Zero: unit -> _Endo<'m>
namespace FSharpPlus.Control
    
    [<Class>]
    type ToSeq =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable<'T> -> seq<'T>
                           when (ToSeq or  ^Foldable<'T>) :
                                  (static member ToSeq:
                                      ^Foldable<'T> * ToSeq -> seq<'T>)
        
        static member
          inline InvokeOnInstance: source:  ^Foldable<'T> -> seq<'T>
                                     when  ^Foldable<'T> :
                                            (static member ToSeq:
                                                ^Foldable<'T> -> seq<'T>)
        
        static member
          inline ToSeq: 'T * Internals.Default1 -> unit
                          when 'T: null and 'T: struct
        
        static member
          inline ToSeq: x:  ^Foldable * _impl: Internals.Default1 -> seq<'a>
                          when  ^Foldable:
                                 (static member ToSeq:  ^Foldable -> seq<'a>)
        
        static member
          inline ToSeq: x:  ^S * _impl: Internals.Default2 -> seq<'T>
                          when  ^S :> System.Collections.IEnumerable and
                                ^S: (member get_Item:  ^S * int -> 'T)
        
        static member ToSeq: x: Internals.Id<'T> * _impl: ToSeq -> seq<'T>
        
        static member ToSeq: x: 'T option * _impl: ToSeq -> seq<'T>
        
        static member ToSeq: x: string * ToSeq -> seq<char>
        
        static member ToSeq: x: System.Text.StringBuilder * ToSeq -> seq<char>
        
        static member ToSeq: x: seq<'T> * _impl: ToSeq -> seq<'T>
    
    [<Class>]
    type ToList =
        inherit Internals.Default1
        
        static member
          inline Invoke: value:  ^a -> 't list
                           when (ToList or  ^a) :
                                  (static member ToList:  ^a * ToList -> 't list)
        
        static member ToList: x: 'a list * _impl: ToList -> 'a list
        
        static member ToList: x: ResizeArray<'a> * _impl: ToList -> 'a list
        
        static member ToList: x: 'a[] * _impl: ToList -> 'a list
        
        static member
          ToList: x: System.Text.StringBuilder * _impl: ToList -> char list
        
        static member ToList: x: string * _impl: ToList -> char list
        
        static member
          ToList: x: Set<'a> * _impl: ToList -> 'a list when 'a: comparison
        
        static member
          inline ToList: x:  ^Foldable * _impl: Internals.Default1 -> 'a list
                           when  ^Foldable:
                                  (static member ToList:  ^Foldable -> 'a list)
        
        static member ToList: x: seq<'a> * _impl: Internals.Default2 -> 'a list
        
        static member
          inline ToList: x:  ^a * _impl: Internals.Default3 -> 'b list
                           when (ToSeq or  ^a) :
                                  (static member ToSeq:  ^a * ToSeq -> seq<'b>)
    
    [<Class>]
    type ToArray =
        inherit Internals.Default1
        
        static member
          inline Invoke: value:  ^a -> 't[]
                           when (ToArray or  ^a) :
                                  (static member ToArray:  ^a * ToArray -> 't[])
        
        static member ToArray: x: 'a list * _impl: ToArray -> 'a[]
        
        static member ToArray: x: ResizeArray<'a> * _impl: ToArray -> 'a[]
        
        static member ToArray: x: 'a[] * _impl: ToArray -> 'a[]
        
        static member
          ToArray: x: System.Text.StringBuilder * _impl: ToArray -> char[]
        
        static member ToArray: x: string * _impl: ToArray -> char[]
        
        static member
          ToArray: x: Set<'a> * _impl: ToArray -> 'a[] when 'a: comparison
        
        static member
          inline ToArray: x:  ^Foldable * _impl: Internals.Default1 -> 'a array
                            when  ^Foldable:
                                   (static member ToArray:
                                       ^Foldable -> 'a array)
        
        static member ToArray: x: seq<'a> * _impl: Internals.Default2 -> 'a[]
        
        static member
          inline ToArray: x:  ^a * _impl: Internals.Default3 -> 'b[]
                            when (ToSeq or  ^a) :
                                   (static member ToSeq:  ^a * ToSeq -> seq<'b>)
    
    [<Class>]
    type FoldBack =
        inherit Internals.Default1
        
        static member
          FoldBack: x: Internals.Id<'a> * f: ('a -> 'a0 -> 'b) * z: 'a0 *
                    _impl: FoldBack -> 'b
        
        static member
          FoldBack: x: System.Text.StringBuilder * f: (char -> 'a -> 'a) * z: 'a *
                    _impl: FoldBack -> 'a
        
        static member
          FoldBack: x: string * f: (char -> 'a -> 'a) * z: 'a * _impl: FoldBack
                      -> 'a
        
        static member
          FoldBack: x: ResizeArray<'a> * f: ('a -> 'b -> 'b) * z: 'b *
                    _impl: FoldBack -> 'b
        
        static member
          FoldBack: x: Set<'a> * f: ('a -> 'b -> 'b) * z: 'b * _impl: FoldBack
                      -> 'b when 'a: comparison
        
        static member
          FoldBack: x: 'a[] * f: ('a -> 'b -> 'b) * z: 'b * _impl: FoldBack
                      -> 'b
        
        static member
          FoldBack: x: 'a list * f: ('a -> 'b -> 'b) * z: 'b * _impl: FoldBack
                      -> 'b
        
        static member
          FoldBack: x: 'a option * f: ('a -> 'b -> 'b) * z: 'b * _impl: FoldBack
                      -> 'b
        
        static member
          FoldBack: x: seq<'a> * f: ('a -> 'b -> 'b) * z: 'b * _impl: FoldBack
                      -> 'b
        
        static member
          inline FoldBack: x:  ^F * f: ('a -> 'b -> 'b) * z: 'b *
                           _impl: Internals.Default1 -> 'b
                             when  ^F:
                                    (static member FoldBack:
                                        ^F * ('a -> 'b -> 'b) * 'b -> 'b)
        
        static member
          inline FoldBack: x:  ^F * f: ('a -> 'b -> 'b) * z: 'b *
                           _impl: Internals.Default2 -> 'b
                             when (ToList or  ^F) :
                                    (static member ToList:
                                        ^F * ToList -> 'a list) and
                                   ^F :> ResizeArray<'a>
        
        static member
          inline FromFoldMap: f: ('a -> 't -> 't) -> z: 't -> x:  ^b -> 't
                                when (FoldMap or  ^b) :
                                       (static member FoldMap:
                                           ^b * ('a -> Internals._Endo<'t>) *
                                          FoldMap -> Internals._Endo<'t>)
        
        static member
          inline Invoke: folder: ('T -> 'State -> 'State) -> state: 'State
                         -> foldable:  ^Foldable'<T> -> 'State
                           when (FoldBack or  ^Foldable'<T>) :
                                  (static member FoldBack:
                                      ^Foldable'<T> * ('T -> 'State -> 'State) *
                                     'State * FoldBack -> 'State)
    
    [<Class>]
    type FoldMap =
        inherit Internals.Default1
        
        static member
          inline FoldMap:  ^t * 'a * Internals.Default1 -> unit
                            when  ^t: null and  ^t: struct
        
        static member
          inline FoldMap: x:  ^F * f: 'a * _impl: Internals.Default1 -> 'b
                            when  ^F: (static member FoldMap:  ^F * 'a -> 'b)
        
        static member
          inline FoldMap: x: seq<'a> * f: ('a ->  ^b) *
                          _impl: Internals.Default2 ->  ^b
                            when (Plus or  ^b) :
                                   (static member ``+`` :
                                       ^b *  ^b * Plus ->  ^b) and
                                 (Zero or  ^b) :
                                   (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline FoldMap: x: 'a[] * f: ('a ->  ^b) * _impl: FoldMap ->  ^b
                            when (Plus or  ^b) :
                                   (static member ``+`` :
                                       ^b *  ^b * Plus ->  ^b) and
                                 (Zero or  ^b) :
                                   (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline FoldMap: x: Set<'a> * f: ('a ->  ^b) * _impl: FoldMap ->  ^b
                            when 'a: comparison and
                                 (Plus or  ^b) :
                                   (static member ``+`` :
                                       ^b *  ^b * Plus ->  ^b) and
                                 (Zero or  ^b) :
                                   (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline FoldMap: x: 'a list * f: ('a ->  ^b) * _impl: FoldMap ->  ^b
                            when (Plus or  ^b) :
                                   (static member ``+`` :
                                       ^b *  ^b * Plus ->  ^b) and
                                 (Zero or  ^b) :
                                   (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline FoldMap: x: 'a option * f: ('a ->  ^b) * _impl: FoldMap ->  ^b
                            when (Zero or  ^b) :
                                   (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline FromFoldFoldBack: f: ('a ->  ^b) -> x:  ^c ->  ^b
                                     when (Plus or  ^b) :
                                            (static member ``+`` :
                                                ^b *  ^b * Plus ->  ^b) and
                                          (Zero or  ^b) :
                                            (static member Zero:
                                                ^b * Zero ->  ^b) and
                                          (FoldBack or  ^c) :
                                            (static member FoldBack:
                                                ^c * ('a ->  ^b ->  ^b) *  ^b *
                                               FoldBack ->  ^b)
        
        static member
          inline Invoke: f: ('T -> 'Monoid) -> x:  ^Foldable'<T> -> 'Monoid
                           when (FoldMap or  ^Foldable'<T>) :
                                  (static member FoldMap:
                                      ^Foldable'<T> * ('T -> 'Monoid) * FoldMap
                                       -> 'Monoid)
    
    [<Class>]
    type Fold =
        inherit Internals.Default1
        
        static member
          Fold: x: 'a[] * f: ('b -> 'a -> 'b) * z: 'b * _impl: Fold -> 'b
        
        static member
          Fold: x: Set<'a> * f: ('b -> 'a -> 'b) * z: 'b * _impl: Fold -> 'b
                  when 'a: comparison
        
        static member
          Fold: x: 'a list * f: ('b -> 'a -> 'b) * z: 'b * _impl: Fold -> 'b
        
        static member
          Fold: x: seq<'a> * f: ('b -> 'a -> 'b) * z: 'b * _impl: Fold -> 'b
        
        static member
          Fold: x: Internals.Id<'a> * f: ('b -> 'a -> 'c) * z: 'b * _impl: Fold
                  -> 'c
        
        static member
          Fold: x: 'a option * f: ('b -> 'a -> 'b) * z: 'b * _impl: Fold -> 'b
        
        static member
          inline Fold: x:  ^F * f: ('b -> 'a -> 'b) * z: 'b *
                       _impl: Internals.Default1 -> 'b
                         when  ^F:
                                (static member Fold:
                                    ^F * ('b -> 'a -> 'b) * 'b -> 'b)
        
        static member
          inline Fold: x:  ^a * f: ('c -> 'b -> 'c) * z: 'c *
                       _impl: Internals.Default2 -> 'c
                         when (ToSeq or  ^a) :
                                (static member ToSeq:  ^a * ToSeq -> seq<'b>)
        
        static member
          inline FromFoldMap: f: ('t -> 'a -> 't) -> z: 't -> t:  ^b -> 't
                                when (FoldMap or  ^b) :
                                       (static member FoldMap:
                                           ^b *
                                          ('a
                                             -> Internals._Dual<Internals._Endo<'t>>) *
                                          FoldMap
                                            -> Internals._Dual<Internals._Endo<'t>>)
        
        static member
          inline Invoke: folder: ('State -> 'T -> 'State) -> state: 'State
                         -> foldable:  ^Foldable'<T> -> 'State
                           when (Fold or  ^Foldable'<T>) :
                                  (static member Fold:
                                      ^Foldable'<T> * ('State -> 'T -> 'State) *
                                     'State * Fold -> 'State)
    
    [<Class>]
    type Exists =
        inherit Internals.Default1
        
        static member
          Exists: x: System.Text.StringBuilder * f: (char -> bool) *
                  _impl: Exists -> bool
        
        static member
          Exists: x: string * f: (char -> bool) * _impl: Exists -> bool
        
        static member
          Exists: x: ResizeArray<'a> * f: ('a -> bool) * _impl: Exists -> bool
        
        static member
          Exists: x: Set<'a> * f: ('a -> bool) * _impl: Exists -> bool
                    when 'a: comparison
        
        static member Exists: x: 'a[] * f: ('a -> bool) * _impl: Exists -> bool
        
        static member
          Exists: x: 'a list * f: ('a -> bool) * _impl: Exists -> bool
        
        static member
          Exists: x: Internals.Id<'T> * f: ('T -> bool) * _impl: Exists -> bool
        
        static member
          inline Exists: x:  ^Foldable<'T> * f: ('T -> bool) *
                         _impl: Internals.Default1 -> bool
                           when  ^Foldable<'T> :
                                  (static member Exists:
                                      ^Foldable<'T> * ('T -> bool) -> bool)
        
        static member
          inline Exists: x:  ^a * f: ('b -> bool) * _impl: Internals.Default2
                           -> bool
                           when (ToSeq or  ^a) :
                                  (static member ToSeq:  ^a * ToSeq -> seq<'b>)
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Foldable'<T>
                           -> bool
                           when (Exists or  ^Foldable'<T>) :
                                  (static member Exists:
                                      ^Foldable'<T> * ('T -> bool) * Exists
                                       -> bool)
    
    [<Class>]
    type ForAll =
        inherit Internals.Default1
        
        static member
          ForAll: x: System.Text.StringBuilder * f: (char -> bool) *
                  _impl: ForAll -> bool
        
        static member
          ForAll: x: ResizeArray<'a> * f: ('a -> bool) * _impl: ForAll -> bool
        
        static member
          ForAll: x: string * f: (char -> bool) * _impl: ForAll -> bool
        
        static member
          ForAll: x: Set<'a> * f: ('a -> bool) * _impl: ForAll -> bool
                    when 'a: comparison
        
        static member ForAll: x: 'a[] * f: ('a -> bool) * _impl: ForAll -> bool
        
        static member
          ForAll: x: 'a list * f: ('a -> bool) * _impl: ForAll -> bool
        
        static member
          ForAll: x: Internals.Id<'T> * f: ('T -> bool) * _impl: ForAll -> bool
        
        static member
          inline ForAll: x:  ^Foldable<'T> * f: ('T -> bool) *
                         _impl: Internals.Default1 -> bool
                           when  ^Foldable<'T> :
                                  (static member ForAll:
                                      ^Foldable<'T> * ('T -> bool) -> bool)
        
        static member
          inline ForAll: x:  ^a * f: ('b -> bool) * _impl: Internals.Default2
                           -> bool
                           when (ToSeq or  ^a) :
                                  (static member ToSeq:  ^a * ToSeq -> seq<'b>)
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Foldable'<T>
                           -> bool
                           when (ForAll or  ^Foldable'<T>) :
                                  (static member ForAll:
                                      ^Foldable'<T> * ('T -> bool) * ForAll
                                       -> bool)
    
    [<Class>]
    type Find =
        inherit Internals.Default1
        
        static member Find: x: 'T[] * f: ('T -> bool) * _impl: Find -> 'T
        
        static member Find: x: 'T list * f: ('T -> bool) * _impl: Find -> 'T
        
        static member
          Find: x: ResizeArray<'T> * f: ('T -> bool) * _impl: Find -> 'T
        
        static member
          Find: x: Internals.Id<'T> * f: ('T -> bool) * _impl: Find -> 'T
        
        static member
          inline Find: x:  ^Foldable<'T> * f: ('T -> bool) *
                       _impl: Internals.Default1 -> 'T
                         when  ^Foldable<'T> :
                                (static member Find:
                                    ^Foldable<'T> * ('T -> bool) -> 'T)
        
        static member
          inline Find: x:  ^a * f: ('T -> bool) * _impl: Internals.Default2
                         -> 'T
                         when (ToSeq or  ^a) :
                                (static member ToSeq:  ^a * ToSeq -> seq<'T>)
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Foldable'<T> -> 'T
                           when (Find or  ^Foldable'<T>) :
                                  (static member Find:
                                      ^Foldable'<T> * ('T -> bool) * Find -> 'T)
    
    [<Class>]
    type TryFind =
        inherit Internals.Default1
        
        static member
          inline Invoke: predicate: ('T -> bool) -> source:  ^Foldable'<T>
                           -> 'T option
                           when (TryFind or  ^Foldable'<T>) :
                                  (static member TryFind:
                                      ^Foldable'<T> * ('T -> bool) * TryFind
                                       -> 'T option)
        
        static member
          TryFind: x: 'T[] * f: ('T -> bool) * _impl: TryFind -> 'T option
        
        static member
          TryFind: x: 'T list * f: ('T -> bool) * _impl: TryFind -> 'T option
        
        static member
          TryFind: x: seq<'T> * f: ('T -> bool) * _impl: TryFind -> 'T option
        
        static member
          TryFind: x: Internals.Id<'T> * f: ('T -> bool) * _impl: TryFind
                     -> 'T option
        
        static member
          inline TryFind: x:  ^a * f: ('T -> bool) * _impl: Internals.Default1
                            -> 'T option
                            when (ToSeq or  ^a) :
                                   (static member ToSeq:  ^a * ToSeq -> seq<'T>)
    
    [<Class>]
    type Head =
        inherit Internals.Default1
        
        static member Head: x: System.Text.StringBuilder * _impl: Head -> char
        
        static member Head: x: string * _impl: Head -> char
        
        static member Head: x: ResizeArray<'T> * _impl: Head -> 'T
        
        static member Head: x: Internals.Id<'T> * _impl: Head -> 'T
        
        static member Head: x: Data.NonEmptySeq<'T> * _impl: Head -> 'T
        
        static member Head: x: 'T[] * _impl: Head -> 'T
        
        static member Head: x: 'T option * _impl: Head -> 'T
        
        static member
          inline Head: x:  ^Foldable<'T> * _impl: Internals.Default1 -> 'T
                         when  ^Foldable<'T> :
                                (member get_Head:  ^Foldable<'T> -> 'T)
        
        static member
          inline Head: x:  ^Foldable<'T> * _impl: Internals.Default2 -> 'T
                         when (ToSeq or  ^Foldable<'T>) :
                                (static member ToSeq:
                                    ^Foldable<'T> * ToSeq -> seq<'T>)
        
        static member
          inline Invoke: source:  ^Foldable'<T> -> 'T
                           when (Head or  ^Foldable'<T>) :
                                  (static member Head:
                                      ^Foldable'<T> * Head -> 'T)
    
    [<Class>]
    type TryHead =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable'<T> -> 'T option
                           when (TryHead or  ^Foldable'<T>) :
                                  (static member TryHead:
                                      ^Foldable'<T> * TryHead -> 'T option)
        
        static member TryHead: x: seq<'t> * _impl: TryHead -> 't option
        
        static member
          TryHead: x: System.Text.StringBuilder * _impl: TryHead -> char option
        
        static member TryHead: x: string * _impl: TryHead -> char option
        
        static member TryHead: x: Internals.Id<'T> * _impl: TryHead -> 'T option
        
        static member
          TryHead: x: Data.NonEmptySeq<'T> * _impl: TryHead -> 'T option
        
        static member TryHead: x: 't[] * _impl: TryHead -> 't option
        
        static member TryHead: x: 't list * _impl: TryHead -> 't option
        
        static member
          inline TryHead: x:  ^a * _impl: Internals.Default1 -> 'b option
                            when (ToSeq or  ^a) :
                                   (static member ToSeq:  ^a * ToSeq -> seq<'b>)
    
    [<Class>]
    type TryLast =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable'<T> -> 'T option
                           when (TryLast or  ^Foldable'<T>) :
                                  (static member TryLast:
                                      ^Foldable'<T> * TryLast -> 'T option)
        
        static member TryLast: x: seq<'t> * _impl: TryLast -> 't option
        
        static member
          TryLast: x: System.Text.StringBuilder * _impl: TryLast -> char option
        
        static member TryLast: x: string * _impl: TryLast -> char option
        
        static member TryLast: x: Internals.Id<'T> * _impl: TryLast -> 'T option
        
        static member
          TryLast: x: Data.NonEmptySeq<'T> * _impl: TryLast -> 'T option
        
        static member TryLast: x: 't[] * _impl: TryLast -> 't option
        
        static member TryLast: x: 't list * _impl: TryLast -> 't option
        
        static member
          inline TryLast: x:  ^a * _impl: Internals.Default1 -> 'b option
                            when (ToSeq or  ^a) :
                                   (static member ToSeq:  ^a * ToSeq -> seq<'b>)
    
    [<Class>]
    type Pick =
        inherit Internals.Default1
        
        static member
          inline Invoke: chooser: ('T -> 'U option) -> source:  ^Foldable'<T>
                           -> 'U
                           when (Pick or  ^Foldable'<T>) :
                                  (static member Pick:
                                      ^Foldable'<T> * ('T -> 'U option) * Pick
                                       -> 'U)
        
        static member Pick: x: 'T[] * f: ('T -> 'U option) * _impl: Pick -> 'U
        
        static member
          Pick: x: 'T list * f: ('T -> 'U option) * _impl: Pick -> 'U
        
        static member
          Pick: x: ResizeArray<'T> * f: ('T -> 'U option) * _impl: Pick -> 'U
        
        static member
          Pick: x: Internals.Id<'T> * f: ('T -> 'U option) * _impl: Pick -> 'U
        
        static member
          inline Pick: x:  ^Foldable<'T> * f: ('T -> 'U option) *
                       _impl: Internals.Default1 -> 'T
                         when  ^Foldable<'T> :
                                (static member Pick:
                                    ^Foldable<'T> * ('T -> 'U option) -> 'T)
        
        static member
          inline Pick: x:  ^Foldable<'T> * f: ('T -> 'U option) *
                       _impl: Internals.Default2 -> 'U
                         when (ToSeq or  ^Foldable<'T>) :
                                (static member ToSeq:
                                    ^Foldable<'T> * ToSeq -> seq<'T>)
    
    [<Class>]
    type TryPick =
        inherit Internals.Default1
        
        static member
          inline Invoke: chooser: ('T -> 'U option) -> source:  ^Foldable'<T>
                           -> 'U option
                           when (TryPick or  ^Foldable'<T>) :
                                  (static member TryPick:
                                      ^Foldable'<T> * ('T -> 'U option) *
                                     TryPick -> 'U option)
        
        static member
          TryPick: x: 'T[] * f: ('T -> 'U option) * _impl: TryPick -> 'U option
        
        static member
          TryPick: x: 'T list * f: ('T -> 'U option) * _impl: TryPick
                     -> 'U option
        
        static member
          TryPick: x: seq<'T> * f: ('T -> 'U option) * _impl: TryPick
                     -> 'U option
        
        static member
          TryPick: Internals.Id<'T> * ('a -> 'U option) * _impl: TryPick
                     -> 'U option
        
        static member
          inline TryPick: x:  ^a * f: ('b -> 'U option) *
                          _impl: Internals.Default1 -> 'U option
                            when (ToSeq or  ^a) :
                                   (static member ToSeq:  ^a * ToSeq -> seq<'b>)
    
    [<Class>]
    type Nth =
        inherit Internals.Default1
        
        static member
          inline Invoke: n: int -> source:  ^Foldable<'T> -> 'T
                           when (Nth or  ^Foldable<'T>) :
                                  (static member Nth:
                                      ^Foldable<'T> * int * Nth -> 'T)
        
        static member Nth: x: Internals.Id<'a> * 'a0 * _impl: Nth -> 'a
        
        static member Nth: x: 'a list * n: int * _impl: Nth -> 'a
        
        static member Nth: x: ResizeArray<'a> * n: int * _impl: Nth -> 'a
        
        static member Nth: x: 'a[] * n: int * _impl: Nth -> 'a
        
        static member
          Nth: x: System.Text.StringBuilder * n: int * _impl: Nth -> char
        
        static member Nth: x: string * n: int * _impl: Nth -> char
        
        static member
          Nth: x: System.Collections.Generic.IList<'a> * n: int *
               _impl: Internals.Default1 -> 'a
        
        static member
          Nth: x: System.Collections.Generic.IReadOnlyList<'a> * n: int *
               _impl: Internals.Default2 -> 'a
        
        static member
          inline Nth: x:  ^Foldable<'T> * n: int * _impl: Internals.Default3
                        -> 'T
                        when (ToSeq or  ^Foldable<'T>) :
                               (static member ToSeq:
                                   ^Foldable<'T> * ToSeq -> seq<'T>)
    
    [<Class>]
    type Max =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable<'T> -> 'T
                           when (Max or  ^Foldable<'T>) :
                                  (static member Max:  ^Foldable<'T> * Max -> 'T) and
                                'T: comparison
        
        static member Max: x: 'T[] * _impl: Max -> 'T when 'T: comparison
        
        static member Max: x: 'T list * _impl: Max -> 'T when 'T: comparison
        
        static member
          Max: x: ResizeArray<'T> * _impl: Max -> 'T when 'T: comparison
        
        static member
          Max: x: Internals.Id<'T> * _impl: Max -> 'T when 'T: comparison
        
        static member
          inline Max: x:  ^Foldable<'T> * _impl: Internals.Default1 -> 'T
                        when  ^Foldable<'T> :
                               (static member Max:  ^Foldable<'T> -> 'T) and
                             'T: comparison
        
        static member
          inline Max: x:  ^Foldable<'T> * _impl: Internals.Default2 -> 'T
                        when (ToSeq or  ^Foldable<'T>) :
                               (static member ToSeq:
                                   ^Foldable<'T> * ToSeq -> seq<'T>) and
                             'T: comparison
    
    [<Class>]
    type MaxBy =
        inherit Internals.Default1
        
        static member
          inline Invoke: projection: ('T -> 'U) -> source:  ^Foldable<'T> -> 'T
                           when 'U: comparison and
                                (MaxBy or  ^Foldable<'T>) :
                                  (static member MaxBy:
                                      ^Foldable<'T> * ('T -> 'U) * MaxBy -> 'T)
        
        static member
          MaxBy: x: 'T[] * f: ('T -> 'U) * _impl: MaxBy -> 'T
                   when 'U: comparison
        
        static member
          MaxBy: x: 'T list * f: ('T -> 'U) * _impl: MaxBy -> 'T
                   when 'U: comparison
        
        static member
          MaxBy: x: ResizeArray<'T> * f: ('T -> 'U) * _impl: MaxBy -> 'T
                   when 'U: comparison
        
        static member
          MaxBy: x: Internals.Id<'T> * ('T -> 'U) * _impl: MaxBy -> 'T
                   when 'U: comparison
        
        static member
          inline MaxBy: x:  ^Foldable<'T> * f: ('T -> 'U) *
                        _impl: Internals.Default1 -> 'T
                          when  ^Foldable<'T> :
                                 (static member MaxBy:
                                     ^Foldable<'T> * ('T -> 'U) -> 'T) and
                               'U: comparison
        
        static member
          inline MaxBy: x:  ^Foldable<'T> * f: ('T -> 'U) *
                        _impl: Internals.Default2 -> 'T
                          when (ToSeq or  ^Foldable<'T>) :
                                 (static member ToSeq:
                                     ^Foldable<'T> * ToSeq -> seq<'T>) and
                               'U: comparison
    
    [<Class>]
    type Min =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable<'T> -> 'T
                           when (Min or  ^Foldable<'T>) :
                                  (static member Min:  ^Foldable<'T> * Min -> 'T) and
                                'T: comparison
        
        static member Min: x: 'T[] * _impl: Min -> 'T when 'T: comparison
        
        static member Min: x: 'T list * _impl: Min -> 'T when 'T: comparison
        
        static member
          Min: x: ResizeArray<'T> * _impl: Min -> 'T when 'T: comparison
        
        static member
          Min: x: Internals.Id<'T> * _impl: Min -> 'T when 'T: comparison
        
        static member
          inline Min: x:  ^Foldable<'T> * _impl: Internals.Default1 -> 'T
                        when  ^Foldable<'T> :
                               (static member Min:  ^Foldable<'T> -> 'T) and
                             'T: comparison
        
        static member
          inline Min: x:  ^Foldable<'T> * _impl: Internals.Default2 -> 'T
                        when (ToSeq or  ^Foldable<'T>) :
                               (static member ToSeq:
                                   ^Foldable<'T> * ToSeq -> seq<'T>) and
                             'T: comparison
    
    [<Class>]
    type MinBy =
        inherit Internals.Default1
        
        static member
          inline Invoke: projection: ('T -> 'U) -> source:  ^Foldable<'T> -> 'T
                           when (MinBy or  ^Foldable<'T>) :
                                  (static member MinBy:
                                      ^Foldable<'T> * ('T -> 'U) * MinBy -> 'T)
        
        static member
          MinBy: x: 'T[] * f: ('T -> 'a) * _impl: MinBy -> 'T
                   when 'a: comparison
        
        static member
          MinBy: x: 'T list * f: ('T -> 'a) * _impl: MinBy -> 'T
                   when 'a: comparison
        
        static member
          MinBy: x: ResizeArray<'T> * f: ('T -> 'a) * _impl: MinBy -> 'T
                   when 'a: comparison
        
        static member
          MinBy: x: Internals.Id<'T> * ('T -> 'U) * _impl: MinBy -> 'T
        
        static member
          inline MinBy: x:  ^Foldable<'T> * f: ('T -> 'U) *
                        _impl: Internals.Default1 -> 'T
                          when  ^Foldable<'T> :
                                 (static member MinBy:
                                     ^Foldable<'T> * ('T -> 'U) -> 'T)
        
        static member
          inline MinBy: x:  ^Foldable<'T> * f: ('T -> 'a) *
                        _impl: Internals.Default2 -> 'T
                          when (ToSeq or  ^Foldable<'T>) :
                                 (static member ToSeq:
                                     ^Foldable<'T> * ToSeq -> seq<'T>) and
                               'a: comparison
    
    [<Class>]
    type Length =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Foldable<'T> -> int
                           when (Length or  ^Foldable<'T>) :
                                  (static member Length:
                                      ^Foldable<'T> * Length -> int)
        
        static member Length: x: 'T[] * _impl: Length -> int
        
        static member Length: x: 'T option * _impl: Length -> int
        
        static member Length: x: 'T list * _impl: Length -> int
        
        static member Length: x: ResizeArray<'T> * _impl: Length -> int
        
        static member Length: Internals.Id<'T> * _impl: Length -> int
        
        static member
          inline Length: x:  ^Foldable<'T> * _impl: Internals.Default1 -> int
                           when  ^Foldable<'T> :
                                  (member get_Length:  ^Foldable<'T> -> int)
        
        static member
          inline Length: x:  ^Foldable<'T> * _impl: Internals.Default2 -> int
                           when (ToSeq or  ^Foldable<'T>) :
                                  (static member ToSeq:
                                      ^Foldable<'T> * ToSeq -> seq<'a>)
    
    [<Class>]
    type Reduce =
        
        static member
          inline Invoke: f: ('T -> 'T -> 'T) -> x:  ^Reducible<'T> -> 'T
                           when  ^Reducible<'T> :
                                  (static member Reduce:
                                      ^Reducible<'T> * ('T -> 'T -> 'T) -> 'T)

