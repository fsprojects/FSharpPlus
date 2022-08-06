namespace FSharpPlus.Control
    
    [<Class>]
    type Iterate =
        
        static member
          inline Invoke: action: ('T -> unit) -> source:  ^Functor<'T> -> unit
                           when (Iterate or  ^Functor<'T>) :
                                  (static member Iterate:
                                      ^Functor<'T> * ('T -> unit) -> unit)
        
        static member
          Iterate: x: Set<'T> * action: ('T -> unit) -> unit when 'T: comparison
        
        static member
          Iterate: x: System.Text.StringBuilder * action: (char -> unit) -> unit
        
        static member Iterate: x: string * action: (char -> unit) -> unit
        
        static member Iterate: x: ResizeArray<'a> * action: ('a -> unit) -> unit
        
        static member
          Iterate: x: System.Collections.Generic.Dictionary<'Key,'T> *
                   action: ('T -> unit) -> unit
        
        static member
          Iterate: x: Map<'Key,'T> * action: ('T -> unit) -> unit
                     when 'Key: comparison
        
        static member
          Iterate: System.Collections.Generic.KeyValuePair<'Key,'T> *
                   action: ('T -> unit) -> unit
        
        static member Iterate: x: Choice<'T,'E> * action: ('T -> unit) -> unit
        
        static member Iterate: x: Result<'T,'E> * action: ('T -> unit) -> unit
        
        static member Iterate: x: Async<'T> * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T[,,,] * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T[,,] * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T[,] * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T[] * action: ('T -> unit) -> unit
        
        static member Iterate: ('W * 'T) * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T list * action: ('T -> unit) -> unit
        
        static member Iterate: x: 'T option * action: ('T -> unit) -> unit
        
        static member Iterate: x: seq<'T> * action: ('T -> unit) -> unit
        
        static member Iterate: x: System.Lazy<'T> * action: ('T -> unit) -> unit
    
    [<Class>]
    type Map =
        inherit Internals.Default1
        
        static member
          inline Invoke: mapping: ('T -> 'U) -> source:  ^Functor<'T>
                           ->  ^Functor<'U>
                           when (Map or  ^Functor<'T> or  ^Functor<'U>) :
                                  (static member Map:
                                     ( ^Functor<'T> * ('T -> 'U)) * Map
                                       ->  ^Functor<'U>)
        
        static member
          inline InvokeOnInstance: mapping: ('T -> 'U) -> source:  ^Functor<'T>
                                     -> 'Functor<'U>
                                     when  ^Functor<'T> :
                                            (static member Map:
                                                ^Functor<'T> * ('T -> 'U)
                                                 -> 'Functor<'U>)
        
        static member
          inline Map: ( ^Profunctor<'B,'C> * ('C -> 'D)) *
                      _mthd: Internals.Default5 -> 'Profunctor<'B,'D>
                        when  ^Profunctor<'B,'C> :
                               (static member Dimap:
                                   ^Profunctor<'B,'C> * ('a -> 'a) * ('C -> 'D)
                                    -> 'Profunctor<'B,'D>)
        
        static member
          inline Map: ( ^Bifunctor<'T,'V> * ('V -> 'W)) *
                      _mthd: Internals.Default6 -> 'b
                        when  ^Bifunctor<'T,'V> :
                               (static member Bimap:
                                   ^Bifunctor<'T,'V> * ('a -> 'a) * ('V -> 'W)
                                    -> 'b)
        
        static member
          inline Map: ( ^t * 'a) * _mthd: Internals.Default1 -> unit
                        when  ^t: null and  ^t: struct
        
        static member
          inline Map: ( ^Functor<'T> * ('T -> 'U)) * _mthd: Internals.Default1
                        -> 'Functor<'U>
                        when  ^Functor<'T> :
                               (static member Map:
                                   ^Functor<'T> * ('T -> 'U) -> 'Functor<'U>)
        
        static member
          Map: (System.Collections.Generic.IReadOnlyCollection<'T> * ('T -> 'U)) *
               _mthd: Internals.Default1
                 -> System.Collections.Generic.IReadOnlyCollection<'U>
        
        static member
          Map: (System.Nullable<'T> * ('T -> 'U)) * _mthd: Internals.Default2
                 -> System.Nullable<'U>
                 when 'T: (new: unit -> 'T) and 'T: struct and
                      'T :> System.ValueType and 'U: (new: unit -> 'U) and
                      'U: struct and 'U :> System.ValueType
        
        static member
          Map: (System.IObservable<'T> * ('T -> 'U)) * _mthd: Internals.Default2
                 -> System.IObservable<'U>
        
        static member
          Map: (System.Collections.Generic.IReadOnlyDictionary<'Key,'T> *
                ('T -> 'U)) * _mthd: Internals.Default2
                 -> System.Collections.Generic.IReadOnlyDictionary<'Key,'U>
                 when 'Key: equality
        
        static member
          Map: (System.Collections.Generic.IDictionary<'Key,'T> * ('T -> 'U)) *
               _mthd: Internals.Default2
                 -> System.Collections.Generic.IDictionary<'Key,'U>
                 when 'Key: equality
        
        static member
          Map: (System.Collections.Generic.IEnumerator<'T> * ('T -> 'U)) *
               _mthd: Internals.Default2
                 -> System.Collections.Generic.IEnumerator<'U>
        
        static member
          Map: (Data.NonEmptySeq<'T> * ('T -> 'U)) * _mthd: Internals.Default2
                 -> Data.NonEmptySeq<'U>
        
        static member
          Map: (seq<'T> * ('T -> 'U)) * _mthd: Internals.Default2 -> seq<'U>
        
        static member
          inline Map: ( ^Applicative<'T> * ('T -> 'U)) *
                      _mthd: Internals.Default3 ->  ^Applicative<'U>
                        when  ^Applicative<'T> :
                               (static member (<*>) :
                                   ^Applicative<'T->'U> *  ^Applicative<'T>
                                    ->  ^Applicative<'U>) and
                             ( ^Applicative<'T->'U> or  ^Applicative<'T> or
                               ^Applicative<'U>) :
                               (static member (<*>) :
                                   ^Applicative<'T->'U> *  ^Applicative<'T>
                                    ->  ^Applicative<'U>) and
                              ^Applicative<'T->'U> :
                               (static member Return:
                                  ('T -> 'U) ->  ^Applicative<'T->'U>)
        
        static member
          inline Map: ( ^Monad<'T> * ('T -> 'U)) * _mthd: Internals.Default4
                        ->  ^Monad<'U>
                        when  ^Monad<'T> :
                               (static member (>>=) :
                                   ^Monad<'T> * ('T ->  ^Monad<'U>)
                                    ->  ^Monad<'U>) and
                             ( ^Monad<'T> or  ^Monad<'U>) :
                               (static member (>>=) :
                                   ^Monad<'T> * ('T ->  ^Monad<'U>)
                                    ->  ^Monad<'U>) and
                              ^Monad<'U> :
                               (static member Return: 'U ->  ^Monad<'U>)
        
        static member
          Map: (Internals.Set2<'T> * ('T -> 'U)) * _mthd: Map
                 -> Internals.Set2<'U> when 'T: comparison and 'U: comparison
        
        static member
          Map: (Set<'a> * ('a -> 'b)) * _mthd: Map -> Set<'b>
                 when 'a: comparison and 'b: comparison
        
        static member
          Map: (System.Text.StringBuilder * (char -> char)) * _mthd: Map
                 -> System.Text.StringBuilder
        
        static member Map: (string * (char -> char)) * _mthd: Map -> string
        
        static member
          Map: (ResizeArray<'T> * ('T -> 'U)) * _mthd: Map -> ResizeArray<'U>
        
        static member
          Map: (Quotations.Expr<'T> * ('T -> 'U)) * _mthd: Map
                 -> Quotations.Expr<'U>
        
        static member
          Map: (System.Collections.Generic.Dictionary<'Key,'T> * ('T -> 'U)) *
               _mthd: Map -> System.Collections.Generic.Dictionary<'Key,'U>
                 when 'Key: equality
        
        static member
          Map: (Map<'Key,'T> * ('T -> 'U)) * _mthd: Map -> Map<'Key,'U>
                 when 'Key: comparison
        
        static member
          Map: (System.Collections.Generic.KeyValuePair<'a,'T> * ('T -> 'U)) *
               _mthd: Map -> System.Collections.Generic.KeyValuePair<'a,'U>
        
        static member
          Map: (Choice<'T,'E> * ('T -> 'U)) * _mthd: Map -> Choice<'U,'E>
        
        static member
          Map: (Result<'T,'E> * ('T -> 'U)) * _mthd: Map -> Result<'U,'E>
        
        static member Map: (Async<'T> * ('T -> 'U)) * _mthd: Map -> Async<'U>
        
        static member Map: ('T[,,,] * ('T -> 'U)) * _mthd: Map -> 'U[,,,]
        
        static member Map: ('T[,,] * ('T -> 'U)) * _mthd: Map -> 'U[,,]
        
        static member Map: ('T[,] * ('T -> 'U)) * _mthd: Map -> 'U[,]
        
        static member Map: ('T[] * ('T -> 'U)) * _mthd: Map -> 'U[]
        
        static member
          Map: (('Monoid * 'T) * ('T -> 'U)) * _mthd: Map -> 'Monoid * 'U
        
        static member
          Map: (System.Func<'R,'T> * ('T -> 'U)) * _mthd: Map
                 -> System.Func<'R,'U>
        
        static member Map: (('R -> 'T) * ('T -> 'U)) * _mthd: Map -> ('R -> 'U)
        
        static member Map: ('T list * ('T -> 'U)) * _mthd: Map -> 'U list
        
        static member Map: ('T option * ('T -> 'U)) * _mthd: Map -> 'U option
        
        static member
          Map: (System.Threading.Tasks.Task<'T> * ('T -> 'U)) * _mthd: Map
                 -> System.Threading.Tasks.Task<'U>
        
        static member
          Map: (System.Lazy<'T> * ('T -> 'U)) * _mthd: Map -> Lazy<'U>
    
    [<Class>]
    type Unzip =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Functor<'T1 * 'T2>
                           -> 'Functor<'T1> * 'Functor<'T2>
                           when (Unzip or  ^Functor<'T1 * 'T2> or
                                 ('Functor<'T1> * 'Functor<'T2>)) :
                                  (static member Unzip:
                                     ( ^Functor<'T1 * 'T2> *
                                      ('Functor<'T1> * 'Functor<'T2>)) * Unzip
                                       -> 'Functor<'T1> * 'Functor<'T2>)
        
        static member
          Unzip: (System.IObservable<'T * 'U> *
                  (System.IObservable<'T> * ResizeArray<'U>)) * _mthd: Unzip
                   -> System.IObservable<'T> * System.IObservable<'U>
        
        static member
          Unzip: (System.Collections.Generic.IReadOnlyDictionary<'Key,('T * 'U)> *
                  (System.Collections.Generic.IReadOnlyDictionary<'a,'T> *
                   System.Collections.Generic.IReadOnlyDictionary<'b,'U>)) *
                 _mthd: Unzip
                   -> System.Collections.Generic.IReadOnlyDictionary<'Key,'T> *
                      System.Collections.Generic.IReadOnlyDictionary<'Key,'U>
                   when 'Key: equality
        
        static member
          Unzip: (System.Collections.Generic.IDictionary<'Key,('T * 'U)> *
                  (System.Collections.Generic.IDictionary<'a,'T> *
                   System.Collections.Generic.IDictionary<'b,'U>)) *
                 _mthd: Unzip
                   -> System.Collections.Generic.IDictionary<'Key,'T> *
                      System.Collections.Generic.IDictionary<'Key,'U>
                   when 'Key: equality
        
        static member
          Unzip: (System.Collections.Generic.IEnumerator<'T * 'U> *
                  (System.Collections.Generic.IEnumerator<'T> * ResizeArray<'U>)) *
                 _mthd: Unzip
                   -> System.Collections.Generic.IEnumerator<'T> *
                      System.Collections.Generic.IEnumerator<'U>
        
        static member
          Unzip: (Data.NonEmptySeq<'T * 'U> *
                  (Data.NonEmptySeq<'T> * Data.NonEmptySeq<'U>)) * _mthd: Unzip
                   -> Data.NonEmptySeq<'T> * Data.NonEmptySeq<'U>
        
        static member
          Unzip: (seq<'T * 'U> * (seq<'T> * seq<'U>)) * _mthd: Unzip
                   -> seq<'T> * seq<'U>
        
        static member
          Unzip: (ResizeArray<'T * 'U> * (ResizeArray<'T> * ResizeArray<'U>)) *
                 _mthd: Unzip -> ResizeArray<'T> * ResizeArray<'U>
        
        static member
          Unzip: (Quotations.Expr<'T * 'U> *
                  (Quotations.Expr<'T> * Quotations.Expr<'U>)) * _mthd: Unzip
                   -> Quotations.Expr<'T> * Quotations.Expr<'U>
        
        static member
          Unzip: (System.Collections.Generic.Dictionary<'Key,('T * 'U)> *
                  (System.Collections.Generic.Dictionary<'a,'T> *
                   System.Collections.Generic.Dictionary<'b,'U>)) * _mthd: Unzip
                   -> System.Collections.Generic.Dictionary<'Key,'T> *
                      System.Collections.Generic.Dictionary<'Key,'U>
                   when 'Key: equality
        
        static member
          Unzip: (Map<'Key,('T * 'U)> * (Map<'a,'T> * Map<'b,'U>)) *
                 _mthd: Unzip -> Map<'Key,'T> * Map<'Key,'U>
                   when 'Key: comparison and 'a: comparison and 'b: comparison
        
        static member
          Unzip: (System.Collections.Generic.KeyValuePair<'Key,('T * 'U)> *
                  (System.Collections.Generic.KeyValuePair<'a,'T> *
                   System.Collections.Generic.KeyValuePair<'b,'U>)) *
                 _mthd: Unzip
                   -> System.Collections.Generic.KeyValuePair<'Key,'T> *
                      System.Collections.Generic.KeyValuePair<'Key,'U>
        
        static member
          Unzip: (Choice<('T * 'U),'E> * (Choice<'T,'E> * Choice<'U,'E>)) *
                 _mthd: Unzip -> Choice<'T,'E> * Choice<'U,'E>
        
        static member
          Unzip: (Result<('T * 'U),'E> * (Result<'T,'E> * Result<'U,'E>)) *
                 _mthd: Unzip -> Result<'T,'E> * Result<'U,'E>
        
        static member
          Unzip: (Async<'T * 'U> * (Async<'T> * Async<'U>)) * _mthd: Unzip
                   -> Async<'T> * Async<'U>
        
        static member
          Unzip: (('T * 'U)[,,,] * ('T[,,,] * 'U[,,,])) * _mthd: Unzip
                   -> 'T[,,,] * 'U[,,,]
        
        static member
          Unzip: (('T * 'U)[,,] * ('T[,,] * 'U[,,])) * _mthd: Unzip
                   -> 'T[,,] * 'U[,,]
        
        static member
          Unzip: (('T * 'U)[,] * ('T[,] * 'U[,])) * _mthd: Unzip
                   -> 'T[,] * 'U[,]
        
        static member
          Unzip: (('T * 'U)[] * ('T[] * 'U[])) * _mthd: Unzip -> 'T[] * 'U[]
        
        static member
          Unzip: (('Monoid * ('T * 'U)) * (('Monoid * 'T) * ('Monoid * 'U))) *
                 _mthd: Unzip -> ('Monoid * 'T) * ('Monoid * 'U)
        
        static member
          Unzip: (System.Func<'R,('T * 'U)> *
                  (System.Func<'R,'T> * System.Func<'R,'U>)) * _mthd: Unzip
                   -> System.Func<'R,'T> * System.Func<'R,'U>
        
        static member
          Unzip: (('R -> 'T * 'U) * (('R -> 'T) * ('R -> 'U))) * _mthd: Unzip
                   -> ('R -> 'T) * ('R -> 'U)
        
        static member
          Unzip: (('T * 'U) list * ('T list * 'U list)) * _mthd: Unzip
                   -> 'T list * 'U list
        
        static member
          Unzip: (('T * 'U) option * ('T option * 'U option)) * _mthd: Unzip
                   -> 'T option * 'U option
        
        static member
          Unzip: (System.Threading.Tasks.Task<'T * 'U> *
                  (System.Threading.Tasks.Task<'T> *
                   System.Threading.Tasks.Task<'U>)) * _mthd: Unzip
                   -> System.Threading.Tasks.Task<'T> *
                      System.Threading.Tasks.Task<'U>
        
        static member
          Unzip: (System.Lazy<'T * 'U> * (System.Lazy<'T> * System.Lazy<'U>)) *
                 _mthd: Unzip -> Lazy<'T> * Lazy<'U>
        
        static member
          inline Unzip: ( ^t * 'a) * 'b -> unit when  ^t: null and  ^t: struct
        
        static member
          inline Unzip: ( ^Functor<'T * 'U> * ('Functor<'T> * 'Functor<'U>)) *
                        _mthd: Internals.Default1 -> 'Functor<'T> * 'Functor<'U>
                          when  ^Functor<'T * 'U> :
                                 (static member Unzip:
                                     ^Functor<'T * 'U>
                                      -> 'Functor<'T> * 'Functor<'U>)
        
        static member
          inline Unzip: ( ^Functor<'T * 'U> * ('Functor<'T> *  ^Functor<'U>)) *
                        _mthd: Internals.Default2
                          -> 'Functor<'T> *  ^Functor<'U>
                          when  ^Functor<'T * 'U> :
                                 (static member Map:
                                     ^Functor<'T * 'U> * ('a * 'b -> 'a)
                                      -> 'Functor<'T>) and
                               (Map or  ^Functor<'T * 'U> or  ^Functor<'U>) :
                                 (static member Map:
                                    ( ^Functor<'T * 'U> * ('c * 'd -> 'd)) * Map
                                      ->  ^Functor<'U>)
    
    [<Class>]
    type Zip =
        inherit Internals.Default1
        
        static member
          inline Invoke: source1:  ^ZipFunctor<'T1>
                         -> source2:  ^ZipFunctor<'T2>
                           ->  ^ZipFunctor<'T1 * 'T2>
                           when (Zip or  ^ZipFunctor<'T1> or  ^ZipFunctor<'T2> or
                                  ^ZipFunctor<'T1 * 'T2>) :
                                  (static member Zip:
                                     ( ^ZipFunctor<'T1> *  ^ZipFunctor<'T2> *
                                       ^ZipFunctor<'T1 * 'T2>) * Zip
                                       ->  ^ZipFunctor<'T1 * 'T2>)
        
        static member
          inline InvokeOnInstance: source1:  ^ZipFunctor<'T1>
                                   -> source2:  ^ZipFunctor<'T2>
                                     ->  ^ZipFunctor<'T1 * 'T2>
                                     when ( ^ZipFunctor<'T1> or
                                            ^ZipFunctor<'T2> or
                                            ^ZipFunctor<'T1 * 'T2>) :
                                            (static member Zip:
                                                ^ZipFunctor<'T1> *
                                                ^ZipFunctor<'T2>
                                                 ->  ^ZipFunctor<'T1 * 'T2>)
        
        static member
          inline Zip: ( ^ZipFunctor<'T1> *  ^ZipFunctor<'T2> *
                        ^ZipFunctor<'T1 * 'T2>) * _mthd: Internals.Default1
                        ->  ^ZipFunctor<'T1 * 'T2>
                        when ( ^ZipFunctor<'T1> or  ^ZipFunctor<'T2> or
                               ^ZipFunctor<'T1 * 'T2>) :
                               (static member Zip:
                                   ^ZipFunctor<'T1> *  ^ZipFunctor<'T2>
                                    ->  ^ZipFunctor<'T1 * 'T2>)
        
        static member
          inline Zip: ( ^t *  ^u *  ^r) * _mthd: Internals.Default1
                        -> ('a -> 'a)
                        when  ^t: null and  ^t: struct and  ^u: null and
                              ^u: struct and  ^r: null and  ^r: struct
        
        static member
          Zip: (System.Threading.Tasks.Task<'T> *
                System.Threading.Tasks.Task<'U> *
                System.Threading.Tasks.Task<'T * 'U>) * _mthd: Zip
                 -> System.Threading.Tasks.Task<'T * 'U>
        
        static member
          Zip: (Async<'T> * Async<'U> * Async<'T * 'U>) * _mthd: Zip
                 -> Async<'T * 'U>
        
        static member
          Zip: ('T option * 'U option * ('T * 'U) option) * _mthd: Zip
                 -> ('T * 'U) option
        
        static member
          Zip: (ResizeArray<'T> * ResizeArray<'U> * ResizeArray<'T * 'U>) *
               _mthd: Zip -> ResizeArray<'T * 'U>
        
        static member
          Zip: ('T[] * 'U[] * ('T * 'U)[]) * _mthd: Zip -> ('T * 'U)[]
        
        static member
          Zip: ('T list * 'U list * ('T * 'U) list) * _mthd: Zip
                 -> ('T * 'U) list
        
        static member
          Zip: (System.Func<'R,'T> * System.Func<'R,'U> *
                System.Func<'R,('T * 'U)>) * _mthd: Zip
                 -> System.Func<'R,('T * 'U)>
        
        static member
          Zip: (('R -> 'T) * ('R -> 'U) * ('R -> 'T * 'U)) * _mthd: Zip
                 -> ('R -> 'T * 'U)
        
        static member
          Zip: (Map<'K,'T> * Map<'K,'U> * Map<'K,('T * 'U)>) * _mthd: Zip
                 -> Map<'K,('T * 'U)> when 'K: comparison
        
        static member
          Zip: (System.Collections.Generic.Dictionary<'K,'T> *
                System.Collections.Generic.Dictionary<'K,'U> *
                System.Collections.Generic.Dictionary<'K,('T * 'U)>) *
               _mthd: Zip -> System.Collections.Generic.Dictionary<'K,('T * 'U)>
                 when 'K: equality
        
        static member
          Zip: (System.Collections.Generic.IReadOnlyDictionary<'K,'T> *
                System.Collections.Generic.IReadOnlyDictionary<'K,'U> *
                System.Collections.Generic.IReadOnlyDictionary<'K,('T * 'U)>) *
               _mthd: Zip
                 -> System.Collections.Generic.IReadOnlyDictionary<'K,('T * 'U)>
                 when 'K: equality
        
        static member
          Zip: (System.Collections.Generic.IDictionary<'K,'T> *
                System.Collections.Generic.IDictionary<'K,'U> *
                System.Collections.Generic.IDictionary<'K,('T * 'U)>) *
               _mthd: Zip
                 -> System.Collections.Generic.IDictionary<'K,('T * 'U)>
                 when 'K: equality
        
        static member
          Zip: (Data.NonEmptySeq<'T> * Data.NonEmptySeq<'U> *
                Data.NonEmptySeq<'T * 'U>) * _mthd: Zip
                 -> Data.NonEmptySeq<'T * 'U>
        
        static member
          Zip: (seq<'T> * seq<'U> * seq<'T * 'U>) * _mthd: Zip -> seq<'T * 'U>
        
        static member
          Zip: (System.Collections.Generic.IEnumerator<'T> *
                System.Collections.Generic.IEnumerator<'U> *
                System.Collections.Generic.IEnumerator<'T * 'U>) * _mthd: Zip
                 -> System.Collections.Generic.IEnumerator<'T * 'U>
    
    [<Class>]
    type Bimap =
        inherit Internals.Default1
        
        static member
          inline Bimap:  ^t * ('T -> 'U) * ('V -> 'W) *
                        _mthd: Internals.Default1 -> unit
                          when  ^t: null and  ^t: struct
        
        static member
          inline Bimap: x:  ^Bifunctor<'T,'V> * f: ('T -> 'U) * g: ('V -> 'W) *
                        _mthd: Internals.Default1 -> 'Bifunctor<'U,'W>
                          when  ^Bifunctor<'T,'V> :
                                 (static member Bimap:
                                     ^Bifunctor<'T,'V> * ('T -> 'U) * ('V -> 'W)
                                      -> 'Bifunctor<'U,'W>)
        
        static member
          inline Bimap: x:  ^Bifunctor<'T,'V> * f: ('T -> 'U) * g: ('V -> 'W) *
                        _mthd: Internals.Default2 -> 'Bifunctor<'U,'W>
                          when  ^Bifunctor<'T,'V> :
                                 (static member First:
                                     ^Bifunctor<'T,'V> * ('T -> 'U) ->  ^a) and
                                ^a:
                                 (static member Map:
                                     ^a * ('V -> 'W) -> 'Bifunctor<'U,'W>)
        
        static member
          Bimap: x: Choice<'T2,'T1> * f: ('T1 -> 'U1) * g: ('T2 -> 'U2) *
                 _mthd: Bimap -> Choice<'U2,'U1>
        
        static member
          Bimap: System.Collections.Generic.KeyValuePair<'T1,'T2> *
                 f: ('T1 -> 'U1) * g: ('T2 -> 'U2) * _mthd: Bimap
                   -> System.Collections.Generic.KeyValuePair<'U1,'U2>
        
        static member
          Bimap: x: Result<'T2,'T1> * f: ('T1 -> 'U1) * g: ('T2 -> 'U2) *
                 _mthd: Bimap -> Result<'U2,'U1>
        
        static member
          Bimap: ('T1 * 'T2) * f: ('T1 -> 'U1) * g: ('T2 -> 'U2) * _mthd: Bimap
                   -> 'U1 * 'U2
        
        static member
          inline Invoke: f: ('T -> 'U) -> g: ('V -> 'W)
                         -> source:  ^Bifunctor<'T,'V> ->  ^Bifunctor<'U,'W>
                           when (Bimap or  ^Bifunctor<'T,'V> or
                                  ^Bifunctor<'U,'W>) :
                                  (static member Bimap:
                                      ^Bifunctor<'T,'V> * ('T -> 'U) *
                                     ('V -> 'W) * Bimap ->  ^Bifunctor<'U,'W>)
        
        static member
          inline InvokeOnInstance: f: ('T -> 'U) -> g: ('V -> 'W)
                                   -> source:  ^Bifunctor<'T,'V>
                                     -> 'Bifunctor<'U,'W>
                                     when  ^Bifunctor<'T,'V> :
                                            (static member Bimap:
                                                ^Bifunctor<'T,'V> * ('T -> 'U) *
                                               ('V -> 'W) -> 'Bifunctor<'U,'W>)
    
    [<Class>]
    type MapFirst =
        inherit Internals.Default1
        
        static member
          inline First:  ^t * ('T -> 'U) * _mthd: Internals.Default1 -> unit
                          when  ^t: null and  ^t: struct
        
        static member
          inline First: x:  ^Bifunctor<'T,'V> * f: ('T -> 'U) *
                        _mthd: Internals.Default1 -> 'Bifunctor<'U,'V>
                          when  ^Bifunctor<'T,'V> :
                                 (static member First:
                                     ^Bifunctor<'T,'V> * ('T -> 'U)
                                      -> 'Bifunctor<'U,'V>)
        
        static member
          inline First: x:  ^Bifunctor<'T,'V> * f: ('T -> 'U) *
                        _mthd: Internals.Default2 -> 'Bifunctor<'U,'V>
                          when  ^Bifunctor<'T,'V> :
                                 (static member Bimap:
                                     ^Bifunctor<'T,'V> * ('T -> 'U) * ('a -> 'a)
                                      -> 'Bifunctor<'U,'V>)
        
        static member
          First: System.Collections.Generic.KeyValuePair<'T1,'T2> *
                 f: ('T1 -> 'U1) * _mthd: MapFirst
                   -> System.Collections.Generic.KeyValuePair<'U1,'T2>
        
        static member
          First: x: Choice<'T2,'T1> * f: ('T1 -> 'U1) * _mthd: MapFirst
                   -> Choice<'T2,'U1>
        
        static member
          First: x: Result<'T2,'T1> * f: ('T1 -> 'U1) * _mthd: MapFirst
                   -> Result<'T2,'U1>
        
        static member
          First: ('T1 * 'T2) * f: ('T1 -> 'U1) * _mthd: MapFirst -> 'U1 * 'T2
        
        static member
          inline Invoke: f: ('T -> 'U) -> source:  ^Bifunctor<'T,'V>
                           ->  ^Bifunctor<'U,'V>
                           when (MapFirst or  ^Bifunctor<'T,'V> or
                                  ^Bifunctor<'U,'V>) :
                                  (static member First:
                                      ^Bifunctor<'T,'V> * ('T -> 'U) * MapFirst
                                       ->  ^Bifunctor<'U,'V>)
        
        static member
          inline InvokeOnInstance: f: ('T -> 'V) -> source:  ^Bifunctor<'T,'V>
                                     -> 'Bifunctor<'U,'V>
                                     when  ^Bifunctor<'T,'V> :
                                            (static member First:
                                                ^Bifunctor<'T,'V> * ('T -> 'V)
                                                 -> 'Bifunctor<'U,'V>)
    
    [<Class>]
    type Dimap =
        inherit Internals.Default1
        
        static member
          inline Dimap:  ^t * ('T -> 'U) * ('V -> 'W) *
                        _mthd: Internals.Default1 -> unit
                          when  ^t: null and  ^t: struct
        
        static member
          inline Dimap: x:  ^Profunctor<'B,'C> * ab: ('A -> 'B) * cd: ('C -> 'D) *
                        _mthd: Internals.Default1 -> 'Profunctor<'A,'D>
                          when  ^Profunctor<'B,'C> :
                                 (static member Dimap:
                                     ^Profunctor<'B,'C> * ('A -> 'B) *
                                    ('C -> 'D) -> 'Profunctor<'A,'D>)
        
        static member
          inline Dimap: x:  ^Profunctor<'B,'C> * ab: ('A -> 'B) * cd: ('C -> 'D) *
                        _mthd: Internals.Default2 -> 'Profunctor<'A,'D>
                          when  ^Profunctor<'B,'C> :
                                 (static member Map:
                                     ^Profunctor<'B,'C> * ('C -> 'D) ->  ^a) and
                                ^a:
                                 (static member Contramap:
                                     ^a * ('A -> 'B) -> 'Profunctor<'A,'D>)
        
        static member
          Dimap: f: System.Func<'B,'C> * g: ('A -> 'B) * h: ('C -> 'D) *
                 _mthd: Dimap -> System.Func<'A,'D>
        
        static member
          Dimap: f: ('B -> 'C) * g: ('A -> 'B) * h: ('C -> 'D) * _mthd: Dimap
                   -> ('A -> 'D)
        
        static member
          inline Invoke: ab: ('A -> 'B) -> cd: ('C -> 'D)
                         -> source:  ^Profunctor<'B,'C> ->  ^Profunctor<'A,'D>
                           when (Dimap or  ^Profunctor<'B,'C> or
                                  ^Profunctor<'A,'D>) :
                                  (static member Dimap:
                                      ^Profunctor<'B,'C> * ('A -> 'B) *
                                     ('C -> 'D) * Dimap ->  ^Profunctor<'A,'D>)
        
        static member
          inline InvokeOnInstance: ab: ('A -> 'B) -> cd: ('C -> 'D)
                                   -> source:  ^Profunctor<'B,'C>
                                     -> 'Profunctor<'A,'D>
                                     when  ^Profunctor<'B,'C> :
                                            (static member Dimap:
                                                ^Profunctor<'B,'C> * ('A -> 'B) *
                                               ('C -> 'D) -> 'Profunctor<'A,'D>)
    
    [<Class>]
    type Contramap =
        inherit Internals.Default1
        
        static member
          inline Contramap:  ^t * ('A -> 'B) * _mthd: Internals.Default1 -> unit
                              when  ^t: null and  ^t: struct
        
        static member
          inline Contramap: x:  ^Contravariant<'T> * f: ('U -> 'T) *
                            _mthd: Internals.Default1 -> 'Contravariant<'U>
                              when  ^Contravariant<'T> :
                                     (static member Contramap:
                                         ^Contravariant<'T> * ('U -> 'T)
                                          -> 'Contravariant<'U>)
        
        static member
          inline Contramap: x:  ^Profunctor<'B,'C> * f: ('A -> 'B) *
                            _mthd: Internals.Default2 -> 'Profunctor<'A,'C>
                              when  ^Profunctor<'B,'C> :
                                     (static member Dimap:
                                         ^Profunctor<'B,'C> * ('A -> 'B) *
                                        ('a -> 'a) -> 'Profunctor<'A,'C>)
        
        static member
          Contramap: c: System.Collections.Generic.IEqualityComparer<'T> *
                     f: ('U -> 'T) * _mthd: Contramap
                       -> System.Collections.Generic.IEqualityComparer<'U>
        
        static member
          Contramap: c: System.Collections.Generic.IComparer<'T> * f: ('U -> 'T) *
                     _mthd: Contramap
                       -> System.Collections.Generic.IComparer<'U>
        
        static member
          Contramap: p: System.Predicate<'T> * f: ('U -> 'T) * _mthd: Contramap
                       -> System.Predicate<'U>
        
        static member
          Contramap: k: System.Func<'T,'C> * f: ('U -> 'T) * _mthd: Contramap
                       -> System.Func<'U,'C>
        
        static member
          Contramap: k: ('T -> 'C) * f: ('U -> 'T) * _mthd: Contramap
                       -> ('U -> 'C)
        
        static member
          inline Invoke: f: ('U -> 'T) -> source:  ^Contravariant<'T>
                           ->  ^Contravariant<'U>
                           when (Contramap or  ^Contravariant<'T> or
                                  ^Contravariant<'U>) :
                                  (static member Contramap:
                                      ^Contravariant<'T> * ('U -> 'T) *
                                     Contramap ->  ^Contravariant<'U>)
        
        static member
          inline InvokeOnInstance: ab: ('A -> 'B) -> source:  ^Profunctor<'B,'C>
                                     -> 'Profunctor<'A,'C>
                                     when  ^Profunctor<'B,'C> :
                                            (static member Contramap:
                                                ^Profunctor<'B,'C> * ('A -> 'B)
                                                 -> 'Profunctor<'A,'C>)
    
    [<Class>]
    type Invmap =
        
        static member
          inline Invoke: f: ('T -> 'U) -> g: ('U -> 'T)
                         -> source:  ^InvariantFunctor<'T>
                           -> 'InvariantFunctor<'U>
                           when  ^InvariantFunctor<'T> :
                                  (static member Invmap:
                                      ^InvariantFunctor<'T> * ('T -> 'U) *
                                     ('U -> 'T) -> 'InvariantFunctor<'U>)

