namespace FSharpPlus.Control
    
    [<Class>]
    type Apply =
        inherit Internals.Default1
        
        static member
          inline Invoke: f:  ^Applicative<'T -> 'U> -> x:  ^Applicative<'T>
                           ->  ^Applicative<'U>
                           when (Apply or  ^Applicative<'T -> 'U> or
                                  ^Applicative<'T> or  ^Applicative<'U>) :
                                  (static member ``<*>`` :
                                      ^Applicative<'T -> 'U> *  ^Applicative<'T> *
                                      ^Applicative<'U> * Apply
                                       ->  ^Applicative<'U>)
        
        static member
          inline InvokeOnInstance: f:  ^Applicative<'T->'U>
                                   -> x:  ^Applicative<'T> ->  ^Applicative<'U>
                                     when ( ^Applicative<'T->'U> or
                                            ^Applicative<'T> or
                                            ^Applicative<'U>) :
                                            (static member (<*>) :
                                                ^Applicative<'T->'U> *
                                                ^Applicative<'T>
                                                 ->  ^Applicative<'U>)
        
        static member
          ``<*>`` : f: ResizeArray<('T -> 'U)> * x: ResizeArray<'T> *
                    _output: ResizeArray<'U> * _mthd: Apply -> ResizeArray<'U>
        
        static member
          ``<*>`` : f: Quotations.Expr<('T -> 'U)> * x: Quotations.Expr<'T> *
                    _output: Quotations.Expr<'U> * _mthd: Apply
                      -> Quotations.Expr<'U>
        
        static member
          ``<*>`` : f: System.Collections.Generic.Dictionary<'Key,('T -> 'U)> *
                    x: System.Collections.Generic.Dictionary<'Key,'T> *
                    _output: System.Collections.Generic.Dictionary<'Key,'U> *
                    _mthd: Apply
                      -> System.Collections.Generic.Dictionary<'Key,'U>
                      when 'Key: equality
        
        static member
          ``<*>`` : f: Map<'Key,('T -> 'U)> * x: Map<'Key,'T> *
                    _output: Map<'Key,'U> * _mthd: Apply -> Map<'Key,'U>
                      when 'Key: comparison
        
        static member
          inline ``<*>`` : System.Collections.Generic.KeyValuePair< ^Key,
                                                                   ('T -> 'U)> *
                           System.Collections.Generic.KeyValuePair< ^Key,'T> *
                           _output: System.Collections.Generic.KeyValuePair< ^Key,
                                                                            'U> *
                           _mthd: Apply
                             -> System.Collections.Generic.KeyValuePair< ^Key,'U>
                             when (Plus or  ^Key) :
                                    (static member ``+`` :
                                        ^Key *  ^Key * Plus ->  ^Key)
        
        static member
          ``<*>`` : f: Choice<('T -> 'U),'E> * x: Choice<'T,'E> *
                    _output: Choice<'b,'E> * _mthd: Apply -> Choice<'U,'E>
        
        static member
          ``<*>`` : f: Result<('T -> 'U),'E> * x: Result<'T,'E> *
                    _output: Result<'b,'E> * _mthd: Apply -> Result<'U,'E>
        
        static member
          ``<*>`` : f: ('T -> 'U) option * x: 'T option * _output: 'U option *
                    _mthd: Apply -> 'U option
        
        static member
          ``<*>`` : f: Async<('T -> 'U)> * x: Async<'T> * _output: Async<'U> *
                    _mthd: Apply -> Async<'U>
        
        static member
          ``<*>`` : f: System.Threading.Tasks.Task<('T -> 'U)> *
                    x: System.Threading.Tasks.Task<'T> *
                    _output: System.Threading.Tasks.Task<'U> * _mthd: Apply
                      -> System.Threading.Tasks.Task<'U>
        
        static member
          inline ``<*>`` : ( ^Monoid * ('T -> 'U)) * ( ^Monoid * 'T) *
                           _output: ( ^Monoid * 'U) * _mthd: Apply
                             ->  ^Monoid * 'U
                             when (Plus or  ^Monoid) :
                                    (static member ``+`` :
                                        ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member
          ``<*>`` : f: ('r -> 'T -> 'U) * g: ('r -> 'T) * _output: ('r -> 'U) *
                    _mthd: Apply -> ('r -> 'U)
        
        static member
          ``<*>`` : f: ('T -> 'U)[] * x: 'T[] * _output: 'U[] * _mthd: Apply
                      -> 'U[]
        
        static member
          ``<*>`` : f: ('T -> 'U) list * x: 'T list * _output: 'U list *
                    _mthd: Apply -> 'U list
        
        static member
          ``<*>`` : f: System.Collections.Generic.IEnumerator<('T -> 'U)> *
                    x: System.Collections.Generic.IEnumerator<'T> *
                    _output: System.Collections.Generic.IEnumerator<'U> *
                    _mthd: Apply -> System.Collections.Generic.IEnumerator<'U>
        
        static member
          ``<*>`` : f: Data.NonEmptySeq<('T -> 'U)> * x: Data.NonEmptySeq<'T> *
                    _output: Data.NonEmptySeq<'U> * _mthd: Apply
                      -> Data.NonEmptySeq<'U>
        
        static member
          ``<*>`` : f: seq<('T -> 'U)> * x: seq<'T> * _output: seq<'U> *
                    _mthd: Apply -> seq<'U>
        
        static member
          ``<*>`` : f: System.Lazy<('T -> 'U)> * x: System.Lazy<'T> *
                    _output: System.Lazy<'U> * _mthd: Apply -> System.Lazy<'U>
        
        static member
          inline ``<*>`` : f:  ^Applicative<'T->'U> * x:  ^Applicative<'T> *
                           _output:  ^Applicative<'U> *
                           _mthd: Internals.Default1 ->  ^Applicative<'U>
                             when ( ^Applicative<'T->'U> or  ^Applicative<'T> or
                                    ^Applicative<'U>) :
                                    (static member (<*>) :
                                        ^Applicative<'T->'U> *  ^Applicative<'T>
                                         ->  ^Applicative<'U>)
        
        static member
          inline ``<*>`` : f:  ^Monad<'T->'U> * x:  ^Monad<'T> *
                           _output:  ^Monad<'U> * _mthd: Internals.Default2
                             ->  ^Monad<'U>
                             when ( ^Monad<'T->'U> or  ^Monad<'U>) :
                                    (static member (>>=) :
                                        ^Monad<'T->'U> *
                                       (('T -> 'U) ->  ^Monad<'U>)
                                         ->  ^Monad<'U>) and
                                  ( ^Monad<'T> or  ^Monad<'U>) :
                                    (static member (>>=) :
                                        ^Monad<'T> * ('T ->  ^Monad<'U>)
                                         ->  ^Monad<'U>) and
                                   ^Monad<'U> :
                                    (static member Return: 'U ->  ^Monad<'U>)
    
    [<Class>]
    type Lift2 =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: ('T -> 'U -> 'V) -> x:  ^Applicative<'T>
                         -> y:  ^Applicative<'U> ->  ^Applicative<'V>
                           when (Lift2 or  ^Applicative<'T> or  ^Applicative<'U> or
                                  ^Applicative<'V>) :
                                  (static member Lift2:
                                     ('T -> 'U -> 'V) *
                                     ( ^Applicative<'T> *  ^Applicative<'U>) *
                                     Lift2 ->  ^Applicative<'V>)
        
        static member
          inline InvokeOnInstance: f: ('T -> 'U -> 'V) -> x:  ^Applicative<'T>
                                   -> y:  ^Applicative<'U> -> 'a
                                     when ( ^Applicative<'T> or
                                            ^Applicative<'U>) :
                                            (static member Lift2:
                                               ('T -> 'U -> 'V) *
                                                ^Applicative<'T> *
                                                ^Applicative<'U> -> 'a)
        
        static member
          inline Lift2: f: ('T -> 'U -> 'V) *
                        ( ^Applicative<'T> *  ^Applicative<'U>) *
                        _mthd: Internals.Default1 -> 'a
                          when ( ^Applicative<'T> or  ^Applicative<'U>) :
                                 (static member Lift2:
                                    ('T -> 'U -> 'V) *  ^Applicative<'T> *
                                     ^Applicative<'U> -> 'a)
        
        static member
          inline Lift2: 'a * ('t *  ^u) * _mthd: Internals.Default1
                          -> ('b -> 'b)
                          when 't: null and 't: struct and  ^u: null and
                                ^u: struct
        
        static member
          inline Lift2: f: 'a * ( ^b *  ^e) * _mthd: Internals.Default2 ->  ^f
                          when ( ^c or  ^b or  ^d) :
                                 (static member (<*>) :  ^c *  ^b ->  ^d) and
                                ^c: (static member Return: 'a ->  ^c) and
                               ( ^d or  ^e or  ^f) :
                                 (static member (<*>) :  ^d *  ^e ->  ^f)
        
        static member
          Lift2: f: ('T -> 'U -> 'a) * (ResizeArray<'T> * ResizeArray<'U>) *
                 _mthd: Lift2 -> ResizeArray<'a>
        
        static member
          Lift2: f: ('T -> 'U -> 'a) *
                 (Quotations.Expr<'T> * Quotations.Expr<'U>) * _mthd: Lift2
                   -> Quotations.Expr<'a>
        
        static member
          Lift2: f: ('T -> 'U -> 'a) *
                 (System.Collections.Generic.Dictionary<'Key,'T> *
                  System.Collections.Generic.Dictionary<'Key,'U>) * _mthd: Lift2
                   -> System.Collections.Generic.Dictionary<'Key,'a>
                   when 'Key: equality
        
        static member
          Lift2: f: ('T -> 'U -> 'a) * (Map<'Key,'T> * Map<'Key,'U>) *
                 _mthd: Lift2 -> Map<'Key,'a> when 'Key: comparison
        
        static member
          Lift2: f: ('T -> 'U -> 'a) * (Choice<'T,'Error> * Choice<'U,'Error>) *
                 _mthd: Lift2 -> Choice<'a,'Error>
        
        static member
          Lift2: f: ('T -> 'U -> 'a) * (Result<'T,'Error> * Result<'U,'Error>) *
                 _mthd: Lift2 -> Result<'a,'Error>
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * ('a option * 'b option) * _mthd: Lift2
                   -> 'c option
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * (Async<'a> * Async<'b>) * _mthd: Lift2
                   -> Async<'c>
        
        static member
          Lift2: f: ('T -> 'U -> 'a) *
                 (System.Threading.Tasks.Task<'T> *
                  System.Threading.Tasks.Task<'U>) * _mthd: Lift2
                   -> System.Threading.Tasks.Task<'a>
        
        static member
          inline Lift2: f: ('T -> 'U -> 'a) *
                        (( ^Monoid * 'T) * ( ^Monoid * 'U)) * _mthd: Lift2
                          ->  ^Monoid * 'a
                          when (Plus or  ^Monoid) :
                                 (static member ``+`` :
                                     ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member
          Lift2: f: ('T -> 'U -> 'a) * (('R -> 'T) * ('R -> 'U)) * _mthd: Lift2
                   -> ('R -> 'a)
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * ('a[] * 'b[]) * _mthd: Lift2 -> 'c[]
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * ('a list * 'b list) * _mthd: Lift2
                   -> 'c list
        
        static member
          Lift2: f: ('a -> 'b -> 'c) *
                 (System.Collections.Generic.IEnumerator<'a> *
                  System.Collections.Generic.IEnumerator<'b>) * _mthd: Lift2
                   -> System.Collections.Generic.IEnumerator<'c>
        
        static member
          Lift2: f: ('a -> 'b -> 'c) *
                 (Data.NonEmptySeq<'a> * Data.NonEmptySeq<'b>) * _mthd: Lift2
                   -> Data.NonEmptySeq<'c>
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * (seq<'a> * seq<'b>) * _mthd: Lift2
                   -> seq<'c>
        
        static member
          Lift2: f: ('a -> 'b -> 'c) * (System.Lazy<'a> * System.Lazy<'b>) *
                 _mthd: Lift2 -> System.Lazy<'c>
    
    [<Class>]
    type Lift3 =
        inherit Internals.Default1
        
        static member
          inline Invoke: f: ('T -> 'U -> 'V -> 'W) -> x:  ^Applicative<'T>
                         -> y:  ^Applicative<'U> -> z:  ^Applicative<'V>
                           ->  ^Applicative<'W>
                           when (Lift3 or  ^Applicative<'T> or  ^Applicative<'U> or
                                  ^Applicative<'V> or  ^Applicative<'W>) :
                                  (static member Lift3:
                                     ('T -> 'U -> 'V -> 'W) *
                                     ( ^Applicative<'T> *  ^Applicative<'U> *
                                       ^Applicative<'V>) * Lift3
                                       ->  ^Applicative<'W>)
        
        static member
          inline InvokeOnInstance: f: ('T -> 'U -> 'V -> 'W)
                                   -> x:  ^Applicative<'T>
                                   -> y:  ^Applicative<'U>
                                   -> z:  ^Applicative<'V> -> 'a
                                     when ( ^Applicative<'T> or
                                            ^Applicative<'U> or
                                            ^Applicative<'V>) :
                                            (static member Lift3:
                                               ('T -> 'U -> 'V -> 'W) *
                                                ^Applicative<'T> *
                                                ^Applicative<'U> *
                                                ^Applicative<'V> -> 'a)
        
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) *
                        ( ^Applicative<'T> *  ^Applicative<'U> *
                          ^Applicative<'V>) * _mthd: Internals.Default1 -> 'a
                          when ( ^Applicative<'T> or  ^Applicative<'U> or
                                 ^Applicative<'V>) :
                                 (static member Lift3:
                                    ('T -> 'U -> 'V -> 'W) *  ^Applicative<'T> *
                                     ^Applicative<'U> *  ^Applicative<'V> -> 'a)
        
        static member
          inline Lift3: 'a * ('t *  ^u *  ^v) * _mthd: Internals.Default1
                          -> ('b -> 'b)
                          when 't: null and 't: struct and  ^u: null and
                                ^u: struct and  ^v: null and  ^v: struct
        
        static member
          inline Lift3: f: 'a * ( ^b *  ^e *  ^g) * _mthd: Internals.Default3
                          ->  ^h
                          when ( ^c or  ^b or  ^d) :
                                 (static member (<*>) :  ^c *  ^b ->  ^d) and
                                ^c: (static member Return: 'a ->  ^c) and
                               ( ^d or  ^e or  ^f) :
                                 (static member (<*>) :  ^d *  ^e ->  ^f) and
                               ( ^f or  ^g or  ^h) :
                                 (static member (<*>) :  ^f *  ^g ->  ^h)
        
        static member
          Lift3: f: ('U -> 'V -> 'T -> 'a) *
                 (ResizeArray<'T> * ResizeArray<'U> * ResizeArray<'V>) *
                 _mthd: Lift3 -> ResizeArray<'a>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (Quotations.Expr<'T> * Quotations.Expr<'U> *
                  Quotations.Expr<'V>) * _mthd: Lift3 -> Quotations.Expr<'a>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (System.Collections.Generic.Dictionary<'Key,'T> *
                  System.Collections.Generic.Dictionary<'Key,'U> *
                  System.Collections.Generic.Dictionary<'Key,'V>) * _mthd: Lift3
                   -> System.Collections.Generic.Dictionary<'Key,'a>
                   when 'Key: equality
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (Map<'Key,'T> * Map<'Key,'U> * Map<'Key,'V>) * _mthd: Lift3
                   -> Map<'Key,'a> when 'Key: comparison
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (Choice<'T,'Error> * Choice<'U,'Error> * Choice<'V,'Error>) *
                 _mthd: Lift3 -> Choice<'a,'Error>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (Result<'T,'Error> * Result<'U,'Error> * Result<'V,'Error>) *
                 _mthd: Lift3 -> Result<'a,'Error>
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) * ('a option * 'b option * 'c option) *
                 _mthd: Lift3 -> 'd option
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) * (Async<'a> * Async<'b> * Async<'c>) *
                 _mthd: Lift3 -> Async<'d>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (System.Threading.Tasks.Task<'T> *
                  System.Threading.Tasks.Task<'U> *
                  System.Threading.Tasks.Task<'V>) * _mthd: Lift3
                   -> System.Threading.Tasks.Task<'a>
        
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'a) *
                 (('R -> 'T) * ('R -> 'U) * ('R -> 'V)) * _mthd: Lift3
                   -> ('R -> 'a)
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) * ('a[] * 'b[] * 'c[]) * _mthd: Lift3
                   -> 'd[]
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) * ('c list * 'a list * 'b list) *
                 _mthd: Lift3 -> 'd list
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) *
                 (System.Collections.Generic.IEnumerator<'a> *
                  System.Collections.Generic.IEnumerator<'b> *
                  System.Collections.Generic.IEnumerator<'c>) * _mthd: Lift3
                   -> System.Collections.Generic.IEnumerator<'d>
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) *
                 (Data.NonEmptySeq<'c> * Data.NonEmptySeq<'a> *
                  Data.NonEmptySeq<'b>) * _mthd: Lift3 -> Data.NonEmptySeq<'d>
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) * (seq<'c> * seq<'a> * seq<'b>) *
                 _mthd: Lift3 -> seq<'d>
        
        static member
          Lift3: f: ('a -> 'b -> 'c -> 'd) *
                 (System.Lazy<'a> * System.Lazy<'b> * System.Lazy<'c>) *
                 _mthd: Lift3 -> System.Lazy<'d>
    
    [<Class>]
    type IsLeftZero =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Applicative<'T> -> bool
                           when (IsLeftZero or  ^Applicative<'T>) :
                                  (static member IsLeftZero:
                                      ^Applicative<'T> ref * IsLeftZero -> bool)
        
        static member
          inline InvokeOnInstance: x:  ^Applicative<'T> -> bool
                                     when  ^Applicative<'T> :
                                            (static member IsLeftZero:
                                                ^Applicative<'T> -> bool)
        
        static member
          inline IsLeftZero:  ^t ref * Internals.Default1 -> unit
                               when  ^t: null and  ^t: struct
        
        static member
          inline IsLeftZero: t:  ^Applicative<'T> ref *
                             _mthd: Internals.Default1 -> 'a
                               when  ^Applicative<'T> :
                                      (static member IsLeftZero:
                                          ^Applicative<'T> -> 'a)
        
        static member
          inline IsLeftZero: t:  ^Alternative<'T> ref *
                             _mthd: Internals.Default2 -> bool
                               when  ^Alternative<'T> : equality and
                                     ^Alternative<'T> :
                                      (static member get_Empty:
                                         ->  ^Alternative<'T>)
        
        static member
          inline IsLeftZero: 'T ref * _mthd: Internals.Default3 -> bool
                               when 'T: not struct
        
        static member
          inline IsLeftZero: 'T ref * _mthd: Internals.Default4 -> bool
                               when 'T: struct
        
        static member
          IsLeftZero: t: Choice<'a,'b> ref * _mthd: IsLeftZero -> bool
        
        static member
          IsLeftZero: t: Result<'a,'b> ref * _mthd: IsLeftZero -> bool
        
        static member IsLeftZero: t: 'a option ref * _mthd: IsLeftZero -> bool
        
        static member IsLeftZero: t: 'a array ref * _mthd: IsLeftZero -> bool
        
        static member IsLeftZero: t: 'a list ref * _mthd: IsLeftZero -> bool
        
        static member
          IsLeftZero: Data.NonEmptySeq<'a> ref * _mthd: IsLeftZero -> bool
        
        static member IsLeftZero: t: seq<'a> ref * _mthd: IsLeftZero -> bool

