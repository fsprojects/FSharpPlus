namespace FSharpPlus
    
    /// Constructs to express generic computations
    module GenericBuilders =
        
        type Ii = | Ii
        
        type Ji = | Ji
        
        type J = | J
        
        type Idiomatic =
            | Idiomatic
            
            static member
              inline ($) : Idiomatic: Idiomatic * J: J -> ( ^a ->  ^c -> 'd)
                             when (Control.Join or  ^a or  ^b) :
                                    (static member Join:
                                        ^a *  ^b * Control.Join ->  ^b) and
                                  (Idiomatic or  ^c) :
                                    (static member ($) :
                                       Idiomatic *  ^c -> ( ^b -> 'd))
            
            static member
              inline ($) : Idiomatic: Idiomatic * Ji: Ji -> ( ^a ->  ^b)
                             when (Control.Join or  ^a or  ^b) :
                                    (static member Join:
                                        ^a *  ^b * Control.Join ->  ^b)
            
            static member ($) : Idiomatic: Idiomatic * Ii: Ii -> ('a -> 'a)
            
            static member
              inline ($) : Idiomatic: Idiomatic * si:  ^a -> ( ^b ->  ^d -> 'e)
                             when (Control.Apply or  ^b or  ^a or  ^c) :
                                    (static member ``<*>`` :
                                        ^b *  ^a *  ^c * Control.Apply ->  ^c) and
                                  (Idiomatic or  ^d) :
                                    (static member ($) :
                                       Idiomatic *  ^d -> ( ^c -> 'e))
        
        val inline idiomatic:
          a: 'a -> b:  ^b -> 'c
            when (Idiomatic or  ^b) :
                   (static member ($) : Idiomatic *  ^b -> ('a -> 'c))
        
        val inline iI:
          x: 'a -> ( ^b -> 'd)
            when (Idiomatic or  ^b) :
                   (static member ($) : Idiomatic *  ^b -> ( ^c -> 'd)) and
                 (Control.Return or  ^c) :
                   (static member Return:  ^c * Control.Return -> ('a ->  ^c))
        
        type Builder<'monad<'t>> =
            
            new: unit -> Builder<'monad<'t>>
            
            member
              inline Bind: p:  ^Monad<'T> * rest: ('T ->  ^Monad<'U>)
                             ->  ^Monad<'U>
                             when (Control.Bind or  ^Monad<'T> or  ^Monad<'U>) :
                                    (static member (>>=) :
                                        ^Monad<'T> * ('T ->  ^Monad<'U>)
                                         ->  ^Monad<'U>)
            
            [<CustomOperation ("chunkBy")>]
            member
              inline ChunkBy: x:  ^a * f: ('T -> 'key) ->  ^b
                                when (Control.ChunkBy or  ^a or  ^b) :
                                       (static member ChunkBy:
                                           ^a * ('T -> 'key) *  ^b *
                                          Control.ChunkBy ->  ^b) and
                                     'key: equality
            
            [<CustomOperation ("groupBy")>]
            member
              inline GroupBy: x:  ^a * f: ('T -> 'key) ->  ^b
                                when (Control.GroupBy or  ^a or  ^b) :
                                       (static member GroupBy:
                                           ^a * ('T -> 'key) *  ^b *
                                          Control.GroupBy ->  ^b) and
                                     'key: equality
            
            member
              inline MergeSources: t1:  ^Monad<'T> * t2:  ^Monad<'U>
                                     ->  ^Monad<'T * 'U>
                                     when (Control.Lift2 or  ^Monad<'T> or
                                            ^Monad<'U> or  ^Monad<'T * 'U>) :
                                            (static member Lift2:
                                               ('a -> 'b -> 'a * 'b) *
                                               ( ^Monad<'T> *  ^Monad<'U>) *
                                               Control.Lift2 ->  ^Monad<'T * 'U>)
            
            member
              inline MergeSources3: t1:  ^Monad<'T> * t2:  ^Monad<'U> *
                                    t3:  ^Monad<'V> ->  ^Monad<'T * 'U * 'V>
                                      when (Control.Lift3 or  ^Monad<'T> or
                                             ^Monad<'U> or  ^Monad<'V> or
                                             ^Monad<'T * 'U * 'V>) :
                                             (static member Lift3:
                                                ('a -> 'b -> 'c -> 'a * 'b * 'c) *
                                                ( ^Monad<'T> *  ^Monad<'U> *
                                                  ^Monad<'V>) * Control.Lift3
                                                  ->  ^Monad<'T * 'U * 'V>)
            
            [<CustomOperation ("orderBy")>]
            member
              inline OrderBy: x:  ^a * f: ('T -> 'key) ->  ^a
                                when (Control.SortBy or  ^a) :
                                       (static member SortBy:
                                           ^a * ('T -> 'key) * Control.SortBy
                                            ->  ^a) and 'key: comparison
            
            member
              inline Return: x: 'T ->  ^Monad<'T>
                               when (Control.Return or  ^Monad<'T>) :
                                      (static member Return:
                                          ^Monad<'T> * Control.Return
                                           -> ('T ->  ^Monad<'T>))
            
            member ReturnFrom: expr: 'monad<'t> -> 'monad<'t>
            
            [<CustomOperation ("select")>]
            member
              inline Select: x:  ^a * f: ('c -> 'd) ->  ^b
                               when (Control.Map or  ^a or  ^b) :
                                      (static member Map:
                                         ( ^a * ('c -> 'd)) * Control.Map ->  ^b)
            
            [<CustomOperation ("top")>]
            member
              inline Top: source:  ^a * n: int ->  ^a
                            when (Control.Limit or  ^a) :
                                   (static member Limit:
                                       ^a * int * Control.Limit ->  ^a)
            
            [<CustomOperation ("where")>]
            member
              inline Where: x:  ^a * p: ('b -> bool) ->  ^a
                              when (Control.Bind or  ^a) :
                                     (static member (>>=) :
                                         ^a * ('b ->  ^a) ->  ^a) and
                                   (Control.Return or  ^a) :
                                     (static member Return:
                                         ^a * Control.Return -> ('b ->  ^a)) and
                                   (Control.Empty or  ^a) :
                                     (static member Empty:
                                         ^a * Control.Empty ->  ^a)
            
            member
              inline Yield: x: 'T ->  ^Monad<'T>
                              when (Control.Return or  ^Monad<'T>) :
                                     (static member Return:
                                         ^Monad<'T> * Control.Return
                                          -> ('T ->  ^Monad<'T>))
        
        type StrictBuilder<'monad<'t>> =
            inherit Builder<'monad<'t>>
            
            new: unit -> StrictBuilder<'monad<'t>>
            
            member Delay: expr: (unit -> 'Monad<'T>) -> (unit -> 'Monad<'T>)
            
            member Run: f: (unit -> 'monad<'t>) -> 'monad<'t>
            
            member
              inline TryFinally: expr: (unit ->  ^Monad<'T>) *
                                 compensation: (unit -> unit) ->  ^Monad<'T>
                                   when (Control.TryFinally or  ^Monad<'T>) :
                                          (static member TryFinally:
                                             ((unit ->  ^Monad<'T>) *
                                              (unit -> unit)) *
                                             Control.TryFinally *
                                             Control.TryFinally *
                                             Control.TryBlock.True
                                               ->  ^Monad<'T>)
            
            member
              inline TryWith: expr: (unit ->  ^Monad<'T>) *
                              handler: (exn ->  ^Monad<'T>) ->  ^Monad<'T>
                                when (Control.TryWith or  ^Monad<'T>) :
                                       (static member TryWith:
                                          (unit ->  ^Monad<'T>) *
                                          ('a ->  ^Monad<'T>) * Control.TryWith *
                                          Control.TryBlock.True ->  ^Monad<'T>) and
                                     'a :> exn
            
            member
              inline Using: disposable: 'a * body: ('a ->  ^b) ->  ^b
                              when 'a :> System.IDisposable and
                                   (Control.Using or  ^b) :
                                     (static member Using:
                                        'a * ('a ->  ^b) * Control.Using ->  ^b)
        
        type DelayedBuilder<'monad<'t>> =
            inherit Builder<'monad<'t>>
            
            new: unit -> DelayedBuilder<'monad<'t>>
            
            member
              inline Delay: expr: (unit ->  ^Monad<'T>) ->  ^Monad<'T>
                              when (Control.Delay or  ^Monad<'T>) :
                                     (static member Delay:
                                        Control.Delay * (unit ->  ^Monad<'T>) *
                                        Control.Delay ->  ^Monad<'T>)
            
            member Run: f: 'monad<'t> -> 'monad<'t>
            
            member
              inline TryFinally: expr:  ^Monad<'T> *
                                 compensation: (unit -> unit) ->  ^Monad<'T>
                                   when (Control.TryFinally or  ^Monad<'T>) :
                                          (static member TryFinally:
                                             ((unit ->  ^Monad<'T>) *
                                              (unit -> unit)) *
                                             Control.TryFinally *
                                             Control.TryFinally *
                                             Control.TryBlock.False
                                               ->  ^Monad<'T>)
            
            member
              inline TryWith: expr:  ^Monad<'T> * handler: (exn ->  ^Monad<'T>)
                                ->  ^Monad<'T>
                                when (Control.TryWith or  ^Monad<'T>) :
                                       (static member TryWith:
                                          (unit ->  ^Monad<'T>) *
                                          ('a ->  ^Monad<'T>) * Control.TryWith *
                                          Control.TryBlock.False ->  ^Monad<'T>) and
                                     'a :> exn
            
            member
              inline Using: disposable: 'a * body: ('a ->  ^Monad<'T>)
                              ->  ^Monad<'T>
                              when 'a :> System.IDisposable and
                                   (Control.Using or  ^Monad<'T>) :
                                     (static member Using:
                                        'a * ('a ->  ^Monad<'T>) * Control.Using
                                          ->  ^Monad<'T>)
        
        type MonadPlusStrictBuilder<'monad<'t>> =
            inherit StrictBuilder<'monad<'t>>
            
            new: unit -> MonadPlusStrictBuilder<'monad<'t>>
            
            member
              inline Combine: a:  ^MonadPlus<'T> * b: (unit ->  ^MonadPlus<'T>)
                                ->  ^MonadPlus<'T>
                                when (Control.Append or  ^MonadPlus<'T>) :
                                       (static member ``<|>`` :
                                           ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                          Control.Append ->  ^MonadPlus<'T>)
            
            member
              inline For: p: #seq<'T> * rest: ('T ->  ^MonadPlus<'U>)
                            ->  ^MonadPlus<'U>
                            when (Control.Using or  ^MonadPlus<'U>) :
                                   (static member Using:
                                      System.IDisposable *
                                      (System.IDisposable ->  ^MonadPlus<'U>) *
                                      Control.Using ->  ^MonadPlus<'U>) and
                                 (Control.Empty or  ^MonadPlus<'U>) :
                                   (static member Empty:
                                       ^MonadPlus<'U> * Control.Empty
                                        ->  ^MonadPlus<'U>) and
                                 (Control.Append or  ^MonadPlus<'U>) :
                                   (static member ``<|>`` :
                                       ^MonadPlus<'U> *  ^MonadPlus<'U> *
                                      Control.Append ->  ^MonadPlus<'U>)
            
            member
              inline While: guard: (unit -> bool) *
                            body: (unit ->  ^MonadPlus<'T>) ->  ^MonadPlus<'T>
                              when (Control.Append or  ^MonadPlus<'T>) :
                                     (static member ``<|>`` :
                                         ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                        Control.Append ->  ^MonadPlus<'T>) and
                                   (Control.Empty or  ^MonadPlus<'T>) :
                                     (static member Empty:
                                         ^MonadPlus<'T> * Control.Empty
                                          ->  ^MonadPlus<'T>)
            
            member YieldFrom: expr: 'monad<'t> -> 'monad<'t>
            
            member
              inline Zero: unit ->  ^MonadPlus<'T>
                             when (Control.Empty or  ^MonadPlus<'T>) :
                                    (static member Empty:
                                        ^MonadPlus<'T> * Control.Empty
                                         ->  ^MonadPlus<'T>)
        
        type MonadFxStrictBuilder<'monad<'t>> =
            inherit StrictBuilder<'monad<'t>>
            
            new: unit -> MonadFxStrictBuilder<'monad<'t>>
            
            member
              inline Combine: a:  ^Monad<unit> * b: (unit ->  ^Monad<'T>)
                                ->  ^Monad<'T>
                                when (Control.Bind or  ^Monad<unit> or
                                       ^Monad<'T>) :
                                       (static member (>>=) :
                                           ^Monad<unit> * (unit ->  ^Monad<'T>)
                                            ->  ^Monad<'T>)
            
            member
              inline For: p: #seq<'T> * rest: ('T ->  ^Monad<unit>)
                            ->  ^Monad<unit>
                            when (Control.Using or  ^Monad<unit>) :
                                   (static member Using:
                                      System.IDisposable *
                                      (System.IDisposable ->  ^Monad<unit>) *
                                      Control.Using ->  ^Monad<unit>) and
                                 (Control.Return or  ^Monad<unit>) :
                                   (static member Return:
                                       ^Monad<unit> * Control.Return
                                        -> (unit ->  ^Monad<unit>)) and
                                 (Control.Bind or  ^Monad<unit>) :
                                   (static member (>>=) :
                                       ^Monad<unit> * (unit ->  ^Monad<unit>)
                                        ->  ^Monad<unit>)
            
            member
              inline While: guard: (unit -> bool) *
                            body: (unit ->  ^Monad<unit>) ->  ^Monad<unit>
                              when (Control.Bind or  ^Monad<unit>) :
                                     (static member (>>=) :
                                         ^Monad<unit> * (unit ->  ^Monad<unit>)
                                          ->  ^Monad<unit>) and
                                   (Control.Return or  ^Monad<unit>) :
                                     (static member Return:
                                         ^Monad<unit> * Control.Return
                                          -> (unit ->  ^Monad<unit>))
            
            member
              inline Zero: unit ->  ^Monad<unit>
                             when (Control.Return or  ^Monad<unit>) :
                                    (static member Return:
                                        ^Monad<unit> * Control.Return
                                         -> (unit ->  ^Monad<unit>))
        
        type MonadPlusBuilder<'monad<'t>> =
            inherit DelayedBuilder<'monad<'t>>
            
            new: unit -> MonadPlusBuilder<'monad<'t>>
            
            member
              inline Combine: a:  ^MonadPlus<'T> * b:  ^MonadPlus<'T>
                                ->  ^MonadPlus<'T>
                                when (Control.Append or  ^MonadPlus<'T>) :
                                       (static member ``<|>`` :
                                           ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                          Control.Append ->  ^MonadPlus<'T>)
            
            member
              inline For: p: #seq<'T> * rest: ('T ->  ^MonadPlus<'U>)
                            ->  ^MonadPlus<'U>
                            when (Control.Delay or  ^MonadPlus<'U>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^MonadPlus<'U>) *
                                      Control.Delay ->  ^MonadPlus<'U>) and
                                 (Control.Empty or  ^MonadPlus<'U>) :
                                   (static member Empty:
                                       ^MonadPlus<'U> * Control.Empty
                                        ->  ^MonadPlus<'U>) and
                                 (Control.Using or  ^MonadPlus<'U>) :
                                   (static member Using:
                                      System.IDisposable *
                                      (System.IDisposable ->  ^MonadPlus<'U>) *
                                      Control.Using ->  ^MonadPlus<'U>) and
                                 (Control.Append or  ^MonadPlus<'U>) :
                                   (static member ``<|>`` :
                                       ^MonadPlus<'U> *  ^MonadPlus<'U> *
                                      Control.Append ->  ^MonadPlus<'U>)
            
            member
              inline While: guard: (unit -> bool) * body:  ^MonadPlus<'T>
                              ->  ^MonadPlus<'T>
                              when (Control.TryWith or  ^MonadPlus<'T>) :
                                     (static member TryWith:
                                        (unit ->  ^MonadPlus<'T>) *
                                        ('a ->  ^MonadPlus<'T>) *
                                        Control.TryWith * Control.TryBlock.While
                                          ->  ^MonadPlus<'T>) and
                                   (Control.Delay or  ^MonadPlus<'T>) :
                                     (static member Delay:
                                        Control.Delay *
                                        (unit ->  ^MonadPlus<'T>) *
                                        Control.Delay ->  ^MonadPlus<'T>) and
                                   (Control.Append or  ^MonadPlus<'T>) :
                                     (static member ``<|>`` :
                                         ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                        Control.Append ->  ^MonadPlus<'T>) and
                                   (Control.Empty or  ^MonadPlus<'T>) :
                                     (static member Empty:
                                         ^MonadPlus<'T> * Control.Empty
                                          ->  ^MonadPlus<'T>) and 'a :> exn
            
            member
              inline WhileImpl: guard: (unit -> bool) * body:  ^MonadPlus<'T>
                                  ->  ^MonadPlus<'T>
                                  when (Control.Delay or  ^MonadPlus<'T>) :
                                         (static member Delay:
                                            Control.Delay *
                                            (unit ->  ^MonadPlus<'T>) *
                                            Control.Delay ->  ^MonadPlus<'T>) and
                                       (Control.Append or  ^MonadPlus<'T>) :
                                         (static member ``<|>`` :
                                             ^MonadPlus<'T> *  ^MonadPlus<'T> *
                                            Control.Append ->  ^MonadPlus<'T>) and
                                       (Control.Empty or  ^MonadPlus<'T>) :
                                         (static member Empty:
                                             ^MonadPlus<'T> * Control.Empty
                                              ->  ^MonadPlus<'T>)
            
            member YieldFrom: expr: 'monad<'t> -> 'monad<'t>
            
            member
              inline Zero: unit ->  ^MonadPlus<'T>
                             when (Control.Empty or  ^MonadPlus<'T>) :
                                    (static member Empty:
                                        ^MonadPlus<'T> * Control.Empty
                                         ->  ^MonadPlus<'T>)
            
            member strict: MonadPlusStrictBuilder<'monad<'t>>
        
        type MonadFxBuilder<'monad<'t>> =
            inherit DelayedBuilder<'monad<'t>>
            
            new: unit -> MonadFxBuilder<'monad<'t>>
            
            member
              inline Combine: a:  ^Monad<unit> * b:  ^Monad<'T> ->  ^Monad<'T>
                                when (Control.Bind or  ^Monad<unit> or
                                       ^Monad<'T>) :
                                       (static member (>>=) :
                                           ^Monad<unit> * (unit ->  ^Monad<'T>)
                                            ->  ^Monad<'T>)
            
            member
              inline For: p: #seq<'T> * rest: ('T ->  ^Monad<unit>)
                            ->  ^Monad<unit>
                            when (Control.Delay or  ^Monad<unit>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^Monad<unit>) *
                                      Control.Delay ->  ^Monad<unit>) and
                                 (Control.Return or  ^Monad<unit>) :
                                   (static member Return:
                                       ^Monad<unit> * Control.Return
                                        -> (unit ->  ^Monad<unit>)) and
                                 (Control.Using or  ^Monad<unit>) :
                                   (static member Using:
                                      System.IDisposable *
                                      (System.IDisposable ->  ^Monad<unit>) *
                                      Control.Using ->  ^Monad<unit>) and
                                 (Control.Bind or  ^Monad<unit>) :
                                   (static member (>>=) :
                                       ^Monad<unit> * (unit ->  ^Monad<unit>)
                                        ->  ^Monad<unit>)
            
            member
              inline While: guard: (unit -> bool) * body:  ^Monad<unit>
                              ->  ^Monad<unit>
                              when (Control.TryWith or  ^Monad<unit>) :
                                     (static member TryWith:
                                        (unit ->  ^Monad<unit>) *
                                        ('a ->  ^Monad<unit>) * Control.TryWith *
                                        Control.TryBlock.While ->  ^Monad<unit>) and
                                   (Control.Bind or  ^Monad<unit>) :
                                     (static member (>>=) :
                                         ^Monad<unit> * (unit ->  ^Monad<unit>)
                                          ->  ^Monad<unit>) and
                                   (Control.Return or  ^Monad<unit>) :
                                     (static member Return:
                                         ^Monad<unit> * Control.Return
                                          -> (unit ->  ^Monad<unit>)) and
                                   'a :> exn
            
            member
              inline WhileImpl: guard: (unit -> bool) * body:  ^Monad<unit>
                                  ->  ^Monad<unit>
                                  when (Control.Bind or  ^Monad<unit>) :
                                         (static member (>>=) :
                                             ^Monad<unit> *
                                            (unit ->  ^Monad<unit>)
                                              ->  ^Monad<unit>) and
                                       (Control.Return or  ^Monad<unit>) :
                                         (static member Return:
                                             ^Monad<unit> * Control.Return
                                              -> (unit ->  ^Monad<unit>))
            
            member
              inline Zero: unit ->  ^Monad<unit>
                             when (Control.Return or  ^Monad<unit>) :
                                    (static member Return:
                                        ^Monad<unit> * Control.Return
                                         -> (unit ->  ^Monad<unit>))
            
            /// Makes it a (lazy) monadic computation expression with side-effects
            member fx: MonadFxBuilder<'monad<'t>>
            
            /// Makes it a strict monadic computation expression with side-effects
            member fx': MonadFxStrictBuilder<'monad<'t>>
            
            /// Makes it a (lazy) monadplus computation expression.
            member plus: MonadPlusBuilder<'monad<'t>>
            
            /// Makes it a strict monadplus computation expression.
            member plus': MonadPlusStrictBuilder<'monad<'t>>
            
            member strict: MonadFxStrictBuilder<'monad<'t>>
        
        /// Creates a (lazy) monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
        val monad<'monad<'t>> : MonadFxBuilder<'monad<'t>>
        
        /// Creates a strict monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
        val monad'<'monad<'t>> : MonadFxStrictBuilder<'monad<'t>>

