namespace FSharpPlus.Control
    
    [<Class>]
    type Bind =
        
        static member
          (>>=) : source: Data.NonEmptySeq<'T> * f: ('T -> Data.NonEmptySeq<'U>)
                    -> Data.NonEmptySeq<'U>
        
        static member
          (>>=) : source: ResizeArray<'T> * f: ('T -> ResizeArray<'U>)
                    -> ResizeArray<'U>
        
        static member
          (>>=) : source: System.Collections.Generic.Dictionary<'Key,'T> *
                  f: ('T -> System.Collections.Generic.Dictionary<'Key,'U>)
                    -> System.Collections.Generic.Dictionary<'Key,'U>
                    when 'Key: equality
        
        static member
          (>>=) : source: Map<'Key,'T> * f: ('T -> Map<'Key,'U>) -> Map<'Key,'U>
                    when 'Key: comparison
        
        static member
          (>>=) : source: Choice<'T,'E> * k: ('T -> Choice<'U,'E>)
                    -> Choice<'U,'E>
        
        static member
          (>>=) : source: Result<'T,'E> * k: ('T -> Result<'U,'E>)
                    -> Result<'U,'E>
        
        static member
          (>>=) : source: Async<'T> * f: ('T -> Async<'U>) -> Async<'U>
        
        static member
          inline (>>=) : ( ^Monoid * 'T) * k: ('T ->  ^Monoid * 'U)
                           ->  ^Monoid * 'U
                           when (Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member
          (>>=) : source: ('R -> 'T) * k: ('T -> 'R -> 'U) -> ('R -> 'U)
        
        static member (>>=) : source: 'T[] * f: ('T -> 'U[]) -> 'U[]
        
        static member (>>=) : source: 'T list * f: ('T -> 'U list) -> 'U list
        
        static member
          (>>=) : source: 'T option * f: ('T -> 'U option) -> 'U option
        
        static member
          (>>=) : source: System.Nullable<'T> * f: ('T -> System.Nullable<'U>)
                    -> System.Nullable<'U>
                    when 'T: (new: unit -> 'T) and 'T: struct and
                         'T :> System.ValueType and 'U: (new: unit -> 'U) and
                         'U: struct and 'U :> System.ValueType
        
        static member
          (>>=) : source: System.Threading.Tasks.Task<'T> *
                  f: ('T -> System.Threading.Tasks.Task<'U>)
                    -> System.Threading.Tasks.Task<'U>
        
        static member (>>=) : source: seq<'T> * f: ('T -> seq<'U>) -> seq<'U>
        
        static member
          (>>=) : source: System.Lazy<'T> * f: ('T -> System.Lazy<'U>)
                    -> System.Lazy<'U>
        
        static member
          inline Invoke: source:  ^Monad<'T> -> binder: ('T ->  ^Monad<'U>)
                           ->  ^Monad<'U>
                           when (Bind or  ^Monad<'T> or  ^Monad<'U>) :
                                  (static member (>>=) :
                                      ^Monad<'T> * ('T ->  ^Monad<'U>)
                                       ->  ^Monad<'U>)
        
        static member
          inline InvokeOnInstance: source:  ^Monad<'T>
                                   -> binder: ('T ->  ^Monad<'U>) ->  ^Monad<'U>
                                     when ( ^Monad<'T> or  ^Monad<'U>) :
                                            (static member (>>=) :
                                                ^Monad<'T> * ('T ->  ^Monad<'U>)
                                                 ->  ^Monad<'U>)
    
    [<Class>]
    type Join =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Monad<Monad<'T>> ->  ^Monad<'T>
                           when (Join or  ^Monad<Monad<'T>> or  ^Monad<'T>) :
                                  (static member Join:
                                      ^Monad<Monad<'T>> *  ^Monad<'T> * Join
                                       ->  ^Monad<'T>)
        
        static member
          Join: x: Data.NonEmptySeq<Data.NonEmptySeq<'T>> *
                _output: Data.NonEmptySeq<'T> * _mthd: Join
                  -> Data.NonEmptySeq<'T>
        
        static member
          Join: x: ResizeArray<ResizeArray<'T>> * _output: ResizeArray<'T> *
                _mthd: Join -> ResizeArray<'T>
        
        static member
          Join: x: System.Collections.Generic.Dictionary<'Key,
                                                         System.Collections.Generic.Dictionary<'Key,
                                                                                               'Value>> *
                _output: System.Collections.Generic.Dictionary<'Key,'Value> *
                _mthd: Join
                  -> System.Collections.Generic.Dictionary<'Key,'Value>
                  when 'Key: equality
        
        static member
          Join: x: Map<'Key,Map<'Key,'Value>> * _output: Map<'Key,'Value> *
                _mthd: Join -> Map<'Key,'Value> when 'Key: comparison
        
        static member
          Join: x: Choice<Choice<'T,'E>,'E> * _output: Choice<'T,'E> *
                _mthd: Join -> Choice<'T,'E>
        
        static member
          Join: x: Result<Result<'T,'E>,'E> * _output: Result<'T,'E> *
                _mthd: Join -> Result<'T,'E>
        
        static member
          Join: x: Async<Async<'T>> * _output: Async<'T> * _mthd: Join
                  -> Async<'T>
        
        static member
          Join: g: ('R -> 'R -> 'T) * _output: ('R -> 'T) * _mthd: Join
                  -> ('R -> 'T)
        
        static member Join: x: 'T[][] * _output: 'T[] * _mthd: Join -> 'T[]
        
        static member
          Join: x: 'T list list * _output: 'T list * _mthd: Join -> 'T list
        
        static member
          Join: x: 'T option option * _output: 'T option * _mthd: Join
                  -> 'T option
        
        static member
          Join: x: System.Threading.Tasks.Task<System.Threading.Tasks.Task<'T>> *
                _output: System.Threading.Tasks.Task<'T> * _mthd: Join
                  -> System.Threading.Tasks.Task<'T>
        
        static member
          Join: x: Internals.Id<#Internals.Id<'T>> * _output: Internals.Id<'T> *
                _mthd: Join -> Internals.Id<'T>
        
        static member
          Join: x: seq<seq<'T>> * _output: seq<'T> * _mthd: Join -> seq<'T>
        
        static member
          Join: x: System.Lazy<System.Lazy<'T>> * _output: System.Lazy<'T> *
                _mthd: Join -> System.Lazy<'T>
        
        static member
          inline Join: x:  ^Monad<'Monad<'T>> * _output:  ^Monad<'T> *
                       _mthd: Internals.Default1 ->  ^Monad<'T>
                         when ( ^Monad<'Monad<'T>> or  ^Monad<'T>) :
                                (static member Join:
                                    ^Monad<'Monad<'T>> ->  ^Monad<'T>)
        
        static member
          inline Join: x:  ^Monad<'Monad<'T>> * _output:  ^Monad<'T> *
                       _mthd: Internals.Default2 ->  ^Monad<'T>
                         when ( ^Monad<'Monad<'T>> or  ^Monad<'T>) :
                                (static member (>>=) :
                                    ^Monad<'Monad<'T>> *
                                   ( ^Monad<'T> ->  ^Monad<'T>) ->  ^Monad<'T>)
        
        static member
          inline Join: m1:  ^Monoid * ( ^Monoid * 'T) * _output: ( ^Monoid * 'T) *
                       _mthd: Join ->  ^Monoid * 'T
                         when (Plus or  ^Monoid) :
                                (static member ``+`` :
                                    ^Monoid *  ^Monoid * Plus ->  ^Monoid)
    
    [<Class>]
    type Return =
        inherit Internals.Default1
        
        static member
          inline Invoke: x: 'T ->  ^Applicative<'T>
                           when (Return or  ^Applicative<'T>) :
                                  (static member Return:
                                      ^Applicative<'T> * Return
                                       -> ('T ->  ^Applicative<'T>))
        
        static member
          inline InvokeOnInstance: x: 'T ->  ^Applicative<'T>
                                     when  ^Applicative<'T> :
                                            (static member Return:
                                               'T ->  ^Applicative<'T>)
        
        static member
          Return: Internals.Set2<'a> * Return -> ('a -> Internals.Set2<'a>)
                    when 'a: comparison
        
        static member
          Return: Set<'a> * Return -> ('a -> Set<'a>) when 'a: comparison
        
        static member
          Return: System.Text.StringBuilder * Return
                    -> (char -> System.Text.StringBuilder)
        
        static member Return: string * Return -> (char -> string)
        
        static member
          Return: ResizeArray<'a> * Return -> ('a -> ResizeArray<'a>)
        
        static member
          Return: Quotations.Expr<'a> * Return -> ('a -> Quotations.Expr<'a>)
        
        static member Return: Choice<'a,'e> * Return -> ('a -> Choice<'a,'e>)
        
        static member Return: Result<'a,'e> * Return -> ('a -> Result<'a,'e>)
        
        static member Return: Async<'a> * Return -> ('a -> Async<'a>)
        
        static member
          inline Return: ( ^m * 'a) * Return -> ('a ->  ^m * 'a)
                           when (Zero or  ^m) :
                                  (static member Zero:  ^m * Zero ->  ^m)
        
        static member Return: ('r -> 'a) * Return -> ('a -> 'r -> 'a)
        
        static member Return: 'a[] * Return -> ('a -> 'a[])
        
        static member Return: 'a list * Return -> ('a -> 'a list)
        
        static member Return: 'a option * Return -> ('a -> 'a option)
        
        static member
          Return: System.Threading.Tasks.Task<'T> * Return
                    -> ('T -> System.Threading.Tasks.Task<'T>)
        
        static member
          Return: System.Lazy<'a> * Return -> ('a -> System.Lazy<'a>)
        
        static member
          inline Return:  ^R * Internals.Default1 -> ('T ->  ^R)
                           when  ^R: (static member Return: 'T ->  ^R)
        
        static member
          Return: System.Collections.Generic.IEnumerator<'a> *
                  Internals.Default2
                    -> ('a -> System.Collections.Generic.IEnumerator<'a>)
        
        static member
          Return: Data.NonEmptySeq<'a> * Internals.Default2
                    -> ('a -> Data.NonEmptySeq<'a>)
        
        static member Return: seq<'a> * Internals.Default2 -> ('a -> seq<'a>)
    
    [<Class>]
    type Delay =
        inherit Internals.Default1
        
        static member
          Delay: _mthd: Delay * x: (unit -> System.Lazy<'T>) * 'a
                   -> System.Lazy<'T>
        
        static member
          Delay: _mthd: Delay * x: (unit -> System.Threading.Tasks.Task<'T>) *
                 'a -> System.Threading.Tasks.Task<'T>
        
        static member
          Delay: _mthd: Delay * x: (unit -> Async<'T>) * 'a -> Async<'T>
        
        static member
          Delay: _mthd: Internals.Default2 * x: (unit -> 'R -> 'a) * 'b
                   -> ('R -> 'a)
        
        static member
          Delay: _mthd: Internals.Default2 * x: (unit -> Data.NonEmptySeq<'T>) *
                 'a -> Data.NonEmptySeq<'T>
        
        static member
          Delay: _mthd: Internals.Default2 * x: (unit -> seq<'T>) * 'a
                   -> seq<'T>
        
        static member
          inline Delay: _mthd: Internals.Default1 * (unit ->  ^t) * 'a -> unit
                          when  ^t: null and  ^t: struct
        
        static member
          inline Delay: _mthd: Internals.Default1 * x: (unit ->  ^I) * Delay
                          ->  ^I
                          when  ^I: (static member Delay: (unit ->  ^I) ->  ^I)
        
        static member
          inline Delay: _mthd: Internals.Default3 * x: (unit ->  ^Monad<'T>) *
                        Internals.Default1 ->  ^Monad<'T>
                          when (Bind or  ^a or  ^Monad<'T>) :
                                 (static member (>>=) :
                                     ^a * (unit ->  ^Monad<'T>) ->  ^Monad<'T>) and
                               (Return or  ^a) :
                                 (static member Return:
                                     ^a * Return -> (unit ->  ^a))
        
        static member
          inline Invoke: source: (unit ->  ^R) ->  ^R
                           when (Delay or  ^R) :
                                  (static member Delay:
                                     Delay * (unit ->  ^R) * Delay ->  ^R)
    
    module TryBlock =
        
        type True = | True
        
        type False = | False
        
        type While = | While
        
        [<Literal>]
        val MessageTryWith: string
          =
          "Method TryWith not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryWith."
        
        [<Literal>]
        val CodeTryWith: int = 10708
        
        [<Literal>]
        val MessageTryFinally: string
          =
          "Method TryFinally not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryFinally."
        
        [<Literal>]
        val CodeTryFinally: int = 10709
        
        [<Literal>]
        val MessageWhile: string
          =
          "This monad doesn't seem to be lazy or at least it doesn't have a try-with method implemented, so using a while loop can lead to runtime errors. Make sure this type is lazy, otherwise use a strict monad by adding .strict"
        
        [<Literal>]
        val CodeWhile: int = 10710
    
    [<Class>]
    type TryWith =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Monad<'T> -> f: (exn ->  ^Monad<'T>)
                           ->  ^Monad<'T>
                           when (TryWith or  ^Monad<'T>) :
                                  (static member TryWith:
                                     (unit ->  ^Monad<'T>) * ('a ->  ^Monad<'T>) *
                                     TryWith * TryBlock.False ->  ^Monad<'T>) and
                                'a :> exn
        
        static member
          inline InvokeForStrict: source: (unit ->  ^Monad<'T>)
                                  -> f: (exn ->  ^Monad<'T>) ->  ^Monad<'T>
                                    when (TryWith or  ^Monad<'T>) :
                                           (static member TryWith:
                                              (unit ->  ^Monad<'T>) *
                                              ('a ->  ^Monad<'T>) * TryWith *
                                              TryBlock.True ->  ^Monad<'T>) and
                                         'a :> exn
        
        static member
          inline InvokeForWhile: source:  ^Monad<'T> -> f: (exn ->  ^Monad<'T>)
                                   ->  ^Monad<'T>
                                   when (TryWith or  ^Monad<'T>) :
                                          (static member TryWith:
                                             (unit ->  ^Monad<'T>) *
                                             ('a ->  ^Monad<'T>) * TryWith *
                                             TryBlock.While ->  ^Monad<'T>) and
                                        'a :> exn
        
        static member
          TryWith: computation: (unit -> System.Lazy<'a>) *
                   catchHandler: (exn -> System.Lazy<'a>) * TryWith * 'b
                     -> System.Lazy<'a>
        
        static member
          TryWith: computation: (unit -> System.Threading.Tasks.Task<'a>) *
                   catchHandler: (exn -> System.Threading.Tasks.Task<'a>) *
                   TryWith * True: TryBlock.True
                     -> System.Threading.Tasks.Task<'a>
        
        static member
          TryWith: computation: (unit -> Async<'a>) *
                   catchHandler: (exn -> Async<'a>) * TryWith * 'b -> Async<'a>
        
        static member
          TryWith: computation: (unit -> 'R -> 'a) *
                   catchHandler: (exn -> 'R -> 'a) * Internals.Default2 * 'b
                     -> ('R -> 'a)
        
        static member
          TryWith: computation: (unit -> Data.NonEmptySeq<'a>) *
                   catchHandler: (exn -> Data.NonEmptySeq<'a>) *
                   Internals.Default2 * 'b -> Data.NonEmptySeq<'a>
        
        static member
          TryWith: computation: (unit -> seq<'a>) *
                   catchHandler: (exn -> seq<'a>) * Internals.Default2 * 'b
                     -> seq<'a>
        
        static member
          inline TryWith: (unit ->  ^t) * (exn ->  ^t) * Internals.Default1 * 'a
                            -> unit when  ^t: null and  ^t: struct
        
        static member
          inline TryWith: computation: (unit ->  ^Monad<'T>) *
                          catchHandler: (exn ->  ^Monad<'T>) *
                          Internals.Default1 * 'b ->  ^Monad<'T>
                            when  ^Monad<'T> :
                                   (static member TryWith:
                                       ^Monad<'T> * ('a ->  ^Monad<'T>)
                                        ->  ^Monad<'T>) and 'a :> exn
        
        static member
          TryWith: computation: (unit -> 'Monad<'T>) *
                   catchHandler: (exn -> 'Monad<'T>) * Internals.Default3 *
                   _defaults: TryBlock.True -> 'Monad<'T>
        
        [<CompilerMessage
          ("Method TryWith not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryWith.",
           10708)>]
        static member
          TryWith: (unit -> 'Monad<'T>) * (exn -> 'Monad<'T>) *
                   Internals.Default3 * _defaults: TryBlock.False -> 'a
        
        [<CompilerMessage
          ("This monad doesn't seem to be lazy or at least it doesn't have a try-with method implemented, so using a while loop can lead to runtime errors. Make sure this type is lazy, otherwise use a strict monad by adding .strict",
           10710)>]
        static member
          TryWith: (unit -> 'Monad<'T>) * (exn -> 'Monad<'T>) *
                   Internals.Default3 * _defaults: TryBlock.While -> 'a
    
    [<Class>]
    type TryFinally =
        inherit Internals.Default1
        
        static member
          inline Invoke: source:  ^Monad<'T> -> f: (unit -> unit) ->  ^Monad<'T>
                           when (TryFinally or  ^Monad<'T>) :
                                  (static member TryFinally:
                                     ((unit ->  ^Monad<'T>) * (unit -> unit)) *
                                     TryFinally * TryFinally * TryBlock.False
                                       ->  ^Monad<'T>)
        
        static member
          inline InvokeForStrict: source: (unit ->  ^Monad<'T>)
                                  -> f: (unit -> unit) ->  ^Monad<'T>
                                    when (TryFinally or  ^Monad<'T>) :
                                           (static member TryFinally:
                                              ((unit ->  ^Monad<'T>) *
                                               (unit -> unit)) * TryFinally *
                                              TryFinally * TryBlock.True
                                                ->  ^Monad<'T>)
        
        static member
          inline InvokeOnInstance: source:  ^Monad<'T> -> f: (unit -> unit)
                                     ->  ^Monad<'T>
                                     when  ^Monad<'T> :
                                            (static member TryFinally:
                                                ^Monad<'T> * (unit -> unit)
                                                 ->  ^Monad<'T>)
        
        static member
          inline TryFinally: ((unit ->  ^t) * (unit -> unit)) *
                             Internals.Default1 * 'a * 'b -> unit
                               when  ^t: null and  ^t: struct
        
        static member
          inline TryFinally: ((unit ->  ^Monad<'T>) * (unit -> unit)) *
                             Internals.Default1 * TryFinally * 'a ->  ^Monad<'T>
                               when  ^Monad<'T> :
                                      (static member TryFinally:
                                          ^Monad<'T> * (unit -> unit)
                                           ->  ^Monad<'T>)
        
        static member
          TryFinally: ((unit -> 'Monad<'T>) * (unit -> unit)) *
                      Internals.Default3 * Internals.Default1 *
                      _defaults: TryBlock.True -> 'Monad<'T>
                        when 'Monad<'T> : not struct
        
        static member
          TryFinally: ((unit -> 'Monad<'T>) * (unit -> unit)) *
                      Internals.Default3 * Internals.Default2 *
                      _defaults: TryBlock.True -> 'Monad<'T>
                        when 'Monad<'T> : struct
        
        [<CompilerMessage
          ("Method TryFinally not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryFinally.",
           10709)>]
        static member
          TryFinally: ((unit -> 'Monad<'T>) * (unit -> unit)) *
                      Internals.Default3 * Internals.Default1 *
                      _defaults: TryBlock.False -> 'a
                        when 'Monad<'T> : not struct
        
        [<CompilerMessage
          ("Method TryFinally not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryFinally.",
           10709)>]
        static member
          TryFinally: ((unit -> 'Monad<'T>) * (unit -> unit)) *
                      Internals.Default3 * Internals.Default2 *
                      _defaults: TryBlock.False -> 'a when 'Monad<'T> : struct
        
        static member
          TryFinally: ((unit -> System.Lazy<'a>) * (unit -> unit)) * TryFinally *
                      'b * 'c -> System.Lazy<'a>
        
        static member
          TryFinally: ((unit -> System.Threading.Tasks.Task<'a>) *
                       (unit -> unit)) * TryFinally * 'b * True: TryBlock.True
                        -> System.Threading.Tasks.Task<'a>
        
        static member
          TryFinally: ((unit -> Async<'a>) * (unit -> unit)) * TryFinally * 'b *
                      'c -> Async<'a>
        
        static member
          TryFinally: ((unit -> Internals.Id<'a>) * (unit -> unit)) * TryFinally *
                      'b * 'c -> Internals.Id<'a>
        
        static member
          TryFinally: ((unit -> 'R -> 'a) * (unit -> unit)) * Internals.Default2 *
                      'b * _defaults: TryBlock.True -> ('R -> 'a)
        
        [<CompilerMessage
          ("Method TryFinally not implemented. If the computation type is not lazy use a strict monad computation expression by adding .strict, otherwise it should have a static member TryFinally.",
           10709)>]
        static member
          TryFinally: ((unit -> 'R -> 'a) * (unit -> unit)) * Internals.Default2 *
                      'b * _defaults: TryBlock.False -> 'c
        
        static member
          TryFinally: ((unit -> Data.NonEmptySeq<'a>) * (unit -> unit)) *
                      Internals.Default2 * 'b * 'c -> Data.NonEmptySeq<'a>
        
        static member
          TryFinally: ((unit -> seq<'a>) * (unit -> unit)) * Internals.Default2 *
                      'b * 'c -> seq<'a>
    
    [<Class>]
    type Using =
        inherit Internals.Default1
        
        static member
          inline Invoke: source: 'T -> f: ('T ->  ^Monad<'U>) ->  ^Monad<'U>
                           when 'T :> System.IDisposable and
                                (Using or  ^Monad<'U>) :
                                  (static member Using:
                                     'T * ('T ->  ^Monad<'U>) * Using
                                       ->  ^Monad<'U>)
        
        static member
          inline InvokeOnInstance: resource: 'T -> body: ('T ->  ^Monad<'U>)
                                     ->  ^Monad<'U>
                                     when 'T :> System.IDisposable and
                                           ^Monad<'U> :
                                            (static member Using:
                                               'T * ('T ->  ^Monad<'U>)
                                                 ->  ^Monad<'U>)
        
        static member
          inline Using: 'a * ('a0 ->  ^t) * Using -> unit
                          when  ^t: null and  ^t: struct
        
        static member
          inline Using: resource: 'T * body: ('T ->  ^Monad<'U>) * Using
                          ->  ^Monad<'U>
                          when 'T :> System.IDisposable and
                                ^Monad<'U> :
                                 (static member Using:
                                    'T * ('T ->  ^Monad<'U>) ->  ^Monad<'U>)
        
        static member
          inline Using: resource: 'T * body: ('T ->  ^Monad<'U>) *
                        Internals.Default1 ->  ^Monad<'U>
                          when 'T :> System.IDisposable and
                                ^Monad<'U> :
                                 (static member TryFinally:
                                     ^Monad<'U> * (unit -> unit) ->  ^Monad<'U>)
        
        static member
          inline Using: resource: 'T * body: ('T -> 'Monad<'U>) *
                        Internals.Default2 -> 'Monad<'U>
                          when 'T :> System.IDisposable and
                               'Monad<'U> : not struct
        
        static member
          inline Using: resource: 'T * body: ('T -> 'Monad<'U>) *
                        Internals.Default3 -> 'Monad<'U>
                          when 'T :> System.IDisposable and 'Monad<'U> : struct
        
        static member
          Using: resource: 'T * body: ('T -> System.Lazy<'U>) * Using
                   -> System.Lazy<'U> when 'T :> System.IDisposable
        
        static member
          Using: resource: 'T * body: ('T -> System.Threading.Tasks.Task<'U>) *
                 Using -> System.Threading.Tasks.Task<'U>
                   when 'T :> System.IDisposable
        
        static member
          Using: resource: 'T * body: ('T -> Async<'U>) * Using -> Async<'U>
                   when 'T :> System.IDisposable
        
        static member
          Using: resource: 'T * body: ('T -> 'R -> 'U) * Using -> ('R -> 'U)
                   when 'T :> System.IDisposable
        
        static member
          Using: resource: 'T * body: ('T -> Data.NonEmptySeq<'U>) * Using
                   -> Data.NonEmptySeq<'U> when 'T :> System.IDisposable
        
        static member
          Using: resource: 'T * body: ('T -> seq<'U>) * Using -> seq<'U>
                   when 'T :> System.IDisposable

