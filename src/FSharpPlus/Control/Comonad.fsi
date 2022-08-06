namespace FSharpPlus.Control
    
    [<Class>]
    type Extract =
        
        static member Extract: f: System.Threading.Tasks.Task<'T> -> 'T
        
        static member
          inline Extract: f: ( ^Monoid -> 'T) -> 'T
                            when (Zero or  ^Monoid) :
                                   (static member Zero:
                                       ^Monoid * Zero ->  ^Monoid)
        
        static member Extract: f: Internals.Id<'T> -> Internals.Id<'T>
        
        static member Extract: ('W * 'T) -> 'T
        
        static member Extract: x: System.Lazy<'T> -> 'T
        
        static member Extract: x: Async<'T> -> 'T
        
        static member
          inline Invoke: x:  ^Comonad<'T> -> 'T
                           when (Extract or  ^Comonad<'T>) :
                                  (static member Extract:  ^Comonad<'T> -> 'T)
    
    [<Class>]
    type Extend =
        
        static member (=>>) : s: seq<'T> * g: (seq<'T> -> 'U) -> seq<'U>
        
        static member (=>>) : s: 'T[] * g: ('T[] -> 'U) -> 'U[]
        
        static member (=>>) : s: 'T list * g: ('T list -> 'U) -> 'U list
        
        static member
          (=>>) : g: System.Threading.Tasks.Task<'T> *
                  f: (System.Threading.Tasks.Task<'T> -> 'U)
                    -> System.Threading.Tasks.Task<'U>
        
        static member
          inline (=>>) : g: ( ^Monoid -> 'T) * f: (( ^Monoid -> 'T) -> 'U)
                           -> ( ^Monoid -> 'U)
                           when (Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member
          (=>>) : g: Internals.Id<'T> * f: (Internals.Id<'T> -> 'U) -> 'U
        
        static member (=>>) : ('W * 'T) * f: ('W * 'T -> 'U) -> 'W * 'U
        
        static member
          (=>>) : g: System.Lazy<'T> * f: (System.Lazy<'T> -> 'U)
                    -> System.Lazy<'U>
        
        static member (=>>) : g: Async<'T> * f: (Async<'T> -> 'U) -> Async<'U>
        
        static member
          inline Invoke: g: ( ^Comonad<'T> -> 'U) -> s:  ^Comonad<'T>
                           ->  ^Comonad<'U>
                           when (Extend or  ^Comonad<'T> or  ^Comonad<'U>) :
                                  (static member (=>>) :
                                      ^Comonad<'T> * ( ^Comonad<'T> -> 'U)
                                       ->  ^Comonad<'U>)
    
    [<Class>]
    type Duplicate =
        inherit Internals.Default1
        
        static member Duplicate: s: 'T array * _mthd: Duplicate -> 'T[][]
        
        static member Duplicate: s: 'T list * _mthd: Duplicate -> 'T list list
        
        static member
          inline Duplicate: f: ( ^Monoid -> 'T) * _mthd: Duplicate
                              -> ( ^Monoid ->  ^Monoid -> 'T)
                              when (Plus or  ^Monoid) :
                                     (static member ``+`` :
                                         ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member Duplicate: ('W * 'T) * _mthd: Duplicate -> 'W * ('W * 'T)
        
        static member
          Duplicate: s: Internals.Id<'T> * _mthd: Duplicate
                       -> Internals.Id<Internals.Id<'T>>
        
        static member
          Duplicate: s: System.Lazy<'T> * _mthd: Duplicate
                       -> System.Lazy<System.Lazy<'T>>
        
        static member
          Duplicate: s: Async<'T> * _mthd: Duplicate -> Async<Async<'T>>
        
        static member
          inline Duplicate: x:  ^Comonad<'T> * _mthd: Internals.Default1
                              ->  ^Comonad<'Comonad<'T>>
                              when (Extend or  ^Comonad<'T> or
                                     ^Comonad<'Comonad<'T>>) :
                                     (static member (=>>) :
                                         ^Comonad<'T> *
                                        ( ^Comonad<'T> ->  ^Comonad<'T>)
                                          ->  ^Comonad<'Comonad<'T>>)
        
        static member
          inline Invoke: x:  ^Comonad<'T> ->  ^Comonad<'Comonad<'T>>
                           when (Duplicate or  ^Comonad<'T> or
                                  ^Comonad<'Comonad<'T>>) :
                                  (static member Duplicate:
                                      ^Comonad<'T> * Duplicate
                                       ->  ^Comonad<'Comonad<'T>>)

