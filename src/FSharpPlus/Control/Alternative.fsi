namespace FSharpPlus.Control
    
    [<Class>]
    type Empty =
        inherit Internals.Default1
        
        static member Empty: _output: 'T[] * _mthd: Empty -> 'T[]
        
        static member Empty: _output: 'T list * _mthd: Empty -> 'T list
        
        static member Empty: _output: 'T option * _mthd: Empty -> 'T option
        
        static member
          inline Empty: _output:  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                          when  ^t: null and  ^t: struct
        
        static member
          inline Empty: _output:  ^Alternative<'T> * _mthd: Internals.Default1
                          ->  ^Alternative<'T>
                          when  ^Alternative<'T> :
                                 (static member get_Empty: ->  ^Alternative<'T>)
        
        static member
          Empty: _output: seq<'T> * _mthd: Internals.Default2 -> seq<'T>
        
        static member
          inline Invoke: unit ->  ^Alternative<'T>
                           when (Empty or  ^Alternative<'T>) :
                                  (static member Empty:
                                      ^Alternative<'T> * Empty
                                       ->  ^Alternative<'T>)
        
        static member
          inline InvokeOnInstance: unit ->  ^Alternative<'T>
                                     when  ^Alternative<'T> :
                                            (static member get_Empty:
                                               ->  ^Alternative<'T>)
    
    [<Class>]
    type Append =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Alt<'T> -> y:  ^Alt<'T> ->  ^Alt<'T>
                           when (Append or  ^Alt<'T>) :
                                  (static member ``<|>`` :
                                      ^Alt<'T> *  ^Alt<'T> * Append ->  ^Alt<'T>)
        
        static member ``<|>`` : x: 'T[] * y: 'T[] * _mthd: Append -> 'T[]
        
        static member
          ``<|>`` : x: 'T list * y: 'T list * _mthd: Append -> 'T list
        
        static member
          ``<|>`` : x: 'T option * y: 'T option * _mthd: Append -> 'T option
        
        static member
          inline ``<|>`` : x: Internals.Either<'a,'b> *
                           y: Internals.Either<'a,'b> * _mthd: Append
                             -> Internals.Either<'a,'b>
        
        static member
          inline ``<|>`` : x: Choice<'a, ^b> * y: Choice<'a, ^b> * _mthd: Append
                             -> Choice<'a, ^b>
                             when (Plus or  ^b) :
                                    (static member ``+`` :
                                        ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``<|>`` : x: Result<'a, ^b> * y: Result<'a, ^b> * _mthd: Append
                             -> Result<'a, ^b>
                             when (Plus or  ^b) :
                                    (static member ``+`` :
                                        ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``<|>`` :  ^t * 'a * _mthd: Internals.Default1 -> unit
                             when  ^t: null and  ^t: struct
        
        static member
          inline ``<|>`` : x:  ^Alt<'T> * y:  ^Alt<'T> *
                           _mthd: Internals.Default1 ->  ^Alt<'T>
                             when  ^Alt<'T> :
                                    (static member (<|>) :
                                        ^Alt<'T> *  ^Alt<'T> ->  ^Alt<'T>)
        
        static member
          ``<|>`` : x: Data.NonEmptySeq<'T> * y: Data.NonEmptySeq<'T> *
                    _mthd: Internals.Default2 -> Data.NonEmptySeq<'T>
        
        static member
          ``<|>`` : x: seq<'T> * y: seq<'T> * _mthd: Internals.Default2
                      -> seq<'T>
    
    [<Class>]
    type IsAltLeftZero =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Applicative<'T> -> bool
                           when (IsAltLeftZero or  ^Applicative<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Applicative<'T> ref * IsAltLeftZero
                                       -> bool)
        
        static member
          inline InvokeOnInstance: x:  ^Applicative<'T> -> bool
                                     when  ^Applicative<'T> :
                                            (static member IsAltLeftZero:
                                                ^Applicative<'T> -> bool)
        
        static member
          IsAltLeftZero: t: Choice<'a,'b> ref * _mthd: IsAltLeftZero -> bool
        
        static member
          IsAltLeftZero: t: Result<'a,'b> ref * _mthd: IsAltLeftZero -> bool
        
        static member
          IsAltLeftZero: t: 'a option ref * _mthd: IsAltLeftZero -> bool
        
        static member
          inline IsAltLeftZero:  ^t ref * _mthd: Internals.Default1 -> unit
                                  when  ^t: null and  ^t: struct
        
        static member
          inline IsAltLeftZero: t:  ^Applicative<'T> ref *
                                _mthd: Internals.Default1 -> 'a
                                  when  ^Applicative<'T> :
                                         (static member IsAltLeftZero:
                                             ^Applicative<'T> -> 'a)
        
        static member
          inline IsAltLeftZero: 'T ref * _mthd: Internals.Default2 -> bool
                                  when 'T: not struct
        
        static member
          inline IsAltLeftZero: 'T ref * _mthd: Internals.Default3 -> bool
                                  when 'T: struct
    
    [<Class>]
    type Choice =
        inherit Internals.Default1
        
        static member
          inline Choice: x:  ^Alternative<'T> array ref * _mthd: Choice
                           ->  ^Alternative<'T>
                           when (Empty or  ^Alternative<'T>) :
                                  (static member Empty:
                                      ^Alternative<'T> * Empty
                                       ->  ^Alternative<'T>) and
                                (IsAltLeftZero or  ^Alternative<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Alternative<'T> ref * IsAltLeftZero
                                       -> bool) and
                                (Append or  ^Alternative<'T>) :
                                  (static member ``<|>`` :
                                      ^Alternative<'T> *  ^Alternative<'T> *
                                     Append ->  ^Alternative<'T>)
        
        static member
          inline Choice: x:  ^Alternative<'T> list ref * _mthd: Choice
                           ->  ^Alternative<'T>
                           when (Empty or  ^Alternative<'T>) :
                                  (static member Empty:
                                      ^Alternative<'T> * Empty
                                       ->  ^Alternative<'T>) and
                                (IsAltLeftZero or  ^Alternative<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Alternative<'T> ref * IsAltLeftZero
                                       -> bool) and
                                (Append or  ^Alternative<'T>) :
                                  (static member ``<|>`` :
                                      ^Alternative<'T> *  ^Alternative<'T> *
                                     Append ->  ^Alternative<'T>)
        
        static member
          inline Choice: x: Data.NonEmptySeq< ^Alternative<'T>> ref *
                         _mthd: Choice ->  ^Alternative<'T>
                           when (IsAltLeftZero or  ^Alternative<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Alternative<'T> ref * IsAltLeftZero
                                       -> bool) and
                                (Append or  ^Alternative<'T>) :
                                  (static member ``<|>`` :
                                      ^Alternative<'T> *  ^Alternative<'T> *
                                     Append ->  ^Alternative<'T>)
        
        static member
          inline Choice: x: seq< ^Alternative<'T>> ref * _mthd: Choice
                           ->  ^Alternative<'T>
                           when (Empty or  ^Alternative<'T>) :
                                  (static member Empty:
                                      ^Alternative<'T> * Empty
                                       ->  ^Alternative<'T>) and
                                (IsAltLeftZero or  ^Alternative<'T>) :
                                  (static member IsAltLeftZero:
                                      ^Alternative<'T> ref * IsAltLeftZero
                                       -> bool) and
                                (Append or  ^Alternative<'T>) :
                                  (static member ``<|>`` :
                                      ^Alternative<'T> *  ^Alternative<'T> *
                                     Append ->  ^Alternative<'T>)
        
        static member
          inline Choice:  ^t ref * _mthd: Internals.Default1 -> unit
                           when  ^t: null and  ^t: struct
        
        static member
          inline Choice: x:  ^Foldable<'Alternative<'T>> ref *
                         _mthd: Internals.Default1 -> 'Alternative<'T>
                           when  ^Foldable<'Alternative<'T>> :
                                  (static member Choice:
                                      ^Foldable<'Alternative<'T>>
                                       -> 'Alternative<'T>)
        
        static member
          inline Choice: x:  ^Reducible<'Alt<'T>> ref *
                         _mthd: Internals.Default2 ->  ^a
                           when  ^Reducible<'Alt<'T>> :
                                  (static member Reduce:
                                      ^Reducible<'Alt<'T>> *
                                     ('Alt<'T>> -> 'Alt<'T>> -> 'Alt<'T>>)
                                       -> 'Alt<'T>>) and
                                (ToSeq or  ^Reducible<'Alt<'T>>) :
                                  (static member ToSeq:
                                      ^Reducible<'Alt<'T>> * ToSeq -> seq< ^a>) and
                                (Append or  ^a) :
                                  (static member ``<|>`` :
                                      ^a *  ^a * Append ->  ^a) and
                                (IsAltLeftZero or  ^a) :
                                  (static member IsAltLeftZero:
                                      ^a ref * IsAltLeftZero -> bool)
        
        static member
          inline Choice: x:  ^Foldable<'Alternative<'T>> ref *
                         _mthd: Internals.Default4 ->  ^a
                           when (ToSeq or  ^Foldable<'Alternative<'T>>) :
                                  (static member ToSeq:
                                      ^Foldable<'Alternative<'T>> * ToSeq
                                       -> seq< ^a>) and
                                (Append or  ^a) :
                                  (static member ``<|>`` :
                                      ^a *  ^a * Append ->  ^a) and
                                (IsAltLeftZero or  ^a) :
                                  (static member IsAltLeftZero:
                                      ^a ref * IsAltLeftZero -> bool) and
                                (Empty or  ^a) :
                                  (static member Empty:  ^a * Empty ->  ^a)
        
        static member
          inline Invoke: x:  ^Foldable<'Alternative<'T>> -> 'Alternative<'T>>
                           when (Choice or  ^Foldable<'Alternative<'T>>) :
                                  (static member Choice:
                                      ^Foldable<'Alternative<'T>> ref * Choice
                                       -> 'Alternative<'T>>)

