namespace FSharpPlus.Control
    
    [<Class>]
    type BifoldMap =
        inherit Internals.Default1
        
        static member
          inline BifoldMap: 't * 'a * 'b * _impl: Internals.Default1
                              -> ('c -> 'c) when 't: null and 't: struct
        
        static member
          inline BifoldMap: x:  ^Bifoldable<'T1,'T2> * f: ('a -> 'b) *
                            g: ('b0 -> 'b) * _impl: Internals.Default1 -> 'b
                              when  ^Bifoldable<'T1,'T2> :
                                     (static member BifoldMap:
                                         ^Bifoldable<'T1,'T2> * ('a -> 'b) *
                                        ('b0 -> 'b) -> 'b)
        
        static member
          inline BifoldMap: ('T1 * 'T2) * f: ('T1 ->  ^U) * g: ('T2 ->  ^U) *
                            _impl: BifoldMap ->  ^U
                              when (Plus or  ^U) :
                                     (static member ``+`` :
                                         ^U *  ^U * Plus ->  ^U)
        
        static member
          BifoldMap: x: Choice<'T2,'T1> * f: ('T1 -> 'U) * g: ('T2 -> 'U) *
                     _impl: BifoldMap -> 'U
        
        static member
          BifoldMap: x: Result<'T2,'T1> * f: ('T1 -> 'U) * g: ('T2 -> 'U) *
                     _impl: BifoldMap -> 'U
        
        static member
          inline Invoke: f: ('T1 -> 'U) -> g: ('T2 -> 'U)
                         -> source:  ^Bifoldable<T1,T2> -> 'U
                           when (BifoldMap or  ^Bifoldable<T1,T2>) :
                                  (static member BifoldMap:
                                      ^Bifoldable<T1,T2> * ('T1 -> 'U) *
                                     ('T2 -> 'U) * BifoldMap -> 'U)
        
        static member
          inline InvokeOnInstance: f: ('T1 -> 'U) -> g: ('T2 -> 'U)
                                   -> source:  ^Bifoldable<'T1,'T2> -> 'U
                                     when  ^Bifoldable<'T1,'T2> :
                                            (static member BifoldMap:
                                                ^Bifoldable<'T1,'T2> *
                                               ('T1 -> 'U) * ('T2 -> 'U) -> 'U)
    
    [<Class>]
    type Bifold =
        inherit Internals.Default1
        
        static member
          inline Bifold: 't * 'a * 'b * 'c * _impl: Internals.Default1
                           -> ('d -> 'd) when 't: null and 't: struct
        
        static member
          inline Bifold: x:  ^Bifoldable<'T1,'T2> * f: ('a -> 'b -> 'a) *
                         g: ('a -> 'c -> 'a) * z: 'a * _impl: Internals.Default1
                           -> 'a
                           when  ^Bifoldable<'T1,'T2> :
                                  (static member Bifold:
                                      ^Bifoldable<'T1,'T2> * ('a -> 'b -> 'a) *
                                     ('a -> 'c -> 'a) * 'a -> 'a)
        
        static member
          inline Bifold: ('T1 * 'T2) * f: ('S -> 'T1 -> 'S) *
                         g: ('S -> 'T2 -> 'S) * z: 'S * _impl: Bifold -> 'S
        
        static member
          inline Bifold: x: Choice<'T2,'T1> * f: ('S -> 'T1 -> 'S) *
                         g: ('S -> 'T2 -> 'S) * z: 'S * _impl: Bifold -> 'S
        
        static member
          inline Bifold: x: Result<'T2,'T1> * f: ('S -> 'T1 -> 'S) *
                         g: ('S -> 'T2 -> 'S) * z: 'S * _impl: Bifold -> 'S
        
        static member
          inline Invoke: f: ('S -> 'T1 -> 'S) -> g: ('S -> 'T2 -> 'S) -> z: 'S
                         -> source:  ^Bifoldable<'T1,'T2> -> 'S
                           when (Bifold or  ^Bifoldable<'T1,'T2>) :
                                  (static member Bifold:
                                      ^Bifoldable<'T1,'T2> * ('S -> 'T1 -> 'S) *
                                     ('S -> 'T2 -> 'S) * 'S * Bifold -> 'S)
        
        static member
          inline InvokeOnInstance: f: ('S -> 'T1 -> 'S) -> g: ('S -> 'T2 -> 'S)
                                   -> z: 'S -> source:  ^Bifoldable<'T1,'T2>
                                     -> 'S
                                     when  ^Bifoldable<'T1,'T2> :
                                            (static member Bifold:
                                                ^Bifoldable<'T1,'T2> *
                                               ('S -> 'T1 -> 'S) *
                                               ('S -> 'T2 -> 'S) * 'S -> 'S)
    
    [<Class>]
    type BifoldBack =
        inherit Internals.Default1
        
        static member
          inline BifoldBack: 't * 'a * 'b * 'c * _impl: Internals.Default1
                               -> ('d -> 'd) when 't: null and 't: struct
        
        static member
          inline BifoldBack: x:  ^Bifoldable<'T1,'T2> * f: ('a -> 'b -> 'b) *
                             g: ('c -> 'b -> 'b) * z: 'b *
                             _impl: Internals.Default1 -> 'b
                               when  ^Bifoldable<'T1,'T2> :
                                      (static member BifoldBack:
                                          ^Bifoldable<'T1,'T2> *
                                         ('a -> 'b -> 'b) * ('c -> 'b -> 'b) *
                                         'b -> 'b)
        
        static member
          inline BifoldBack: ('T1 * 'T2) * f: ('T1 -> 'S -> 'S) *
                             g: ('T2 -> 'S -> 'S) * z: 'S * _impl: BifoldBack
                               -> 'S
        
        static member
          inline BifoldBack: x: Choice<'T2,'T1> * f: ('T1 -> 'S -> 'S) *
                             g: ('T2 -> 'S -> 'S) * z: 'S * _impl: BifoldBack
                               -> 'S
        
        static member
          inline BifoldBack: x: Result<'T2,'T1> * f: ('T1 -> 'S -> 'S) *
                             g: ('T2 -> 'S -> 'S) * z: 'S * _impl: BifoldBack
                               -> 'S
        
        static member
          inline Invoke: f: ('T1 -> 'S -> 'S) -> g: ('T2 -> 'S -> 'S) -> z: 'S
                         -> source:  ^Bifoldable<'T1,'T2> -> 'S
                           when (BifoldBack or  ^Bifoldable<'T1,'T2>) :
                                  (static member BifoldBack:
                                      ^Bifoldable<'T1,'T2> * ('T1 -> 'S -> 'S) *
                                     ('T2 -> 'S -> 'S) * 'S * BifoldBack -> 'S)
        
        static member
          inline InvokeOnInstance: f: ('T1 -> 'S -> 'S) -> g: ('T2 -> 'S -> 'S)
                                   -> z: 'S -> source:  ^Bifoldable<'T1,'T2>
                                     -> 'S
                                     when  ^Bifoldable<'T1,'T2> :
                                            (static member BifoldBack:
                                                ^Bifoldable<'T1,'T2> *
                                               ('T1 -> 'S -> 'S) *
                                               ('T2 -> 'S -> 'S) * 'S -> 'S)
    
    [<Class>]
    type Bisum =
        inherit Internals.Default1
        
        static member
          inline Bisum: 'Bifoldable<'T,'T> * Internals.Default1 -> unit
                          when 'Bifoldable<'T,'T> : null and
                               'Bifoldable<'T,'T> : struct
        
        static member
          inline Bisum: x:  ^Bifoldable<'T,'T> * _impl: Internals.Default1 -> 'T
                          when  ^Bifoldable<'T,'T> :
                                 (static member Bisum:  ^Bifoldable<'T,'T> -> 'T)
        
        static member
          inline Bisum: x:  ^Bifoldable<'T,'T> * _impl: Internals.Default2 -> 'T
                          when  ^Bifoldable<'T,'T> :
                                 (static member BifoldMap:
                                     ^Bifoldable<'T,'T> * ('T -> 'T) *
                                    ('T -> 'T) -> 'T)
        
        static member
          inline Bisum: ( ^a *  ^a) * _impl: Bisum ->  ^a
                          when (Plus or  ^a) :
                                 (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member Bisum: x: Choice<'a,'a> * _impl: Bisum -> 'a
        
        static member Bisum: x: Result<'a,'a> * _impl: Bisum -> 'a
        
        static member
          inline Invoke: source:  ^Bifoldable<'T1,'T2> -> 'U
                           when (Bisum or  ^Bifoldable<'T1,'T2>) :
                                  (static member Bisum:
                                      ^Bifoldable<'T1,'T2> * Bisum -> 'U)
        
        static member
          inline InvokeOnInstance: source:  ^Bifoldable<'T1,'T2> -> 'U
                                     when  ^Bifoldable<'T1,'T2> :
                                            (static member Bisum:
                                                ^Bifoldable<'T1,'T2> -> 'U)

