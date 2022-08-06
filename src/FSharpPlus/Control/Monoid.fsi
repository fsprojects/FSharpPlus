namespace FSharpPlus.Control
    
    [<System.Runtime.CompilerServices.Extension; Sealed; Class>]
    type Plus =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Plus -> y:  ^Plus ->  ^Plus
                           when (Plus or  ^Plus) :
                                  (static member ``+`` :
                                      ^Plus *  ^Plus * Plus ->  ^Plus)
        
        static member
          inline ``+`` : x: Data.NonEmptySeq<'a> * y: Data.NonEmptySeq<'a> *
                         _mthd: Internals.Default3 -> Data.NonEmptySeq<'a>
        
        static member
          inline ``+`` : x: System.Collections.Generic.IReadOnlyDictionary<'K,
                                                                            ^V> *
                         y: System.Collections.Generic.IReadOnlyDictionary<'K,
                                                                            ^V> *
                         _mthd: Internals.Default3
                           -> System.Collections.Generic.IReadOnlyDictionary<'K,
                                                                              ^V>
                           when 'K: equality and
                                (Plus or  ^V) :
                                  (static member ``+`` :  ^V *  ^V * Plus ->  ^V)
        
        static member
          inline ``+`` : x: System.Collections.Generic.IDictionary<'K, ^V> *
                         y: System.Collections.Generic.IDictionary<'K, ^V> *
                         _mthd: Internals.Default3
                           -> System.Collections.Generic.IDictionary<'K, ^V>
                           when 'K: equality and
                                (Plus or  ^V) :
                                  (static member ``+`` :  ^V *  ^V * Plus ->  ^V)
        
        static member
          ``+`` : x: System.Collections.Generic.IEnumerator<'a> *
                  y: System.Collections.Generic.IEnumerator<'a> *
                  _mthd: Internals.Default3
                    -> System.Collections.Generic.IEnumerator<'a>
        
        static member
          ``+`` : x: seq<'a> * y: seq<'a> * _mthd: Internals.Default3 -> seq<'a>
        
        static member
          ``+`` : x: System.IObservable<'a> * y: System.IObservable<'a> *
                  _mthd: Internals.Default3 -> System.IObservable<'a>
        
        static member
          ``+`` : x: ResizeArray<'a> * y: ResizeArray<'a> * _mthd: Plus
                    -> ResizeArray<'a>
        
        static member
          inline ``+`` : x: System.Lazy< ^a> * y: System.Lazy< ^a> * _mthd: Plus
                           -> Lazy< ^a>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member
          inline ``+`` : x: Quotations.Expr< ^a> * y: Quotations.Expr< ^a> *
                         _mthd: Plus -> Quotations.Expr< ^a>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member
          inline ``+`` : x: Async< ^S> * y: Async< ^S> * _mthd: Plus
                           -> Async< ^S>
                           when (Plus or  ^S) :
                                  (static member ``+`` :  ^S *  ^S * Plus ->  ^S)
        
        static member
          inline ``+`` : f: ('T ->  ^Monoid) * g: ('T ->  ^Monoid) * _mthd: Plus
                           -> ('T ->  ^Monoid)
                           when (Plus or  ^Monoid) :
                                  (static member ``+`` :
                                      ^Monoid *  ^Monoid * Plus ->  ^Monoid)
        
        static member
          inline ``+`` : x: System.Collections.Generic.Dictionary<'Key, ^Value> *
                         y: System.Collections.Generic.Dictionary<'Key, ^Value> *
                         _mthd: Plus
                           -> System.Collections.Generic.Dictionary<'Key, ^Value>
                           when 'Key: equality and
                                (Plus or  ^Value) :
                                  (static member ``+`` :
                                      ^Value *  ^Value * Plus ->  ^Value)
        
        static member
          inline ``+`` : x: Map<'a, ^b> * y: Map<'a, ^b> * _mthd: Plus
                           -> Map<'a, ^b>
                           when 'a: comparison and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``+`` : x: System.Threading.Tasks.Task< ^a> *
                         y: System.Threading.Tasks.Task< ^a> * _mthd: Plus
                           -> System.Threading.Tasks.Task< ^a>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member
          inline ``+`` : ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f *  ^g) *
                         ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f *  ^g) * _mthd: Plus
                           ->  ^a *  ^b *  ^c *  ^d *  ^e *  ^f *  ^g
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b) and
                                (Plus or  ^c) :
                                  (static member ``+`` :  ^c *  ^c * Plus ->  ^c) and
                                (Plus or  ^d) :
                                  (static member ``+`` :  ^d *  ^d * Plus ->  ^d) and
                                (Plus or  ^e) :
                                  (static member ``+`` :  ^e *  ^e * Plus ->  ^e) and
                                (Plus or  ^f) :
                                  (static member ``+`` :  ^f *  ^f * Plus ->  ^f) and
                                (Plus or  ^g) :
                                  (static member ``+`` :  ^g *  ^g * Plus ->  ^g)
        
        static member
          inline ``+`` : ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f) *
                         ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f) * _mthd: Plus
                           ->  ^a *  ^b *  ^c *  ^d *  ^e *  ^f
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b) and
                                (Plus or  ^c) :
                                  (static member ``+`` :  ^c *  ^c * Plus ->  ^c) and
                                (Plus or  ^d) :
                                  (static member ``+`` :  ^d *  ^d * Plus ->  ^d) and
                                (Plus or  ^e) :
                                  (static member ``+`` :  ^e *  ^e * Plus ->  ^e) and
                                (Plus or  ^f) :
                                  (static member ``+`` :  ^f *  ^f * Plus ->  ^f)
        
        static member
          inline ``+`` : ( ^a *  ^b *  ^c *  ^d *  ^e) *
                         ( ^a *  ^b *  ^c *  ^d *  ^e) * _mthd: Plus
                           ->  ^a *  ^b *  ^c *  ^d *  ^e
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b) and
                                (Plus or  ^c) :
                                  (static member ``+`` :  ^c *  ^c * Plus ->  ^c) and
                                (Plus or  ^d) :
                                  (static member ``+`` :  ^d *  ^d * Plus ->  ^d) and
                                (Plus or  ^e) :
                                  (static member ``+`` :  ^e *  ^e * Plus ->  ^e)
        
        static member
          inline ``+`` : ( ^a *  ^b *  ^c *  ^d) * ( ^a *  ^b *  ^c *  ^d) *
                         _mthd: Plus ->  ^a *  ^b *  ^c *  ^d
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b) and
                                (Plus or  ^c) :
                                  (static member ``+`` :  ^c *  ^c * Plus ->  ^c) and
                                (Plus or  ^d) :
                                  (static member ``+`` :  ^d *  ^d * Plus ->  ^d)
        
        static member
          inline ``+`` : ( ^a *  ^b *  ^c) * ( ^a *  ^b *  ^c) * _mthd: Plus
                           ->  ^a *  ^b *  ^c
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b) and
                                (Plus or  ^c) :
                                  (static member ``+`` :  ^c *  ^c * Plus ->  ^c)
        
        static member
          inline ``+`` : ( ^a *  ^b) * ( ^a *  ^b) * _mthd: Plus ->  ^a *  ^b
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``+`` : x: System.Tuple< ^a> * y: System.Tuple< ^a> *
                         _mthd: Plus -> System.Tuple< ^a>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member
          inline ``+`` : x:  ^t * y:  ^t * _mthd: Plus ->  ^t
                           when  ^t: (member get_Rest:  ^t ->  ^tr) and
                                 ^t: (member get_Item7:  ^t ->  ^t7) and
                                 ^t: (member get_Item6:  ^t ->  ^t6) and
                                 ^t: (member get_Item5:  ^t ->  ^t5) and
                                 ^t: (member get_Item4:  ^t ->  ^t4) and
                                 ^t: (member get_Item3:  ^t ->  ^t3) and
                                 ^t: (member get_Item2:  ^t ->  ^t2) and
                                 ^t: (member get_Item1:  ^t ->  ^t1) and
                                (Plus or  ^tr) :
                                  (static member ``+`` :
                                      ^tr *  ^tr * Plus ->  ^tr) and
                                (Plus or  ^t7) :
                                  (static member ``+`` :
                                      ^t7 *  ^t7 * Plus ->  ^t7) and
                                (Plus or  ^t6) :
                                  (static member ``+`` :
                                      ^t6 *  ^t6 * Plus ->  ^t6) and
                                (Plus or  ^t5) :
                                  (static member ``+`` :
                                      ^t5 *  ^t5 * Plus ->  ^t5) and
                                (Plus or  ^t4) :
                                  (static member ``+`` :
                                      ^t4 *  ^t4 * Plus ->  ^t4) and
                                (Plus or  ^t3) :
                                  (static member ``+`` :
                                      ^t3 *  ^t3 * Plus ->  ^t3) and
                                (Plus or  ^t2) :
                                  (static member ``+`` :
                                      ^t2 *  ^t2 * Plus ->  ^t2) and
                                (Plus or  ^t1) :
                                  (static member ``+`` :
                                      ^t1 *  ^t1 * Plus ->  ^t1)
        
        static member
          inline ``+`` : x: Choice< ^a, ^b> * y: Choice< ^a, ^b> * _mthd: Plus
                           -> Choice< ^a, ^b>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``+`` : x: Result< ^a, ^b> * y: Result< ^a, ^b> * _mthd: Plus
                           -> Result< ^a, ^b>
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                                (Plus or  ^b) :
                                  (static member ``+`` :  ^b *  ^b * Plus ->  ^b)
        
        static member
          inline ``+`` : x:  ^a option * y:  ^a option * _mthd: Plus
                           ->  ^a option
                           when (Plus or  ^a) :
                                  (static member ``+`` :  ^a *  ^a * Plus ->  ^a)
        
        static member ``+`` : x: exn * y: exn * _mthd: Plus -> exn
        
        static member
          ``+`` : x: System.AggregateException * y: System.AggregateException *
                  _mthd: Plus -> System.AggregateException
        
        static member
          ``+`` : Internals.Id0 * Internals.Id0 * _mthd: Plus -> Internals.Id0
        
        static member
          ``+`` : x: System.Text.StringBuilder * y: System.Text.StringBuilder *
                  _mthd: Plus -> System.Text.StringBuilder
        
        static member
          ``+`` : x: Set<'a> * y: Set<'a> * _mthd: Plus -> Set<'a>
                    when 'a: comparison
        
        static member ``+`` : x: bool * y: bool * _mthd: Plus -> bool
        
        static member ``+`` : unit * unit * _mthd: Plus -> unit
        
        static member ``+`` : x: 'a array * y: 'a[] * _mthd: Plus -> 'a[]
        
        static member ``+`` : x: 'a list * y: 'a list * _mthd: Plus -> 'a list
        
        static member
          inline ``+`` :  ^t *  ^t * _mthd: Internals.Default1 -> ('a -> 'a)
                           when  ^t: null and  ^t: struct
        
        static member
          inline ``+`` : x:  ^Plus * y:  ^Plus * _mthd: Internals.Default1
                           ->  ^Plus
                           when  ^Plus:
                                  (static member (+) :  ^Plus *  ^Plus ->  ^Plus)
        
        static member
          inline ``+`` : x:  ^Plus * y:  ^Plus * _mthd: Internals.Default2
                           ->  ^Plus
                           when  ^Plus:
                                  (static member (<|>) :
                                      ^Plus *  ^Plus ->  ^Plus)
    
    [<System.Runtime.CompilerServices.Extension; Sealed; Class>]
    type Sum =
        inherit Internals.Default1
        
        static member
          inline Invoke: x: seq< ^T> ->  ^T
                           when (Sum or seq< ^T> or  ^T) :
                                  (static member Sum:
                                     seq< ^T> *  ^T * Sum ->  ^T)
        
        static member
          inline InvokeOnInstance: x: seq< ^Monoid> ->  ^Monoid
                                     when  ^Monoid:
                                            (static member Sum:
                                               seq< ^Monoid> ->  ^Monoid)
        
        static member
          inline Sum: seq< ^R> *  ^t * Internals.Default1 -> (unit -> 'a -> 'a)
                        when  ^t: null and  ^t: struct
        
        static member
          inline Sum: x: seq< ^R> * _output:  ^R * Internals.Default1 ->  ^R
                        when  ^R: (static member Sum: seq< ^R> ->  ^R)
        
        static member
          inline Sum: x: seq< ^a> * _output:  ^a * Internals.Default2 ->  ^a
                        when (Plus or  ^a) :
                               (static member ``+`` :  ^a *  ^a * Plus ->  ^a) and
                             (Zero or  ^a) :
                               (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Sum: x: seq< ^a *  ^b *  ^c *  ^d> *
                      _output: ( ^a *  ^b *  ^c *  ^d) * _impl: Sum
                        ->  ^a *  ^b *  ^c *  ^d
                        when (Sum or seq< ^a> or  ^a) :
                               (static member Sum: seq< ^a> *  ^a * Sum ->  ^a) and
                             (Sum or seq< ^b> or  ^b) :
                               (static member Sum: seq< ^b> *  ^b * Sum ->  ^b) and
                             (Sum or seq< ^c> or  ^c) :
                               (static member Sum: seq< ^c> *  ^c * Sum ->  ^c) and
                             (Sum or seq< ^d> or  ^d) :
                               (static member Sum: seq< ^d> *  ^d * Sum ->  ^d)
        
        static member
          inline Sum: x: seq< ^a *  ^b *  ^c> * _output: ( ^a *  ^b *  ^c) *
                      _impl: Sum ->  ^a *  ^b *  ^c
                        when (Sum or seq< ^a> or  ^a) :
                               (static member Sum: seq< ^a> *  ^a * Sum ->  ^a) and
                             (Sum or seq< ^b> or  ^b) :
                               (static member Sum: seq< ^b> *  ^b * Sum ->  ^b) and
                             (Sum or seq< ^c> or  ^c) :
                               (static member Sum: seq< ^c> *  ^c * Sum ->  ^c)
        
        static member
          inline Sum: x: seq< ^a *  ^b> * _output: ( ^a *  ^b) * _impl: Sum
                        ->  ^a *  ^b
                        when (Sum or seq< ^a> or  ^a) :
                               (static member Sum: seq< ^a> *  ^a * Sum ->  ^a) and
                             (Sum or seq< ^b> or  ^b) :
                               (static member Sum: seq< ^b> *  ^b * Sum ->  ^b)
        
        static member
          Sum: x: seq<System.Text.StringBuilder> *
               _output: System.Text.StringBuilder * _impl: Sum
                 -> System.Text.StringBuilder
        
        static member
          Sum: x: seq<string> * _output: string * _impl: Sum -> string
        
        static member
          Sum: x: seq<'a array> * _output: 'a array * _impl: Sum -> 'a[]
        
        static member
          Sum: x: seq<'a list> * _output: 'a list * _impl: Sum -> 'a list
        
        static member
          inline Sum: x: seq<ResizeArray<'a>> * _output: ResizeArray<'a> *
                      _impl: Sum -> ResizeArray<'a>
        
        static member
          inline Sum: x: seq<System.Collections.Generic.IDictionary<'a, ^b>> *
                      _output: System.Collections.Generic.IDictionary<'a, ^b> *
                      _impl: Sum
                        -> System.Collections.Generic.IDictionary<'a, ^b>
                        when 'a: equality and
                             (Plus or  ^b) :
                               (static member ``+`` :  ^b *  ^b * Plus ->  ^b)
        
        static member
          inline Sum: x: seq<System.Collections.Generic.Dictionary<'a, ^b>> *
                      _output: System.Collections.Generic.Dictionary<'a, ^b> *
                      _impl: Sum
                        -> System.Collections.Generic.Dictionary<'a, ^b>
                        when 'a: equality and
                             (Plus or  ^b) :
                               (static member ``+`` :  ^b *  ^b * Plus ->  ^b)

