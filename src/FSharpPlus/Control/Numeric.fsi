namespace FSharpPlus.Control
    
    [<Class>]
    type FromBigInt =
        inherit Internals.Default1
        
        static member FromBigInt: decimal * FromBigInt -> (bigint -> decimal)
        
        static member FromBigInt: float32 * FromBigInt -> (bigint -> float32)
        
        static member FromBigInt: uint64 * FromBigInt -> (bigint -> uint64)
        
        static member FromBigInt: uint32 * FromBigInt -> (bigint -> uint32)
        
        static member FromBigInt: uint16 * FromBigInt -> (bigint -> uint16)
        
        static member FromBigInt: byte * FromBigInt -> (bigint -> byte)
        
        static member FromBigInt: int16 * FromBigInt -> (bigint -> int16)
        
        static member FromBigInt: sbyte * FromBigInt -> (bigint -> sbyte)
        
        static member FromBigInt: float * FromBigInt -> (bigint -> float)
        
        static member FromBigInt: bigint * FromBigInt -> (bigint -> bigint)
        
        static member
          FromBigInt: unativeint * FromBigInt -> (bigint -> unativeint)
        
        static member
          FromBigInt: nativeint * FromBigInt -> (bigint -> nativeint)
        
        static member FromBigInt: int64 * FromBigInt -> (bigint -> int64)
        
        static member FromBigInt: int32 * FromBigInt -> (bigint -> int)
        
        static member
          inline FromBigInt: Internals.Default1 * Internals.Default1
                               -> (bigint ->  ^R)
                               when  ^R:
                                      (static member FromBigInt: bigint ->  ^R)
        
        static member
          inline FromBigInt:  ^R * Internals.Default1 -> (bigint ->  ^R)
                               when  ^R:
                                      (static member FromBigInt: bigint ->  ^R)
        
        static member
          inline FromBigInt:  ^R * Internals.Default2 -> (bigint ->  ^R)
                               when ( ^R or bigint) :
                                      (static member op_Implicit: bigint ->  ^R)
        
        static member
          inline FromBigInt:  ^R * Internals.Default3 -> (bigint ->  ^R)
                               when ( ^R or int64) :
                                      (static member op_Implicit: int64 ->  ^R)
        
        static member
          inline FromBigInt:  ^R * Internals.Default4 -> (bigint ->  ^R)
                               when (Explicit or  ^R or bigint) :
                                      (static member Explicit:
                                          ^R * Explicit -> (bigint ->  ^R))
        
        static member
          inline Invoke: x: bigint ->  ^Num
                           when (FromBigInt or  ^Num) :
                                  (static member FromBigInt:
                                      ^Num * FromBigInt -> (bigint ->  ^Num))
    
    [<Class>]
    type FromInt64 =
        inherit Internals.Default1
        
        static member FromInt64: uint64 * FromInt64 -> (int64 -> uint64)
        
        static member FromInt64: uint32 * FromInt64 -> (int64 -> uint32)
        
        static member FromInt64: uint16 * FromInt64 -> (int64 -> uint16)
        
        static member FromInt64: byte * FromInt64 -> (int64 -> byte)
        
        static member FromInt64: int16 * FromInt64 -> (int64 -> int16)
        
        static member FromInt64: sbyte * FromInt64 -> (int64 -> sbyte)
        
        static member FromInt64: decimal * FromInt64 -> (int64 -> decimal)
        
        static member FromInt64: float32 * FromInt64 -> (int64 -> float32)
        
        static member FromInt64: float * FromInt64 -> (int64 -> float)
        
        static member FromInt64: bigint * FromInt64 -> (int64 -> bigint)
        
        static member FromInt64: unativeint * FromInt64 -> (int64 -> unativeint)
        
        static member FromInt64: nativeint * FromInt64 -> (int64 -> nativeint)
        
        static member FromInt64: int64 * FromInt64 -> (int64 -> int64)
        
        static member FromInt64: int32 * FromInt64 -> (int64 -> int32)
        
        static member
          inline FromInt64: Internals.Default1 * Internals.Default1
                              -> (int64 ->  ^R)
                              when  ^R: (static member FromInt64: int64 ->  ^R)
        
        static member
          inline FromInt64:  ^R * Internals.Default1 -> (int64 ->  ^R)
                              when  ^R: (static member FromInt64: int64 ->  ^R)
        
        static member
          inline FromInt64:  ^R * Internals.Default2 -> (int64 ->  ^R)
                              when ( ^R or int64) :
                                     (static member op_Implicit: int64 ->  ^R)
        
        static member
          inline FromInt64:  ^R * Internals.Default3 -> (int64 ->  ^R)
                              when (FromBigInt or  ^R) :
                                     (static member FromBigInt:
                                         ^R * FromBigInt -> (bigint ->  ^R))
        
        static member
          inline FromInt64:  ^R * Internals.Default4 -> (int64 ->  ^R)
                              when (Explicit or  ^R or int64) :
                                     (static member Explicit:
                                         ^R * Explicit -> (int64 ->  ^R))
        
        static member
          inline Invoke: x: int64 ->  ^Num
                           when (FromInt64 or  ^Num) :
                                  (static member FromInt64:
                                      ^Num * FromInt64 -> (int64 ->  ^Num))
    
    [<Class>]
    type FromInt32 =
        inherit Internals.Default1
        
        static member FromInt32: decimal * FromInt32 -> (int32 -> decimal)
        
        static member FromInt32: float32 * FromInt32 -> (int32 -> float32)
        
        static member FromInt32: uint64 * FromInt32 -> (int32 -> uint64)
        
        static member FromInt32: uint32 * FromInt32 -> (int32 -> uint32)
        
        static member FromInt32: uint16 * FromInt32 -> (int32 -> uint16)
        
        static member FromInt32: byte * FromInt32 -> (int32 -> byte)
        
        static member FromInt32: int16 * FromInt32 -> (int32 -> int16)
        
        static member FromInt32: sbyte * FromInt32 -> (int32 -> sbyte)
        
        static member FromInt32: float * FromInt32 -> (int32 -> float)
        
        static member FromInt32: bigint * FromInt32 -> (int32 -> bigint)
        
        static member FromInt32: unativeint * FromInt32 -> (int32 -> unativeint)
        
        static member FromInt32: nativeint * FromInt32 -> (int32 -> nativeint)
        
        static member FromInt32: int64 * FromInt32 -> (int32 -> int64)
        
        static member FromInt32: int32 * FromInt32 -> (int32 -> int32)
        
        static member
          inline FromInt32: Internals.Default1 * Internals.Default1
                              -> (int32 ->  ^R)
                              when  ^R: (static member FromInt32: int32 ->  ^R)
        
        static member
          inline FromInt32:  ^R * Internals.Default1 -> (int32 ->  ^R)
                              when  ^R: (static member FromInt32: int32 ->  ^R)
        
        static member
          inline FromInt32:  ^R * Internals.Default2 -> (int32 ->  ^R)
                              when ( ^R or int32) :
                                     (static member op_Implicit: int32 ->  ^R)
        
        static member
          inline FromInt32:  ^R * Internals.Default3 -> (int32 ->  ^R)
                              when (FromInt64 or  ^R) :
                                     (static member FromInt64:
                                         ^R * FromInt64 -> (int64 ->  ^R))
        
        static member
          inline FromInt32:  ^R * Internals.Default4 -> (int32 ->  ^R)
                              when (Explicit or  ^R or int32) :
                                     (static member Explicit:
                                         ^R * Explicit -> (int32 ->  ^R))
        
        static member
          inline Invoke: x: int32 ->  ^Num
                           when (FromInt32 or  ^Num) :
                                  (static member FromInt32:
                                      ^Num * FromInt32 -> (int32 ->  ^Num))
        
        static member
          inline InvokeOnInstance: x: int32 ->  ^Num
                                     when  ^Num:
                                            (static member FromInt32:
                                               int32 ->  ^Num)
    
    [<Class>]
    type One =
        inherit Internals.Default1
        
        static member
          inline Invoke: unit ->  ^Num
                           when (One or  ^Num) :
                                  (static member One:  ^Num * One ->  ^Num)
        
        static member
          inline One:  ^t * One -> ('a -> 'a) when  ^t: null and  ^t: struct
        
        static member
          inline One:  ^t * One ->  ^t when  ^t: (static member get_One: ->  ^t)
        
        static member
          inline One:  ^t * Internals.Default1 ->  ^t
                        when (FromInt32 or  ^t) :
                               (static member FromInt32:
                                   ^t * FromInt32 -> (int32 ->  ^t))
    
    [<Class>]
    type Zero =
        inherit Internals.Default1
        
        static member
          inline Invoke: unit ->  ^a
                           when (Zero or  ^a) :
                                  (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero:  ^t * Internals.Default1 -> ('a -> 'a)
                         when  ^t: null and  ^t: struct
        
        static member
          inline Zero:  ^t * Internals.Default1 ->  ^t
                         when  ^t: (static member get_Zero: ->  ^t)
        
        static member
          inline Zero:  ^t * Internals.Default2 -> ('a -> 'a)
                         when  ^t: null and  ^t: struct
        
        static member
          inline Zero:  ^t * Internals.Default2 ->  ^t
                         when  ^t: (static member FromInt32: int32 ->  ^t)
        
        static member
          inline Zero:  ^t * Internals.Default3 ->  ^t
                         when  ^t: (static member get_Empty: ->  ^t)
        
        static member
          Zero: System.Collections.Generic.IReadOnlyDictionary<'a,'b> *
                Internals.Default4
                  -> System.Collections.Generic.IReadOnlyDictionary<'a,'b>
                  when 'a: equality
        
        static member
          Zero: System.Collections.Generic.IDictionary<'a,'b> *
                Internals.Default4
                  -> System.Collections.Generic.IDictionary<'a,'b>
                  when 'a: equality
        
        static member
          Zero: System.Collections.Generic.IEnumerator<'a> * Internals.Default4
                  -> System.Collections.Generic.IEnumerator<'a>
        
        static member Zero: seq<'a> * Internals.Default4 -> seq<'a>
        
        static member
          inline Zero:  ^R * Internals.Default5 ->  ^R
                         when ( ^R or int) :
                                (static member op_Implicit: int ->  ^R)
        
        static member
          inline Zero:  ^R * Internals.Default6 ->  ^R
                         when (FromInt64 or  ^R) :
                                (static member FromInt64:
                                    ^R * FromInt64 -> (int64 ->  ^R))
        
        static member Zero: ResizeArray<'a> * Zero -> ResizeArray<'a>
        
        static member
          Zero: System.Collections.Generic.Dictionary<'a,'b> * Zero
                  -> System.Collections.Generic.Dictionary<'a,'b>
                  when 'a: equality
        
        static member
          inline Zero: System.Lazy< ^a> * Zero -> Lazy< ^a>
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero: Quotations.Expr< ^a> * Zero -> Quotations.Expr< ^a>
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero: Async< ^a> * Zero -> Async< ^a>
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero: ('T ->  ^Monoid) * Zero -> ('T ->  ^Monoid)
                         when (Zero or  ^Monoid) :
                                (static member Zero:  ^Monoid * Zero ->  ^Monoid)
        
        static member
          inline Zero: System.Threading.Tasks.Task< ^a> * Zero
                         -> System.Threading.Tasks.Task< ^a>
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero: ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f *  ^g) * Zero
                         ->  ^a *  ^b *  ^c *  ^d *  ^e *  ^f *  ^g
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b) and
                              (Zero or  ^c) :
                                (static member Zero:  ^c * Zero ->  ^c) and
                              (Zero or  ^d) :
                                (static member Zero:  ^d * Zero ->  ^d) and
                              (Zero or  ^e) :
                                (static member Zero:  ^e * Zero ->  ^e) and
                              (Zero or  ^f) :
                                (static member Zero:  ^f * Zero ->  ^f) and
                              (Zero or  ^g) :
                                (static member Zero:  ^g * Zero ->  ^g)
        
        static member
          inline Zero: ( ^a *  ^b *  ^c *  ^d *  ^e *  ^f) * Zero
                         ->  ^a *  ^b *  ^c *  ^d *  ^e *  ^f
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b) and
                              (Zero or  ^c) :
                                (static member Zero:  ^c * Zero ->  ^c) and
                              (Zero or  ^d) :
                                (static member Zero:  ^d * Zero ->  ^d) and
                              (Zero or  ^e) :
                                (static member Zero:  ^e * Zero ->  ^e) and
                              (Zero or  ^f) :
                                (static member Zero:  ^f * Zero ->  ^f)
        
        static member
          inline Zero: ( ^a *  ^b *  ^c *  ^d *  ^e) * Zero
                         ->  ^a *  ^b *  ^c *  ^d *  ^e
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b) and
                              (Zero or  ^c) :
                                (static member Zero:  ^c * Zero ->  ^c) and
                              (Zero or  ^d) :
                                (static member Zero:  ^d * Zero ->  ^d) and
                              (Zero or  ^e) :
                                (static member Zero:  ^e * Zero ->  ^e)
        
        static member
          inline Zero: ( ^a *  ^b *  ^c *  ^d) * Zero ->  ^a *  ^b *  ^c *  ^d
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b) and
                              (Zero or  ^c) :
                                (static member Zero:  ^c * Zero ->  ^c) and
                              (Zero or  ^d) :
                                (static member Zero:  ^d * Zero ->  ^d)
        
        static member
          inline Zero: ( ^a *  ^b *  ^c) * Zero ->  ^a *  ^b *  ^c
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b) and
                              (Zero or  ^c) :
                                (static member Zero:  ^c * Zero ->  ^c)
        
        static member
          inline Zero: ( ^a *  ^b) * Zero ->  ^a *  ^b
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a) and
                              (Zero or  ^b) :
                                (static member Zero:  ^b * Zero ->  ^b)
        
        static member
          inline Zero: Internals.Id<'a> * Zero -> Internals.Id< ^a0>
                         when (Zero or  ^a0) :
                                (static member Zero:  ^a0 * Zero ->  ^a0)
        
        static member
          inline Zero: System.Tuple< ^a> * Zero -> System.Tuple< ^a>
                         when (Zero or  ^a) :
                                (static member Zero:  ^a * Zero ->  ^a)
        
        static member
          inline Zero: t:  ^t * Zero ->  ^t
                         when  ^t: (member get_Item1:  ^t ->  ^t1) and
                               ^t: (member get_Item2:  ^t ->  ^t2) and
                               ^t: (member get_Item3:  ^t ->  ^t3) and
                               ^t: (member get_Item4:  ^t ->  ^t4) and
                               ^t: (member get_Item5:  ^t ->  ^t5) and
                               ^t: (member get_Item6:  ^t ->  ^t6) and
                               ^t: (member get_Item7:  ^t ->  ^t7) and
                               ^t: (member get_Rest:  ^t ->  ^tr) and
                              (Zero or  ^t1) :
                                (static member Zero:  ^t1 * Zero ->  ^t1) and
                              (Zero or  ^t2) :
                                (static member Zero:  ^t2 * Zero ->  ^t2) and
                              (Zero or  ^t3) :
                                (static member Zero:  ^t3 * Zero ->  ^t3) and
                              (Zero or  ^t4) :
                                (static member Zero:  ^t4 * Zero ->  ^t4) and
                              (Zero or  ^t5) :
                                (static member Zero:  ^t5 * Zero ->  ^t5) and
                              (Zero or  ^t6) :
                                (static member Zero:  ^t6 * Zero ->  ^t6) and
                              (Zero or  ^t7) :
                                (static member Zero:  ^t7 * Zero ->  ^t7) and
                              (Zero or  ^tr) :
                                (static member Zero:  ^tr * Zero ->  ^tr)
        
        static member Zero: Map<'a,'b> * Zero -> Map<'a,'b> when 'a: comparison
        
        static member Zero: Set<'a> * Zero -> Set<'a> when 'a: comparison
        
        static member Zero: bool * Zero -> bool
        
        static member Zero: unit * Zero -> unit
        
        static member
          Zero: System.Text.StringBuilder * Zero -> System.Text.StringBuilder
        
        static member Zero: string * Zero -> string
        
        static member Zero: 'a array * Zero -> 'a array
        
        static member Zero: 'a option * Zero -> 'a option
        
        static member Zero: 'a list * Zero -> 'a list
        
        static member Zero: Internals.DmStruct * Zero -> Internals.DmStruct
        
        static member Zero: System.TimeSpan * Zero -> System.TimeSpan
    
    [<Class>]
    type Abs =
        inherit Internals.Default1
        
        static member
          inline Abs: Internals.Default1 * Abs -> ('a ->  ^R)
                        when  ^R: (static member Abs: 'a ->  ^R)
        
        static member
          inline Abs: x:  ^t * Abs ->  ^t
                        when  ^t: (static member Abs:  ^t ->  ^t)
        
        static member
          inline Abs: x:  ^t * Internals.Default1 ->  ^t
                        when ( ^t or  ^u) :
                               (static member op_Implicit:  ^u ->  ^t) and
                              ^t: (static member Abs:  ^t ->  ^u)
        
        static member
          inline Abs: x:  ^t * Internals.Default2 ->  ^t
                        when (Explicit or  ^t or  ^u) :
                               (static member Explicit:
                                   ^t * Explicit -> ( ^u ->  ^t)) and
                              ^t: (static member Abs:  ^t ->  ^u)
        
        static member
          inline Invoke: x:  ^Num ->  ^Num
                           when (Abs or  ^Num) :
                                  (static member Abs:  ^Num * Abs ->  ^Num)
    
    [<Class>]
    type Abs' =
        inherit Abs
        
        static member Abs: x: unativeint * Abs' -> unativeint
        
        static member Abs: x: uint64 * Abs' -> uint64
        
        static member Abs: x: uint32 * Abs' -> uint32
        
        static member Abs: x: uint16 * Abs' -> uint16
        
        static member Abs: x: byte * Abs' -> byte
        
        static member
          inline Invoke: x:  ^Num ->  ^Num
                           when (Abs' or  ^Num) :
                                  (static member Abs:  ^Num * Abs' ->  ^Num)
    
    [<Class>]
    type Signum =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Num ->  ^Num
                           when (Signum or  ^Num) :
                                  (static member Signum:  ^Num * Signum ->  ^Num)
        
        static member
          inline Signum: x:  ^t * Internals.Default1 ->  ^t
                           when (FromInt32 or  ^t) :
                                  (static member FromInt32:
                                      ^t * FromInt32 -> (int32 ->  ^t)) and
                                 ^t: (member get_Sign:  ^t -> int)
        
        static member
          inline Signum:  ^t * Internals.Default1 -> ('a -> 'a)
                           when  ^t: null and  ^t: struct
        
        static member
          inline Signum: x:  ^t * Internals.Default2 ->  ^t
                           when  ^t: equality and
                                (Zero or  ^t) :
                                  (static member Zero:  ^t * Zero ->  ^t) and
                                 ^t: (static member (/) :  ^t *  ^t ->  ^t) and
                                (Abs or  ^t) :
                                  (static member Abs:  ^t * Abs ->  ^t)
    
    [<Class>]
    type Signum' =
        inherit Signum
        
        static member
          inline Invoke: x:  ^Num ->  ^Num
                           when (Signum' or  ^Num) :
                                  (static member Signum:
                                      ^Num * Signum' ->  ^Num)
        
        static member Signum: x: unativeint * Signum' -> unativeint
        
        static member Signum: x: uint64 * Signum' -> uint64
        
        static member Signum: x: uint32 * Signum' -> uint32
        
        static member Signum: x: uint16 * Signum' -> uint16
        
        static member Signum: x: byte * Signum' -> byte
    
    [<Class>]
    type TryNegate =
        
        static member
          inline Invoke: x:  ^Num -> Result< ^Num,exn>
                           when (TryNegate or  ^Num) :
                                  (static member TryNegate:
                                      ^Num -> Result< ^Num,exn>)
        
        static member
          inline TryNegate: x:  ^t -> Result< ^t,'a>
                              when  ^t: (static member (~-) :  ^t ->  ^t)
        
        static member
          inline TryNegate:  ^t -> unit when  ^t: null and  ^t: struct
    
    [<Class>]
    type TryNegate' =
        
        static member
          inline Invoke: x:  ^Num -> Result< ^Num,exn>
                           when (TryNegate' or  ^Num) :
                                  (static member TryNegate:
                                      ^Num -> Result< ^Num,exn>)
        
        static member
          TryNegate: x: unativeint -> Result<unativeint,System.Exception>
        
        static member TryNegate: x: uint64 -> Result<uint64,System.Exception>
        
        static member TryNegate: x: uint32 -> Result<uint32,System.Exception>
        
        static member TryNegate: x: uint16 -> Result<uint16,System.Exception>
        
        static member TryNegate: x: byte -> Result<byte,System.Exception>
    
    [<Class>]
    type DivRem =
        inherit Internals.Default1
        
        static member
          inline DivRem: D:  ^T * d:  ^T * _impl: DivRem -> 'b *  ^T
                           when  ^T:
                                  (static member DivRem:
                                      ^T *  ^T * byref< ^T> -> 'b)
        
        static member
          inline DivRem: D:  ^T * d:  ^T * _impl: Internals.Default1
                           ->  ^a *  ^c
                           when  ^T: (static member (/) :  ^T *  ^T ->  ^a) and
                                ( ^T or  ^b) :
                                  (static member (-) :  ^T *  ^b ->  ^c) and
                                ( ^a or  ^T) :
                                  (static member ( * ) :  ^a *  ^T ->  ^b)
        
        static member
          inline DivRem: x:  ^t * y:  ^t * _thisClass: DivRem ->  ^t *  ^t
                           when  ^t: null and  ^t: struct
        
        static member
          inline Invoke: D:  ^T -> d:  ^T ->  ^T *  ^T
                           when (DivRem or  ^T) :
                                  (static member DivRem:
                                      ^T *  ^T * DivRem ->  ^T *  ^T)
    
    [<Class>]
    type ToBigInt =
        
        static member
          inline Invoke: x:  ^Integral -> bigint
                           when (ToBigInt or  ^Integral) :
                                  (static member ToBigInt:  ^Integral -> bigint)
        
        static member ToBigInt: x: uint64 -> bigint
        
        static member ToBigInt: x: uint32 -> bigint
        
        static member ToBigInt: x: bigint -> bigint
        
        static member ToBigInt: x: unativeint -> bigint
        
        static member ToBigInt: x: uint16 -> bigint
        
        static member ToBigInt: x: byte -> bigint
        
        static member ToBigInt: x: nativeint -> bigint
        
        static member ToBigInt: x: int64 -> bigint
        
        static member ToBigInt: x: int32 -> bigint
        
        static member ToBigInt: x: int16 -> bigint
        
        static member ToBigInt: x: sbyte -> bigint
    
    module internal Numerics =
        
        val inline (+.) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when  ^Num: (static member (+) :  ^Num *  ^Num ->  ^Num)
        
        val inline (-.) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when  ^Num: (static member (-) :  ^Num *  ^Num ->  ^Num)
        
        val inline ( *. ) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when  ^Num: (static member ( * ) :  ^Num *  ^Num ->  ^Num)
        
        val inline fromIntegral:
          x:  ^Integral ->  ^Num
            when (ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                 (FromBigInt or  ^Num) :
                   (static member FromBigInt:
                       ^Num * FromBigInt -> (bigint ->  ^Num))
namespace FSharpPlus.Control
    
    [<Class>]
    type Pi =
        inherit Internals.Default1
        
        static member
          inline Invoke: unit ->  ^Floating
                           when (Pi or  ^Floating) :
                                  (static member Pi:
                                      ^Floating * Pi ->  ^Floating)
        
        static member Pi: decimal * Pi -> decimal
        
        static member Pi: float * Pi -> float
        
        static member Pi: float32 * Pi -> float32
        
        static member
          inline Pi: Internals.Default1 * Internals.Default1 ->  ^R
                       when  ^R: (static member get_PI: ->  ^R)
        
        static member
          inline Pi:  ^R * Internals.Default1 ->  ^R
                       when  ^R: (static member get_PI: ->  ^R)
        
        static member
          inline Pi:  ^R * Internals.Default2 ->  ^R
                       when ( ^R or float) :
                              (static member op_Implicit: float ->  ^R)
        
        static member
          inline Pi:  ^R * Internals.Default3 ->  ^R
                       when ( ^R or float32) :
                              (static member op_Implicit: float32 ->  ^R)
    
    [<Class>]
    type Subtract =
        
        static member
          inline Invoke: x:  ^Num -> y:  ^Num ->  ^Num
                           when (Subtract or  ^Num) :
                                  (static member Subtract:
                                      ^Num *  ^Num ->  ^Num)
        
        static member Subtract: x: unativeint * y: unativeint -> unativeint
        
        static member Subtract: x: uint64 * y: uint64 -> uint64
        
        static member Subtract: x: uint32 * y: uint32 -> uint32
        
        static member Subtract: x: uint16 * y: uint16 -> uint16
        
        static member Subtract: x: byte * y: byte -> byte
        
        static member
          inline Subtract: x:  ^Num * y:  ^Num ->  ^Num
                             when  ^Num:
                                    (static member TrySubtract:
                                        ^Num *  ^Num -> Result< ^Num,exn>)
        
        static member
          inline Subtract: x:  ^a * y:  ^b ->  ^c
                             when ( ^a or  ^b) :
                                    (static member (-) :  ^a *  ^b ->  ^c)
    
    [<Class>]
    type TrySubtract =
        
        static member
          inline Invoke: x:  ^Num -> y:  ^Num -> Result< ^Num,exn>
                           when (TrySubtract or  ^Num) :
                                  (static member TrySubtract:
                                      ^Num *  ^Num -> Result< ^Num,exn>)
        
        static member
          TrySubtract: x: unativeint * y: unativeint
                         -> Result<unativeint,System.Exception>
        
        static member
          TrySubtract: x: uint64 * y: uint64 -> Result<uint64,System.Exception>
        
        static member
          TrySubtract: x: uint32 * y: uint32 -> Result<uint32,System.Exception>
        
        static member
          TrySubtract: x: uint16 * y: uint16 -> Result<uint16,System.Exception>
        
        static member
          TrySubtract: x: byte * y: byte -> Result<byte,System.Exception>
        
        static member
          inline TrySubtract:  ^t * TrySubtract -> ('a -> 'a)
                                when  ^t: null and  ^t: struct
        
        static member
          inline TrySubtract: x:  ^t * y:  ^a -> Result< ^b,'c>
                                when ( ^t or  ^a) :
                                       (static member (-) :  ^t *  ^a ->  ^b)
    
    [<Class>]
    type Divide =
        
        static member Divide: x: float32 * y: float32 -> float32
        
        static member Divide: x: float * y: float -> float
        
        static member
          inline Divide: x:  ^Num * y:  ^Num ->  ^Num
                           when  ^Num:
                                  (static member TryDivide:
                                      ^Num *  ^Num -> Result< ^Num,exn>)
        
        static member
          inline Divide: x:  ^a * y:  ^b ->  ^c
                           when ( ^a or  ^b) :
                                  (static member (/) :  ^a *  ^b ->  ^c) and
                                 ^a: equality and
                                ( ^c or  ^b) :
                                  (static member ( * ) :  ^c *  ^b ->  ^a)
        
        static member
          inline Invoke: x:  ^Num -> y:  ^Num ->  ^Num
                           when (Divide or  ^Num) :
                                  (static member Divide:  ^Num *  ^Num ->  ^Num)
    
    [<Class>]
    type TryDivide =
        
        static member
          inline Invoke: x:  ^Num -> y:  ^Num -> Result< ^Num,exn>
                           when (TryDivide or  ^Num) :
                                  (static member TryDivide:
                                      ^Num *  ^Num -> Result< ^Num,exn>)
        
        static member TryDivide: x: float32 * y: float32 -> Result<float32,'a>
        
        static member TryDivide: x: float * y: float -> Result<float,'a>
        
        static member
          inline TryDivide:  ^t * Internals.Default1 -> ('a -> Result<'b,'c>)
                              when  ^t: null and  ^t: struct and 'c: null
        
        static member
          inline TryDivide: x:  ^a * y:  ^b -> Result< ^t,exn>
                              when ( ^a or  ^b) :
                                     (static member (/) :  ^a *  ^b ->  ^t) and
                                    ^a: equality and  ^b: equality and
                                   (Zero or  ^b) :
                                     (static member Zero:  ^b * Zero ->  ^b) and
                                   ( ^t or  ^b) :
                                     (static member ( * ) :  ^t *  ^b ->  ^a)
    
    [<Class>]
    type TrySqrtRem =
        
        static member
          inline Invoke: x:  ^Integral -> Result<( ^Integral *  ^Integral),exn>
                           when (TrySqrtRem or  ^Integral) :
                                  (static member TrySqrtRem:
                                      ^Integral
                                       -> Result<( ^Integral *  ^Integral),exn>)
        
        static member TrySqrtRem: x: unativeint -> unativeint * unativeint
        
        static member
          TrySqrtRem: x: nativeint -> Result<(nativeint * nativeint),'a>
        
        static member TrySqrtRem: x: byte -> Result<(byte * byte),'b>
        
        static member TrySqrtRem: x: uint64 -> Result<(uint64 * uint64),'c>
        
        static member TrySqrtRem: x: uint32 -> Result<(uint32 * uint32),'d>
        
        static member TrySqrtRem: x: uint16 -> Result<(uint16 * uint16),'e>
        
        static member
          TrySqrtRem: x: sbyte -> Result<(sbyte * sbyte),System.Exception>
        
        static member
          TrySqrtRem: x: int64 -> Result<(int64 * int64),System.Exception>
        
        static member
          TrySqrtRem: x: int32 -> Result<(int * int32),System.Exception>
        
        static member
          TrySqrtRem: x: int16 -> Result<(int16 * int16),System.Exception>
        
        static member
          TrySqrtRem: x: bigint
                        -> Result<(System.Numerics.BigInteger *
                                   System.Numerics.BigInteger),System.Exception>
    
    [<Class>]
    type TrySqrt =
        
        static member
          inline Invoke: x:  ^Integral -> Result< ^Integral,exn>
                           when (TrySqrt or  ^Integral) :
                                  (static member TrySqrt:
                                      ^Integral -> Result< ^Integral,exn>)
        
        static member
          inline TrySqrt: x: decimal -> Result<decimal,System.Exception>
        
        static member
          inline TrySqrt: x: float32 -> Result<float32,System.Exception>
        
        static member inline TrySqrt: x: float -> Result<float,System.Exception>
        
        static member
          inline TrySqrt: x:  ^Rational -> Result< ^a,System.Exception>
                            when  ^Rational: comparison and
                                 (Zero or  ^Rational) :
                                   (static member Zero:
                                       ^Rational * Zero ->  ^Rational) and
                                  ^Rational:
                                   (member get_Numerator:  ^Rational ->  ^i) and
                                  ^Rational:
                                   (member get_Denominator:  ^Rational ->  ^i) and
                                 (FromBigInt or  ^Rational) :
                                   (static member FromBigInt:
                                       ^Rational * FromBigInt
                                        -> (bigint ->  ^Rational)) and
                                  ^Rational:
                                   (static member (/) :
                                       ^Rational *  ^Rational ->  ^a) and
                                 (ToBigInt or  ^i) :
                                   (static member ToBigInt:  ^i -> bigint) and
                                 (TrySqrt or  ^i) :
                                   (static member TrySqrt:
                                       ^i -> Result< ^i,exn>)
        
        static member
          inline TrySqrt: x:  ^Z -> Result< ^Z,System.Exception>
                            when  ^Z: comparison and
                                 (Zero or  ^Z) :
                                   (static member Zero:  ^Z * Zero ->  ^Z) and
                                 (TrySqrtRem or  ^Z) :
                                   (static member TrySqrtRem:
                                       ^Z -> Result<( ^Z *  ^Z),exn>)
        
        static member
          inline TrySqrt: x:  ^T -> Result< ^a,exn>
                            when  ^T: (static member Sqrt:  ^T ->  ^a)
    
    [<Class>]
    type Sqrt =
        inherit Internals.Default1
        
        static member
          inline Invoke: x:  ^Integral ->  ^Integral
                           when (Sqrt or  ^Integral) :
                                  (static member Sqrt:
                                      ^Integral * Sqrt ->  ^Integral)
        
        static member
          inline Sqrt: x:  ^Rational * Sqrt ->  ^a
                         when  ^Rational: comparison and
                              (Zero or  ^Rational) :
                                (static member Zero:
                                    ^Rational * Zero ->  ^Rational) and
                               ^Rational:
                                (member get_Numerator:  ^Rational ->  ^i) and
                               ^Rational:
                                (member get_Denominator:  ^Rational ->  ^i) and
                              (FromBigInt or  ^Rational) :
                                (static member FromBigInt:
                                    ^Rational * FromBigInt
                                     -> (bigint ->  ^Rational)) and
                               ^Rational:
                                (static member (/) :
                                    ^Rational *  ^Rational ->  ^a) and
                              (ToBigInt or  ^i) :
                                (static member ToBigInt:  ^i -> bigint) and
                              (Sqrt or  ^i) :
                                (static member Sqrt:  ^i * Sqrt ->  ^i)
        
        static member inline Sqrt: x: decimal * Sqrt -> decimal
        
        static member inline Sqrt: x: float32 * Sqrt -> float32
        
        static member
          inline Sqrt: x:  ^Z * Internals.Default1 ->  ^Z
                         when  ^Z: comparison and
                              (Zero or  ^Z) :
                                (static member Zero:  ^Z * Zero ->  ^Z) and
                              (TrySqrtRem or  ^Z) :
                                (static member TrySqrtRem:
                                    ^Z -> Result<( ^Z *  ^Z),exn>)
        
        static member
          inline Sqrt: x:  ^T * Internals.Default1 ->  ^a
                         when  ^T: (static member Sqrt:  ^T ->  ^a)
        
        static member
          inline Sqrt: x:  ^Num * Internals.Default2 ->  ^Num
                         when  ^Num:
                                (static member TrySqrt:
                                    ^Num -> Result< ^Num,exn>)
    
    [<Class>]
    type MinValue =
        inherit Internals.Default1
        
        static member
          inline Invoke: unit ->  ^a9
                           when (MinValue or  ^a9) :
                                  (static member MinValue:
                                      ^a9 * MinValue ->  ^a9)
        
        static member
          inline MinValue: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * MinValue
                             ->  ^a0 *  ^b1 *  ^c2 *  ^d3 *  ^e4 *  ^f5 *  ^g6
                             when (MinValue or  ^a0) :
                                    (static member MinValue:
                                        ^a0 * MinValue ->  ^a0) and
                                  (MinValue or  ^b1) :
                                    (static member MinValue:
                                        ^b1 * MinValue ->  ^b1) and
                                  (MinValue or  ^c2) :
                                    (static member MinValue:
                                        ^c2 * MinValue ->  ^c2) and
                                  (MinValue or  ^d3) :
                                    (static member MinValue:
                                        ^d3 * MinValue ->  ^d3) and
                                  (MinValue or  ^e4) :
                                    (static member MinValue:
                                        ^e4 * MinValue ->  ^e4) and
                                  (MinValue or  ^f5) :
                                    (static member MinValue:
                                        ^f5 * MinValue ->  ^f5) and
                                  (MinValue or  ^g6) :
                                    (static member MinValue:
                                        ^g6 * MinValue ->  ^g6)
        
        static member
          inline MinValue: ('a * 'b * 'c * 'd * 'e * 'f) * MinValue
                             ->  ^h *  ^i *  ^j *  ^k *  ^l *  ^m
                             when (MinValue or  ^h) :
                                    (static member MinValue:
                                        ^h * MinValue ->  ^h) and
                                  (MinValue or  ^i) :
                                    (static member MinValue:
                                        ^i * MinValue ->  ^i) and
                                  (MinValue or  ^j) :
                                    (static member MinValue:
                                        ^j * MinValue ->  ^j) and
                                  (MinValue or  ^k) :
                                    (static member MinValue:
                                        ^k * MinValue ->  ^k) and
                                  (MinValue or  ^l) :
                                    (static member MinValue:
                                        ^l * MinValue ->  ^l) and
                                  (MinValue or  ^m) :
                                    (static member MinValue:
                                        ^m * MinValue ->  ^m)
        
        static member
          inline MinValue: ('a * 'b * 'c * 'd * 'e) * MinValue
                             ->  ^n *  ^o *  ^p *  ^q *  ^r
                             when (MinValue or  ^n) :
                                    (static member MinValue:
                                        ^n * MinValue ->  ^n) and
                                  (MinValue or  ^o) :
                                    (static member MinValue:
                                        ^o * MinValue ->  ^o) and
                                  (MinValue or  ^p) :
                                    (static member MinValue:
                                        ^p * MinValue ->  ^p) and
                                  (MinValue or  ^q) :
                                    (static member MinValue:
                                        ^q * MinValue ->  ^q) and
                                  (MinValue or  ^r) :
                                    (static member MinValue:
                                        ^r * MinValue ->  ^r)
        
        static member
          inline MinValue: ('a * 'b * 'c * 'd) * MinValue
                             ->  ^s *  ^t *  ^a1 *  ^a2
                             when (MinValue or  ^s) :
                                    (static member MinValue:
                                        ^s * MinValue ->  ^s) and
                                  (MinValue or  ^t) :
                                    (static member MinValue:
                                        ^t * MinValue ->  ^t) and
                                  (MinValue or  ^a1) :
                                    (static member MinValue:
                                        ^a1 * MinValue ->  ^a1) and
                                  (MinValue or  ^a2) :
                                    (static member MinValue:
                                        ^a2 * MinValue ->  ^a2)
        
        static member
          inline MinValue: ('a * 'b * 'c) * MinValue ->  ^a3 *  ^a4 *  ^a5
                             when (MinValue or  ^a3) :
                                    (static member MinValue:
                                        ^a3 * MinValue ->  ^a3) and
                                  (MinValue or  ^a4) :
                                    (static member MinValue:
                                        ^a4 * MinValue ->  ^a4) and
                                  (MinValue or  ^a5) :
                                    (static member MinValue:
                                        ^a5 * MinValue ->  ^a5)
        
        static member
          inline MinValue: ('a * 'b) * MinValue ->  ^a6 *  ^a7
                             when (MinValue or  ^a6) :
                                    (static member MinValue:
                                        ^a6 * MinValue ->  ^a6) and
                                  (MinValue or  ^a7) :
                                    (static member MinValue:
                                        ^a7 * MinValue ->  ^a7)
        
        static member
          inline MinValue: Internals.Id<'a> * MinValue -> Internals.Id< ^a8>
                             when (MinValue or  ^a8) :
                                    (static member MinValue:
                                        ^a8 * MinValue ->  ^a8)
        
        static member
          inline MinValue: System.Tuple< ^a> * MinValue -> System.Tuple< ^a>
                             when (MinValue or  ^a) :
                                    (static member MinValue:
                                        ^a * MinValue ->  ^a)
        
        static member
          inline MinValue: t:  ^t * MinValue ->  ^t
                             when  ^t: (member get_Item1:  ^t ->  ^t1) and
                                   ^t: (member get_Item2:  ^t ->  ^t2) and
                                   ^t: (member get_Item3:  ^t ->  ^t3) and
                                   ^t: (member get_Item4:  ^t ->  ^t4) and
                                   ^t: (member get_Item5:  ^t ->  ^t5) and
                                   ^t: (member get_Item6:  ^t ->  ^t6) and
                                   ^t: (member get_Item7:  ^t ->  ^t7) and
                                   ^t: (member get_Rest:  ^t ->  ^tr) and
                                  (MinValue or  ^t1) :
                                    (static member MinValue:
                                        ^t1 * MinValue ->  ^t1) and
                                  (MinValue or  ^t2) :
                                    (static member MinValue:
                                        ^t2 * MinValue ->  ^t2) and
                                  (MinValue or  ^t3) :
                                    (static member MinValue:
                                        ^t3 * MinValue ->  ^t3) and
                                  (MinValue or  ^t4) :
                                    (static member MinValue:
                                        ^t4 * MinValue ->  ^t4) and
                                  (MinValue or  ^t5) :
                                    (static member MinValue:
                                        ^t5 * MinValue ->  ^t5) and
                                  (MinValue or  ^t6) :
                                    (static member MinValue:
                                        ^t6 * MinValue ->  ^t6) and
                                  (MinValue or  ^t7) :
                                    (static member MinValue:
                                        ^t7 * MinValue ->  ^t7) and
                                  (MinValue or  ^tr) :
                                    (static member MinValue:
                                        ^tr * MinValue ->  ^tr)
        
        static member MinValue: System.TimeSpan * MinValue -> System.TimeSpan
        
        static member
          MinValue: System.DateTimeOffset * MinValue -> System.DateTimeOffset
        
        static member MinValue: System.DateTime * MinValue -> System.DateTime
        
        static member MinValue: decimal * MinValue -> decimal
        
        static member MinValue: uint64 * MinValue -> uint64
        
        static member MinValue: uint32 * MinValue -> uint32
        
        static member MinValue: uint16 * MinValue -> uint16
        
        static member MinValue: float32 * MinValue -> float32
        
        static member MinValue: int64 * MinValue -> int64
        
        static member MinValue: int * MinValue -> int
        
        static member MinValue: int16 * MinValue -> int16
        
        static member MinValue: float * MinValue -> float
        
        static member MinValue: sbyte * MinValue -> sbyte
        
        static member MinValue: byte * MinValue -> byte
        
        static member MinValue: char * MinValue -> char
        
        static member MinValue: bool * MinValue -> bool
        
        static member MinValue: unit * MinValue -> unit
        
        static member
          inline MinValue:  ^t * Internals.Default1 ->  ^t
                             when  ^t: (static member get_MinValue: ->  ^t)
    
    [<Class>]
    type MaxValue =
        inherit Internals.Default1
        
        static member
          inline Invoke: unit ->  ^a9
                           when (MaxValue or  ^a9) :
                                  (static member MaxValue:
                                      ^a9 * MaxValue ->  ^a9)
        
        static member
          inline MaxValue: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * MaxValue
                             ->  ^a0 *  ^b1 *  ^c2 *  ^d3 *  ^e4 *  ^f5 *  ^g6
                             when (MaxValue or  ^a0) :
                                    (static member MaxValue:
                                        ^a0 * MaxValue ->  ^a0) and
                                  (MaxValue or  ^b1) :
                                    (static member MaxValue:
                                        ^b1 * MaxValue ->  ^b1) and
                                  (MaxValue or  ^c2) :
                                    (static member MaxValue:
                                        ^c2 * MaxValue ->  ^c2) and
                                  (MaxValue or  ^d3) :
                                    (static member MaxValue:
                                        ^d3 * MaxValue ->  ^d3) and
                                  (MaxValue or  ^e4) :
                                    (static member MaxValue:
                                        ^e4 * MaxValue ->  ^e4) and
                                  (MaxValue or  ^f5) :
                                    (static member MaxValue:
                                        ^f5 * MaxValue ->  ^f5) and
                                  (MaxValue or  ^g6) :
                                    (static member MaxValue:
                                        ^g6 * MaxValue ->  ^g6)
        
        static member
          inline MaxValue: ('a * 'b * 'c * 'd * 'e * 'f) * MaxValue
                             ->  ^h *  ^i *  ^j *  ^k *  ^l *  ^m
                             when (MaxValue or  ^h) :
                                    (static member MaxValue:
                                        ^h * MaxValue ->  ^h) and
                                  (MaxValue or  ^i) :
                                    (static member MaxValue:
                                        ^i * MaxValue ->  ^i) and
                                  (MaxValue or  ^j) :
                                    (static member MaxValue:
                                        ^j * MaxValue ->  ^j) and
                                  (MaxValue or  ^k) :
                                    (static member MaxValue:
                                        ^k * MaxValue ->  ^k) and
                                  (MaxValue or  ^l) :
                                    (static member MaxValue:
                                        ^l * MaxValue ->  ^l) and
                                  (MaxValue or  ^m) :
                                    (static member MaxValue:
                                        ^m * MaxValue ->  ^m)
        
        static member
          inline MaxValue: ('a * 'b * 'c * 'd * 'e) * MaxValue
                             ->  ^n *  ^o *  ^p *  ^q *  ^r
                             when (MaxValue or  ^n) :
                                    (static member MaxValue:
                                        ^n * MaxValue ->  ^n) and
                                  (MaxValue or  ^o) :
                                    (static member MaxValue:
                                        ^o * MaxValue ->  ^o) and
                                  (MaxValue or  ^p) :
                                    (static member MaxValue:
                                        ^p * MaxValue ->  ^p) and
                                  (MaxValue or  ^q) :
                                    (static member MaxValue:
                                        ^q * MaxValue ->  ^q) and
                                  (MaxValue or  ^r) :
                                    (static member MaxValue:
                                        ^r * MaxValue ->  ^r)
        
        static member
          inline MaxValue: ('a * 'b * 'c * 'd) * MaxValue
                             ->  ^s *  ^t *  ^a1 *  ^a2
                             when (MaxValue or  ^s) :
                                    (static member MaxValue:
                                        ^s * MaxValue ->  ^s) and
                                  (MaxValue or  ^t) :
                                    (static member MaxValue:
                                        ^t * MaxValue ->  ^t) and
                                  (MaxValue or  ^a1) :
                                    (static member MaxValue:
                                        ^a1 * MaxValue ->  ^a1) and
                                  (MaxValue or  ^a2) :
                                    (static member MaxValue:
                                        ^a2 * MaxValue ->  ^a2)
        
        static member
          inline MaxValue: ('a * 'b * 'c) * MaxValue ->  ^a3 *  ^a4 *  ^a5
                             when (MaxValue or  ^a3) :
                                    (static member MaxValue:
                                        ^a3 * MaxValue ->  ^a3) and
                                  (MaxValue or  ^a4) :
                                    (static member MaxValue:
                                        ^a4 * MaxValue ->  ^a4) and
                                  (MaxValue or  ^a5) :
                                    (static member MaxValue:
                                        ^a5 * MaxValue ->  ^a5)
        
        static member
          inline MaxValue: ('a * 'b) * MaxValue ->  ^a6 *  ^a7
                             when (MaxValue or  ^a6) :
                                    (static member MaxValue:
                                        ^a6 * MaxValue ->  ^a6) and
                                  (MaxValue or  ^a7) :
                                    (static member MaxValue:
                                        ^a7 * MaxValue ->  ^a7)
        
        static member
          inline MaxValue: Internals.Id<'a> * MaxValue -> Internals.Id< ^a8>
                             when (MaxValue or  ^a8) :
                                    (static member MaxValue:
                                        ^a8 * MaxValue ->  ^a8)
        
        static member
          inline MaxValue: System.Tuple< ^a> * MaxValue -> System.Tuple< ^a>
                             when (MaxValue or  ^a) :
                                    (static member MaxValue:
                                        ^a * MaxValue ->  ^a)
        
        static member
          inline MaxValue: t:  ^t * MaxValue ->  ^t
                             when  ^t: (member get_Item1:  ^t ->  ^t1) and
                                   ^t: (member get_Item2:  ^t ->  ^t2) and
                                   ^t: (member get_Item3:  ^t ->  ^t3) and
                                   ^t: (member get_Item4:  ^t ->  ^t4) and
                                   ^t: (member get_Item5:  ^t ->  ^t5) and
                                   ^t: (member get_Item6:  ^t ->  ^t6) and
                                   ^t: (member get_Item7:  ^t ->  ^t7) and
                                   ^t: (member get_Rest:  ^t ->  ^tr) and
                                  (MaxValue or  ^t1) :
                                    (static member MaxValue:
                                        ^t1 * MaxValue ->  ^t1) and
                                  (MaxValue or  ^t2) :
                                    (static member MaxValue:
                                        ^t2 * MaxValue ->  ^t2) and
                                  (MaxValue or  ^t3) :
                                    (static member MaxValue:
                                        ^t3 * MaxValue ->  ^t3) and
                                  (MaxValue or  ^t4) :
                                    (static member MaxValue:
                                        ^t4 * MaxValue ->  ^t4) and
                                  (MaxValue or  ^t5) :
                                    (static member MaxValue:
                                        ^t5 * MaxValue ->  ^t5) and
                                  (MaxValue or  ^t6) :
                                    (static member MaxValue:
                                        ^t6 * MaxValue ->  ^t6) and
                                  (MaxValue or  ^t7) :
                                    (static member MaxValue:
                                        ^t7 * MaxValue ->  ^t7) and
                                  (MaxValue or  ^tr) :
                                    (static member MaxValue:
                                        ^tr * MaxValue ->  ^tr)
        
        static member MaxValue: System.TimeSpan * MaxValue -> System.TimeSpan
        
        static member
          MaxValue: System.DateTimeOffset * MaxValue -> System.DateTimeOffset
        
        static member MaxValue: System.DateTime * MaxValue -> System.DateTime
        
        static member MaxValue: decimal * MaxValue -> decimal
        
        static member MaxValue: uint64 * MaxValue -> uint64
        
        static member MaxValue: uint32 * MaxValue -> uint32
        
        static member MaxValue: uint16 * MaxValue -> uint16
        
        static member MaxValue: float32 * MaxValue -> float32
        
        static member MaxValue: int64 * MaxValue -> int64
        
        static member MaxValue: int * MaxValue -> int
        
        static member MaxValue: int16 * MaxValue -> int16
        
        static member MaxValue: float * MaxValue -> float
        
        static member MaxValue: sbyte * MaxValue -> sbyte
        
        static member MaxValue: byte * MaxValue -> byte
        
        static member MaxValue: char * MaxValue -> char
        
        static member MaxValue: bool * MaxValue -> bool
        
        static member MaxValue: unit * MaxValue -> unit
        
        static member
          inline MaxValue:  ^t * Internals.Default1 ->  ^t
                             when  ^t: (static member get_MaxValue: ->  ^t)

