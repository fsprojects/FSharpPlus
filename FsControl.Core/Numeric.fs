namespace FsControl.Core.TypeMethods

open System.Numerics
open FsControl.Core.Prelude
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


type FromBigInt() =
    static member val Instance = FromBigInt()
    static member        FromBigInt (_:int32     ,_:FromBigInt) = fun (x:bigint) -> int             x
    static member        FromBigInt (_:int64     ,_:FromBigInt) = fun (x:bigint) -> int64           x
    static member        FromBigInt (_:nativeint ,_:FromBigInt) = fun (x:bigint) -> nativeint  (int x)
    static member        FromBigInt (_:unativeint,_:FromBigInt) = fun (x:bigint) -> unativeint (int x)
    static member        FromBigInt (_:bigint    ,_:FromBigInt) = fun (x:bigint) ->                 x
    static member        FromBigInt (_:float     ,_:FromBigInt) = fun (x:bigint) -> float           x
#if NOTNET35
    static member        FromBigInt (_:sbyte     ,_:FromBigInt) = fun (x:bigint) -> sbyte           x
    static member        FromBigInt (_:int16     ,_:FromBigInt) = fun (x:bigint) -> int16           x
    static member        FromBigInt (_:byte      ,_:FromBigInt) = fun (x:bigint) -> byte            x
    static member        FromBigInt (_:uint16    ,_:FromBigInt) = fun (x:bigint) -> uint16          x
    static member        FromBigInt (_:uint32    ,_:FromBigInt) = fun (x:bigint) -> uint32          x
    static member        FromBigInt (_:uint64    ,_:FromBigInt) = fun (x:bigint) -> uint64          x
    static member        FromBigInt (_:float32   ,_:FromBigInt) = fun (x:bigint) -> float32         x
    static member        FromBigInt (_:decimal   ,_:FromBigInt) = fun (x:bigint) -> decimal         x
    static member        FromBigInt (_:Complex   ,_:FromBigInt) = fun (x:bigint) -> Complex (float  x, 0.0)
#else
    static member        FromBigInt (_:sbyte     ) = fun (x:bigint) -> sbyte      (int x)
    static member        FromBigInt (_:int16     ) = fun (x:bigint) -> int16      (int x)
    static member        FromBigInt (_:byte      ) = fun (x:bigint) -> byte       (int x)
    static member        FromBigInt (_:uint16    ) = fun (x:bigint) -> uint16     (int x)
    static member        FromBigInt (_:uint32    ) = fun (x:bigint) -> uint32     (int x)
    static member        FromBigInt (_:uint64    ) = fun (x:bigint) -> uint64     (int64 x)
    static member        FromBigInt (_:float32   ) = fun (x:bigint) -> float32    (int x)
    static member        FromBigInt (_:decimal   ) = fun (x:bigint) -> decimal    (int x)
#endif

    static member inline Invoke (x:bigint)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInt: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromBigInt.Instance x

type FromInt64() =
    static member val Instance = FromInt64()
    static member        FromInt64 (_:int32     , _:FromInt64) = fun (x:int64) -> int32           x
    static member        FromInt64 (_:int64     , _:FromInt64) = fun (x:int64) ->                 x
    static member        FromInt64 (_:nativeint , _:FromInt64) = fun (x:int64) -> nativeint  (int x)
    static member        FromInt64 (_:unativeint, _:FromInt64) = fun (x:int64) -> unativeint (int x)
    static member        FromInt64 (_:bigint    , _:FromInt64) = fun (x:int64) -> bigint          x
    static member        FromInt64 (_:float     , _:FromInt64) = fun (x:int64) -> float           x
#if NOTNET35
    static member        FromInt64 (_:sbyte     , _:FromInt64) = fun (x:int64) -> sbyte           x
    static member        FromInt64 (_:int16     , _:FromInt64) = fun (x:int64) -> int16           x
    static member        FromInt64 (_:byte      , _:FromInt64) = fun (x:int64) -> byte            x
    static member        FromInt64 (_:uint16    , _:FromInt64) = fun (x:int64) -> uint16          x
    static member        FromInt64 (_:uint32    , _:FromInt64) = fun (x:int64) -> uint32          x
    static member        FromInt64 (_:uint64    , _:FromInt64) = fun (x:int64) -> uint64          x
    static member        FromInt64 (_:float32   , _:FromInt64) = fun (x:int64) -> float32         x
    static member        FromInt64 (_:decimal   , _:FromInt64) = fun (x:int64) -> decimal         x
#else
    static member        FromInt64 (_:sbyte     , _:FromInt64) = fun (x:int64) -> sbyte      (int x)
    static member        FromInt64 (_:int16     , _:FromInt64) = fun (x:int64) -> int16      (int x)
    static member        FromInt64 (_:byte      , _:FromInt64) = fun (x:int64) -> byte       (int x)
    static member        FromInt64 (_:uint16    , _:FromInt64) = fun (x:int64) -> uint16     (int x)
    static member        FromInt64 (_:uint32    , _:FromInt64) = fun (x:int64) -> uint32     (int x)
    static member        FromInt64 (_:uint64    , _:FromInt64) = fun (x:int64) -> uint64          x
    static member        FromInt64 (_:float32   , _:FromInt64) = fun (x:int64) -> float32    (int x)
    static member        FromInt64 (_:decimal   , _:FromInt64) = fun (x:int64) -> decimal    (int x)
#endif

    static member inline Invoke (x:int64)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromInt64: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromInt64.Instance x


type FromInt32() =
    inherit  Default1()
    static member val Instance = FromInt32()
    static member        FromInt32 (_:int32     , _:FromInt32) = fun (x:int32) ->                 x
    static member        FromInt32 (_:int64     , _:FromInt32) = fun (x:int32) -> int64           x
    static member        FromInt32 (_:nativeint , _:FromInt32) = fun (x:int32) -> nativeint  (int x)
    static member        FromInt32 (_:unativeint, _:FromInt32) = fun (x:int32) -> unativeint (int x)
    static member        FromInt32 (_:bigint    , _:FromInt32) = fun (x:int32) -> bigint          x
    static member        FromInt32 (_:float     , _:FromInt32) = fun (x:int32) -> float           x
#if NOTNET35
    static member        FromInt32 (_:sbyte     , _:FromInt32) = fun (x:int32) -> sbyte           x
    static member        FromInt32 (_:int16     , _:FromInt32) = fun (x:int32) -> int16           x
    static member        FromInt32 (_:byte      , _:FromInt32) = fun (x:int32) -> byte            x
    static member        FromInt32 (_:uint16    , _:FromInt32) = fun (x:int32) -> uint16          x
    static member        FromInt32 (_:uint32    , _:FromInt32) = fun (x:int32) -> uint32          x
    static member        FromInt32 (_:uint64    , _:FromInt32) = fun (x:int32) -> uint64          x
    static member        FromInt32 (_:float32   , _:FromInt32) = fun (x:int32) -> float32         x
    static member        FromInt32 (_:decimal   , _:FromInt32) = fun (x:int32) -> decimal         x
#else
    static member        FromInt32 (_:sbyte     , _:FromInt32) = fun (x:int32) -> sbyte      (int x)
    static member        FromInt32 (_:int16     , _:FromInt32) = fun (x:int32) -> int16      (int x)
    static member        FromInt32 (_:byte      , _:FromInt32) = fun (x:int32) -> byte       (int x)
    static member        FromInt32 (_:uint16    , _:FromInt32) = fun (x:int32) -> uint16     (int x)
    static member        FromInt32 (_:uint32    , _:FromInt32) = fun (x:int32) -> uint32     (int x)
    static member        FromInt32 (_:uint64    , _:FromInt32) = fun (x:int32) -> uint64          x
    static member        FromInt32 (_:float32   , _:FromInt32) = fun (x:int32) -> float32    (int x)
    static member        FromInt32 (_:decimal   , _:FromInt32) = fun (x:int32) -> decimal    (int x)
#endif

    static member inline Invoke (x:int32)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromInt32: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromInt32.Instance x




type GenericOne() =
    inherit Default1()
    static member val Instance = GenericOne()
    static member inline One (_:'t             ,_:Default1  ) = FromInt32.Invoke 1 :'t
    static member inline One (_:'t             ,_:GenericOne) = LanguagePrimitives.GenericOne :'t
    static member inline One (_:^t when ^t: null and ^t: struct, _:GenericOne) = id

    static member inline Invoke ()   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member One: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call GenericOne.Instance

type GenericZero() =
    inherit Default1()
    static member val Instance = GenericZero()
    static member inline Zero (_:'t             ,_:Default1)    = FromInt32.Invoke 0 :'t
    static member inline Zero (_:'t             ,_:GenericZero) = LanguagePrimitives.GenericZero :'t
    static member inline Zero (_:^t when ^t: null and ^t: struct, _:GenericZero) = id

    static member inline Invoke ()   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Zero: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call GenericZero.Instance


type Abs() =
    static member val Instance = Abs()
    static member inline Abs (_:^t when ^t: null and ^t: struct, _:Abs) = id
    static member inline Abs (x:'t        , [<Optional>]impl) = abs x
    static member        Abs (x:byte      , [<Optional>]impl) =     x
    static member        Abs (x:uint16    , [<Optional>]impl) =     x
    static member        Abs (x:uint32    , [<Optional>]impl) =     x
    static member        Abs (x:uint64    , [<Optional>]impl) =     x
    static member        Abs (x:unativeint, [<Optional>]impl) =     x
#if NOTNET35
    static member        Abs (x:Complex   , [<Optional>]impl :Abs ) = Complex(x.Magnitude, 0.0)
#endif

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Abs: _*_ -> _) b, a)
        call_2 (Abs.Instance, x)

// TODO add a default, review the existing one
type Signum() =
    static member val Instance = Signum()
    static member inline Signum (_:^t when ^t: null and ^t: struct) = id
    static member inline Signum (x:'t        ) = FromBigInt.Invoke (bigint (sign x)) :'t
    static member        Signum (x:byte      ) = if x = 0uy then 0uy else 1uy
    static member        Signum (x:uint16    ) = if x = 0us then 0us else 1us
    static member        Signum (x:uint32    ) = if x = 0u  then 0u  else 1u
    static member        Signum (x:uint64    ) = if x = 0UL then 0UL else 1UL
    static member        Signum (x:unativeint) = if x = 0un then 0un else 1un
#if NOTNET35
    static member        Signum (x:Complex   ) = if x.Magnitude = 0. then Complex.Zero else Complex (x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
#endif

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Signum: _ -> _) b)
        call_2 (Signum.Instance, x)


type Negate() =
    static member val Instance = Negate()
    static member inline Negate (_:^t when ^t: null and ^t: struct) = id
    static member inline Negate (x:'t        ) = -x
    static member        Negate (x:byte      ) = 0uy - x
    static member        Negate (x:uint16    ) = 0us - x
    static member        Negate (x:uint32    ) = 0u  - x
    static member        Negate (x:uint64    ) = 0UL - x
    static member        Negate (x:unativeint) = 0un - x

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Negate: _ -> _) b)
        call_2 (Negate.Instance, x)


[<Extension; Sealed>]
type DivRem() =
    inherit Default1()
    static member val Instance = DivRem()
    static member inline DivRem (x:^t when ^t: null and ^t: struct, y:^t, thisclass:DivRem) = (x, y)
    [<Extension>]static member inline DivRem (D:'T, d:'T, [<Optional>]impl:Default1) = let q = D / d in q,  D - q * d
    [<Extension>]static member inline DivRem (D:'T, d:'T, [<Optional>]impl:DivRem  ) =
                    let mutable r = Unchecked.defaultof<'T>
                    (^T: (static member DivRem: _ * _ -> _ -> _) (D, d, &r)), r

    static member inline Invoke (D:'T) (d:'T) :'T*'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member DivRem: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b, c:'c) = call_3 (a, b, c)
        call (DivRem.Instance, D, d)    



// Integral class ---------------------------------------------------------

[<Extension; Sealed>]
type ToBigInt() =
    static member val Instance = ToBigInt()
    [<Extension>]static member        ToBigInt (x:sbyte     ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:int16     ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:int32     ) = bigint      x
    [<Extension>]static member        ToBigInt (x:int64     ) = bigint      x
    [<Extension>]static member        ToBigInt (x:nativeint ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:byte      ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:uint16    ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:unativeint) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:bigint    ) =             x
#if NOTNET35
    [<Extension>]static member        ToBigInt (x:uint32    ) = bigint      x
    [<Extension>]static member        ToBigInt (x:uint64    ) = bigint      x
#else
    [<Extension>]static member        ToBigInt (x:uint32    ) = bigint (int x)
    [<Extension>]static member        ToBigInt (x:uint64    ) = bigint (int64 x)
#endif

    static member inline Invoke    (x:'Integral) :bigint =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToBigInt: _ -> _) b)
        call_2 (ToBigInt.Instance, x)

open System.Numerics

module internal Numerics =

    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b

    let inline internal fromIntegral (x:'Integral) :'Num = (FromBigInt.Invoke << ToBigInt.Invoke) x

    let inline internal zero() = GenericZero.Invoke()
    let inline internal one()  = GenericOne.Invoke()





namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open Numerics
open FsControl.Core
open FsControl.Core.Types
open FsControl.Core.TypeMethods

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Numerics

// Fractional class -------------------------------------------------------
(* Code removed. May be it can be defined by using the methods .Numerator and .Denominator *)


// RealFrac class ---------------------------------------------------------
(* Code removed. Will see later if we need those methods *)


// Real class -------------------------------------------------------------
(* Code removed. May be it can be defined by using the methods FromIntFraction , FromBigIntFraction *)


// Floating class ---------------------------------------------------------


type Pi() =
    static member val Instance = Pi()
    static member        Pi (_:float32 , _:Pi      ) = 3.14159274f
    static member        Pi (_:float   , _:Pi      ) = System.Math.PI

#if NOTNET35
    static member Pi (_:Complex, _:Pi) = Complex(System.Math.PI, 0.0)
#endif

    static member inline Invoke() :'Floating =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pi: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Pi.Instance


// Bounded class ----------------------------------------------------------

open System

type MinValue() =
    static member val Instance = MinValue()
    static member MinValue (_:unit          , _:MinValue) = ()
    static member MinValue (_:bool          , _:MinValue) = false
    static member MinValue (_:char          , _:MinValue) = Char.MinValue
    static member MinValue (_:byte          , _:MinValue) = Byte.MinValue
    static member MinValue (_:sbyte         , _:MinValue) = SByte.MinValue
    static member MinValue (_:float         , _:MinValue) = Double.MinValue
    static member MinValue (_:int16         , _:MinValue) = Int16.MinValue
    static member MinValue (_:int           , _:MinValue) = Int32.MinValue
    static member MinValue (_:int64         , _:MinValue) = Int64.MinValue
    static member MinValue (_:float32       , _:MinValue) = Single.MinValue
    static member MinValue (_:uint16        , _:MinValue) = UInt16.MinValue
    static member MinValue (_:uint32        , _:MinValue) = UInt32.MinValue
    static member MinValue (_:uint64        , _:MinValue) = UInt64.MinValue
    static member MinValue (_:decimal       , _:MinValue) = Decimal.MinValue
    static member MinValue (_:DateTime      , _:MinValue) = DateTime.MinValue
    static member MinValue (_:DateTimeOffset, _:MinValue) = DateTimeOffset.MinValue
    static member MinValue (_:TimeSpan      , _:MinValue) = TimeSpan.MinValue

    static member inline Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MinValue: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call MinValue.Instance

    static member inline MinValue ((_:'a*'b                  ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c               ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d            ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e         ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f      ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f*'g   ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f*'g*'h), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())

type MaxValue() =
    static member val Instance = MaxValue()
    static member MaxValue (_:unit          , _:MaxValue) = ()
    static member MaxValue (_:bool          , _:MaxValue) = true
    static member MaxValue (_:char          , _:MaxValue) = Char.MaxValue
    static member MaxValue (_:byte          , _:MaxValue) = Byte.MaxValue
    static member MaxValue (_:sbyte         , _:MaxValue) = SByte.MaxValue
    static member MaxValue (_:float         , _:MaxValue) = Double.MaxValue
    static member MaxValue (_:int16         , _:MaxValue) = Int16.MaxValue
    static member MaxValue (_:int           , _:MaxValue) = Int32.MaxValue
    static member MaxValue (_:int64         , _:MaxValue) = Int64.MaxValue
    static member MaxValue (_:float32       , _:MaxValue) = Single.MaxValue
    static member MaxValue (_:uint16        , _:MaxValue) = UInt16.MaxValue
    static member MaxValue (_:uint32        , _:MaxValue) = UInt32.MaxValue
    static member MaxValue (_:uint64        , _:MaxValue) = UInt64.MaxValue
    static member MaxValue (_:decimal       , _:MaxValue) = Decimal.MaxValue
    static member MaxValue (_:DateTime      , _:MaxValue) = DateTime.MaxValue
    static member MaxValue (_:DateTimeOffset, _:MaxValue) = DateTimeOffset.MaxValue
    static member MaxValue (_:TimeSpan      , _:MaxValue) = TimeSpan.MaxValue

    static member inline Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MaxValue: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call MaxValue.Instance

    static member inline MaxValue ((_:'a*'b                  ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c               ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d            ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e         ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f      ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f*'g   ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f*'g*'h), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())