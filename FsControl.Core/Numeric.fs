#nowarn "77" 
// Warn FS0077 -> Member constraints with the name 'Abs' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// But the first Abs overload relies in the standard abs function which handle all those simulated cases.

namespace FsControl.Core.TypeMethods

open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


type FromBigInt =
    inherit Default1
    static member inline FromBigInt (_:^R        ,_:Default4  ) = fun (x:bigint) -> Explicit.Invoke x         :^R
    static member inline FromBigInt (_:^R        ,_:Default3  ) = fun (x:bigint) -> Implicit.Invoke (int64 x) :^R
    static member inline FromBigInt (_:^R        ,_:Default2  ) = fun (x:bigint) -> Implicit.Invoke x         :^R
    static member inline FromBigInt (_:^R        ,_:Default1  ) = fun (x:bigint) -> (^R: (static member FromBigInt: _ -> ^R) x)
    static member inline FromBigInt (_:Default1  ,_:Default1  ) = fun (x:bigint) -> (^R: (static member FromBigInt: _ -> ^R) x)
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
#else
    static member        FromBigInt (_:sbyte     ,_:FromBigInt) = fun (x:bigint) -> sbyte      (int x)
    static member        FromBigInt (_:int16     ,_:FromBigInt) = fun (x:bigint) -> int16      (int x)
    static member        FromBigInt (_:byte      ,_:FromBigInt) = fun (x:bigint) -> byte       (int x)
    static member        FromBigInt (_:uint16    ,_:FromBigInt) = fun (x:bigint) -> uint16     (int x)
    static member        FromBigInt (_:uint32    ,_:FromBigInt) = fun (x:bigint) -> uint32     (int x)
    static member        FromBigInt (_:uint64    ,_:FromBigInt) = fun (x:bigint) -> uint64     (int64 x)
    static member        FromBigInt (_:float32   ,_:FromBigInt) = fun (x:bigint) -> float32    (int x)
    static member        FromBigInt (_:decimal   ,_:FromBigInt) = fun (x:bigint) -> decimal    (int x)
#endif

    static member inline Invoke (x:bigint)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInt: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<FromBigInt> x

type FromInt64 =
    inherit Default1
    static member inline FromInt64 (_:^R        ,_:Default4  ) = fun (x:int64) -> Explicit.Invoke x            : ^R
    static member inline FromInt64 (_:^R        ,_:Default3  ) = fun (x:int64) -> FromBigInt.Invoke (bigint x) : ^R
    static member inline FromInt64 (_:^R        ,_:Default2  ) = fun (x:int64) -> Implicit.Invoke x            : ^R
    static member inline FromInt64 (_:^R        ,_:Default1  ) = fun (x:int64) -> (^R: (static member FromInt64: _ -> ^R) x)
    static member inline FromInt64 (_:Default1  ,_:Default1  ) = fun (x:int64) -> (^R: (static member FromInt64: _ -> ^R) x)
    static member        FromInt64 (_:int32     , _:FromInt64) = fun (x:int64) -> int32           x
    static member        FromInt64 (_:int64     , _:FromInt64) = fun (x:int64) ->                 x
    static member        FromInt64 (_:nativeint , _:FromInt64) = fun (x:int64) -> nativeint  (int x)
    static member        FromInt64 (_:unativeint, _:FromInt64) = fun (x:int64) -> unativeint (int x)
    static member        FromInt64 (_:bigint    , _:FromInt64) = fun (x:int64) -> bigint          x
    static member        FromInt64 (_:float     , _:FromInt64) = fun (x:int64) -> float           x
#if NOTNET35
    static member        FromInt64 (_:float32   , _:FromInt64) = fun (x:int64) -> float32         x
    static member        FromInt64 (_:decimal   , _:FromInt64) = fun (x:int64) -> decimal         x
    static member        FromInt64 (_:sbyte     , _:FromInt64) = fun (x:int64) -> sbyte           x
    static member        FromInt64 (_:int16     , _:FromInt64) = fun (x:int64) -> int16           x
    static member        FromInt64 (_:byte      , _:FromInt64) = fun (x:int64) -> byte            x
    static member        FromInt64 (_:uint16    , _:FromInt64) = fun (x:int64) -> uint16          x
    static member        FromInt64 (_:uint32    , _:FromInt64) = fun (x:int64) -> uint32          x
#else
    static member        FromInt64 (_:float32   , _:FromInt64) = fun (x:int64) -> float32    (int x)
    static member        FromInt64 (_:decimal   , _:FromInt64) = fun (x:int64) -> decimal    (int x)
    static member        FromInt64 (_:sbyte     , _:FromInt64) = fun (x:int64) -> sbyte      (int x)
    static member        FromInt64 (_:int16     , _:FromInt64) = fun (x:int64) -> int16      (int x)
    static member        FromInt64 (_:byte      , _:FromInt64) = fun (x:int64) -> byte       (int x)
    static member        FromInt64 (_:uint16    , _:FromInt64) = fun (x:int64) -> uint16     (int x)
    static member        FromInt64 (_:uint32    , _:FromInt64) = fun (x:int64) -> uint32     (int x)
#endif
    static member        FromInt64 (_:uint64    , _:FromInt64) = fun (x:int64) -> uint64          x

    static member inline Invoke (x:int64)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromInt64: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<FromInt64> x


type FromInt32 =
    inherit Default1
    static member inline FromInt32 (_:^R        ,_:Default4  ) = fun (x:int32) -> Explicit.Invoke x          : ^R
    static member inline FromInt32 (_:^R        ,_:Default3  ) = fun (x:int32) -> FromInt64.Invoke (int64 x) : ^R
    static member inline FromInt32 (_:^R        ,_:Default2  ) = fun (x:int32) -> Implicit.Invoke x          : ^R
    static member inline FromInt32 (_:^R        ,_:Default1  ) = fun (x:int32) -> (^R: (static member FromInt32: _ -> ^R) x)
    static member inline FromInt32 (_:Default1  ,_:Default1  ) = fun (x:int32) -> (^R: (static member FromInt32: _ -> ^R) x)
    static member        FromInt32 (_:int32     , _:FromInt32) = fun (x:int32) ->                 x
    static member        FromInt32 (_:int64     , _:FromInt32) = fun (x:int32) -> int64           x
    static member        FromInt32 (_:nativeint , _:FromInt32) = fun (x:int32) -> nativeint  (int x)
    static member        FromInt32 (_:unativeint, _:FromInt32) = fun (x:int32) -> unativeint (int x)
    static member        FromInt32 (_:bigint    , _:FromInt32) = fun (x:int32) -> bigint          x
    static member        FromInt32 (_:float     , _:FromInt32) = fun (x:int32) -> float           x
    static member        FromInt32 (_:sbyte     , _:FromInt32) = fun (x:int32) -> sbyte           x
    static member        FromInt32 (_:int16     , _:FromInt32) = fun (x:int32) -> int16           x
    static member        FromInt32 (_:byte      , _:FromInt32) = fun (x:int32) -> byte            x
    static member        FromInt32 (_:uint16    , _:FromInt32) = fun (x:int32) -> uint16          x
    static member        FromInt32 (_:uint32    , _:FromInt32) = fun (x:int32) -> uint32          x
    static member        FromInt32 (_:uint64    , _:FromInt32) = fun (x:int32) -> uint64          x
    static member        FromInt32 (_:float32   , _:FromInt32) = fun (x:int32) -> float32         x
    static member        FromInt32 (_:decimal   , _:FromInt32) = fun (x:int32) -> decimal         x

    static member inline Invoke (x:int32)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromInt32: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<FromInt32> x




type GenericOne =
    inherit Default1
    static member inline One (_:'t             ,_:Default1  ) = FromInt32.Invoke 1 :'t
    static member inline One (_:'t             ,_:GenericOne) = LanguagePrimitives.GenericOne :'t
    static member inline One (_:^t when ^t: null and ^t: struct, _:GenericOne) = id

    static member inline Invoke ()   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member One: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<GenericOne>

type GenericZero =
    inherit Default1
    static member inline Zero (_:'t             ,_:Default1)    = FromInt32.Invoke 0 :'t
    static member inline Zero (_:'t             ,_:GenericZero) = LanguagePrimitives.GenericZero :'t
    static member inline Zero (_:^t when ^t: null and ^t: struct, _:GenericZero) = id

    static member inline Invoke ()   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Zero: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<GenericZero>



type Abs =
    inherit Default1
    static member inline Abs (x:'t        , _:Default2) = (Explicit.Invoke ((^t ) : (static member Abs: ^t -> ^u) x)) :'t
    static member inline Abs (x:'t        , _:Default1) = (Implicit.Invoke ((^t ) : (static member Abs: ^t -> ^u) x)) :'t
    static member inline Abs (x:'t        , _:Abs) = abs x :'t
    static member inline Abs (x:Default1  , _:Abs) = fun (x) -> (^R: (static member Abs: _ -> ^R) x)

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Abs: ^b*_ -> ^t) b, a)
        call_2 (Unchecked.defaultof<Abs>, x)

type Abs' =
    inherit Abs
    static member        Abs (x:byte      , _:Abs') =     x
    static member        Abs (x:uint16    , _:Abs') =     x
    static member        Abs (x:uint32    , _:Abs') =     x
    static member        Abs (x:uint64    , _:Abs') =     x
    static member        Abs (x:unativeint, _:Abs') =     x

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Abs: ^b*_ -> ^t) b, a)
        call_2 (Unchecked.defaultof<Abs'>, x)



type Signum =
    inherit Default1
    static member inline Signum (x:'t        , _:Default2) =
        let zero = GenericZero.Invoke()
        if x = zero then zero
        else x / Abs.Invoke x :'t
    static member inline Signum (_:^t when ^t: null and ^t: struct, _:Default1) = id
    static member inline Signum (x:'t        , _:Default1) = FromInt32.Invoke (sign x) :'t

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Signum: _*_ -> _) b, a)
        call_2 (Unchecked.defaultof<Signum>, x)

type Signum' =
    inherit Signum
    static member        Signum (x:byte      , _:Signum') = if x = 0uy then 0uy else 1uy
    static member        Signum (x:uint16    , _:Signum') = if x = 0us then 0us else 1us
    static member        Signum (x:uint32    , _:Signum') = if x = 0u  then 0u  else 1u
    static member        Signum (x:uint64    , _:Signum') = if x = 0UL then 0UL else 1UL
    static member        Signum (x:unativeint, _:Signum') = if x = 0un then 0un else 1un

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Signum: _*_ -> _) b, a)
        call_2 (Unchecked.defaultof<Signum'>, x)



type Negate' =
    static member inline Negate (_:^t when ^t: null and ^t: struct) = id
    static member inline Negate (x:'t        ) = -x
    static member        Negate (x:byte      ) = 0uy - x
    static member        Negate (x:uint16    ) = 0us - x
    static member        Negate (x:uint32    ) = 0u  - x
    static member        Negate (x:uint64    ) = 0UL - x
    static member        Negate (x:unativeint) = 0un - x

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Negate: _ -> _) b)
        call_2 (Unchecked.defaultof<Negate'>, x)


[<Extension; Sealed>]
type DivRem =
    inherit Default1
    static member inline DivRem (x:^t when ^t: null and ^t: struct, y:^t, thisclass:DivRem) = (x, y)
    [<Extension>]static member inline DivRem (D:'T, d:'T, [<Optional>]impl:Default1) = let q = D / d in q,  D - q * d
    [<Extension>]static member inline DivRem (D:'T, d:'T, [<Optional>]impl:DivRem  ) =
                    let mutable r = Unchecked.defaultof<'T>
                    (^T: (static member DivRem: _ * _ -> _ -> _) (D, d, &r)), r

    static member inline Invoke (D:'T) (d:'T) :'T*'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member DivRem: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b, c:'c) = call_3 (a, b, c)
        call (Unchecked.defaultof<DivRem>, D, d)    



// Integral class ---------------------------------------------------------

[<Extension; Sealed>]
type ToBigInt =
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
        call_2 (Unchecked.defaultof<ToBigInt>, x)


module internal Numerics =

    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b

    let inline internal fromIntegral (x:'Integral) :'Num = (FromBigInt.Invoke << ToBigInt.Invoke) x

    let inline internal zero() = GenericZero.Invoke()
    let inline internal one()  = GenericOne.Invoke()





namespace FsControl.Core.TypeMethods

open Numerics
open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open FsControl.Core.Types
open FsControl.Core.TypeMethods

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

// Fractional class -------------------------------------------------------
(* Code removed. May be it can be defined by using the methods .Numerator and .Denominator *)


// RealFrac class ---------------------------------------------------------
(* Code removed. Will see later if we need those methods *)


// Real class -------------------------------------------------------------
(* Code removed. May be it can be defined by using the methods FromIntFraction , FromBigIntFraction *)


// Floating class ---------------------------------------------------------


type Pi =
    inherit Default1
    static member inline Pi (_:^R      , _:Default3) = Implicit.Invoke 3.14159274f    :^R
    static member inline Pi (_:^R      , _:Default2) = Implicit.Invoke System.Math.PI :^R
    static member inline Pi (_:^R      , _:Default1) = (^R: (static member PI:  ^R) ())
    static member inline Pi (_:Default1, _:Default1) = (^R: (static member PI:  ^R) ())
    static member        Pi (_:float32 , _:Pi      ) = 3.14159274f
    static member        Pi (_:float   , _:Pi      ) = System.Math.PI

    static member inline Invoke() :'Floating =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pi: _*_ -> _) b, a)
        let inline call (a:'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Pi>


// Bounded class ----------------------------------------------------------

open System
// TODO: can we have a (working) default ? It's a field, maybe we should call to a property.

type MinValue =
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
        call Unchecked.defaultof<MinValue>

    static member inline MinValue ((_:'a*'b                  ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c               ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d            ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e         ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f      ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f*'g   ), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue ((_:'a*'b*'c*'d*'e*'f*'g*'h), _:MinValue) = (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())

type MaxValue =
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
        call Unchecked.defaultof<MaxValue>

    static member inline MaxValue ((_:'a*'b                  ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c               ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d            ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e         ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f      ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f*'g   ), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue ((_:'a*'b*'c*'d*'e*'f*'g*'h), _:MaxValue) = (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())