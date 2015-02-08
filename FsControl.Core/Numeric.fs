namespace FsControl.Core.TypeMethods

open System.Numerics
open FsControl.Core.Prelude

module Num =
    type FromBigInteger = FromBigInteger with

        static member        instance (FromBigInteger, _:int32     ) = fun (x:bigint) -> int             x
        static member        instance (FromBigInteger, _:int64     ) = fun (x:bigint) -> int64           x
        static member        instance (FromBigInteger, _:nativeint ) = fun (x:bigint) -> nativeint  (int x)
        static member        instance (FromBigInteger, _:unativeint) = fun (x:bigint) -> unativeint (int x)
        static member        instance (FromBigInteger, _:bigint    ) = fun (x:bigint) ->                 x
        static member        instance (FromBigInteger, _:float     ) = fun (x:bigint) -> float           x
#if NOTNET35
        static member        instance (FromBigInteger, _:sbyte     ) = fun (x:bigint) -> sbyte           x
        static member        instance (FromBigInteger, _:int16     ) = fun (x:bigint) -> int16           x
        static member        instance (FromBigInteger, _:byte      ) = fun (x:bigint) -> byte            x
        static member        instance (FromBigInteger, _:uint16    ) = fun (x:bigint) -> uint16          x
        static member        instance (FromBigInteger, _:uint32    ) = fun (x:bigint) -> uint32          x
        static member        instance (FromBigInteger, _:uint64    ) = fun (x:bigint) -> uint64          x
        static member        instance (FromBigInteger, _:float32   ) = fun (x:bigint) -> float32         x
        static member        instance (FromBigInteger, _:decimal   ) = fun (x:bigint) -> decimal         x
        static member        instance (FromBigInteger, _:Complex   ) = fun (x:bigint) -> Complex (float  x, 0.0)
#else
        static member        instance (FromBigInteger, _:sbyte     ) = fun (x:bigint) -> sbyte      (int x)
        static member        instance (FromBigInteger, _:int16     ) = fun (x:bigint) -> int16      (int x)
        static member        instance (FromBigInteger, _:byte      ) = fun (x:bigint) -> byte       (int x)
        static member        instance (FromBigInteger, _:uint16    ) = fun (x:bigint) -> uint16     (int x)
        static member        instance (FromBigInteger, _:uint32    ) = fun (x:bigint) -> uint32     (int x)
        static member        instance (FromBigInteger, _:uint64    ) = fun (x:bigint) -> uint64     (int64 x)
        static member        instance (FromBigInteger, _:float32   ) = fun (x:bigint) -> float32    (int x)
        static member        instance (FromBigInteger, _:decimal   ) = fun (x:bigint) -> decimal    (int x)
#endif

    type Abs = Abs with
        static member inline instance (Abs, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Abs, x:'t        , _) = fun () -> abs x
        static member        instance (Abs, x:byte      , _) = fun () ->     x
        static member        instance (Abs, x:uint16    , _) = fun () ->     x
        static member        instance (Abs, x:uint32    , _) = fun () ->     x
        static member        instance (Abs, x:uint64    , _) = fun () ->     x
        static member        instance (Abs, x:unativeint, _) = fun () ->     x
#if NOTNET35
        static member        instance (Abs, x:Complex   , _) = fun () -> Complex(x.Magnitude, 0.0)
#endif

    type Signum = Signum with
        static member inline instance (Signum, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Signum, x:'t        , _) = fun () -> Inline.instance FromBigInteger (bigint (sign x)) :'t
        static member        instance (Signum, x:byte      , _) = fun () -> if x = 0uy then 0uy else 1uy
        static member        instance (Signum, x:uint16    , _) = fun () -> if x = 0us then 0us else 1us
        static member        instance (Signum, x:uint32    , _) = fun () -> if x = 0u  then 0u  else 1u
        static member        instance (Signum, x:uint64    , _) = fun () -> if x = 0UL then 0UL else 1UL
        static member        instance (Signum, x:unativeint, _) = fun () -> if x = 0un then 0un else 1un
#if NOTNET35
        static member        instance (Signum, x:Complex   , _) = fun () -> 
            if x.Magnitude = 0.0 then Complex.Zero
            else Complex (x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
#endif

    type Negate = Negate with
        static member inline instance (Negate, _:^t when ^t: null and ^t: struct, _) = fun () -> id
        static member inline instance (Negate, x:'t        , _) = fun () -> -x
        static member        instance (Negate, x:byte      , _) = fun () -> 0uy - x
        static member        instance (Negate, x:uint16    , _) = fun () -> 0us - x
        static member        instance (Negate, x:uint32    , _) = fun () -> 0u  - x
        static member        instance (Negate, x:uint64    , _) = fun () -> 0UL - x
        static member        instance (Negate, x:unativeint, _) = fun () -> 0un - x

    type DivRem() =
        inherit Typ1()
        static member inline instance (_:DivRem, x:^t when ^t: null and ^t: struct, y:^t, _) = fun () -> (x, y)
        static member inline instance (_:Typ1  , D:'T, d:'T, _:'T*'T) = fun () -> let q = D / d in q,  D - q * d
        static member inline instance (_:DivRem, D:'T, d:'T, r:'T*'T) = fun () ->
            let mutable r = Unchecked.defaultof<'T>
            (^T: (static member DivRem: _ * _ -> _ -> _) (D, d, &r)), r
    
    let DivRem = DivRem()
    let inline internal divRem (x:'T) (y:'T) :'T*'T = Inline.instance (DivRem, x, y) ()

    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b


// Integral class ---------------------------------------------------------
module Integral =
    type ToBigInteger = ToBigInteger with
        static member        instance (ToBigInteger, x:sbyte     , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:int16     , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:int32     , _) = fun () -> bigint      x
        static member        instance (ToBigInteger, x:int64     , _) = fun () -> bigint      x
        static member        instance (ToBigInteger, x:nativeint , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:byte      , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:uint16    , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:unativeint, _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:bigint    , _) = fun () ->             x
#if NOTNET35
        static member        instance (ToBigInteger, x:uint32    , _) = fun () -> bigint      x
        static member        instance (ToBigInteger, x:uint64    , _) = fun () -> bigint      x
#else
        static member        instance (ToBigInteger, x:uint32    , _) = fun () -> bigint (int x)
        static member        instance (ToBigInteger, x:uint64    , _) = fun () -> bigint (int64 x)
#endif

open System.Numerics

module internal Numerics =
    let inline internal fromBigInteger (x:bigint) :'Num = Inline.instance Num.FromBigInteger x
    let inline internal abs    (x:'Num) :'Num = Inline.instance (Num.Abs   , x) ()
    let inline internal signum (x:'Num) :'Num = Inline.instance (Num.Signum, x) ()
    let inline internal negate (x:'Num) :'Num = Inline.instance (Num.Negate, x) ()

    let inline internal toBigInteger (x:'Integral) :bigint = Inline.instance (Integral.ToBigInteger, x) ()

    let inline internal fromIntegral (x:'Integral) :'Num = (fromBigInteger << toBigInteger) x

    let inline internal G0() = fromIntegral 0
    let inline internal G1() = fromIntegral 1

    let inline internal whenIntegral a = let _ = if false then toBigInteger a else 0I in ()


    // Numeric Functions ------------------------------------------------------

    let inline internal gcd x y :'Integral =
        let zero = G0()
        let rec loop a b =
            if b = zero then a
            else loop b (a % b)
        if (x, y) = (zero, zero) then failwith "gcd 0 0 is undefined"
        else loop (abs x) (abs y)


// Ratio ------------------------------------------------------------------
namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Numerics
open Num
open Integral

module Ratio = 
    let inline internal (</) x = (|>) x
    let inline internal (/>) x = flip x

    type Ratio<'Integral> = Ratio of 'Integral * 'Integral with
        override this.ToString() =
            let (Ratio(n,d)) = this
            n.ToString() + " % " + d.ToString()

    let inline internal ratio (a:'Integral) (b:'Integral) :Ratio<'Integral> =
        whenIntegral a
        let zero = G0()
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a, b) = if b < zero then (negate a, negate b) else (a, b)
        let gcd = gcd a b
        Ratio (a / gcd, b / gcd)

    let inline internal Ratio (x,y) = x </ratio/> y

    let inline  internal numerator   (Ratio(x,_)) = x
    let inline  internal denominator (Ratio(_,x)) = x

    type Ratio<'Integral> with
        static member inline (/) (Ratio(a,b), Ratio(c,d)) = (a *. d) </ratio/> (b *. c)
                                              
        static member inline (+) (Ratio(a,b), Ratio(c,d)) = (a *. d +. c *. b) </ratio/> (b *. d)
        static member inline (-) (Ratio(a,b), Ratio(c,d)) = (a *. d -. c *. b) </ratio/> (b *. d)
        static member inline (*) (Ratio(a,b), Ratio(c,d)) = (a *. c) </ratio/> (b *. d)

    type Ratio<'RA> with static member inline instance (_:Num.Abs           , r:Ratio<_>, _) = fun () -> (abs    (numerator r)) </ratio/> (denominator r)
    type Ratio<'RA> with static member inline instance (_:Num.Signum        , r:Ratio<_>, _) = fun () -> (signum (numerator r)) </ratio/> G1()
    type Ratio<'RA> with static member inline instance (_:Num.FromBigInteger, _:Ratio<_>) = fun (x:bigint) -> Inline.instance Num.FromBigInteger x </ratio/> G1()
    type Ratio<'RA> with static member inline instance (_:Num.Negate        , r:Ratio<_>, _) = fun () -> -(numerator r) </ratio/> (denominator r)

type Rational = Ratio.Ratio<bigint>



namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open Num
open Integral
open Numerics
open FsControl.Core
open FsControl.Core.Types
open FsControl.Core.TypeMethods
open Ratio

open System.Numerics

// Fractional class -------------------------------------------------------
module Fractional =
    type FromRational = FromRational with
        static member        instance (FromRational, _:float   ) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
        static member inline instance (FromRational, _:Ratio<_>) = fun (r:Rational) -> ratio (fromIntegral  (numerator r))  (fromIntegral (denominator r))
#if NOTNET35
        static member        instance (FromRational, _:float32 ) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
        static member        instance (FromRational, _:decimal ) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
        static member        instance (FromRational, _:Complex ) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)
#else
        static member        instance (FromRational, _:float32 ) = fun (r:Rational) -> float32 (int (numerator r)) / float32 (int (denominator r))    
        static member        instance (FromRational, _:decimal ) = fun (r:Rational) -> decimal (int (numerator r)) / decimal (int (denominator r))
#endif

// RealFrac class ---------------------------------------------------------
module RealFrac =
    type ProperFraction = ProperFraction with

#if NOTNET35
        static member        instance (ProperFraction, x:float   , _) = fun () -> let t = truncate x in (bigint (decimal t), x -. t)
        static member        instance (ProperFraction, x:float32 , _) = fun () -> let t = truncate x in (bigint (decimal t), x -. t)
        static member        instance (ProperFraction, x:decimal , _) = fun () -> let t = truncate x in (bigint          t , x -. t)
#else
        static member        instance (ProperFraction, x:float   , _) = fun () -> let t = truncate x in (bigint (int (decimal t)), x -. t)
        static member        instance (ProperFraction, x:float32 , _) = fun () -> let t = truncate x in (bigint (int (decimal t)), x -. t)
        static member        instance (ProperFraction, x:decimal , _) = fun () -> let t = truncate x in (bigint (int          t ), x -. t)
#endif

        static member inline instance (ProperFraction, r:Ratio<_>, _) = fun () -> 
            let (a,b) = (numerator r, denominator r)
            let (i,f) = divRem a b
            (i, ratio f b)


// Real class -------------------------------------------------------------
module Real =
    let inline internal (</) x = (|>) x
    let inline internal (/>) x = flip x
    type ToRational = ToRational with
        static member inline instance (ToRational, r:Ratio<_>, _) = fun () -> toBigInteger (numerator r) </ratio/> toBigInteger (denominator r) :Rational
        static member inline instance (ToRational, x:'t      , _) = fun () ->
            let inline fromRational (x:Rational) :'Fractional = Inline.instance Fractional.FromRational x
            let inline whenFractional a = let _ = if false then fromRational (1I </ratio/> 1I) else a in () 
            whenFractional x
            let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
                let (a, b:'RealFrac) = Inline.instance (RealFrac.ProperFraction, x) ()
                (fromIntegral a, b)        
            let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x
            let (i:bigint,d) = properFraction x
            (i </ratio/> 1I) + (truncate (decimal d *. 1000000000000000000000000000M) </ratio/> 1000000000000000000000000000I) :Rational
        static member inline instance (ToRational, x:'t, _) = fun () -> (toBigInteger x) </ratio/> 1I


// Floating class ---------------------------------------------------------
module Floating =
    type Pi = Pi with
        static member instance (Pi, _:float32) = fun () -> 3.14159274f
        static member instance (Pi, _:float  ) = fun () -> System.Math.PI

#if NOTNET35
        static member instance (Pi, _:Complex) = fun () -> Complex(System.Math.PI, 0.0)
#endif



// Bounded class ----------------------------------------------------------

open System

module Bounded =
    type MinValue = MinValue with
        static member instance (MinValue, _:unit          ) = fun () -> ()
        static member instance (MinValue, _:bool          ) = fun () -> false
        static member instance (MinValue, _:char          ) = fun () -> Char.MinValue
        static member instance (MinValue, _:byte          ) = fun () -> Byte.MinValue
        static member instance (MinValue, _:sbyte         ) = fun () -> SByte.MinValue
        static member instance (MinValue, _:float         ) = fun () -> Double.MinValue
        static member instance (MinValue, _:int16         ) = fun () -> Int16.MinValue
        static member instance (MinValue, _:int           ) = fun () -> Int32.MinValue
        static member instance (MinValue, _:int64         ) = fun () -> Int64.MinValue
        static member instance (MinValue, _:float32       ) = fun () -> Single.MinValue
        static member instance (MinValue, _:uint16        ) = fun () -> UInt16.MinValue
        static member instance (MinValue, _:uint32        ) = fun () -> UInt32.MinValue
        static member instance (MinValue, _:uint64        ) = fun () -> UInt64.MinValue
        static member instance (MinValue, _:decimal       ) = fun () -> Decimal.MinValue
        static member instance (MinValue, _:DateTime      ) = fun () -> DateTime.MinValue
        static member instance (MinValue, _:DateTimeOffset) = fun () -> DateTimeOffset.MinValue
        static member instance (MinValue, _:TimeSpan      ) = fun () -> TimeSpan.MinValue

    let inline internal minValue() = Inline.instance MinValue ()

    type MinValue with
        static member inline instance (MinValue, (_:'a*'b                  )) = fun () -> (minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c               )) = fun () -> (minValue(), minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c*'d            )) = fun () -> (minValue(), minValue(), minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c*'d*'e         )) = fun () -> (minValue(), minValue(), minValue(), minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c*'d*'e*'f      )) = fun () -> (minValue(), minValue(), minValue(), minValue(), minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c*'d*'e*'f*'g   )) = fun () -> (minValue(), minValue(), minValue(), minValue(), minValue(), minValue(), minValue())
        static member inline instance (MinValue, (_:'a*'b*'c*'d*'e*'f*'g*'h)) = fun () -> (minValue(), minValue(), minValue(), minValue(), minValue(), minValue(), minValue(), minValue())

    type MaxValue = MaxValue with
        static member instance (MaxValue, _:unit          ) = fun () -> ()
        static member instance (MaxValue, _:bool          ) = fun () -> true
        static member instance (MaxValue, _:char          ) = fun () -> Char.MaxValue
        static member instance (MaxValue, _:byte          ) = fun () -> Byte.MaxValue
        static member instance (MaxValue, _:sbyte         ) = fun () -> SByte.MaxValue
        static member instance (MaxValue, _:float         ) = fun () -> Double.MaxValue
        static member instance (MaxValue, _:int16         ) = fun () -> Int16.MaxValue
        static member instance (MaxValue, _:int           ) = fun () -> Int32.MaxValue
        static member instance (MaxValue, _:int64         ) = fun () -> Int64.MaxValue
        static member instance (MaxValue, _:float32       ) = fun () -> Single.MaxValue
        static member instance (MaxValue, _:uint16        ) = fun () -> UInt16.MaxValue
        static member instance (MaxValue, _:uint32        ) = fun () -> UInt32.MaxValue
        static member instance (MaxValue, _:uint64        ) = fun () -> UInt64.MaxValue
        static member instance (MaxValue, _:decimal       ) = fun () -> Decimal.MaxValue
        static member instance (MaxValue, _:DateTime      ) = fun () -> DateTime.MaxValue
        static member instance (MaxValue, _:DateTimeOffset) = fun () -> DateTimeOffset.MaxValue
        static member instance (MaxValue, _:TimeSpan      ) = fun () -> TimeSpan.MaxValue

    let inline internal maxValue() = Inline.instance MaxValue ()

    type MaxValue with
        static member inline instance (MaxValue, (_:'a*'b                  )) = fun () -> (maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c               )) = fun () -> (maxValue(), maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c*'d            )) = fun () -> (maxValue(), maxValue(), maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c*'d*'e         )) = fun () -> (maxValue(), maxValue(), maxValue(), maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c*'d*'e*'f      )) = fun () -> (maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c*'d*'e*'f*'g   )) = fun () -> (maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue())
        static member inline instance (MaxValue, (_:'a*'b*'c*'d*'e*'f*'g*'h)) = fun () -> (maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue(), maxValue())