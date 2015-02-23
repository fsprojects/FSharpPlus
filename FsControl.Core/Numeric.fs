namespace FsControl.Core.TypeMethods

open System.Numerics
open FsControl.Core.Prelude
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


type FromBigInteger() =
    static member val Instance = FromBigInteger()
    static member        FromBigInteger (_:int32     ) = fun (x:bigint) -> int             x
    static member        FromBigInteger (_:int64     ) = fun (x:bigint) -> int64           x
    static member        FromBigInteger (_:nativeint ) = fun (x:bigint) -> nativeint  (int x)
    static member        FromBigInteger (_:unativeint) = fun (x:bigint) -> unativeint (int x)
    static member        FromBigInteger (_:bigint    ) = fun (x:bigint) ->                 x
    static member        FromBigInteger (_:float     ) = fun (x:bigint) -> float           x
#if NOTNET35
    static member        FromBigInteger (_:sbyte     ) = fun (x:bigint) -> sbyte           x
    static member        FromBigInteger (_:int16     ) = fun (x:bigint) -> int16           x
    static member        FromBigInteger (_:byte      ) = fun (x:bigint) -> byte            x
    static member        FromBigInteger (_:uint16    ) = fun (x:bigint) -> uint16          x
    static member        FromBigInteger (_:uint32    ) = fun (x:bigint) -> uint32          x
    static member        FromBigInteger (_:uint64    ) = fun (x:bigint) -> uint64          x
    static member        FromBigInteger (_:float32   ) = fun (x:bigint) -> float32         x
    static member        FromBigInteger (_:decimal   ) = fun (x:bigint) -> decimal         x
    static member        FromBigInteger (_:Complex   ) = fun (x:bigint) -> Complex (float  x, 0.0)
#else
    static member        FromBigInteger (_:sbyte     ) = fun (x:bigint) -> sbyte      (int x)
    static member        FromBigInteger (_:int16     ) = fun (x:bigint) -> int16      (int x)
    static member        FromBigInteger (_:byte      ) = fun (x:bigint) -> byte       (int x)
    static member        FromBigInteger (_:uint16    ) = fun (x:bigint) -> uint16     (int x)
    static member        FromBigInteger (_:uint32    ) = fun (x:bigint) -> uint32     (int x)
    static member        FromBigInteger (_:uint64    ) = fun (x:bigint) -> uint64     (int64 x)
    static member        FromBigInteger (_:float32   ) = fun (x:bigint) -> float32    (int x)
    static member        FromBigInteger (_:decimal   ) = fun (x:bigint) -> decimal    (int x)
#endif

    static member inline Invoke (x:bigint)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInteger: _ -> _) b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromBigInteger.Instance x

type Abs() =
    static member val Instance = Abs()
    static member inline Abs (_:^t when ^t: null and ^t: struct, _:Abs) = id
    static member inline Abs (x:'t        , [<Optional>]impl :Abs) = abs x
    static member        Abs (x:byte      , [<Optional>]impl :Abs) =     x
    static member        Abs (x:uint16    , [<Optional>]impl :Abs) =     x
    static member        Abs (x:uint32    , [<Optional>]impl :Abs) =     x
    static member        Abs (x:uint64    , [<Optional>]impl :Abs) =     x
    static member        Abs (x:unativeint, [<Optional>]impl :Abs) =     x
#if NOTNET35
    static member        Abs (x:Complex   , [<Optional>]impl:Abs ) = Complex(x.Magnitude, 0.0)
#endif

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Abs: _*_ -> _) b, a)
        call_2 (Abs.Instance, x)

type Signum() =
    static member val Instance = Signum()
    static member inline Signum (_:^t when ^t: null and ^t: struct) = id
    static member inline Signum (x:'t        ) = FromBigInteger.Invoke (bigint (sign x)) :'t
    static member        Signum (x:byte      ) = if x = 0uy then 0uy else 1uy
    static member        Signum (x:uint16    ) = if x = 0us then 0us else 1us
    static member        Signum (x:uint32    ) = if x = 0u  then 0u  else 1u
    static member        Signum (x:uint64    ) = if x = 0UL then 0UL else 1UL
    static member        Signum (x:unativeint) = if x = 0un then 0un else 1un
//#if NOTNET35
    static member        Signum (x:Complex   ) = if x.Magnitude = 0. then Complex.Zero else Complex (x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
//#endif

    static member inline Invoke (x:'Num) :'Num =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Sign: _ -> _) b)
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
type ToBigInteger() =
    static member val Instance = ToBigInteger()
    [<Extension>]static member        ToBigInteger (x:sbyte     ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:int16     ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:int32     ) = bigint      x
    [<Extension>]static member        ToBigInteger (x:int64     ) = bigint      x
    [<Extension>]static member        ToBigInteger (x:nativeint ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:byte      ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:uint16    ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:unativeint) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:bigint    ) =             x
#if NOTNET35
    [<Extension>]static member        ToBigInteger (x:uint32    ) = bigint      x
    [<Extension>]static member        ToBigInteger (x:uint64    ) = bigint      x
#else
    [<Extension>]static member        ToBigInteger (x:uint32    ) = bigint (int x)
    [<Extension>]static member        ToBigInteger (x:uint64    ) = bigint (int64 x)
#endif

    static member inline Invoke    (x:'Integral) :bigint =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToBigInteger: _ -> _) b)
        call_2 (ToBigInteger.Instance, x)

open System.Numerics

module internal Numerics =

    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b

    let inline internal fromIntegral (x:'Integral) :'Num = (FromBigInteger.Invoke << ToBigInteger.Invoke) x

    let inline internal G0() = fromIntegral 0
    let inline internal G1() = fromIntegral 1

    let inline internal whenIntegral a = let _ = if false then ToBigInteger.Invoke a else 0I in ()


    // Numeric Functions ------------------------------------------------------

    let inline internal gcd x y :'Integral =
        let zero = G0()
        let rec loop a b =
            if b = zero then a
            else loop b (a % b)
        if (x, y) = (zero, zero) then failwith "gcd 0 0 is undefined"
        else loop (Abs.Invoke x) (Abs.Invoke y)


// Ratio ------------------------------------------------------------------
namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Numerics


module Ratio = 
    let inline internal (</) x = (|>) x
    let inline internal (/>) x = flip x

    type Ratio<'Integral> = //Ratio of 'Integral * 'Integral with
        struct
            val Numerator   :'Integral
            val Denominator :'Integral
            new (numerator: 'Integral, denominator: 'Integral) = {Numerator = numerator; Denominator = denominator}
        end
        override this.ToString() = this.Numerator.ToString() + " % " + this.Denominator.ToString()

    let inline internal ratio (a:'Integral) (b:'Integral) :Ratio<'Integral> =
        whenIntegral a
        let zero = G0()
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a, b) = if b < zero then (Negate.Invoke a, Negate.Invoke b) else (a, b)
        let gcd = gcd a b
        Ratio (a / gcd, b / gcd)

    let inline internal Ratio (x,y) = x </ratio/> y

    let inline internal numerator   (r:Ratio<_>) = r.Numerator
    let inline internal denominator (r:Ratio<_>) = r.Denominator

    type Ratio<'Integral> with
        static member inline (/) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator) </ratio/> (a.Numerator *. b.Numerator)                                              
        static member inline (+) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator +. b.Numerator *. a.Numerator) </ratio/> (a.Numerator *. b.Denominator)
        static member inline (-) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator -. b.Numerator *. a.Numerator) </ratio/> (a.Numerator *. b.Denominator)
        static member inline (*) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Numerator) </ratio/> (a.Numerator *. b.Denominator)

    //type Ratio<'RA> with 
        static member inline Abs            (r:Ratio<_>) = (Abs.Invoke    (numerator r)) </ratio/> (denominator r)
        static member inline Signum         (r:Ratio<_>) = (Signum.Invoke (numerator r)) </ratio/> G1()
        static member inline FromBigInteger (_:Ratio<_>) = fun (x:bigint) -> FromBigInteger.Invoke x </ratio/> G1()
        static member inline Negate         (r:Ratio<_>) = -(numerator r) </ratio/> (denominator r)


    let (|Ratio|) (ratio:Ratio<_>) = (ratio.Numerator, ratio.Denominator)

type Rational = Ratio.Ratio<bigint>



namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open Numerics
open FsControl.Core
open FsControl.Core.Types
open FsControl.Core.TypeMethods
open Ratio

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Numerics

// Fractional class -------------------------------------------------------

type FromRational() =
    inherit Default1()
    static member val Instance = FromRational()
    // add -> static member inline FromRational (_:'R      , _:Default1    ) = fun (r:Rational) -> (^R: (static member FromRational: _ -> _) r)
    static member        FromRational (_:float   , _:FromRational) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
    static member inline FromRational (_:Ratio<_>, _:FromRational) = fun (r:Rational) -> ratio (fromIntegral  (numerator r))  (fromIntegral (denominator r))
#if NOTNET35
    static member        FromRational (_:float32 , _:FromRational) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
    static member        FromRational (_:decimal , _:FromRational) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
    static member        FromRational (_:Complex , _:FromRational) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)
#else
    static member        FromRational (_:float32 , _:FromRational) = fun (r:Rational) -> float32 (int (numerator r)) / float32 (int (denominator r))    
    static member        FromRational (_:decimal , _:FromRational) = fun (r:Rational) -> decimal (int (numerator r)) / decimal (int (denominator r))
#endif
    static member inline Invoke (x:Rational) :'Fractional =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromRational: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromRational.Instance x

// RealFrac class ---------------------------------------------------------


type ProperFraction() =
    static member val Instance = ProperFraction()

#if NOTNET35
    static member        ProperFraction (_:ProperFraction, x:float   , _) = let t = truncate x in (bigint (decimal t), x -. t)
    static member        ProperFraction (_:ProperFraction, x:float32 , _) = let t = truncate x in (bigint (decimal t), x -. t)
    static member        ProperFraction (_:ProperFraction, x:decimal , _) = let t = truncate x in (bigint          t , x -. t)
#else
    static member        ProperFraction (_:ProperFraction, x:float   , _) = let t = truncate x in (bigint (int (decimal t)), x -. t)
    static member        ProperFraction (_:ProperFraction, x:float32 , _) = let t = truncate x in (bigint (int (decimal t)), x -. t)
    static member        ProperFraction (_:ProperFraction, x:decimal , _) = let t = truncate x in (bigint (int          t ), x -. t)
#endif

    static member inline ProperFraction (_:ProperFraction, r:Ratio<_>, _) =
        let (a,b) = (numerator r, denominator r)
        let (i,f) = DivRem.Invoke a b
        (i, ratio f b)

    static member inline Invoke x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ProperFraction: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (ProperFraction.Instance, x)

// Real class -------------------------------------------------------------

[<Extension; Sealed>]
type ToRational() =
    static member val Instance = ToRational()
    [<Extension>]static member inline ToRational (r:Ratio<_>, [<Optional>]impl:ToRational) = ToBigInteger.Invoke (numerator r) </ratio/> ToBigInteger.Invoke (denominator r) :Rational
    [<Extension>]static member inline ToRational (x:'t      , [<Optional>]impl:ToRational) = 
                    let inline fromRational (x:Rational) :'Fractional = FromRational.Invoke x
                    let inline whenFractional a = let _ = if false then fromRational (1I </ratio/> 1I) else a in () 
                    whenFractional x
                    let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
                        let (a, b:'RealFrac) = ProperFraction.Invoke x
                        (fromIntegral a, b)        
                    let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x
                    let (i:bigint,d) = ProperFraction.Invoke x
                    (i </ratio/> 1I) + (truncate (decimal d *. 1000000000000000000000000000M) </ratio/> 1000000000000000000000000000I) :Rational
    [<Extension>]static member inline ToRational (x:'t, [<Optional>]impl:ToRational) = (ToBigInteger.Invoke x) </ratio/> 1I

    static member inline Invoke (x:'Real) :Rational =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToRational: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToRational.Instance, x)

// Floating class ---------------------------------------------------------

type Pi() =
    static member val Instance = Pi()
    static member Pi (_:float32, _:Pi) = 3.14159274f
    static member Pi (_:float  , _:Pi) = System.Math.PI

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