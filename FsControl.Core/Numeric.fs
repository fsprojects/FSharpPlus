namespace FsControl.Core.TypeMethods

open System.Numerics
open FsControl.Core.Prelude


type FromBigInteger() =
    static member val Instance = FromBigInteger()
    static member        FromBigInteger (_:FromBigInteger, _:int32     ) = fun (x:bigint) -> int             x
    static member        FromBigInteger (_:FromBigInteger, _:int64     ) = fun (x:bigint) -> int64           x
    static member        FromBigInteger (_:FromBigInteger, _:nativeint ) = fun (x:bigint) -> nativeint  (int x)
    static member        FromBigInteger (_:FromBigInteger, _:unativeint) = fun (x:bigint) -> unativeint (int x)
    static member        FromBigInteger (_:FromBigInteger, _:bigint    ) = fun (x:bigint) ->                 x
    static member        FromBigInteger (_:FromBigInteger, _:float     ) = fun (x:bigint) -> float           x
#if NOTNET35
    static member        FromBigInteger (_:FromBigInteger, _:sbyte     ) = fun (x:bigint) -> sbyte           x
    static member        FromBigInteger (_:FromBigInteger, _:int16     ) = fun (x:bigint) -> int16           x
    static member        FromBigInteger (_:FromBigInteger, _:byte      ) = fun (x:bigint) -> byte            x
    static member        FromBigInteger (_:FromBigInteger, _:uint16    ) = fun (x:bigint) -> uint16          x
    static member        FromBigInteger (_:FromBigInteger, _:uint32    ) = fun (x:bigint) -> uint32          x
    static member        FromBigInteger (_:FromBigInteger, _:uint64    ) = fun (x:bigint) -> uint64          x
    static member        FromBigInteger (_:FromBigInteger, _:float32   ) = fun (x:bigint) -> float32         x
    static member        FromBigInteger (_:FromBigInteger, _:decimal   ) = fun (x:bigint) -> decimal         x
    static member        FromBigInteger (_:FromBigInteger, _:Complex   ) = fun (x:bigint) -> Complex (float  x, 0.0)
#else
    static member        FromBigInteger (_:FromBigInteger, _:sbyte     ) = fun (x:bigint) -> sbyte      (int x)
    static member        FromBigInteger (_:FromBigInteger, _:int16     ) = fun (x:bigint) -> int16      (int x)
    static member        FromBigInteger (_:FromBigInteger, _:byte      ) = fun (x:bigint) -> byte       (int x)
    static member        FromBigInteger (_:FromBigInteger, _:uint16    ) = fun (x:bigint) -> uint16     (int x)
    static member        FromBigInteger (_:FromBigInteger, _:uint32    ) = fun (x:bigint) -> uint32     (int x)
    static member        FromBigInteger (_:FromBigInteger, _:uint64    ) = fun (x:bigint) -> uint64     (int64 x)
    static member        FromBigInteger (_:FromBigInteger, _:float32   ) = fun (x:bigint) -> float32    (int x)
    static member        FromBigInteger (_:FromBigInteger, _:decimal   ) = fun (x:bigint) -> decimal    (int x)
#endif


type Abs() =
    static member val Instance = Abs()
    static member inline Abs (_:Abs, _:^t when ^t: null and ^t: struct, _) = fun () -> id
    static member inline Abs (_:Abs, x:'t        , _) = fun () -> abs x
    static member        Abs (_:Abs, x:byte      , _) = fun () ->     x
    static member        Abs (_:Abs, x:uint16    , _) = fun () ->     x
    static member        Abs (_:Abs, x:uint32    , _) = fun () ->     x
    static member        Abs (_:Abs, x:uint64    , _) = fun () ->     x
    static member        Abs (_:Abs, x:unativeint, _) = fun () ->     x
#if NOTNET35
    static member        Abs (_:Abs, x:Complex   , _) = fun () -> Complex(x.Magnitude, 0.0)
#endif

type Signum() =
    static member val Instance = Signum()
    static member inline Signum (_:Signum, _:^t when ^t: null and ^t: struct, _) = fun () -> id
    static member inline Signum (_:Signum, x:'t        , _) = fun () ->
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInteger: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance FromBigInteger.Instance (bigint (sign x)) :'t

    static member        Signum (_:Signum, x:byte      , _) = fun () -> if x = 0uy then 0uy else 1uy
    static member        Signum (_:Signum, x:uint16    , _) = fun () -> if x = 0us then 0us else 1us
    static member        Signum (_:Signum, x:uint32    , _) = fun () -> if x = 0u  then 0u  else 1u
    static member        Signum (_:Signum, x:uint64    , _) = fun () -> if x = 0UL then 0UL else 1UL
    static member        Signum (_:Signum, x:unativeint, _) = fun () -> if x = 0un then 0un else 1un
#if NOTNET35
    static member        Signum (_:Signum, x:Complex   , _) = fun () -> 
        if x.Magnitude = 0.0 then Complex.Zero
        else Complex (x.Real / x.Magnitude, x.Imaginary / x.Magnitude)
#endif


type Negate() =
    static member val Instance = Negate()
    static member inline Negate (_:Negate, _:^t when ^t: null and ^t: struct, _) = fun () -> id
    static member inline Negate (_:Negate, x:'t        , _) = fun () -> -x
    static member        Negate (_:Negate, x:byte      , _) = fun () -> 0uy - x
    static member        Negate (_:Negate, x:uint16    , _) = fun () -> 0us - x
    static member        Negate (_:Negate, x:uint32    , _) = fun () -> 0u  - x
    static member        Negate (_:Negate, x:uint64    , _) = fun () -> 0UL - x
    static member        Negate (_:Negate, x:unativeint, _) = fun () -> 0un - x


type DivRem() =
    inherit Default1()
    static member val Instance = DivRem()
    static member inline DivRem (_:DivRem  , x:^t when ^t: null and ^t: struct, y:^t, _) = fun () -> (x, y)
    static member inline DivRem (_:Default1, D:'T, d:'T, _:'T*'T) = fun () -> let q = D / d in q,  D - q * d
    static member inline DivRem (_:DivRem  , D:'T, d:'T, r:'T*'T) = fun () ->
        let mutable r = Unchecked.defaultof<'T>
        (^T: (static member DivRem: _ * _ -> _ -> _) (D, d, &r)), r

    



// Integral class ---------------------------------------------------------

type ToBigInteger() =
    static member val Instance = ToBigInteger()
    static member        ToBigInteger (_:ToBigInteger, x:sbyte     , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:int16     , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:int32     , _) = fun () -> bigint      x
    static member        ToBigInteger (_:ToBigInteger, x:int64     , _) = fun () -> bigint      x
    static member        ToBigInteger (_:ToBigInteger, x:nativeint , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:byte      , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:uint16    , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:unativeint, _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:bigint    , _) = fun () ->             x
#if NOTNET35
    static member        ToBigInteger (_:ToBigInteger, x:uint32    , _) = fun () -> bigint      x
    static member        ToBigInteger (_:ToBigInteger, x:uint64    , _) = fun () -> bigint      x
#else
    static member        ToBigInteger (_:ToBigInteger, x:uint32    , _) = fun () -> bigint (int x)
    static member        ToBigInteger (_:ToBigInteger, x:uint64    , _) = fun () -> bigint (int64 x)
#endif


open System.Numerics

module internal Numerics =

    let inline internal divRem (x:'T) (y:'T) :'T*'T =
        let inline instance_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member DivRem: _*_*_*_ -> _) a, b, c, d)
        let inline instance (a:'a, b:'b, c:'c) = fun (x:'x) -> instance_4 (a, b, c, Unchecked.defaultof<'r>) x :'r
        instance (DivRem.Instance, x, y) ()

    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b

    let inline internal fromBigInteger (x:bigint) :'Num =
        let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInteger: _*_ -> _) a, b)
        let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
        instance FromBigInteger.Instance x

    let inline internal abs    (x:'Num) :'Num =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Abs: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (Abs.Instance, x) ()

    let inline internal signum (x:'Num) :'Num =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Signum: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (Signum.Instance, x) ()

    let inline internal negate (x:'Num) :'Num =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Negate: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (Negate.Instance, x) ()

    let inline internal toBigInteger (x:'Integral) :bigint =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToBigInteger: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (ToBigInteger.Instance, x) ()

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
        let (a, b) = if b < zero then (negate a, negate b) else (a, b)
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

    type Ratio<'RA> with static member inline Abs            (_:Abs           , r:Ratio<_>, _) = fun () -> (abs    (numerator r)) </ratio/> (denominator r)
    type Ratio<'RA> with static member inline Signum         (_:Signum        , r:Ratio<_>, _) = fun () -> (signum (numerator r)) </ratio/> G1()
    type Ratio<'RA> with static member inline FromBigInteger (_:FromBigInteger, _:Ratio<_>) = fun (x:bigint) ->
                            let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInteger: _*_ -> _) a, b)
                            let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
                            instance FromBigInteger.Instance x </ratio/> G1()
    type Ratio<'RA> with static member inline Negate (_:Negate        , r:Ratio<_>, _) = fun () -> -(numerator r) </ratio/> (denominator r)

    let (|Ratio|) (ratio:Ratio<_>) = (ratio.Numerator, ratio.Denominator)

type Rational = Ratio.Ratio<bigint>



namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open Numerics
open FsControl.Core
open FsControl.Core.Types
open FsControl.Core.TypeMethods
open Ratio

open System.Numerics

// Fractional class -------------------------------------------------------

type FromRational() =
    static member val Instance = FromRational()
    static member        FromRational (_:FromRational, _:float   ) = fun (r:Rational) -> float   (numerator r) / float   (denominator r)
    static member inline FromRational (_:FromRational, _:Ratio<_>) = fun (r:Rational) -> ratio (fromIntegral  (numerator r))  (fromIntegral (denominator r))
#if NOTNET35
    static member        FromRational (_:FromRational, _:float32 ) = fun (r:Rational) -> float32 (numerator r) / float32 (denominator r)    
    static member        FromRational (_:FromRational, _:decimal ) = fun (r:Rational) -> decimal (numerator r) / decimal (denominator r)
    static member        FromRational (_:FromRational, _:Complex ) = fun (r:Rational) -> Complex(float (numerator r) / float (denominator r), 0.0)
#else
    static member        FromRational (_:FromRational, _:float32 ) = fun (r:Rational) -> float32 (int (numerator r)) / float32 (int (denominator r))    
    static member        FromRational (_:FromRational, _:decimal ) = fun (r:Rational) -> decimal (int (numerator r)) / decimal (int (denominator r))
#endif


// RealFrac class ---------------------------------------------------------


type ProperFraction() =
    static member val Instance = ProperFraction()

#if NOTNET35
    static member        ProperFraction (_:ProperFraction, x:float   , _) = fun () -> let t = truncate x in (bigint (decimal t), x -. t)
    static member        ProperFraction (_:ProperFraction, x:float32 , _) = fun () -> let t = truncate x in (bigint (decimal t), x -. t)
    static member        ProperFraction (_:ProperFraction, x:decimal , _) = fun () -> let t = truncate x in (bigint          t , x -. t)
#else
    static member        ProperFraction (_:ProperFraction, x:float   , _) = fun () -> let t = truncate x in (bigint (int (decimal t)), x -. t)
    static member        ProperFraction (_:ProperFraction, x:float32 , _) = fun () -> let t = truncate x in (bigint (int (decimal t)), x -. t)
    static member        ProperFraction (_:ProperFraction, x:decimal , _) = fun () -> let t = truncate x in (bigint (int          t ), x -. t)
#endif

    static member inline ProperFraction (_:ProperFraction, r:Ratio<_>, _) = fun () -> 
        let (a,b) = (numerator r, denominator r)
        let (i,f) = divRem a b
        (i, ratio f b)


// Real class -------------------------------------------------------------

type ToRational() =
    static member val Instance = ToRational()
    static member inline ToRational (_:ToRational, r:Ratio<_>, _) = fun () -> toBigInteger (numerator r) </ratio/> toBigInteger (denominator r) :Rational
    static member inline ToRational (_:ToRational, x:'t      , _) = fun () ->
        let inline fromRational (x:Rational) :'Fractional =
            let inline instance_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromRational: _*_ -> _) a, b)
            let inline instance (a:'a) = fun (x:'x) -> instance_2 (a, Unchecked.defaultof<'r>) x :'r
            instance FromRational.Instance x
        let inline whenFractional a = let _ = if false then fromRational (1I </ratio/> 1I) else a in () 
        whenFractional x
        let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
            let (a, b:'RealFrac) =
                let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ProperFraction: _*_*_ -> _) a, b, c)
                let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
                instance (ProperFraction.Instance, x) ()
            (fromIntegral a, b)        
        let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x
        let (i:bigint,d) = properFraction x
        (i </ratio/> 1I) + (truncate (decimal d *. 1000000000000000000000000000M) </ratio/> 1000000000000000000000000000I) :Rational
    static member inline ToRational (_:ToRational, x:'t, _) = fun () -> (toBigInteger x) </ratio/> 1I


// Floating class ---------------------------------------------------------


type Pi() =
    static member val Instance = Pi()
    static member Pi (_:Pi, _:float32) = fun () -> 3.14159274f
    static member Pi (_:Pi, _:float  ) = fun () -> System.Math.PI

#if NOTNET35
    static member Pi (_:Pi, _:Complex) = fun () -> Complex(System.Math.PI, 0.0)
#endif


// Bounded class ----------------------------------------------------------

open System

type MinValue() =
    static member val Instance = MinValue()
    static member MinValue (_:MinValue, _:unit          ) = fun () -> ()
    static member MinValue (_:MinValue, _:bool          ) = fun () -> false
    static member MinValue (_:MinValue, _:char          ) = fun () -> Char.MinValue
    static member MinValue (_:MinValue, _:byte          ) = fun () -> Byte.MinValue
    static member MinValue (_:MinValue, _:sbyte         ) = fun () -> SByte.MinValue
    static member MinValue (_:MinValue, _:float         ) = fun () -> Double.MinValue
    static member MinValue (_:MinValue, _:int16         ) = fun () -> Int16.MinValue
    static member MinValue (_:MinValue, _:int           ) = fun () -> Int32.MinValue
    static member MinValue (_:MinValue, _:int64         ) = fun () -> Int64.MinValue
    static member MinValue (_:MinValue, _:float32       ) = fun () -> Single.MinValue
    static member MinValue (_:MinValue, _:uint16        ) = fun () -> UInt16.MinValue
    static member MinValue (_:MinValue, _:uint32        ) = fun () -> UInt32.MinValue
    static member MinValue (_:MinValue, _:uint64        ) = fun () -> UInt64.MinValue
    static member MinValue (_:MinValue, _:decimal       ) = fun () -> Decimal.MinValue
    static member MinValue (_:MinValue, _:DateTime      ) = fun () -> DateTime.MinValue
    static member MinValue (_:MinValue, _:DateTimeOffset) = fun () -> DateTimeOffset.MinValue
    static member MinValue (_:MinValue, _:TimeSpan      ) = fun () -> TimeSpan.MinValue

    static member inline internal Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MinValue: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MinValue.Instance ()

    static member inline MinValue (_:MinValue, (_:'a*'b                  )) = fun () -> (MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c               )) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c*'d            )) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c*'d*'e         )) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c*'d*'e*'f      )) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c*'d*'e*'f*'g   )) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())
    static member inline MinValue (_:MinValue, (_:'a*'b*'c*'d*'e*'f*'g*'h)) = fun () -> (MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke(), MinValue.Invoke())

type MaxValue() =
    static member val Instance = MaxValue()
    static member MaxValue (_:MaxValue, _:unit          ) = fun () -> ()
    static member MaxValue (_:MaxValue, _:bool          ) = fun () -> true
    static member MaxValue (_:MaxValue, _:char          ) = fun () -> Char.MaxValue
    static member MaxValue (_:MaxValue, _:byte          ) = fun () -> Byte.MaxValue
    static member MaxValue (_:MaxValue, _:sbyte         ) = fun () -> SByte.MaxValue
    static member MaxValue (_:MaxValue, _:float         ) = fun () -> Double.MaxValue
    static member MaxValue (_:MaxValue, _:int16         ) = fun () -> Int16.MaxValue
    static member MaxValue (_:MaxValue, _:int           ) = fun () -> Int32.MaxValue
    static member MaxValue (_:MaxValue, _:int64         ) = fun () -> Int64.MaxValue
    static member MaxValue (_:MaxValue, _:float32       ) = fun () -> Single.MaxValue
    static member MaxValue (_:MaxValue, _:uint16        ) = fun () -> UInt16.MaxValue
    static member MaxValue (_:MaxValue, _:uint32        ) = fun () -> UInt32.MaxValue
    static member MaxValue (_:MaxValue, _:uint64        ) = fun () -> UInt64.MaxValue
    static member MaxValue (_:MaxValue, _:decimal       ) = fun () -> Decimal.MaxValue
    static member MaxValue (_:MaxValue, _:DateTime      ) = fun () -> DateTime.MaxValue
    static member MaxValue (_:MaxValue, _:DateTimeOffset) = fun () -> DateTimeOffset.MaxValue
    static member MaxValue (_:MaxValue, _:TimeSpan      ) = fun () -> TimeSpan.MaxValue

    static member inline internal Invoke() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MaxValue: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MaxValue.Instance ()

    static member inline MaxValue (_:MaxValue, (_:'a*'b                  )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c               )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c*'d            )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c*'d*'e         )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c*'d*'e*'f      )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c*'d*'e*'f*'g   )) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())
    static member inline MaxValue (_:MaxValue, (_:'a*'b*'c*'d*'e*'f*'g*'h)) = fun () -> (MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke(), MaxValue.Invoke())