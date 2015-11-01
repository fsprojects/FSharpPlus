#nowarn "3186"
#r @"..\bin\Release\FsControl.dll"

open FsControl.Operators
open FsControl.Core.Types

let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip



// Numerics
type Integer = bigint
open System.Numerics
open FsControl

let inline fromInteger  (x:Integer)   :'Num    = FsControl.Operators.fromBigInt x
let inline toInteger    (x:'Integral) :Integer = FsControl.Operators.toBigInt   x
let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

module NumericLiteralG =
    let inline FromZero() = Zero.Invoke()
    let inline FromOne () = One.Invoke()
    let inline FromInt32  (i:int   ) = FromInt32.Invoke i
    let inline FromInt64  (i:int64 ) = FromInt64.Invoke i
    let inline FromString (i:string) = fromInteger <| BigInteger.Parse i




let inline whenIntegral a = let _ = if false then toInteger a else 0I in ()


module Ratio =
    // Strict version of math operators
    let inline internal ( +.) (a:'Num) (b:'Num) :'Num = a + b
    let inline internal ( -.) (a:'Num) (b:'Num) :'Num = a - b
    let inline internal ( *.) (a:'Num) (b:'Num) :'Num = a * b

    let inline internal gcd x y :'Integral =
        let zero = zero()
        let rec loop a b =
            if b = zero then a
            else loop b (a % b)
        if (x, y) = (zero, zero) then failwith "gcd 0 0 is undefined"
        else loop (Abs.Invoke x) (Abs.Invoke y)

    type Ratio<'Integral> =
        struct
            val Numerator   :'Integral
            val Denominator :'Integral
            new (numerator: 'Integral, denominator: 'Integral) = {Numerator = numerator; Denominator = denominator}
        end
        override this.ToString() = this.Numerator.ToString() + " % " + this.Denominator.ToString()

    let inline internal ratio (a:'Integral) (b:'Integral) :Ratio<'Integral> =
        whenIntegral a
        let zero = zero()
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a, b) = if b < zero then (-a, -b) else (a, b)
        let gcd = gcd a b
        Ratio (a / gcd, b / gcd)

    let inline internal Ratio (x,y) = x </ratio/> y

    let inline internal numerator   (r:Ratio<_>) = r.Numerator
    let inline internal denominator (r:Ratio<_>) = r.Denominator

    type Ratio<'Integral> with
        static member inline (/) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator) </ratio/> (a.Denominator *. b.Numerator)                                              
        static member inline (+) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator +. b.Numerator *. a.Denominator) </ratio/> (a.Denominator *. b.Denominator)
        static member inline (-) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Denominator -. b.Numerator *. a.Denominator) </ratio/> (a.Denominator *. b.Denominator)
        static member inline (*) (a:Ratio<_>, b:Ratio<_>) = (a.Numerator *. b.Numerator) </ratio/> (a.Denominator *. b.Denominator)

        static member inline Abs        (r:Ratio<_>) = (Abs.Invoke    (numerator r)) </ratio/> (denominator r)
        static member inline Signum     (r:Ratio<_>) = (Signum.Invoke (numerator r)) </ratio/> (one())
        static member inline FromBigInt (x:bigint) = FromBigInt.Invoke x </ratio/> (one())
        static member inline (~-)       (r:Ratio<_>) = -(numerator r) </ratio/> (denominator r)


    let (|Ratio|) (ratio:Ratio<_>) = (ratio.Numerator, ratio.Denominator)

type Rational = Ratio.Ratio<bigint>




let inline abs    (x:'Num) :'Num = FsControl.Operators.abs    x
let inline signum (x:'Num) :'Num = FsControl.Operators.signum x

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

let inline negate (x:'Num) :'Num = FsControl.Operators.negate x
let inline (~-)   (x:'Num) :'Num = FsControl.Operators.negate x


let inline div (a:'Integral) b :'Integral =
    whenIntegral a
    let (a,b) = if b < 0G then (-a,-b) else (a,b)
    (if a < 0G then (a - b + 1G) else a) / b

let inline quot (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a / b
let inline rem  (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a % b
let inline quotRem a b :'Integral * 'Integral = whenIntegral a; FsControl.Operators.divRem a b
let inline mod'   a b :'Integral = whenIntegral a; ((a % b) + b) % b  
let inline divMod D d :'Integral * 'Integral =
    let q, r = quotRem D d
    if (r < 0G) then
        if (d > 0G) then (q - 1G, r + d)
        else             (q + 1G, r - d)
    else (q, r)





let inline (/) (a:'Fractional) (b:'Fractional) :'Fractional = (* whenFractional a;*) a / b
let inline recip x :'Fractional = 1G / x

// Exp functions
let inline ( **^ ) (x:'Num) (n:'Integral)  = 
    whenIntegral n
    let rec f a b n = if n == 0G then a else f (b * a) b (n - 1G)
    if (n < 0G) then failwith "Negative exponent" else f 1G x n
let inline ( **^^ ) (x:'Fractional) (n:'Integral) = if n >= 0G then x**^n else recip (x**^(negate n))

let inline pi() :'Floating = FsControl.Operators.pi ()

let inline ( **) a (b:'Floating) :'Floating = a ** b

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y  :'Floating =  log y / log x


// Test Numerics

let piComplex:System.Numerics.Complex = pi()

let c2 = System.Numerics.Complex(25.2, 3.1)

let a = abs' 2u
let b = abs' -2
let c = abs' -2.f
let d = abs' -2.M
let e = abs' c2
let f = abs' (System.Numerics.Complex(32. , 2.))

let a' = signum' 2u
let b' = signum' -2
let c' = signum' -2.f
let d' = signum' -2.M
let e' = signum' c2
let f' = signum' (System.Numerics.Complex(32. , 2.))


let divisions = List.map ( quot /> 5G) [5;8;10;15;20]

let inline quadratic a b c =
    let root1 = ( -b + sqrt (  b * b - 4G * a * c) )  / (2G * a)
    let root2 = ( -b - sqrt (  b * b - 4G * a * c) )  / (2G * a)
    (root1,root2)

let res30_15  = quadratic 2.0  -3G -9G
let res30_15f = quadratic 2.0f -3G -9G

let resCmplx:System.Numerics.Complex * _ = quadratic 2G -3G 9G

(* Warning, works but very slow compile time with current version of F#
let res30_15r:Rational * _ = quadratic 2G -3G -9G
*)