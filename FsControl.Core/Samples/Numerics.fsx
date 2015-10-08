#nowarn "3186"
#r @"..\bin\Release\FsControl.Core.dll"

open FsControl.Operators
open FsControl.Core.Types

let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip



// Numerics
type Integer = bigint
open System.Numerics
open FsControl.Core.TypeMethods

let inline fromInteger  (x:Integer)   :'Num    = FsControl.Operators.fromBigInt x
let inline toInteger    (x:'Integral) :Integer = FsControl.Operators.toBigInt   x
let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

module NumericLiteralG =
    let inline FromZero() = GenericZero.Invoke()
    let inline FromOne () = GenericOne.Invoke()
    let inline FromInt32  (i:int   ) = fromIntegral i
    let inline FromInt64  (i:int64 ) = fromIntegral i
    let inline FromString (i:string) = fromInteger <| BigInteger.Parse i

let inline abs    (x:'Num) :'Num = FsControl.Operators.abs    x
let inline signum (x:'Num) :'Num = FsControl.Operators.signum x

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

let inline negate (x:'Num) :'Num = FsControl.Operators.negate x
let inline (~-)   (x:'Num) :'Num = FsControl.Operators.negate x

let inline whenIntegral a = let _ = if false then toInteger a else 0I in ()

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

// type Rational = Ratio<Integer>

let inline G0() = GenericZero.Invoke()
let inline G1() = GenericZero.Invoke()

let inline gcd x y :'Integral =
    let zero = G0()
    let rec gcd' a = function
        | b when b = zero -> a
        | b -> gcd' b (rem a b)
    match(x,y) with
    | t when t = (zero,zero) -> failwith "Prelude.gcd: gcd 0 0 is undefined"
    | _                      -> gcd' (abs x) (abs y)

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
let inline sqrt    (x:'Floating) :'Floating = sqrt x

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y  :'Floating =  log y / log x


// Test Numerics

let divisions = List.map ( quot /> 5G) [5;8;10;15;20]

let inline quadratic a b c =
    let root1 = ( -b + sqrt (  b ** 2G - 4G * a * c) )  / (2G * a)
    let root2 = ( -b - sqrt (  b ** 2G - 4G * a * c) )  / (2G * a)
    (root1,root2)

let res30_15  = quadratic 2.0  -3G -9G
let res30_15f = quadratic 2.0f -3G -9G
let resCmplx:System.Numerics.Complex * _ = quadratic 2G -3G 9G