namespace FSharpPlus.Math

open FSharpPlus
open FSharpPlus.Control

#if !FABLE_COMPILER

/// <summary>
/// Generic numbers, functions and operators.
/// By opening this module some common operators become restricted, like (+) to 'T->'T->'T
/// </summary>
module Generic =

    open System.Numerics

    let inline fromIntegral (x: 'Integral) : 'Num = (fromBigInt << toBigInt) x

    module NumericLiteralG =
        let inline FromZero () = getZero ()
        let inline FromOne  () = getOne ()
        let inline FromInt32 (i: int   ) = FromInt32.Invoke i
        let inline FromInt64 (i: int64 ) = FromInt64.Invoke i
        let inline FromString (i: string) = fromBigInt <| BigInteger.Parse i

    let inline (+) (a: 'Num) (b: 'Num) : 'Num = Plus.Invoke a b
    let inline (-) (a: 'Num) (b: 'Num) : 'Num = a - b
    let inline (*) (a: 'Num) (b: 'Num) : 'Num = a * b
    let inline (/) (a: 'Fractional) (b: 'Fractional) : 'Fractional = (* whenFractional a;*) a / b

    let inline internal whenIntegral a = let _ = if false then toBigInt a else 0I in ()

    /// Integer division. Same as (/) for Integral types.
    let inline div (a: 'Integral) (b: 'Integral) : 'Integral = whenIntegral a; a / b

    /// Euclidean integer division, following the mathematical convention where the mod is always positive.
    let inline divE (a: 'Integral) b : 'Integral =
        whenIntegral a
        let (a, b) = if b < 0G then (-a, -b) else (a, b)
        (if a < 0G then (a - b + 1G) else a) / b

    /// Remainder of Integer division. Same as (%).
    let inline rem (a: 'Integral) (b: 'Integral) : 'Integral = whenIntegral a; a % b

    /// Euclidean remainder of integer division, following the mathematical convention where the mod is always positive.
    let inline remE (a: 'Integral) (b: 'Integral) : 'Integral = whenIntegral a; ((a % b) + b) % b

    /// Euclidean division-remainder, following the mathematical convention where the mod is always positive.
    let inline divRemE D d =
        let q, r = divRem D d
        if r < 0G then
            if d > 0G then q - 1G, r + d
            else           q + 1G, r - d
        else q, r

    /// Greatest Common Divisor.
    let inline gcd x y : 'Integral =
        let zero = getZero ()
        let rec loop a = function
            | b when b = zero -> a
            | b -> loop b (rem a b)
        match x, y with
        | t when t = (zero, zero) -> failwith "gcd 0 0 is undefined"
        | _                       -> loop (abs x) (abs y)

#endif