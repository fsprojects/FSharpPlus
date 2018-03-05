namespace FSharpPlus.Internals

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline tupleToOption x = match x with true, value -> Some value | _ -> None



[<RequireQualifiedAccess>]
module internal Implicit = let inline Invoke (x : ^t) = ((^R or ^t) : (static member op_Implicit : ^t -> ^R) x) :^R

module Errors =
    let exnDivByZero      = new System.DivideByZeroException() :> exn
    let exnNoDivision     = new System.Exception "These numbers are not divisible in this domain."
    let exnSqrtOfNegative = new System.Exception "Cannot calculate square root of a negative number"
    let exnNoSqrt         = new System.Exception "No square root defined for this value in this domain."
    let exnNoSubtraction  = new System.Exception "No subtraction defined for these values in this domain."

module Decimal =
    let inline trySqrt x =
        match sign x with
        | -1 -> Error Errors.exnSqrtOfNegative
        |  0 -> Ok    0.M
        | _  ->
            let rec loop previous =
                let current = (previous + x / previous) / 2.0M
                if previous - current = 0.0M then current else loop current
            x |> float |> sqrt |> decimal |> loop |> Ok

module Rational =
    let inline numerator   x = (^F: (member Numerator  : 'R) x)
    let inline denominator x = (^F: (member Denominator: 'R) x)

module BigInteger =
    open System.Numerics
    let trySqrtRem x =
        if sign x = -1 then Error Errors.exnSqrtOfNegative
        else
            let rec loop previous =
                let current = (previous + x / previous) >>> 1
                if abs (previous - current) < 2I then current else loop current
            let guess = 10I ** (((int (BigInteger.Log10 (x + 1I))) + 1) >>> 1)
            let r = loop guess
            let r2 = r * r
            match compare r2 x with
            | 0 -> Ok (r, 0I)
            | 1 -> let root = r - 1I in Ok (root, x - root * root)
            | _ -> Ok (r, x - r2)


// Dummy types

type Id<'t>(v:'t) =
   let value = v
   member this.getValue = value

[<RequireQualifiedAccess>]
module Id =
    let run   (x:Id<_>) = x.getValue
    let map f (x:Id<_>) = Id (f x.getValue)
    let create x = Id (x)

type Id0(v:string) =
   let value = v
   member this.getValue = value

type DmStruct = struct end