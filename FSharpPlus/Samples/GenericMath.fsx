#nowarn "3186"
#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus
open GenericMath

// 1. Generic functions

// Often you need to define generic constants when defining generic function.
// Since there is no way to define generic decimal literals in F# at the moment of writing this, we use ratio:

let inline areaOfCircle radio =
    let pi = 
        314159265358979323846264338I 
            </ratio/> 
        100000000000000000000000000I
    fromRational pi * radio * radio

let area1 = areaOfCircle 5.
let area2 = areaOfCircle 5.0f
let area3 = areaOfCircle 5.0M



// 2. Defining custom types, support generic operations

type Vector2d<'T> = Vector2d of 'T * 'T  with
    static member inline (+) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a + c):'t), ((b + d):'t))
    static member inline (-) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a - c):'t), ((b - d):'t))
    static member inline (*) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a * c):'t), ((b * d):'t))
    static member        Return x                               = Vector2d (x, x)
    static member        Map(Vector2d(x, y), f)                 = Vector2d (f x, f y)
    static member inline FromBigInteger (_:Vector2d<'t>)        = fun (x:bigint) -> 
        let y:'t = fromBigInteger x
        Vector2d (y, y)

// Note we don't define overloads for adding a vector to a number
// Why? Apart from being tedious they will break math operators strictness 
// so we will have problems type inferencing generic functions.
// OK, but then how to add (subtract, multiply) to a number?

// Option 1, explicitely 'lift' the number.
// Requires Return and (+,-,*)

let x1  = Vector2d (32,5) + result 7
let x1' = result 7 + Vector2d (32,5)

// Option 2, use Generic Numbers
// Requires FromBigInteger and (+,-,*,/)

open FSharpPlus.Operators.GenericMath
let x2  = Vector2d (32,5) + 7G
let x2' = 7G + Vector2d (32,5)

// Option 3, use Applicative Math Operators
// Requires only Map

open FSharpPlus.Operators.ApplicativeMath
let x3 = Vector2d (32,5) |+ 7
let x3' = 7 +| Vector2d (32,5)
