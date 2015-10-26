#nowarn "3186"
#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus
open GenericMath

// 1. Generic functions

// Often you need to define generic constants when defining generic functions.
// Since there is no way to define generic decimal literals in F# at the moment of writing this, we use divisions:

let inline areaOfCircle radio =
    let pi = 
        314159265358979323846264338G 
                    / 
        100000000000000000000000000G
    pi * radio * radio

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
    static member inline FromBigInt x = let y = fromBigInt x in Vector2d (y, y)

// Note we don't define overloads for adding a vector to a number
// Why? Apart from being tedious they will break math operators strictness 
// so we will have problems type inferencing generic functions.
// OK, but then how to add (subtract, multiply) to a number?

// Option 1, explicitely 'lift' the number.
// Requires Return and (+,-,*)

let x1  = Vector2d (32,5) + result 7
let x1' = result 7 + Vector2d (32,5)

// Option 2, use Generic Numbers
// Requires FromBigInt and (+,-,*,/)

open FSharpPlus.Operators.GenericMath
let x2  = Vector2d (32,5) + 7G
let x2' = 7G + Vector2d (32,5)

// Option 3, use Applicative Math Operators
// Requires only Map

open FSharpPlus.Operators.ApplicativeMath
let x3 = Vector2d (32,5) |+ 7
let x3' = 7 +| Vector2d (32,5)



// 3. Integrate with 3rd party libraries

// We may use types defined in other libraries, let's suppose we have this type Ratio defined somewhere.

type Ratio =
    struct
        val Numerator   :bigint
        val Denominator :bigint
        new (numerator: bigint, denominator: bigint) = {Numerator = numerator; Denominator = denominator}
    end
    override this.ToString() = this.Numerator.ToString() + " % " + this.Denominator.ToString()

let ratio (a:bigint) (b:bigint) :Ratio =
    if b = 0I then failwith "Ratio.%: zero denominator"
    let a, b = if b < 0I then (-a, -b) else (a, b)
    let gcd = gcd a b
    Ratio (a / gcd, b / gcd)

let Ratio (x,y) = x </ratio/> y

type Ratio with
    static member inline (/) (a:Ratio, b:Ratio) = (a.Numerator * b.Denominator) </ratio/> (a.Denominator * b.Numerator)                                              
    static member inline (+) (a:Ratio, b:Ratio) = (a.Numerator * b.Denominator + b.Numerator * a.Denominator) </ratio/> (a.Denominator * b.Denominator)
    static member inline (-) (a:Ratio, b:Ratio) = (a.Numerator * b.Denominator - b.Numerator * a.Denominator) </ratio/> (a.Denominator * b.Denominator)
    static member inline (*) (a:Ratio, b:Ratio) = (a.Numerator * b.Numerator) </ratio/> (a.Denominator * b.Denominator)

    static member inline Abs        (r:Ratio) = (abs    r.Numerator) </ratio/> r.Denominator
    static member inline Signum     (r:Ratio) = (signum r.Numerator) </ratio/> 1I
    static member inline FromBigInt (x:bigint) = fromBigInt x </ratio/> 1I
    static member inline (~-)       (r:Ratio) = -(r.Numerator) </ratio/> r.Denominator

// Since most Rational implementations have Numerator and Denominator defined we can just use our generic functions on it:

let some3_2 = trySqrt (Ratio(9I, 4I))