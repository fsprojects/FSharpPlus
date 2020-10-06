(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../bin"

#r @"../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

(**
Numeric functions
=================

This library comes with some additional numeric functions and constants.

These functions work over many numeric types

*)

let qr0  = divRem 7  3  //val qr0 : int * int = (2, 1)
let qr1  = divRem 7I 3I //val qr1 : System.Numerics.BigInteger * System.Numerics.BigInteger = (2, 1)
let qr2  = divRem 7. 3. //val qr2 : float * float = (2.333333333, 0.0) -> using default method.

(**

Numeric constants
=================

Apart from typical math constants, bounded types comes with <code>minValue</code> and <code>maxValue</code> constants.

Here's an example how can this be used to implement an efficient <code>findMin</code> function

*)

let inline findMin (lst: 'a list) =
    let rec loop acc = function
        | [] -> acc
        | x::_ when x = minValue -> x
        | x::xs -> loop (if x < acc then x else acc) xs
    loop maxValue lst
    
let minInt  = findMin [1;0;12;2]
let minUInt = findMin [1u;0u;12u;2u]  // loops only twice


(**
Generic operations over numeric types
=====================================

*)

(**

Writing code that is generic over different numeric types can be really tedious in F#.

Using this library it becomes an easy task, but it's important to understand the numeric abstractions and its limitations.

In order to have a reasonable type inference over generic types we need strict operations.

For example the F# definition of <code>(+)</code> can take 2 different types, this makes possible to interact with some .NET types that have defined the <code>(+)</code> operator in a very arbitrary way.

For instance you can add a <code>float</code> to a <code>DateTime</code> with the <code>(+)</code> operator, and that <code>float</code> will be interpreted as seconds.

By opening the <code>FSharpPlus.Math.Generic</code> namespace this will no longer be possible, because that's the tradeoff in order to get decent type inference.



Generic number literals
=======================

Numbers with a G suffix are generics.

*)

open FSharpPlus.Math.Generic

let res5Int  : int    = 5G
let res5UInt : uint32 = 5G

(**
Often you need to define generic constants when defining generic functions.
Since there is no way to define generic decimal literals in F# at the moment of writing this, we can use divisions:
*)


let inline areaOfCircle radio =
    let pi = 
        314159265358979323846264338G 
                    / 
        100000000000000000000000000G
    pi * radio * radio

let area1 = areaOfCircle 5.
let area2 = areaOfCircle 5.0f
let area3 = areaOfCircle 5.0M



(**
Defining custom types, support generic operations
=================================================
*)

type Vector2d<'T> = Vector2d of 'T * 'T  with
    static member inline (+) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a + c):'t), ((b + d):'t))
    static member inline (-) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a - c):'t), ((b - d):'t))
    static member inline (*) (Vector2d(a:'t, b:'t), Vector2d(c:'t, d:'t)) = Vector2d (((a * c):'t), ((b * d):'t))
    static member        Return x                               = Vector2d (x, x)
    static member        Map(Vector2d(x, y), f)                 = Vector2d (f x, f y)
    static member inline FromBigInt x = let y = fromBigInt x in Vector2d (y, y)

(**
Note we don't define overloads for adding a vector to a number

Why? Apart from being tedious they will break math operators strictness 

so we will have problems type inferencing generic functions.

OK, but then how to add (subtract, multiply) to a number?



Option 1, explicitely 'lift' the number.

Requires Return and ( + , - , * )

*)

let x1  = Vector2d (32,5) + result 7
let x1' = result 7 + Vector2d (32,5)

(**
Option 2, use Generic Numbers

Requires <code>FromBigInt</code> and (+,-,*,/)
*)

open FSharpPlus.Math.Generic
let x2  = Vector2d (32,5) + 7G
let x2' = 7G + Vector2d (32,5)

(**
Option 3, use Applicative Math Operators
Requires only <code>Map</code>
*)

open FSharpPlus.Math.Applicative
let x3 = Vector2d (32,5) .+ 7
let x3' = 7 +. Vector2d (32,5)


(**
Integrate with 3rd party libraries
==================================

We may use types defined in other libraries, let's suppose we have this type Ratio defined somewhere.
*)

type Ratio =
    struct
        val Numerator   : bigint
        val Denominator : bigint
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

(**
Since most Rational implementations have Numerator and Denominator defined we can just use our generic functions on it:
*)

let some3_2 = trySqrt (Ratio(9I, 4I))


(**

Example: creating a polymorphic quadratic function
==================================================

The quadratic function has different results depending on which domain it operates.

For example for real numbers it can have 0 or 2 solutions (arguably also 1 that is a double solution).

But for complex numbers it always has 2 solutions.

*)

open FSharpPlus.Math.Generic

let inline quadratic a b c =
    let root1 = ( -b + sqrt (  b * b - 4G * a * c) )  / (2G * a)
    let root2 = ( -b - sqrt (  b * b - 4G * a * c) )  / (2G * a)
    (root1,root2)


let noRes  = quadratic 2.0  3G 9G
// val noRes : float * float = (nan, nan)

let res30_15  = quadratic 2.0  -3G -9G
// val res30_15 : float * float = (3.0, -1.5)

let res30_15f = quadratic 2.0f -3G -9G
// val res30_15f : float32 * float32 = (3.0f, -1.5f)

let resCmplx:System.Numerics.Complex * _ = quadratic 2G -3G 9G
// val resCmplx : System.Numerics.Complex * System.Numerics.Complex = ((0.75, -1.98431348329844), (0.75, 1.98431348329844))

let res30_15r:Ratio * _ = quadratic 2G -3G -9G
// val res30_15r : Ratio * Ratio = (3 % 1, -3 % 2)