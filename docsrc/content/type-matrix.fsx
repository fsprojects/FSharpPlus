(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Matrix<'NumType,'Rows,'Cols>
========================================

This is a fixed size matrix of a (typically) numeric type.

Related Tyes
------------

 - [Vector](type-vector.html): Similar but for vectors
 



Abstractions
------------

 -  [Semigroup](abstraction-semigroup.html)
 -  [Monoid](abstraction-monoid.html)
 -  [Functor](abstraction-functor.html)
 -  [Applicative](abstraction-applicative.html)



Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net46/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

let matrix3x4_1 =
    matrix (
      (1, 0, 0, 0),
      (0, 1, 0, 0),
      (0, 0, 0, 0)
    )

let matrix3x4_2 =
    matrix (
      (10, 0, 100, 0),
      (30, 1, 100, 0),
      (60, 0, 100, 0)
    )


// Add two matrices

let matrix3x4_sum = matrix3x4_1 + matrix3x4_2


// Add a scalar

let matrix3x4_3 = matrix3x4_1 + result 5


// Another way

open FSharpPlus.Math.Generic

let vector3d_4 = matrix3x4_1 + 5G