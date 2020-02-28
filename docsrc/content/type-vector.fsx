(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Vector<'NumType,'Dimension>
===========================

This is a fixed size vector of a (typically) numeric type.

Related Tyes
------------

 - [Matrix](type-matrix.html): Similar but for matrices
 



Abstractions
------------

 -  [Semigroup](abstraction-semigroup.html)
 -  [Monoid](abstraction-monoid.html)
 -  [Functor](abstraction-functor.html)
 -  [Applicative](abstraction-applicative.html)
 -  [Foldable](abstraction-foldable.html)



Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data


let vector3d_1 = vector (1, 2, 3)
let vector3d_2 = vector (10, 20, 30)


// Add two vectors

let vector3d_sum = vector3d_1 + vector3d_2


// Add a scalar

let vector3d_3 = vector (1, 2, 3) + result 5