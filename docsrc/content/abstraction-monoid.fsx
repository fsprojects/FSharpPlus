(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Monoid
======
Types with an associative binary operation that has an identity.
___
Minimal complete definition
---------------------------
 * `zero``
 * ``(+) x y``/``(++) x y``
*)
(**
    static member get_Zero () :'Monoid
    static member (+) (x:'Monoid, y:'Monoid) :'Monoid
*)
(**
Other operations
----------------
 * ``Seq.sum``
*)
(**
    static member Sum (x:Seq<'Monoid>) :'Monoid
*)
(**
Rules
-----
*)
(**
	zero + x = x
    x + zero = x
    (x + y) + z = x + (y + z)
    Seq.sum = Seq.fold (+) zero
    sum = fold (+) zero (generic to all foldables)
*)
(**
Related Abstractions
--------------------
 - [Semigroup](abstraction-semigroup.html): A monoid is a Semigroup with an additional ``zero`` operation
 
 - MonadPlus: Monads that are also Monoids
Concrete implementations
------------------------
From .Net/F#
 
 -  ``list<'T>``
 -  ``option<'T>``
 -  ``array<'T>``
 -  ``string``
 -  ``StringBuilder``
 -  ``unit``
 -  ``Set<'T>``
 -  ``Map<'T,'U>``
 -  ``TimeSpan`` 
 -  ``'T*'U``
 -  ``'T*'U*'V``
 -  ``'T*'U*'V*'W``
 -  ``'T*'U*'V*'W*'X``
 -  ``Task<'T>``
 -  ``'T->'Monoid``
 -  ``Async<'T>``
 -  ``Expr<'T>``
 -  ``Lazy<'T>``
 -  ``Dictionary<'T,'U>``
 -  ``IDictionary<'T,'U>``
 -  ``ResizeArray<'T>``
 -  ``seq<'T>``
 -  ``IEnumerator<'T>``
 
From F#+
 
 -  ``ZipList<'S>``
 -  ``Dual<'T>``
 -  ``Endo<'T>``
 -  ``All``
 -  ``Any``
 -  ``Const<'T,'U>``
 -  ``First<'T>``
 -  ``Last<'T>``
 -  ``DList<'T>``
 
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation

 Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Operators.GenericMath
open FSharpPlus.Data


/// A monoid that represents results of comparisons
type Ordering = LT|EQ|GT with
    static member        Zero = EQ
    static member        (+) (x:Ordering, y) = 
        match x, y with
        | LT, _ -> LT
        | EQ, a -> a
        | GT, _ -> GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _            -> EQ

let resGreater = compare' 7 6

/// A monoid of all numbers from 0 to 4
type Mod5 = Mod5 of uint32 with
    static member inline get_Zero() = Mod5 0u
    static member inline (+) (Mod5 x, Mod5 y) = Mod5 ( (x + y) % 5u)
let Mod5 x = Mod5 (x % 5u)


// Results of Monoid operations
let emptyLst:list<int> = zero
let zeroUint:Mod5   = zero
let res1 = zero ++ Mod5 11u
let res2  = sum <| map Mod5 [4u; 2u; 1u]
let res3  = sum [zero; Mod5 2G; Mod5 6G]
let res8n4 = [zero; [8;4]]
let res15 = Mult 15 ++ zero
let resTrue = sum [zero; Any true]
let resFalse = sum (map All [true;false])
let resHi = zero ++ "Hi"
let resGT = zero ++  GT
let resLT = sum [zero; LT ; EQ ;GT]
let res9823 = sum (map Dual [zero;"3";"2";"8";"9"])
let resBA = Dual "A" ++ Dual "B" 
let resEl00:list<int>*float = zero
let resS3P20     = (1G, Mult 5.0) ++  (2, Mult 4G)
let res230       = (zero,zero) ++ ([2],[3.0])
let res243       = ([2;4],[3]) ++ zero
let res23        = zero ++ ([2],"3")
let resLtDualGt  =  (LT,Dual GT) ++ zero
let res230hiSum2 = (zero, zero, 2) ++ ([2], ([3.0], "hi"), zero)
let res230hiS4P3 = (zero, zero   ) ++ ([2], ([3.0], "hi", 4, Mult (6 % 2)))
let tuple5 :string*(Any*string)*(All*All*All)*int*string = zero