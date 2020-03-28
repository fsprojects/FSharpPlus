(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Applicative
===========
A functor with application, providing operations to embed pure expressions (``return``), and sequence computations and combine their results (``<*>``).
___
Minimal complete definition
---------------------------
 * ``return x``/``result x`` 
 * ``(<*>) f x``
*)
(**
    static member Return (x:'T) : 'Applicative<'T>
    static member (<*>) (f: 'Applicative<'T->'U>, x: 'Applicative<'T>) : 'Applicative<'U>
*)
(**
Note: ``return`` can't be used outside computation expressions, use ``result`` instead.


Other operations
----------------

* ``lift2``
*)
(**
   static member Lift2 (f: 'T1->'T2->'T, x1: 'Applicative<'T1>, x2: 'Applicative<'T2>) : 'Applicative<'T>
*)
(**


Rules
-----
*)
(**
    result id <*> v = v
    result (<<) <*> u <*> v <*> w = u <*> (v <*> w)
    result f <*> result x = result (f x)
    u <*> result y = result ((|>) y) <*> u
*)
(**
Related Abstractions
--------------------
 - [Functor](abstraction-functor.html): An applicative is a functor whose ``map`` operation can be splitted in ``return`` and ``(<*>)`` operations,
 
 - [Monad](abstraction-monad.html) : Monads are functors with an additional ``Join`` operation,
Concrete implementations
------------------------
From F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``'T [,]``
 -  ``'T [,,]``
 -  ``'T [,,,]``
 -  ``option<'T>`` 
 -  ``IObservable<'T>``
 -  ``Lazy<'T>``
 -  ``Async<'T>``
 -  ``Result<'T,'U>`` 
 -  ``Choice<'T,'U>``
 -  ``KeyValuePair<'Key,'T>``
 -  ``'Monoid * 'T``
 -  ``Task<'T>``
 -  ``'R->'T``
 -  ``Expr<'T>``
 -  ``ResizeArray<'T>``
 
From F#+

 -  ``Cont<'R,'T>`` 
 -  ``ContT<'R,'T>``
 -  ``Reader<'R,'T>`` 
 -  ``ReaderT<'R,'Monad<'T>>``
 -  ``Writer<'Monoid,'T>``
 -  ``WriterT<'Monad<'T * 'Monoid>>``
 -  ``State<'S,'T * 'S>`` 
 -  ``StateT<'S,'Monad<'T * 'S>>``
 -  ``Free<'Functor<'T>,'T>``
 -  ``NonEmptyList<'T>``
 -  ``ZipList<'T>``
 -  ``ParallelArray<'T>``
 -  ``Const<'C,'T>``
 -  ``Compose<'F<'G<'T>>>``
 -  ``DList<'T>``
 
Restricted:
 -  ``string``
 -  ``StringBuilder``
 -  ``Set<'T>``
 -  ``IEnumerator<'T>``
 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data

// Apply +4 to a list
let lst5n6  = map ((+) 4) [ 1;2 ]

// Apply +4 to an array
let arr5n6  = map ((+) 4) [|1;2|]

// I could have written this
let arr5n6' = (+) <!> [|4|] <*> [|1;2|]

// Add two options
let opt120  = (+) <!> Some 20 <*> tryParse "100"


// Applicatives need Return (result)

// Test return
let resSome22 : option<_> = result 22
let resSing22 : list<_>   = result 22
let resLazy22 : Lazy<_>   = result 22
let (quot5 : Microsoft.FSharp.Quotations.Expr<int>) = result 5

// Example
type Person = { name: string; age: int } with static member create n a = {name = n; age = a}

let person1 = Person.create <!> tryHead ["gus"] <*> tryParse "42"
let person2 = Person.create <!> tryHead ["gus"] <*> tryParse "fourty two"
let person3 = Person.create <!> tryHead ["gus"] <*> (tryHead ["42"] >>= tryParse)


// Other ways to write applicative expressions


// Function lift2 helps in many cases

let person1' = (tryHead ["gus"], tryParse "42")               ||> lift2 Person.create 
let person2' = (tryHead ["gus"], tryParse "fourty two")       ||> lift2 Person.create 
let person3' = (tryHead ["gus"], tryHead ["42"] >>= tryParse) ||> lift2 Person.create 


// Using Idiom brackets from http://www.haskell.org/haskellwiki/Idiom_brackets

open FSharpPlus.Builders

let res3n4   = iI ((+) 2) [1;2] Ii
let res3n4'  = iI (+) (result 2) [1;2] Ii
let res18n24 = iI (+) (ZipList(seq [8;4])) (ZipList(seq [10;20])) Ii

let tryDiv x y = if y = 0 then None else Some (x </div/> y)
let resSome3   = join (iI tryDiv (Some 6) (Some 2) Ii)
let resSome3'  =       iI tryDiv (Some 6) (Some 2) Ji

let tryDivBy y = if y = 0 then None else Some (fun x -> x </div/> y)
let resSome2  = join (result tryDivBy  <*> Some 4) <*> Some 8
let resSome2' = join (   iI tryDivBy (Some 4) Ii) <*> Some 8

let resSome2'' = iI tryDivBy (Some 4) J (Some 8) Ii
let resNone    = iI tryDivBy (Some 0) J (Some 8) Ii
let res16n17   = iI (+) (iI (+) (result 4) [2; 3] Ii) [10] Ii

let opt121  = iI (+) (Some 21) (tryParse "100") Ii
let opt122  = iI tryDiv (tryParse "488") (trySqrt 16) Ji


// Using applicative math operators

open FSharpPlus.Math.Applicative

let opt121'  = Some 21 .+. tryParse "100"
let optTrue  = 30 >. tryParse "29"
let optFalse = tryParse "30" .< 29
let m1m2m3 = -.[1;2;3]




// Composing applicatives

let res4 = (+) <!> Compose [Some 3] <*> Compose [Some 1]

let getName s = async { return tryHead s }
let getAge  s = async { return tryParse s }

let person4 = Person.create <!> Compose (getName ["gus"]) <*> Compose (getAge "42")






// A Monad is automatically an Applicative

type MyList<'s> = MyList of 's seq with
    static member Return (x:'a)     = MyList (Seq.singleton x)
    static member (>>=)  (MyList x: MyList<'T>, f) = MyList (Seq.collect (f >> (fun (MyList x) -> x)) x)

let mappedMyList : MyList<_> = (MyList [(+) 1;(+) 2;(+) 3]) <*> (MyList [1;2;3])
