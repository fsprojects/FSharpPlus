(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Monad
=====

Defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of an F# programmer, however, it is best to think of a monad as an abstract datatype of actions. F#+ generic computation expressions provide a convenient syntax for writing monadic expressions.

___



Minimal complete definition
---------------------------


 * ``return x``/``result x``
 * ``(>>=) x f``
*)
(**
    static member Return (x: 'T) : 'Monad<'T>
    static member (>>=) (x: Monad<'T>, f: 'T->Monad<'U>) : Monad<'U>
*)
(**

Note: ``return`` can't be used outside computation expressions, use ``result`` instead.

Other operations
----------------

 * ``join``
*)
(**
    static member Join (x:'Monad<'Monad<'T>>) :'Monad<'T>
*)
(**



Rules
-----
*)
(**
    return a >>= k = k a
    m >>= return = m
    m >>= (fun x -> k x >>= h) = (m >>= k) >>= h
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): Monads are automatically functors.
 
 - [Applicative](abstraction-applicative.html) : Monads are automatically applicatives.


Concrete implementations
------------------------

From F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``option<'T>`` 
 -  ``Lazy<'T>``
 -  ``Async<'T>``
 -  ``Result<'T,'U>`` 
 -  ``Choice<'T,'U>``
 -  ``'Monoid * 'T``
 -  ``Task<'T>``
 -  ``'R->'T``
 -  ``ResizeArray<'T>``

 
From F#+

 -  ``Identity<'T>`` 
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
 -  ``DList<'T>``
 
 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation


Examples
--------

*)


#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Data


// Monads allow us to use our generic computation expressions

// This will return the list [11;21;12;22] which is both lists combined in different ways with the (+) operation
let lst11n21n12n22 =
    monad {
        let! x1 = [1;   2]
        let! x2 = [10; 20]
        return ((+) x1 x2) }

// This is the same example but with a non-empty list
let neLst11n21n12n22 = 
    monad {
        let! x1 = { NonEmptyList.Head =  1; Tail =  [2] }
        let! x2 = { NonEmptyList.Head = 10; Tail = [20] }
        return ((+) x1 x2)}

// And now an example with options
let some14 =
    monad {
        let! x1 = Some 4
        let! x2 = tryParse "10"
        return ((+) x1 x2) }


// Monads do not compose well, we need to use Monad Transformers

open System
open FSharpPlus.Data

// First let's define some functions we'll use later

let getLine    = async { return System.Console.ReadLine()}
let putStrLn x = async { printfn "%s" x}
let isValid s =
    String.length s >= 8
        && String.exists System.Char.IsLetter s
        && String.exists System.Char.IsNumber s
        && String.exists Char.IsPunctuation s

let decodeError = function
    | -1 -> "Password not valid"
    | _  -> "Unknown"


// The following functions compose the Error monad with the Async one.

let getValidPassword : ResultT<_> =
    monad {
        let! s = liftAsync getLine
        if isValid s then return s
        else return! throw -1}
    </catch/>
        (fun s -> throw ("The error was: " + decodeError s))

let askPassword = monad {
    do! lift <| putStrLn "Insert your new password:"
    let! value = getValidPassword
    do! lift <| putStrLn "Storing in database..."
    return value}

//try -> Async.RunSynchronously (ResultT.run askPassword)
