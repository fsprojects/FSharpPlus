(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

(**
Arrow
=====

Arrow<'T, 'U> represents a process that takes as input something of type 'T and outputs something of type 'U.

___



Minimal complete definition
---------------------------


 * ``arr f`` and  ``first f``

*)
(**
    static member Arr(f: 'T -> 'U) : 'Arrow<'T, 'U>
    static member First (f: 'Arrow<'T, 'U>) : 'Arrow<('T * 'V),('U * 'V)>
*)
(**



Other operations
----------------

 * ``second f``
*)
(**
    static member Second (f: 'Arrow<'T, 'U>) : 'Arrow<('V * 'T),('V * 'U)>
*)
(**
 * ``(***) f g``
*)
(**
    static member ``***`` (f : 'Arrow<'T1,'U1>) (g : 'Arrow<'T2,'U2>) : 'Arrow<('T1 * 'T2),('U1 * 'U2)>
*)
(**
 * ``(&&&) f g``
*)
(**
    static member  (&&&) (f : 'Arrow<'T,'U1>) (g : 'Arrow<'T,'U2>) : 'Arrow<'T,('U1 * 'U2)>
*)
(**



Rules
-----
*)
(**
    arr id = id
    arr (f >>> g) = arr f >>> arr g
    first (arr f) = arr (first f)
    first (f >>> g) = first f >>> first g
    first f >>> arr fst = arr fst >>> f
    first f >>> arr (id *** g) = arr (id *** g) >>> first f
    first (first f) >>> arr assoc = arr assoc >>> first f

    where assoc ((a,b),c) = (a,(b,c))
*)
(**


Tutorial
--------

A value of type ``Arrow<'T, 'U>`` represents a process that takes as input a value of type ``'T`` and outputs a value of type ``'U``.

Let's define a simple arrow type based on a function:

*)

#if INTERACTIVE
#r "nuget: FSharpPlus"
#endif

open FSharpPlus
open FSharpPlus.Data

// A simple arrow type wrapping a function
type SimpleFunc<'a, 'b> = SimpleFunc of ('a -> 'b)

// Helper to run the function
let runF (SimpleFunc f) = f

// Arrow instance for SimpleFunc
type SimpleFunc<'a, 'b> with
    static member Arr f = SimpleFunc f
    static member First (SimpleFunc f) = 
        SimpleFunc (fun (a, b) -> (f a, b))
    static member Second (SimpleFunc f) = 
        SimpleFunc (fun (a, b) -> (a, f b))

// Category instance for SimpleFunc  
type SimpleFunc<'a, 'b> with
    static member get_Id() = SimpleFunc id
    static member (>>>) (SimpleFunc f, SimpleFunc g) = SimpleFunc (f >> g)

(**

Now let's define some operations that work with any arrow type:

*)

// split: duplicates a single value into a pair
let split<'a> : SimpleFunc<'a, 'a * 'a> = 
    arr (fun x -> (x, x))

// unsplit: combines a pair using a binary operation
let unsplit op = arr (fun (x, y) -> op x y)

// liftA2: combines output from two arrows using a binary operation
let liftA2 op (f: SimpleFunc<'a, 'b>) (g: SimpleFunc<'a, 'c>) = 
    SimpleFunc (fun x -> 
        let fx = runF f x
        let gx = runF g x
        op fx gx)

(**

Let's build an example using our simple arrow definition:

*)

// f halves its input
let f : SimpleFunc<int, int> = arr (fun x -> x / 2)

// g triples its input and adds one  
let g : SimpleFunc<int, int> = arr (fun x -> x * 3 + 1)

// h combines f and g by adding their results
let h : SimpleFunc<int, int> = liftA2 (+) f g

let hOutput = runF h 8
// Result: 29 (because (8/2) + (8*3+1) = 4 + 25 = 29)

(**

How does ``h`` work? The process is: ``split >>> first f >>> second g >>> unsplit (+)``

When applied to ``8``:
1. ``8`` → ``(8, 8)`` via ``split``
2. ``(8, 8)`` → ``(4, 8)`` via ``first f`` (halve first component)  
3. ``(4, 8)`` → ``(4, 25)`` via ``second g`` (transform second component)
4. ``(4, 25)`` → ``29`` via ``unsplit (+)`` (add the components)

*)

(**

## Kleisli Arrows

Kleisli arrows correspond to functions of type ``'a -> 'M<'b>`` where ``'M`` is a monad.
All multi-value functions (like ``'a -> 'b list``) are Kleisli arrows because ``list`` is monadic.

*)

// Multi-value functions using Kleisli arrows
let plusminus = 
    Kleisli (fun x -> [x; -x])

let double = 
    Kleisli (fun x -> [x * 2])

// Combine using Kleisli composition
let h2 = plusminus >>= fun x -> double

let h2Output = Kleisli.run h2 8
// Result: [16; -16] (because [8; -8] each get doubled to [16; -16])

(**

## String Transformations Example

Here's an example of building string transformations using Kleisli arrows:

*)

// String transformation example (simplified for F#+)
let prepend x = Kleisli (fun s -> [x + s])
let append x = Kleisli (fun s -> [s + x])

// Simple example - compose transformations using monadic bind
let simpleTransform = 
    prepend "<" >>= fun _ -> append ">"

let transformStrings () =
    ["test"; "foobar"] 
    |> List.collect (Kleisli.run simpleTransform)
    |> List.iter (printfn "%s")

// Example usage
let _ = transformStrings ()

(**

This demonstrates how ``f >>> g`` creates multi-valued composition, and 
``(withId f) >>> (withId g)`` applies all permutations of using the arrow values.

When applied to input ``x``, it returns:
``x ++ (f x) ++ (g x) ++ ((g . f) x)``

which gives all permutations of applying the transformations.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``'T->'U``
 -  ``Func<'T,'U>``

 
From F#+

 -  [``Kleisli<'T, 'Monad<'U>>``](type-kleisli.html)

 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation
*)
