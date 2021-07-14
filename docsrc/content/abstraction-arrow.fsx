(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

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


Concrete implementations
------------------------

From .Net/F#
 
 -  ``'T->'U``
 -  ``Func<'T,'U>``

 
From F#+

 -  [``Kleisli<'T, 'Monad<'U>>``](type-kleisli.html)

 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation
*)