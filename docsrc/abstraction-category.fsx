(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../bin"

(**
Category
========

A category has an id and a composition operation.

___



Minimal complete definition
---------------------------


 * ``catId``

 * ``catComp f g`` / ``(<<<)`` f g

*)
(**
    static member get_Id() : 'Category<'T,'T>
    static member (<<<) (f:Category<'U,'V>, g:'Category<'T,'U>) : 'Category<'T,'V>
*)
(**



Other operations
----------------

 * ``(>>>)``
*)
(**
    static member (>>>) (g:'Category<'T,'U>, f:Category<'U,'V>) : 'Category<'T,'V>
*)
(**



Rules
-----
*)
(**
    catId <<< f = f <<< catId = f
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