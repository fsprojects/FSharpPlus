(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Bifunctor
=======

Intuitively a bifunctor is a functor with 2 arguments which are covariant.

___



Minimal complete definition
---------------------------


 * ``bimap f g x``
*)
(**
    static member Bimap (x:'Bifunctor<'T,'V>, f:'T->'U, g:'V->'W) :'Bifunctor<'U,'W>
*)
(**


Other operations
----------------

 * ``first f x``
*)
(**
    static member First (x:Bifunctor<'T,'V>, f:'T->'U) :'Bifunctor<'U,'V>
*)
(**

 * ``second g x``
*)
(**
    static member Second (x:Bifunctor<'T,'V>, f:'V->'W) :'Bifunctor<'T,'W>
*)
(**




Rules
-----
*)
(**
    bimap f g = first f << second g
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): All bifunctors are also functors over the second parameter.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``'T * 'U``
 -  ``Choice<'T,'U>``
 -  ``KeyValuePair<'T,'U>``

 
From F#+

 -  ``Const<'C,'T>``

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)