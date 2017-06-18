(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Profunctor
==========

A bifunctor that is a contravariant in the first argument and covariant in the second.

___



Minimal complete definition
---------------------------


 * ``dimap f g x``
*)
(**
    static member Dimap (x:'Bifunctor<'T,'V>, f:'U->'T, g:'V->'W) :'Bifunctor<'U,'W>
*)
(**


Other operations
----------------

 * ``lmap f x``
*)
(**
    static member Contramap (x:Profunctor<'T,'V>, f:'U->'T) :'Bifunctor<'U,'V>
*)
(**

 * ``rmap g x``
*)
(**
    static member Map (x:Profunctor<'T,'V>, f:'V->'W) :'Bifunctor<'T,'W>
*)
(**




Rules
-----
*)
(**
    dimap id id = id
    dimap (h' << h) (f << f') = dimap h f << dimap h' f'
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): All profunctors are also functors over the second parameter.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``('T -> 'U)``
 -  ``Func<'T,'U>``

From F#+

 -  ``Kleisli<'T, 'Monad<'U>>``

 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)