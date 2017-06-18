(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Comonad
=======

Comonads are the categorical dual of monads.

___



Minimal complete definition
---------------------------


 * ``extract s``

 * ``extend g s`` / ``(=>>) x f`` 

*)
(**
    static member Extract (s:'Comonad<'T>) : 'T
    static member Extend (s:'Comonad<'T>, f:'Comonad<'T> -> 'U) : Applicative<'U>
*)
(**

Other operations
----------------

 * ``duplicate x``
*)
(**
    static member Duplicate (x : 'Comonad<'T>) : 'Comonad<'Comonad<'T>>
*)
(**



Rules
-----
*)
(**
    extend extract       = id
    extract << extend f  = f
    extend f << extend g = extend (f << extend g)
*)
(**


Related Abstractions
--------------------

 - [Monad](abstraction-monad.html): Comonads are the categorical dual of monads.


Concrete implementations
------------------------

From .Net/F#
 
 -  ``Async<'T>``    
 -  ``Lazy<'T>``     
 -  ``Id<'T>``       
 -  ``('W * 'T)``
 -  ``'Monoid -> 'T``
 
 
From F#+

 -  ``Reader<'R,'T>`` 
 -  ``Writer<'Monoid,'T>``

 
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)