(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Monoid
======

Applicative Functors which also have a monoid structure.

___



Minimal complete definition
---------------------------

 * ``return``/``result``, ``(<*>)``, ``mzero`` and ``mplus``/``(<|>)``
*)
(**
    static member Return (x:'T) : 'Applicative<'T>
    static member (<*>) (f:'T->'U, x:Applicative<'T>) : Applicative<'U>
    static member Empty () :'Monoid
    static member Append (x:'Monoid, y:'Monoid) :'Monoid
*)
(**

Note: ``return`` can't be used outside computation expressions, use ``result`` instead.

Other operations
----------------

 * ``mfilter``
*)
(**
    static member MFilter (x:seq<'Alternative>) :'Alternative
*)
(**




Rules
-----
*)
(**
    mzero <|> x = x
    x <|> mzero = x
    (x <|> y) <|> z = x <|> (y <|> z)
    f <!> (x <|> y) = (f <!> x) <|> (f <!> y)
    (f <|> g) <*> x = (f <*> x) <|> (g <*> x)
    mzero <*> f = mzero
*)
(**


Related Abstractions
--------------------

 - [Monoid](abstraction-monoid.html): An Alternative is a Monoid that is also an Applicative Functor
 - [Applicative](abstraction-applicative.html): An Alternative is a Monoid that is also an Applicative Functor
 - MonadPlus: Alternatives that are also Monads


Concrete implementations
------------------------

From .Net/F#
 
 -  ``list<'a>``
 -  ``option<'a>``
 -  ``array<'a>``
 -  ``seq<'a>``
 
From F#+
 
 -  ``ReaderT<'R, 'MonadPlus<'T>>``
 -  ``WriterT<'MonadPlus<'T * 'Monoid>>``
 -  ``StateT<'S,'MonadPlus<'T * 'S>>``
 
 [Suggest another](https://github.com/gusty/FSharpPlus/issues/new) concrete implementation
*)