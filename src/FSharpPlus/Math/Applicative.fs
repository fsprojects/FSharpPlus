namespace FSharpPlus.Math

/// <namespacedoc>
/// <summary>
/// Math operators and generic functions
/// </summary>
/// </namespacedoc>
 
#if !FABLE_COMPILER

open FSharpPlus


/// <summary>Math Operators ready to use over Applicative Functors.</summary>
module Applicative =

    let inline ( ~-. ) (x: '``Functor<'T>``)                               = map ((~-)) x    : '``Functor<'T>``

    let inline ( .+  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((+)/> y) x : '``Functor<'T>``
    let inline (  +. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((+)   x) y : '``Functor<'T>``
    let inline ( .+. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (+) <!> x <*> y : '``Applicative<'T>``

    let inline ( .-  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((-)/> y) x : '``Functor<'T>``
    let inline (  -. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((-)   x) y : '``Functor<'T>``
    let inline ( .-. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (-) <!> x <*> y : '``Applicative<'T>``

    let inline ( .*  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((*)/> y) x : '``Functor<'T>``
    let inline (  *. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((*)   x) y : '``Functor<'T>``
    let inline ( .*. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (*) <!> x <*> y : '``Applicative<'T>``

    let inline ( .%  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((%)/> y) x : '``Functor<'T>``
    let inline (  %. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((%)   x) y : '``Functor<'T>``
    let inline ( .%. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (%) <!> x <*> y : '``Applicative<'T>``

    let inline ( ./  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((/)/> y) x : '``Functor<'T>``
    let inline (  /. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((/)   x) y : '``Functor<'T>``
    let inline ( ./. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (/) <!> x <*> y : '``Applicative<'T>``

    let inline ( .=  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((=)/> y) x : '``Functor<bool>``
    let inline (  =. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((=)   x) y : '``Functor<bool>``
    let inline ( .=. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (=) <!> x <*> y : '``Applicative<bool>``

    let inline ( .>  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((>)/> y) x : '``Functor<bool>``
    let inline (  >. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((>)   x) y : '``Functor<bool>``
    let inline ( .>. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (>) <!> x <*> y : '``Applicative<bool>``

    let inline ( .<  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((<)/> y) x : '``Functor<bool>``
    let inline (  <. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((<)   x) y : '``Functor<bool>``
    let inline ( .<. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (<) <!> x <*> y : '``Applicative<bool>``

    let inline (.|| ) (x: '``Functor<bool>``)     (y: bool)                   = map ((||)/> y) x : '``Functor<bool>``
    let inline ( ||.) (x: bool)                   (y: '``Functor<bool>``)     = map ((||)   x) y : '``Functor<bool>``
    let inline (.||.) (x: '``Applicative<bool>``) (y: '``Applicative<bool>``) = (||) <!> x <*> y : '``Applicative<bool>``

    let inline (.&& ) (x: '``Functor<bool>``)     (y: bool)                   = map ((&&)/> y) x : '``Functor<bool>``
    let inline ( &&.) (x: bool)                   (y: '``Functor<bool>``)     = map ((&&)   x) y : '``Functor<bool>``
    let inline (.&&.) (x: '``Applicative<bool>``) (y: '``Applicative<bool>``) = (&&) <!> x <*> y : '``Applicative<bool>``

    let inline ( .<=  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((<=)/> y) x : '``Functor<bool>``
    let inline (  <=. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((<=)   x) y : '``Functor<bool>``
    let inline ( .<=. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (<=) <!> x <*> y : '``Applicative<bool>``

    let inline ( .>=  ) (x: '``Functor<'T>``)     (y: 'T)                   = map ((>=)/> y) x : '``Functor<bool>``
    let inline (  >=. ) (x: 'T)                   (y: '``Functor<'T>``)     = map ((>=)   x) y : '``Functor<bool>``
    let inline ( .>=. ) (x: '``Applicative<'T>``) (y: '``Applicative<'T>``) = (>=) <!> x <*> y : '``Applicative<bool>``

#endif