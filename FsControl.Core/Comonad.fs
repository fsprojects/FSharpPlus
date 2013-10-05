namespace FsControl.Core.Abstractions

open System
open FsControl.Core.Abstractions.Monoid
open FsControl.Core.Abstractions.Functor

module Comonad =

    type Extract = Extract with
        static member        instance (Extract, (w:'w,a:'a) ,_) = fun () -> a
        static member inline instance (Extract, f:'m->'t ,_:'t) = fun () -> f (mempty())

    let inline internal extract x = Inline.instance (Extract, x) ()


    type Duplicate = Duplicate with
        static member        instance (Duplicate, (w:'w, a:'a), _:'w * ('w*'a)) = fun () -> (w,(w,a))
        static member inline instance (Duplicate, f:'m->'a, _:'m->'m->'a) = fun () a b -> f (mappend a b)

    let inline internal duplicate x = Inline.instance (Duplicate, x) ()


    let inline internal extend g s = fmap g (duplicate s)
    let inline internal (=>>)  s g = fmap g (duplicate s)