namespace FsControl

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Core.Internals

[<Extension;Sealed>]
type Mapi =
    [<Extension>]static member Mapi (x:Id<'T>    , f:_->'T->'U , [<Optional>]impl:Mapi) = f () x.getValue
    [<Extension>]static member Mapi (x:seq<'T>   , f           , [<Optional>]impl:Mapi) = Seq.mapi   f x
    [<Extension>]static member Mapi (x:list<'T>  , f           , [<Optional>]impl:Mapi) = List.mapi  f x
    [<Extension>]static member Mapi (x:'T []     , f           , [<Optional>]impl:Mapi) = Array.mapi f x
    [<Extension>]static member Mapi ((k:'K, a:'T), f           , [<Optional>]impl:Mapi) = (k, ((f k a):'U))
    [<Extension>]static member Mapi (g           , f:'K->'T->'U, [<Optional>]impl:Mapi) = fun x -> f x (g x)
    [<Extension>]static member Mapi (x:Map<'K,'T>, f           , [<Optional>]impl:Mapi) = Map.map f x :Map<'K,'U>

    static member inline Invoke    (mapping:'K->'T->'U)    (source:'Indexable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Mapi: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Mapi>,   source, mapping)     :'Indexable'U


[<Extension;Sealed>]
type Iteri =
    [<Extension>]static member Iteri (x:Id<'T>  , f:_->'T->unit, [<Optional>]impl:Iteri) = f () x.getValue
    [<Extension>]static member Iteri (x:seq<'T> , f            , [<Optional>]impl:Iteri) = Seq.iteri   f x
    [<Extension>]static member Iteri (x:list<'T>, f            , [<Optional>]impl:Iteri) = List.iteri  f x
    [<Extension>]static member Iteri (x:'T []   , f            , [<Optional>]impl:Iteri) = Array.iteri f x
    [<Extension>]static member Iteri (x:Map<'K,'T>, f          , [<Optional>]impl:Iteri) = Map.iter f x

    static member inline Invoke (action:'K->'T->unit)     (source:'Indexable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Iteri: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<Iteri>,  source, action)    :unit



type Foldi =
    static member        Foldi (x:seq<_>    , f, z, impl:Foldi   ) = x |> Seq.fold   (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        Foldi (x:list<_>   , f, z, impl:Foldi   ) = x |> List.fold  (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        Foldi (x: _ []     , f, z, impl:Foldi   ) = x |> Array.fold (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        Foldi (x:Map<'k,'t>, f, z, impl:Foldi   ) = Map.fold f z

    static member inline Invoke (folder:'State->'Key->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Foldi: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<Foldi>, foldable, folder, state)

type Traversei =
    [<Extension>]static member inline Traversei ((k:'K, a:'T),f , [<Optional>]output              , [<Optional>]impl:Traversei) = Map.Invoke ((fun x y -> (x, y)) k) (f k a)
    [<Extension>]static member inline Traversei (Identity a  ,f , [<Optional>]output              , [<Optional>]impl:Traversei) = Map.Invoke Identity (f () a)

    static member inline Invoke f t =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Traversei: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Traversei>, t, f)