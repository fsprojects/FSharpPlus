namespace FsControl

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Core.Internals

[<Extension;Sealed>]
type MapIndexed =
    [<Extension>]static member MapIndexed (x:Id<'T>    , f:_->'T->'U , [<Optional>]impl:MapIndexed) = f () x.getValue
    [<Extension>]static member MapIndexed (x:seq<'T>   , f           , [<Optional>]impl:MapIndexed) = Seq.mapi   f x
    [<Extension>]static member MapIndexed (x:list<'T>  , f           , [<Optional>]impl:MapIndexed) = List.mapi  f x
    [<Extension>]static member MapIndexed (x:'T []     , f           , [<Optional>]impl:MapIndexed) = Array.mapi f x
    [<Extension>]static member MapIndexed ((k:'K, a:'T), f           , [<Optional>]impl:MapIndexed) = (k, ((f k a):'U))
    [<Extension>]static member MapIndexed (g           , f:'K->'T->'U, [<Optional>]impl:MapIndexed) = fun x -> f x (g x)
    [<Extension>]static member MapIndexed (x:Map<'K,'T>, f           , [<Optional>]impl:MapIndexed) = Map.map f x :Map<'K,'U>

    static member inline Invoke    (mapping:'K->'T->'U)    (source:'Indexable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member MapIndexed: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<MapIndexed>,   source, mapping)     :'Indexable'U


[<Extension;Sealed>]
type IterateIndexed =
    [<Extension>]static member IterateIndexed (x:Id<'T>  , f:_->'T->unit, [<Optional>]impl:IterateIndexed) = f () x.getValue
    [<Extension>]static member IterateIndexed (x:seq<'T> , f            , [<Optional>]impl:IterateIndexed) = Seq.iteri   f x
    [<Extension>]static member IterateIndexed (x:list<'T>, f            , [<Optional>]impl:IterateIndexed) = List.iteri  f x
    [<Extension>]static member IterateIndexed (x:'T []   , f            , [<Optional>]impl:IterateIndexed) = Array.iteri f x
    [<Extension>]static member IterateIndexed (x:Map<'K,'T>, f          , [<Optional>]impl:IterateIndexed) = Map.iter f x

    static member inline Invoke (action:'K->'T->unit)     (source:'Indexable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member IterateIndexed: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<IterateIndexed>,  source, action)    :unit



type FoldIndexed =
    static member        FoldIndexed (x:seq<_>    , f, z, impl:FoldIndexed   ) = x |> Seq.fold   (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        FoldIndexed (x:list<_>   , f, z, impl:FoldIndexed   ) = x |> List.fold  (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        FoldIndexed (x: _ []     , f, z, impl:FoldIndexed   ) = x |> Array.fold (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member        FoldIndexed (x:Map<'k,'t>, f, z, impl:FoldIndexed   ) = Map.fold f z

    static member inline Invoke (folder:'State->'Key->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member FoldIndexed: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<FoldIndexed>, foldable, folder, state)

type TraverseIndexed =
    [<Extension>]static member inline TraverseIndexed ((k:'K, a:'T),f , [<Optional>]output, [<Optional>]impl:TraverseIndexed) = Map.Invoke ((fun x y -> (x, y)) k) (f k a)
    [<Extension>]static member inline TraverseIndexed (Identity a  ,f , [<Optional>]output, [<Optional>]impl:TraverseIndexed) = Map.Invoke Identity (f () a)

    static member inline Invoke f t =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member TraverseIndexed: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<TraverseIndexed>, t, f)