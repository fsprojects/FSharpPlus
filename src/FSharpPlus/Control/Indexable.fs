namespace FSharpPlus.Control

#nowarn "77"
// Warn FS0077 -> Member constraints with the name 'get_Item' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// Those .NET types are string and array but they are explicitely handled here.

#if !FABLE_COMPILER

open System
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Internals
open FSharpPlus.Internals.MonadOps


type Item =
    inherit Default1
    
    static member inline Item (x: '``Indexable<'T>`` , k        , [<Optional>]_impl: Default1) = (^``Indexable<'T>`` : (member get_Item : _ -> 'T) x, k) : 'T
    static member inline Item (_: 'T when 'T: null and 'T: struct, _,         _impl: Default1) = ()
    
    static member        Item (x: string             , n        , [<Optional>]_impl: Item    ) = String.item n x
    static member        Item (x: StringBuilder      , n        , [<Optional>]_impl: Item    ) = x.ToString().[n]
    static member        Item (x: 'T []              , n        , [<Optional>]_impl: Item    ) = x.[n]       : 'T    
    static member        Item (x: 'T [,]             , (i,j)    , [<Optional>]_impl: Item    ) = x.[i,j]     : 'T
    static member        Item (x: 'T [,,]            , (i,j,k)  , [<Optional>]_impl: Item    ) = x.[i,j,k]   : 'T
    static member        Item (x: 'T [,,,]           , (i,j,k,l), [<Optional>]_impl: Item    ) = x.[i,j,k,l] : 'T

    static member inline Invoke (n: 'K) (source: '``Indexed<'T>``) : 'T =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Item: _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Item>, source, n)


type TryItem =
    inherit Default1
    static member inline TryItem (x: '``Indexable<'T>``, k, [<Optional>]_impl: Default2) =
        let mutable r = Unchecked.defaultof< ^R>
        if (^``Indexable<'T>``: (member TryGetValue: _ * _ -> _) (x, k, &r)) then Some r else None
    
    static member inline TryItem (x: '``Indexable<'T>``, k        , [<Optional>]_impl: Default1) = (^``Indexable<'T>`` : (static member TryItem : _ * _ -> _) k, x) : 'T option
    static member inline TryItem (_: 'T when 'T: null and 'T: struct, _       , _impl: Default1) = ()

    static member        TryItem (x: string            , n        , [<Optional>]_impl: TryItem ) = String.tryItem n x
    static member        TryItem (x: StringBuilder     , n        , [<Optional>]_impl: TryItem ) = if n >= 0 && n < x.Length then Some ((string x).[n]) else None
    static member        TryItem (x: 'a []             , n        , [<Optional>]_impl: TryItem ) = if n >= x.GetLowerBound 0 && n <= x.GetUpperBound 0 then Some x.[n] else None : 'a option
    static member        TryItem (x: 'a [,]            , (i,j)    , [<Optional>]_impl: TryItem ) = if (i, j)       >= (x.GetLowerBound 0, x.GetLowerBound 1                                      ) && (i, j)       <= (x.GetUpperBound 0, x.GetUpperBound 1                                      ) then Some x.[i,j]     else None : 'a option
    static member        TryItem (x: 'a [,,]           , (i,j,k)  , [<Optional>]_impl: TryItem ) = if (i, j, k)    >= (x.GetLowerBound 0, x.GetLowerBound 1, x.GetLowerBound 2)                    && (i, j, k)    <= (x.GetUpperBound 0, x.GetUpperBound 1, x.GetUpperBound 2                   ) then Some x.[i,j,k]   else None : 'a option
    static member        TryItem (x: 'a [,,,]          , (i,j,k,l), [<Optional>]_impl: TryItem ) = if (i, j, k, l) >= (x.GetLowerBound 0, x.GetLowerBound 1, x.GetLowerBound 2, x.GetLowerBound 3) && (i, j, k, l) <= (x.GetUpperBound 0, x.GetUpperBound 1, x.GetUpperBound 2, x.GetUpperBound 3) then Some x.[i,j,k,l] else None : 'a option
    static member        TryItem (x: 'a ResizeArray    , n        , [<Optional>]_impl: TryItem ) = if n >= 0 && n < x.Count then Some x.[n] else None
    static member        TryItem (x: list<'a>          , n        , [<Optional>]_impl: TryItem ) = List.tryItem n x
    static member        TryItem (x: IList<'a>         , n        , [<Optional>]_impl: Default2) = if n >= 0 && n < x.Count then Some x.[n] else None
    static member        TryItem (x: IReadOnlyList<'a> , n        , [<Optional>]_impl: Default3) = if n >= 0 && n < x.Count then Some x.[n] else None
    static member        TryItem (x: Map<'K,'T>        , k        , [<Optional>]_impl: TryItem ) = x.TryFind k : 'T option

    static member inline Invoke (n: 'K) (source: '``Indexed<'T>``) : 'T option =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member TryItem : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<TryItem>, source, n)



type MapIndexed =
    inherit Default1
    static member MapIndexed (x: list<'T>   , f            , [<Optional>]_impl: MapIndexed) = List.mapi  f x
    static member MapIndexed (x: 'T []      , f            , [<Optional>]_impl: MapIndexed) = Array.mapi f x
    static member MapIndexed ((k: 'K, a: 'T), f            , [<Optional>]_impl: MapIndexed) = (k, ((f k a) : 'U))
    static member MapIndexed (g             , f: 'K->'T->'U, [<Optional>]_impl: MapIndexed) = fun x -> f x (g x)
    static member MapIndexed (x: Map<'K,'T> , f            , [<Optional>]_impl: MapIndexed) = Map.map f x : Map<'K,'U>

    static member inline Invoke (mapping: 'K->'T->'U) (source: '``Indexable<'T>``) =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member MapIndexed : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MapIndexed>, source, mapping)
    static member inline InvokeOnInstance (mapping: 'K->'T->'Key) (source: '``Indexable<'T>``) : '``Indexable<'U>`` = (^``Indexable<'T>`` : (static member MapIndexed : _*_->_) source, mapping) : ^``Indexable<'U>``

    static member inline MapIndexed (x: seq<'T>   , f: int->'T->'U, _impl: Default2) = x |> Seq.mapi f : seq<'U>
    static member inline MapIndexed (x: ^``I<'T>``, f: 'K->'T->'U , _impl: Default1) = (^``I<'T>`` : (static member MapIndexed : _*_->_) x, f) : '``I<'U>``
    static member inline MapIndexed (_: ^t when ^t: null and ^t: struct, _: 'K->'T->'U, _mthd: Default1) = ()


type IterateIndexed =
    static member IterateIndexed (x: Id<'T>    , f: _->'T->unit, [<Optional>]_impl: IterateIndexed) = f () x.getValue
    static member IterateIndexed (x: seq<'T>   , f             , [<Optional>]_impl: IterateIndexed) = Seq.iteri   f x
    static member IterateIndexed (x: list<'T>  , f             , [<Optional>]_impl: IterateIndexed) = List.iteri  f x
    static member IterateIndexed (x: 'T []     , f             , [<Optional>]_impl: IterateIndexed) = Array.iteri f x
    static member IterateIndexed (x: Map<'K,'T>, f             , [<Optional>]_impl: IterateIndexed) = Map.iter f x

    static member inline Invoke (action: 'K->'T->unit) (source: '``Indexable<'T>``)        =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member IterateIndexed : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<IterateIndexed>,  source, action) : unit



type FoldIndexed =
    static member FoldIndexed (x: seq<_>    , f, z, _impl: FoldIndexed) = x |> Seq.fold   (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member FoldIndexed (x: list<_>   , f, z, _impl: FoldIndexed) = x |> List.fold  (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member FoldIndexed (x: _ []      , f, z, _impl: FoldIndexed) = x |> Array.fold (fun (p, i) t -> (f p i t, i + 1)) (z, 0) |> fst
    static member FoldIndexed (_: Map<'k,'t>, f, z, _impl: FoldIndexed) = Map.fold f z

    static member inline Invoke (folder: 'State->'Key->'T->'State) (state: 'State) (foldable: '``Foldable<'T>``) : 'State =
        let inline call_2 (a: ^a, b: ^b, f, z) = ((^a or ^b) : (static member FoldIndexed : _*_*_*_ -> _) b, f, z, a)
        let inline call (a: 'a, b: 'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<FoldIndexed>, foldable, folder, state)


type TraverseIndexed =
    static member inline TraverseIndexed ((k: 'K, a: 'T), f , [<Optional>]_output: 'R, [<Optional>]_impl: TraverseIndexed) : 'R = Map.Invoke ((fun x y -> (x, y)) k) (f k a)
    static member inline TraverseIndexed (a: Tuple<_>   , f , [<Optional>]_output: 'R, [<Optional>]_impl: TraverseIndexed) : 'R = Map.Invoke Tuple (f () a.Item1)
    
    static member inline TraverseIndexed (t: Map<_,_>   , f , [<Optional>]_output: 'R, [<Optional>]_impl: TraverseIndexed) : 'R =
        let insert_f k x ys = Map.Invoke (Map.add k) (f k x) <*> ys
        Map.foldBack insert_f t (result Map.empty)

    static member inline Invoke f t =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f) = ((^a or ^b or ^c) : (static member TraverseIndexed : _*_*_*_ -> _) b, f, c, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) : 'r
        call (Unchecked.defaultof<TraverseIndexed>, t, f)



type FindIndex =
    inherit Default1
    static member        FindIndex (x: string           , p            , [<Optional>]_impl: FindIndex) = String.findIndex p x
    static member        FindIndex (x: 'a []            , p            , [<Optional>]_impl: FindIndex) = Array.findIndex p x
    static member        FindIndex (x: 'a ResizeArray   , p: 'a -> bool, [<Optional>]_impl: FindIndex) = Seq.findIndex p x
    static member        FindIndex (x: list<'a>         , p            , [<Optional>]_impl: FindIndex) = List.findIndex p x
    static member        FindIndex (x: seq<'a>          , p            , [<Optional>]_impl: FindIndex) = Seq.findIndex p x
    static member        FindIndex (x: 'a Id            , p: 'a -> bool, [<Optional>]_impl: FindIndex) = List.findIndex p [x.getValue]

    static member inline Invoke (p: 'T -> bool) (source: '``Collection<'T>``) : 'Index =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member FindIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<FindIndex>, source, p)

type TryFindIndex =
    inherit Default1
    static member        TryFindIndex (x: string           , p            , [<Optional>]_impl: TryFindIndex) = String.tryFindIndex p x
    static member        TryFindIndex (x: 'a []            , p            , [<Optional>]_impl: TryFindIndex) = Array.tryFindIndex p x
    static member        TryFindIndex (x: 'a ResizeArray   , p: 'a -> bool, [<Optional>]_impl: TryFindIndex) = Seq.tryFindIndex p x
    static member        TryFindIndex (x: list<'a>         , p            , [<Optional>]_impl: TryFindIndex) = List.tryFindIndex p x
    static member        TryFindIndex (x: seq<'a>          , p            , [<Optional>]_impl: TryFindIndex) = Seq.tryFindIndex p x
    static member        TryFindIndex (x: 'a Id            , p: 'a -> bool, [<Optional>]_impl: TryFindIndex) = List.tryFindIndex p [x.getValue]

    static member inline Invoke (p: 'T -> bool) (source: '``Collection<'T>``) : 'Index option =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member TryFindIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<TryFindIndex>, source, p)


type FindSliceIndex =
    inherit Default1
    static member        FindSliceIndex (x: string           , e                   , [<Optional>]_impl: FindSliceIndex) = String.findSliceIndex e x    
    static member        FindSliceIndex (x: 'a []            , e                   , [<Optional>]_impl: FindSliceIndex) = Array.findSliceIndex e x
    static member        FindSliceIndex (x: 'a ResizeArray   , e: 'a ResizeArray   , [<Optional>]_impl: FindSliceIndex) = Seq.findSliceIndex e x
    static member        FindSliceIndex (x: list<'a>         , e                   , [<Optional>]_impl: FindSliceIndex) = List.findSliceIndex e x
    static member        FindSliceIndex (x: seq<'a>          , e                   , [<Optional>]_impl: FindSliceIndex) = Seq.findSliceIndex e x
    static member        FindSliceIndex (x: 'a Id            , e: 'a Id            , [<Optional>]_impl: FindSliceIndex) = List.findSliceIndex [e.getValue] [x.getValue]

    static member inline Invoke (slice: '``Collection<'T>``) (source: '``Collection<'T>``) : 'Index =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member FindSliceIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<FindSliceIndex>, source, slice)

type TryFindSliceIndex =
    inherit Default1
    static member        TryFindSliceIndex (x: string           , e                   , [<Optional>]_impl: TryFindSliceIndex) = String.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a []            , e                   , [<Optional>]_impl: TryFindSliceIndex) = Array.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a ResizeArray   , e: 'a ResizeArray   , [<Optional>]_impl: TryFindSliceIndex) = Seq.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: list<'a>         , e                   , [<Optional>]_impl: TryFindSliceIndex) = List.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: seq<'a>          , e                   , [<Optional>]_impl: TryFindSliceIndex) = Seq.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a Id            , e: 'a Id            , [<Optional>]_impl: TryFindSliceIndex) = List.tryFindSliceIndex [e.getValue] [x.getValue]

    static member inline Invoke (slice: '``Collection<'T>``) (source: '``Collection<'T>``) : 'Index option =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member TryFindSliceIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<TryFindSliceIndex>, source, slice)

#endif
