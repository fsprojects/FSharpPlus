#nowarn "77"
// Warn FS0077 -> Member constraints with the name 'get_Item' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// Those .NET types are string and array but they are explicitely handled here.

namespace FsControl

open System
open System.Text
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Internals


[<Extension;Sealed>]
type Nth =
    inherit Default1
    [<Extension>]static member inline Nth (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.skip n |> Seq.head :'T
    [<Extension>]static member        Nth (x:string        , n, [<Optional>]impl:Nth     ) = x.[n]
    [<Extension>]static member        Nth (x:StringBuilder , n, [<Optional>]impl:Nth     ) = x.ToString().[n]
    [<Extension>]static member        Nth (x:'a []         , n, [<Optional>]impl:Nth     ) = x.[n] : 'a
    [<Extension>]static member        Nth (x:'a ResizeArray, n, [<Optional>]impl:Nth     ) = x.[n]
    [<Extension>]static member        Nth (x:list<'a>      , n, [<Optional>]impl:Nth     ) = x.[n]
    [<Extension>]static member        Nth (x:'a Id         , n, [<Optional>]impl:Nth     ) = x.getValue

    static member inline Invoke (n:int) (source:'Collection'T)  :'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Nth: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Nth>, source, n)

[<Extension;Sealed>]
type Skip =
    inherit Default1
    [<Extension>]static member inline Skip (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.skip n |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Skip (x:string        , n, [<Optional>]impl:Skip    ) = x.[n..]
    [<Extension>]static member        Skip (x:StringBuilder , n, [<Optional>]impl:Skip    ) = new StringBuilder(x.ToString().[n..])
    [<Extension>]static member        Skip (x:'a []         , n, [<Optional>]impl:Skip    ) = x.[n..] : 'a []
    [<Extension>]static member        Skip (x:'a ResizeArray, n, [<Optional>]impl:Skip    ) = ResizeArray<'a> (Seq.skip n x)
    [<Extension>]static member        Skip (x:list<'a>      , n, [<Optional>]impl:Skip    ) = n |> let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) in listSkip x
    [<Extension>]static member        Skip (x:'a Id         , n, [<Optional>]impl:Skip    ) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Skip: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Skip>, source, n)


[<Extension;Sealed>]
type Take =
    inherit Default1
    [<Extension>]static member inline Take (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.take n |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Take (x:string        , n, [<Optional>]impl:Take    ) = x.[..n-1]
    [<Extension>]static member        Take (x:StringBuilder , n, [<Optional>]impl:Take    ) = new StringBuilder(x.ToString().[..n-1])
    [<Extension>]static member        Take (x:'a []         , n, [<Optional>]impl:Take    ) = x.[..n-1] : 'a []
    [<Extension>]static member        Take (x:'a ResizeArray, n, [<Optional>]impl:Take    ) = ResizeArray<'a> (Seq.take n x)
    [<Extension>]static member        Take (x:list<'a>      , n, [<Optional>]impl:Take    ) = Seq.take n x |> Seq.toList
    [<Extension>]static member        Take (x:'a Id         , n, [<Optional>]impl:Take    ) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Take: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Take>, source, n)


[<Extension;Sealed>]
type Drop =
    inherit Default1
    [<Extension>]static member inline Drop (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.drop n |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Drop (x:string        , n, [<Optional>]impl:Drop) = if n > 0 then (if x.Length > n then x.[n..] else "") else x
    [<Extension>]static member        Drop (x:StringBuilder , n, [<Optional>]impl:Drop) = if n > 0 then (if x.Length > n then new StringBuilder(x.ToString().[n..]) else new StringBuilder()) else new StringBuilder(x.ToString())
    [<Extension>]static member        Drop (x:'a []         , n, [<Optional>]impl:Drop) = if n > 0 then (if x.Length > n then x.[n..] else [||]) else x : 'a []
    [<Extension>]static member        Drop (x:'a ResizeArray, n, [<Optional>]impl:Drop) = ResizeArray<'a> (Seq.drop n x)
    [<Extension>]static member        Drop (x:list<'a>      , n, [<Optional>]impl:Drop) = List.drop n x
    [<Extension>]static member        Drop (x:'a Id         , n, [<Optional>]impl:Drop) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Drop: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Drop>, source, n)



[<Extension;Sealed>]
type Limit =
    inherit Default1
    [<Extension>]static member inline Limit (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.truncate n |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Limit (x:string        , n, [<Optional>]impl:Limit) = if n < 1 then "" elif n < x.Length then x.[..n-1] else x
    [<Extension>]static member        Limit (x:StringBuilder , n, [<Optional>]impl:Limit) = new StringBuilder(x.ToString().[..n-1])
    [<Extension>]static member        Limit (x:'a []         , n, [<Optional>]impl:Limit) = if n < 1 then [||] elif n < x.Length then x.[..n-1] else x : 'a []
    [<Extension>]static member        Limit (x:'a ResizeArray, n, [<Optional>]impl:Limit) = ResizeArray<'a> (Seq.truncate n x)
    [<Extension>]static member        Limit (x:list<'a>      , n, [<Optional>]impl:Limit) = Seq.truncate n x |> Seq.toList
    [<Extension>]static member        Limit (x:'a Id         , n, [<Optional>]impl:Limit) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Limit: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Limit>, source, n)
 


type Choose =
    static member Choose (x:Id<'T>  , f:_->'U option, [<Optional>]impl:Choose) = invalidOp "Choose on ID" :Id<'U>
    static member Choose (x:seq<'T> , f:_->'U option, [<Optional>]impl:Choose) = Seq.choose   f x
    static member Choose (x:list<'T>, f:_->'U option, [<Optional>]impl:Choose) = List.choose  f x
    static member Choose (x:'T []   , f:_->'U option, [<Optional>]impl:Choose) = Array.choose f x

    static member inline Invoke (chooser:'T->'U option)   (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Choose: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, c) = call_3 (a, b, Unchecked.defaultof<'r>, c) :'r
        call (Unchecked.defaultof<Choose>, source, chooser)  :'Collection'U


[<Extension;Sealed>]
type Distinct =
    inherit Default1
    [<Extension>]static member inline Distinct (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.distinct |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Distinct (x:list<'T>   , [<Optional>]impl:Distinct) = Seq.distinct x |> Seq.toList
    [<Extension>]static member        Distinct (x:'T []      , [<Optional>]impl:Distinct) = Seq.distinct x |> Seq.toArray

    static member inline Invoke                         (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Distinct: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Distinct>, source)  :'Collection'T


type DistinctBy =
    inherit Default1
    static member inline DistinctBy (x:'Foldable'T, f, [<Optional>]impl:Default1  ) = x |> ToSeq.Invoke |> Seq.distinctBy f |> OfSeq.Invoke :'Foldable'T
    static member        DistinctBy (x:list<'T>   , f, [<Optional>]impl:DistinctBy) = Seq.distinctBy f x |> Seq.toList
    static member        DistinctBy (x:'T []      , f, [<Optional>]impl:DistinctBy) = Seq.distinctBy f x |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, p) = ((^a or ^b) : (static member DistinctBy: _*_*_ -> _) b, p, a)
        let inline call (a:'a, b:'b, p) = call_2 (a, b, p)
        call (Unchecked.defaultof<DistinctBy>, source, projection)    :'Collection'T


type GroupBy =
    static member GroupBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , [<Optional>]impl:GroupBy) = let a = Id.run x in Id.create (f a, x)
    static member GroupBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , [<Optional>]impl:GroupBy) = Seq.groupBy f x
    static member GroupBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, [<Optional>]impl:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , [<Optional>]impl:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke    (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member GroupBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (Unchecked.defaultof<GroupBy>, source, projection)


type ChunkBy =
    static member ChunkBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , [<Optional>]impl:ChunkBy) = let a = Id.run x in Id.create (f a, x)
    static member ChunkBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , [<Optional>]impl:ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    static member ChunkBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, [<Optional>]impl:ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member ChunkBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , [<Optional>]impl:ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member ChunkBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (Unchecked.defaultof<ChunkBy>, source, projection)


[<Extension;Sealed>]
type Length =
    inherit Default1
    [<Extension>]static member inline Length (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.length   
    [<Extension>]static member Length (x:Id<'T>  , [<Optional>]impl:Length) = 1
    [<Extension>]static member Length (x:seq<'T> , [<Optional>]impl:Length) = Seq.length   x
    [<Extension>]static member Length (x:list<'T>, [<Optional>]impl:Length) = List.length  x
    [<Extension>]static member Length (x:'T []   , [<Optional>]impl:Length) = Array.length x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Length: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Length>, source)            :int



[<Extension;Sealed>]
type Max =
    inherit Default1
    [<Extension>]static member inline Max (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.max :'T
    [<Extension>]static member Max (x:Id<'T>  , [<Optional>]impl:Max) = x.getValue
    [<Extension>]static member Max (x:seq<'T> , [<Optional>]impl:Max) = Seq.max   x
    [<Extension>]static member Max (x:list<'T>, [<Optional>]impl:Max) = List.max  x
    [<Extension>]static member Max (x:'T []   , [<Optional>]impl:Max) = Array.max x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Max: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Max>, source)                  :'T


type MaxBy =
    inherit Default1
    static member inline MaxBy (x:'Foldable'T, f, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.maxBy f :'T
    static member MaxBy (x:Id<'T>  , f:'T->'U, [<Optional>]impl:MaxBy) = x.getValue
    static member MaxBy (x:seq<'T> , f       , [<Optional>]impl:MaxBy) = Seq.maxBy   f x
    static member MaxBy (x:list<'T>, f       , [<Optional>]impl:MaxBy) = List.maxBy  f x
    static member MaxBy (x:'T []   , f       , [<Optional>]impl:MaxBy) = Array.maxBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member MaxBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MaxBy>, source, projection)      :'T


[<Extension;Sealed>]
type Min =
    inherit Default1
    [<Extension>]static member inline Min (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.min :'T
    [<Extension>]static member Min (x:Id<'T>  , [<Optional>]impl:Min) = x.getValue
    [<Extension>]static member Min (x:seq<'T> , [<Optional>]impl:Min) = Seq.min   x
    [<Extension>]static member Min (x:list<'T>, [<Optional>]impl:Min) = List.min  x
    [<Extension>]static member Min (x:'T []   , [<Optional>]impl:Min) = Array.min x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Min: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Min>, source)                  :'T


type MinBy =
    inherit Default1
    static member inline MinBy (x:'Foldable'T, f, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.minBy f :'T
    static member MinBy (x:Id<'T>  , f:'T->'U, [<Optional>]impl:MinBy) = x.getValue
    static member MinBy (x:seq<'T> , f       , [<Optional>]impl:MinBy) = Seq.minBy   f x
    static member MinBy (x:list<'T>, f       , [<Optional>]impl:MinBy) = List.minBy  f x
    static member MinBy (x:'T []   , f       , [<Optional>]impl:MinBy) = Array.minBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member MinBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MinBy>, source, projection)      :'T


[<Extension;Sealed>]
type Replace =
    inherit Default1
                 static member inline Replace (x:'Collection  , o:'Collection  , n:'Collection  , [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.replace (ToSeq.Invoke o) (ToSeq.Invoke n) |> OfSeq.Invoke : 'Collection
                 static member        Replace (x:Id<'T>       , o:Id<'T>       , n:Id<'T>       , [<Optional>]impl:Default1) = if x = o then n else x
    [<Extension>]static member        Replace (x:list<'T>     , o:list<'T>     , n:list<'T>     , [<Optional>]impl:Replace ) = x |> List.toSeq   |> Seq.replace o n |> Seq.toList
    [<Extension>]static member        Replace (x:'T []        , o:'T []        , n:'T []        , [<Optional>]impl:Replace ) = x |> Array.toSeq  |> Seq.replace o n |> Seq.toArray
    [<Extension>]static member        Replace (x:string       , o:string       , n:string       , [<Optional>]impl:Replace ) = if o.Length = 0 then x else x.Replace(o, n)
    [<Extension>]static member        Replace (x:StringBuilder, o:StringBuilder, n:StringBuilder, [<Optional>]impl:Replace ) = if o.Length = 0 then x else StringBuilder(x.ToString().Replace(o.ToString(), n.ToString()))
 
    static member inline Invoke      (o:'Collection) (n:'Collection) (source:'Collection) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Replace: _*_*_*_ -> _) b, o, n, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Replace>, source) :'Collection


[<Extension;Sealed>]
type Rev =
    inherit Default1
    [<Extension>]static member inline Rev (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.toArray |> Array.rev |> Array.toSeq |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member        Rev (x:list<'a>   , [<Optional>]impl:Rev     ) = List.rev  x
    [<Extension>]static member        Rev (x:'a []      , [<Optional>]impl:Rev     ) = Array.rev x

    static member inline Invoke  (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Rev: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Rev>, source)                   :'Collection'T


type Scan =
    static member Scan (x:Id<'T>  , f ,z:'S, [<Optional>]output:Id<'S>  , [<Optional>]impl:Scan) = Id.create (f z x.getValue)
    static member Scan (x:seq<'T> , f ,z:'S, [<Optional>]output:seq<'S> , [<Optional>]impl:Scan) = Seq.scan   f z x
    static member Scan (x:list<'T>, f ,z:'S, [<Optional>]output:list<'S>, [<Optional>]impl:Scan) = List.scan  f z x
    static member Scan (x:'T []   , f ,z:'S, [<Optional>]output:'S []   , [<Optional>]impl:Scan) = Array.scan f z x

    static member inline Invoke (folder:'State'->'T->'State) (state:'State) (source:'Collection'T) =
        let inline call_3 (a:^a, b:^b, c:^c, f, z) = ((^a or ^b or ^c) : (static member Scan: _*_*_*_*_ -> _) b, f, z, c, a)
        let inline call (a:'a, b:'b, f, z) = call_3 (a, b, Unchecked.defaultof<'r>, f, z) :'r
        call (Unchecked.defaultof<Scan>, source, folder, state) :'Collection'State


[<Extension;Sealed>]
type Sort =
    inherit Default1
    [<Extension>]static member inline Sort (x:'Foldable'T, [<Optional>]impl:Default2) = x |> ToSeq.Invoke |> Seq.sort |> OfSeq.Invoke :'Foldable'T
    [<Extension>]static member inline Sort (x:^Foldable'T, [<Optional>]impl:Default1) = ((^Foldable'T) : (static member Sort: _->_) x) : ^Foldable'T
    [<Extension>]static member        Sort (x:list<'a>   , [<Optional>]impl:Sort    ) = List.sort  x
    [<Extension>]static member        Sort (x:'a []      , [<Optional>]impl:Sort    ) = Array.sort x

    static member inline Invoke (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Sort: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Sort>, source)                 :'Collection'T


type SortBy =
    inherit Default1

    static member        SortBy (x:list<'a>   , f      , [<Optional>]impl:SortBy  ) = List.sortBy  f x
    static member        SortBy (x:'a []      , f      , [<Optional>]impl:SortBy  ) = Array.sortBy f x

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'T =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member SortBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<SortBy>, source, projection)
    static member inline InvokeOnInstance (projection:'T->'Key) (source:'Collection'T) : 'Collection'T = (^Collection'T : (static member SortBy: _*_->_) projection, source) : ^Collection'T

    static member inline SortBy (x:'Foldable'T, f      , [<Optional>]impl:Default2) = x |> ToSeq.Invoke |> Seq.sortBy f |> OfSeq.Invoke :'Foldable'T
    static member inline SortBy (x:^Foldable'T, f      , [<Optional>]impl:Default1) = ((^Foldable'T) : (static member SortBy: _*_->_) f, x) : ^Foldable'T
    static member inline SortBy (_ : ^t when ^t : null and ^t : struct, f : 'T -> 'U, mthd : Default1) = id


[<Extension;Sealed>]
type Split =
    inherit Default1
    [<Extension>]static member        Split (x:seq<'T>      , e:seq<seq<'T>>      , [<Optional>]impl:Split) = x |>                 Seq.split StringSplitOptions.None e
    [<Extension>]static member        Split (x:list<'T>     , e:seq<list<'T>>     , [<Optional>]impl:Split) = x |> List.toSeq   |> Seq.split StringSplitOptions.None e |> Seq.map Seq.toList
    [<Extension>]static member        Split (x:'T []        , e:seq<'T []>        , [<Optional>]impl:Split) = x |> Array.toSeq  |> Seq.split StringSplitOptions.None e |> Seq.map Seq.toArray
    [<Extension>]static member        Split (x:string       , e:seq<string>       , [<Optional>]impl:Split) = x.Split(Seq.toArray e, StringSplitOptions.None) :> seq<_>
    [<Extension>]static member        Split (x:StringBuilder, e:seq<StringBuilder>, [<Optional>]impl:Split) = x.ToString().Split(e |> Seq.map (fun x -> x.ToString()) |> Seq.toArray, StringSplitOptions.None) |> Array.map StringBuilder :> seq<_>
 
    static member inline Invoke      (sep:seq<'Collection>)        (source:'Collection)       =
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member Split: _*_*_ -> _) b, s, a)
        let inline call (a:'a, b:'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Split>, source,sep) :seq<'Collection>


[<Extension;Sealed>]
type Zip =
    [<Extension>]static member Zip (x:Id<'T>  , y:Id<'U>  , [<Optional>]output:Id<'T*'U>  , [<Optional>]impl:Zip) = Id.create(x.getValue,y.getValue)
    [<Extension>]static member Zip (x:seq<'T> , y:seq<'U> , [<Optional>]output:seq<'T*'U> , [<Optional>]impl:Zip) = Seq.zip   x y
    [<Extension>]static member Zip (x:list<'T>, y:list<'U>, [<Optional>]output:list<'T*'U>, [<Optional>]impl:Zip) = List.zip  x y
    [<Extension>]static member Zip (x:'T []   , y:'U []   , [<Optional>]output:('T*'U) [] , [<Optional>]impl:Zip) = Array.zip x y

    static member inline Invoke (source1:'Collection'T1) (source2:'Collection'T2)          =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member Zip: _*_*_*_ -> _) b, c, d, a)
        let inline call (a:'a, b:'b, c:'c) = call_4 (a, b, c, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<Zip>, source1, source2)           :'Collection'T1'T2