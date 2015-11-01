namespace FsControl

open System
open System.Text
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Core.Internals


[<Extension;Sealed>]
type Item =
    inherit Default1
    [<Extension>]static member inline Item (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.skip n |> Seq.head :'T
    [<Extension>]static member        Item (x:string        , n, [<Optional>]impl:Item    ) = x.[n]
    [<Extension>]static member        Item (x:StringBuilder , n, [<Optional>]impl:Item    ) = x.ToString().[n]
    [<Extension>]static member        Item (x:'a []         , n, [<Optional>]impl:Item    ) = x.[n] : 'a
    [<Extension>]static member        Item (x:'a ResizeArray, n, [<Optional>]impl:Item    ) = x.[n]
    [<Extension>]static member        Item (x:list<'a>      , n, [<Optional>]impl:Item    ) = x.[n]
    [<Extension>]static member        Item (x:'a Id         , n, [<Optional>]impl:Item    ) = x.getValue

    static member inline Invoke (n:int) (source:'Collection'T)  :'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Item: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Item>, source, n)

[<Extension;Sealed>]
type Skip =
    inherit Default1
    [<Extension>]static member inline Skip (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.skip n |> FromSeq.Invoke :'Foldable'T
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
    [<Extension>]static member inline Take (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.take n |> FromSeq.Invoke :'Foldable'T
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
    [<Extension>]static member inline Drop (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.drop n |> FromSeq.Invoke :'Foldable'T
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
    [<Extension>]static member inline Limit (x:'Foldable'T   , n, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.truncate n |> FromSeq.Invoke :'Foldable'T
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



type FromList =

#if NOTNET35
    static member FromList (_:string        , _:FromList) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList)
    static member FromList (_:StringBuilder , _:FromList) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList))
#else
    static member FromList (_:string        , _:FromList) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList |> Array.map string)
    static member FromList (_:StringBuilder , _:FromList) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList |> Array.map string))
#endif
    static member FromList (_:'a []         , _:FromList) = Array.ofList<'a>
    static member FromList (_:'a ResizeArray, _:FromList) = fun (x:list<'a>)   -> ResizeArray x
    static member FromList (_:list<'a>      , _:FromList) = id<list<'a>>
    static member FromList (_:Set<'a>       , _:FromList) = Set.ofList<'a>
    static member FromList (_:seq<'a>       , _:FromList) = Seq.ofList<'a>

    static member inline Invoke (value :list<'t>) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromList: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<FromList> value


 


[<Extension;Sealed>]
type Choose =
    [<Extension>]static member Choose (x:Id<'T>  , f:_->'U option, [<Optional>]impl:Choose) = invalidOp "Choose on ID" :Id<'U>
    [<Extension>]static member Choose (x:seq<'T> , f:_->'U option, [<Optional>]impl:Choose) = Seq.choose   f x
    [<Extension>]static member Choose (x:list<'T>, f:_->'U option, [<Optional>]impl:Choose) = List.choose  f x
    [<Extension>]static member Choose (x:'T []   , f:_->'U option, [<Optional>]impl:Choose) = Array.choose f x

    static member inline Invoke (chooser:'T->'U option)   (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Choose: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, c) = call_3 (a, b, Unchecked.defaultof<'r>, c) :'r
        call (Unchecked.defaultof<Choose>, source, chooser)  :'Collection'U


[<Extension;Sealed>]
type Distinct =
    inherit Default1
    [<Extension>]static member inline Distinct (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.distinct |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        Distinct (x:list<'T>   , [<Optional>]impl:Distinct) = Seq.distinct x |> Seq.toList
    [<Extension>]static member        Distinct (x:'T []      , [<Optional>]impl:Distinct) = Seq.distinct x |> Seq.toArray

    static member inline Invoke                         (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Distinct: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Distinct>, source)  :'Collection'T


[<Extension;Sealed>]
type DistinctBy =
    inherit Default1
    [<Extension>]static member inline DistinctBy (x:'Foldable'T, f, [<Optional>]impl:Default1  ) = x |> ToSeq.Invoke |> Seq.distinctBy f |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        DistinctBy (x:list<'T>   , f, [<Optional>]impl:DistinctBy) = Seq.distinctBy f x |> Seq.toList
    [<Extension>]static member        DistinctBy (x:'T []      , f, [<Optional>]impl:DistinctBy) = Seq.distinctBy f x |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, p) = ((^a or ^b) : (static member DistinctBy: _*_*_ -> _) b, p, a)
        let inline call (a:'a, b:'b, p) = call_2 (a, b, p)
        call (Unchecked.defaultof<DistinctBy>, source, projection)    :'Collection'T


[<Extension;Sealed>]
type GroupBy =
    [<Extension>]static member GroupBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , [<Optional>]impl:GroupBy) = let a = Id.run x in Id.create (f a, x)
    [<Extension>]static member GroupBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , [<Optional>]impl:GroupBy) = Seq.groupBy f x
    [<Extension>]static member GroupBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, [<Optional>]impl:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    [<Extension>]static member GroupBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , [<Optional>]impl:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke    (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member GroupBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (Unchecked.defaultof<GroupBy>, source, projection)


[<Extension;Sealed>]
type GroupAdjBy =
    [<Extension>]static member GroupAdjBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , [<Optional>]impl:GroupAdjBy) = let a = Id.run x in Id.create (f a, x)
    [<Extension>]static member GroupAdjBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , [<Optional>]impl:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    [<Extension>]static member GroupAdjBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, [<Optional>]impl:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    [<Extension>]static member GroupAdjBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , [<Optional>]impl:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member GroupAdjBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (Unchecked.defaultof<GroupAdjBy>, source, projection)


[<Extension;Sealed>]
type Intersperse =
    inherit Default1
    [<Extension>]static member inline Intersperse (x:'Foldable'T, e:'T, [<Optional>]impl:Default1   ) = x |> ToSeq.Invoke |> Seq.intersperse e |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        Intersperse (x:list<'T>   , e:'T, [<Optional>]impl:Intersperse) = x |> List.toSeq   |> Seq.intersperse e |> Seq.toList
    [<Extension>]static member        Intersperse (x:'T []      , e:'T, [<Optional>]impl:Intersperse) = x |> Array.toSeq  |> Seq.intersperse e |> Seq.toArray
 
    static member inline Invoke      (sep:'T)        (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member Intersperse: _*_*_ -> _) b, s, a)
        let inline call (a:'a, b:'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Intersperse>, source,sep) :'Collection'T
    

[<Extension;Sealed>]
type Iteri =
    [<Extension>]static member Iteri (x:Id<'T>  , f:int->'T->unit, [<Optional>]impl:Iteri) = f 0 x.getValue
    [<Extension>]static member Iteri (x:seq<'T> , f              , [<Optional>]impl:Iteri) = Seq.iteri   f x
    [<Extension>]static member Iteri (x:list<'T>, f              , [<Optional>]impl:Iteri) = List.iteri  f x
    [<Extension>]static member Iteri (x:'T []   , f              , [<Optional>]impl:Iteri) = Array.iteri f x

    static member inline Invoke (action:int->'T->unit)     (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Iteri: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<Iteri>,  source, action)    :unit


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
type Mapi =
    [<Extension>]static member Mapi (x:Id<'T>  , f:int->'T->'U, [<Optional>]impl:Mapi) = f 0 x.getValue
    [<Extension>]static member Mapi (x:seq<'T> , f            , [<Optional>]impl:Mapi) = Seq.mapi   f x
    [<Extension>]static member Mapi (x:list<'T>, f            , [<Optional>]impl:Mapi) = List.mapi  f x
    [<Extension>]static member Mapi (x:'T []   , f            , [<Optional>]impl:Mapi) = Array.mapi f x

    static member inline Invoke    (mapping:int->'T->'U)    (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Mapi: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Mapi>,   source, mapping)     :'Collection'U


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


[<Extension;Sealed>]
type MaxBy =
    inherit Default1
    [<Extension>]static member inline MaxBy (x:'Foldable'T, f, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.maxBy f :'T
    [<Extension>]static member MaxBy (x:Id<'T>  , f:'T->'U, [<Optional>]impl:MaxBy) = x.getValue
    [<Extension>]static member MaxBy (x:seq<'T> , f       , [<Optional>]impl:MaxBy) = Seq.maxBy   f x
    [<Extension>]static member MaxBy (x:list<'T>, f       , [<Optional>]impl:MaxBy) = List.maxBy  f x
    [<Extension>]static member MaxBy (x:'T []   , f       , [<Optional>]impl:MaxBy) = Array.maxBy f x

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


[<Extension;Sealed>]
type MinBy =
    inherit Default1
    [<Extension>]static member inline MinBy (x:'Foldable'T, f, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.minBy f :'T
    [<Extension>]static member MinBy (x:Id<'T>  , f:'T->'U, [<Optional>]impl:MinBy) = x.getValue
    [<Extension>]static member MinBy (x:seq<'T> , f       , [<Optional>]impl:MinBy) = Seq.minBy   f x
    [<Extension>]static member MinBy (x:list<'T>, f       , [<Optional>]impl:MinBy) = List.minBy  f x
    [<Extension>]static member MinBy (x:'T []   , f       , [<Optional>]impl:MinBy) = Array.minBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member MinBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MinBy>, source, projection)      :'T


[<Extension;Sealed>]
type Rev =
    inherit Default1
    [<Extension>]static member inline Rev (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.toArray |> Array.rev |> Array.toSeq |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        Rev (x:list<'a>   , [<Optional>]impl:Rev     ) = List.rev  x
    [<Extension>]static member        Rev (x:'a []      , [<Optional>]impl:Rev     ) = Array.rev x

    static member inline Invoke  (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Rev: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Rev>, source)                   :'Collection'T


[<Extension;Sealed>]
type Scan =
    [<Extension>]static member Scan (x:Id<'T>  , f ,z:'S, [<Optional>]output:Id<'S>  , [<Optional>]impl:Scan) = Id.create (f z x.getValue)
    [<Extension>]static member Scan (x:seq<'T> , f ,z:'S, [<Optional>]output:seq<'S> , [<Optional>]impl:Scan) = Seq.scan   f z x
    [<Extension>]static member Scan (x:list<'T>, f ,z:'S, [<Optional>]output:list<'S>, [<Optional>]impl:Scan) = List.scan  f z x
    [<Extension>]static member Scan (x:'T []   , f ,z:'S, [<Optional>]output:'S []   , [<Optional>]impl:Scan) = Array.scan f z x

    static member inline Invoke (folder:'State'->'T->'State) (state:'State) (source:'Collection'T) =
        let inline call_3 (a:^a, b:^b, c:^c, f, z) = ((^a or ^b or ^c) : (static member Scan: _*_*_*_*_ -> _) b, f, z, c, a)
        let inline call (a:'a, b:'b, f, z) = call_3 (a, b, Unchecked.defaultof<'r>, f, z) :'r
        call (Unchecked.defaultof<Scan>, source, folder, state) :'Collection'State


[<Extension;Sealed>]
type Sort =
    inherit Default1
    [<Extension>]static member inline Sort (x:'Foldable'T, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.sort |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        Sort (x:list<'a>   , [<Optional>]impl:Sort    ) = List.sort  x
    [<Extension>]static member        Sort (x:'a []      , [<Optional>]impl:Sort    ) = Array.sort x

    static member inline Invoke (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Sort: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Sort>, source)                 :'Collection'T


[<Extension;Sealed>]
type SortBy =
    inherit Default1
    [<Extension>]static member inline SortBy (x:'Foldable'T, f      , [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.sortBy f |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        SortBy (x:list<'a>   , f      , [<Optional>]impl:SortBy  ) = List.sortBy  f x
    [<Extension>]static member        SortBy (x:'a []      , f      , [<Optional>]impl:SortBy  ) = Array.sortBy f x

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'T =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member SortBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<SortBy>, source, projection)


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