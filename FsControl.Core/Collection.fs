namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Types
open System
open System.Text


type Skip() =
    static member val Instance = Skip()
    static member Skip (x:string        , n, _:Skip) = x.[n..]
    static member Skip (x:StringBuilder , n, _:Skip) = new StringBuilder(x.ToString().[n..])
    static member Skip (x:'a []         , n, _:Skip) = x.[n..] : 'a []
    static member Skip (x:'a ResizeArray, n, _:Skip) = ResizeArray<'a> (Seq.skip n x)
    static member Skip (x:list<'a>      , n, _:Skip) = n |> let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) in listSkip x
    static member Skip (x:seq<'a>       , n, _:Skip) = Seq.skip n x
    static member Skip (x:'a Id         , n, _:Skip) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Skip: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Skip.Instance, source, n)


type Take() =
    static member val Instance = Take()
    static member Take (x:string        , n, _:Take) = x.[..n-1]
    static member Take (x:StringBuilder , n, _:Take) = new StringBuilder(x.ToString().[..n-1])
    static member Take (x:'a []         , n, _:Take) = x.[n..] : 'a []
    static member Take (x:'a ResizeArray, n, _:Take) = ResizeArray<'a> (Seq.take n x)
    static member Take (x:list<'a>      , n, _:Take) = Seq.take n x |> Seq.toList
    static member Take (x:seq<'a>       , n, _:Take) = Seq.take n x
    static member Take (x:'a Id         , n, _:Take) = x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b, n) = ((^a or ^b) : (static member Take: _*_*_ -> _) b, n, a)
        let inline call (a:'a, b:'b, n) = call_2 (a, b, n)
        call (Take.Instance, source, n)


type FromList() =
    static member val Instance = FromList()

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
        call FromList.Instance value


type FromSeq() =
    inherit Default1()
    static member val Instance = FromSeq()

#if NOTNET35
    static member FromSeq (_:string        , _:FromSeq) = fun (x:seq<char>) -> String.Join("", Array.ofSeq x)
    static member FromSeq (_:StringBuilder , _:FromSeq) = fun (x:seq<char>) -> new StringBuilder(String.Join("", Array.ofSeq x))
#else
    static member FromSeq (_:string        , _:FromSeq) = fun (x:seq<char>) -> String.Join("",  x |> Array.ofSeq |> Array.map string)
    static member FromSeq (_:StringBuilder , _:FromSeq) = fun (x:seq<char>) -> new StringBuilder(String.Join("", x |> Array.ofSeq |> Array.map string))
#endif
    static member FromSeq (_:'a []         , _:FromSeq) = Array.ofSeq<'a>
    static member FromSeq (_:'a ResizeArray, _:FromSeq) = fun (x:seq<'a>)   -> ResizeArray x
    static member FromSeq (_:list<'a>      , _:FromSeq) = List.ofSeq<'a>
    static member FromSeq (_:Set<'a>       , _:FromSeq) = Set.ofSeq<'a>
    static member FromSeq (_:seq<'a>       , _:FromSeq) = id<seq<'a>>
    static member FromSeq (_:'a Id         , _:FromSeq) = fun (x:seq<'a>)   -> Id.create (Seq.head x)

    static member inline Invoke  (value :seq<'t> ) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromSeq: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromSeq.Instance value


type ToSeq() =
    static member val Instance = ToSeq()
    static member ToSeq (x:#seq<'T>  , _:ToSeq) = x :> seq<_>
    static member ToSeq (x:Id<'T>    , _:ToSeq) = Seq.singleton x.getValue
    static member ToSeq (x:option<'T>, _:ToSeq) = match x with Some x -> Seq.singleton x | None -> Seq.empty

    static member inline Invoke (source:'Collection'T)  : seq<'T>  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToSeq: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToSeq.Instance, source)   


type Choose() =
    static member val Instance = Choose()
    static member Choose (x:Id<'T>  , f:_->'U option, _:Id<'U>  , _:Choose) = invalidOp "Choose on ID" :Id<'U> 
    static member Choose (x:seq<'T> , f:_->'U option, _:seq<'U> , _:Choose) = Seq.choose   f x
    static member Choose (x:list<'T>, f:_->'U option, _:list<'U>, _:Choose) = List.choose  f x
    static member Choose (x:'T []   , f:_->'U option, _:'U []   , _:Choose) = Array.choose f x

    static member inline Invoke (chooser:'T->'U option)   (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Choose: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, c) = call_3 (a, b, Unchecked.defaultof<'r>, c) :'r
        call (Choose.Instance, source, chooser)  :'Collection'U


type Distinct() =
    static member val Instance = Distinct()
    static member Distinct (x:Id<'T>  , _:Distinct) = x
    static member Distinct (x:seq<'T> , _:Distinct) = Seq.distinct x
    static member Distinct (x:list<'T>, _:Distinct) = Seq.distinct x |> Seq.toList
    static member Distinct (x:'T []   , _:Distinct) = Seq.distinct x |> Seq.toArray

    static member inline Invoke                         (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Distinct: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Distinct.Instance, source)  :'Collection'T


type DistinctBy() =
    static member val Instance = DistinctBy()
    static member DistinctBy (x:Id<'T>  , f, _:DistinctBy) = x
    static member DistinctBy (x:seq<'T> , f, _:DistinctBy) = Seq.distinctBy f x
    static member DistinctBy (x:list<'T>, f, _:DistinctBy) = Seq.distinctBy f x |> Seq.toList
    static member DistinctBy (x:'T []   , f, _:DistinctBy) = Seq.distinctBy f x |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, p) = ((^a or ^b) : (static member DistinctBy: _*_*_ -> _) b, p, a)
        let inline call (a:'a, b:'b, p) = call_2 (a, b, p)
        call (DistinctBy.Instance, source, projection)    :'Collection'T


type GroupBy() =
    static member val Instance = GroupBy()
    static member GroupBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , _:GroupBy) = let a = Id.run x in Id.create (f a, x)
    static member GroupBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , _:GroupBy) = Seq.groupBy f x
    static member GroupBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, _:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , _:GroupBy) = Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke    (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member GroupBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (GroupBy.Instance, source, projection)


type GroupAdjBy() =
    static member val Instance = GroupAdjBy()
    static member GroupAdjBy (x:Id<'T>  , f:'T->'Key, _:Id<'Key*Id<'T>>    , _:GroupAdjBy) = let a = Id.run x in Id.create (f a, x)
    static member GroupAdjBy (x:seq<'T> , f:'T->'Key, _:seq<'Key*seq<'T>>  , _:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    static member GroupAdjBy (x:list<'T>, f:'T->'Key, _:list<'Key*list<'T>>, _:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupAdjBy (x:'T []   , f:'T->'Key, _:('Key*('T [])) []  , _:GroupAdjBy) = Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c, p) = ((^a or ^b or ^c) : (static member GroupAdjBy: _*_*_*_ -> _) b, p, c, a)
        let inline call (a:'a, b:'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) :'r
        call (GroupAdjBy.Instance, source, projection)


type Intersperse() =
    static member val Instance = Intersperse()

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    static member inline internal intersperse sep list = seq {
        let notFirst = ref false
        for element in list do 
            if !notFirst then yield sep
            yield element
            notFirst := true}

    static member Intersperse (x:Id<'T>  , e:'T, _:Intersperse) = x
    static member Intersperse (x:seq<'T> , e:'T, _:Intersperse) = Intersperse.intersperse e x
    static member Intersperse (x:list<'T>, e:'T, _:Intersperse) = x |> List.toSeq  |> Intersperse.intersperse e |> Seq.toList
    static member Intersperse (x:'T []   , e:'T, _:Intersperse) = x |> Array.toSeq |> Intersperse.intersperse e |> Seq.toArray
 
    static member inline Invoke      (sep:'T)        (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member Intersperse: _*_*_ -> _) b, s, a)
        let inline call (a:'a, b:'b, s) = call_2 (a, b, s)
        call (Intersperse.Instance, source,sep) :'Collection'T
    

type Iteri() =
    static member val Instance = Iteri()
    static member Iteri (x:Id<'T>  , f:int->'T->unit, _:Iteri) = f 0 x.getValue
    static member Iteri (x:seq<'T> , f              , _:Iteri) = Seq.iteri   f x
    static member Iteri (x:list<'T>, f              , _:Iteri) = List.iteri  f x
    static member Iteri (x:'T []   , f              , _:Iteri) = Array.iteri f x

    static member inline Invoke (action:int->'T->unit)     (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Iteri: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Iteri.Instance,  source, action)    :unit


type Length() =
    static member val Instance = Length()
    static member Length (x:Id<'T>  , _:Length) = 1
    static member Length (x:seq<'T> , _:Length) = Seq.length   x
    static member Length (x:list<'T>, _:Length) = List.length  x
    static member Length (x:'T []   , _:Length) = Array.length x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Length: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Length.Instance, source)            :int


type Mapi() =
    static member val Instance = Mapi()
    static member Mapi (_:Id<'U>  , f:int->'T->'U, _:Mapi, x:Id<'T>  ) = f 0 x.getValue
    static member Mapi (_:seq<'U> , f            , _:Mapi, x:seq<'T> ) = Seq.mapi   f x
    static member Mapi (_:list<'U>, f            , _:Mapi, x:list<'T>) = List.mapi  f x
    static member Mapi (_:'U []   , f            , _:Mapi, x:'T []   ) = Array.mapi f x

    static member inline Invoke    (mapping:int->'T->'U)    (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Mapi: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Mapi.Instance,   source, mapping)     :'Collection'U


type Max() =
    static member val Instance = Max()
    static member Max (x:Id<'T>  , _:Max) = x.getValue
    static member Max (x:seq<'T> , _:Max) = Seq.max   x
    static member Max (x:list<'T>, _:Max) = List.max  x
    static member Max (x:'T []   , _:Max) = Array.max x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Max: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Max.Instance, source)                  :'T


type MaxBy() =
    static member val Instance = MaxBy()
    static member MaxBy (x:Id<'T>  , f:'T->'U, _:MaxBy) = x.getValue
    static member MaxBy (x:seq<'T> , f       , _:MaxBy) = Seq.maxBy   f x
    static member MaxBy (x:list<'T>, f       , _:MaxBy) = List.maxBy  f x
    static member MaxBy (x:'T []   , f       , _:MaxBy) = Array.maxBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member MaxBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (MaxBy.Instance, source, projection)      :'T


type Min() =
    static member val Instance = Min()
    static member Min (x:Id<'T>  , _:Min) = x.getValue
    static member Min (x:seq<'T> , _:Min) = Seq.min   x
    static member Min (x:list<'T>, _:Min) = List.min  x
    static member Min (x:'T []   , _:Min) = Array.min x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Min: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Min.Instance, source)                  :'T


type MinBy() =
    static member val Instance = MinBy()
    static member MinBy (x:Id<'T>  , f:'T->'U, _:MinBy) = x.getValue
    static member MinBy (x:seq<'T> , f       , _:MinBy) = Seq.minBy   f x
    static member MinBy (x:list<'T>, f       , _:MinBy) = List.minBy  f x
    static member MinBy (x:'T []   , f       , _:MinBy) = Array.minBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member MinBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (MinBy.Instance, source, projection)      :'T

type Rev() =
    static member val Instance = Rev()
    static member Rev (x:Id<'a>  , _:Rev) = x
    static member Rev (x:seq<'a> , _:Rev) = x |> Seq.toArray |> Array.rev |> Array.toSeq
    static member Rev (x:list<'a>, _:Rev) = List.rev  x
    static member Rev (x:'a []   , _:Rev) = Array.rev x

    static member inline Invoke  (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Rev: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Rev.Instance, source)                   :'Collection'T


type Scan() =
    static member val Instance = Scan()
    static member Scan (x:Id<'T>  , f ,z:'S, _:Id<'S>  , _:Scan) = Id.create (f z x.getValue)
    static member Scan (x:seq<'T> , f ,z:'S, _:seq<'S> , _:Scan) = Seq.scan   f z x
    static member Scan (x:list<'T>, f ,z:'S, _:list<'S>, _:Scan) = List.scan  f z x
    static member Scan (x:'T []   , f ,z:'S, _:'S []   , _:Scan) = Array.scan f z x

    static member inline Invoke (folder:'State'->'T->'State) (state:'State) (source:'Collection'T) =
        let inline call_3 (a:^a, b:^b, c:^c, f, z) = ((^a or ^b or ^c) : (static member Scan: _*_*_*_*_ -> _) b, f, z, c, a)
        let inline call (a:'a, b:'b, f, z) = call_3 (a, b, Unchecked.defaultof<'r>, f, z) :'r
        call (Scan.Instance, source, folder, state) :'Collection'State


type Sort() =
    static member val Instance = Sort()
    static member Sort (x:Id<'a>  , _:Sort) = x
    static member Sort (x:seq<'a> , _:Sort) = Seq.sort   x
    static member Sort (x:list<'a>, _:Sort) = List.sort  x
    static member Sort (x:'a []   , _:Sort) = Array.sort x

    static member inline Invoke (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Sort: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Sort.Instance, source)                 :'Collection'T


type SortBy() =
    static member val Instance = SortBy()
    static member SortBy (x:Id<'a>  , f:'a->_, _:SortBy) = x
    static member SortBy (x:seq<'a> , f      , _:SortBy) = Seq.sortBy   f x
    static member SortBy (x:list<'a>, f      , _:SortBy) = List.sortBy  f x
    static member SortBy (x:'a []   , f      , _:SortBy) = Array.sortBy f x

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'T =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member SortBy: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (SortBy.Instance, source, projection)


type Zip() =
    static member val Instance = Zip()
    static member Zip (x:Id<'T>  , y:Id<'U>  , _:Id<'T*'U>  , _:Zip) = Id.create(x.getValue,y.getValue)
    static member Zip (x:seq<'T> , y:seq<'U> , _:seq<'T*'U> , _:Zip) = Seq.zip   x y
    static member Zip (x:list<'T>, y:list<'U>, _:list<'T*'U>, _:Zip) = List.zip  x y
    static member Zip (x:'T []   , y:'U []   , _:('T*'U) [] , _:Zip) = Array.zip x y

    static member inline Invoke (source1:'Collection'T1) (source2:'Collection'T2)          =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member Zip: _*_*_*_ -> _) b, c, d, a)
        let inline call (a:'a, b:'b, c:'c) = call_4 (a, b, c, Unchecked.defaultof<'r>) :'r
        call (Zip.Instance, source1, source2)           :'Collection'T1'T2