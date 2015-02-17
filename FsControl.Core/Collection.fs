namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Types
open System
open System.Text


type Skip() =
    static member val Instance = Skip()
    static member Skip (_:Skip, x:string        , _:string        ) = fun n -> x.[n..]
    static member Skip (_:Skip, x:StringBuilder , _:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[n..])
    static member Skip (_:Skip, x:'a []         , _:'a []         ) = fun n -> x.[n..] : 'a []
    static member Skip (_:Skip, x:'a ResizeArray, _:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.skip n x)
    static member Skip (_:Skip, x:list<'a>      , _:list<'a>      ) =
        let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) 
        listSkip x
    static member Skip (_:Skip, x:seq<'a>       , _:seq<'a>       ) = fun n -> Seq.skip n x


type Take() =
    static member val Instance = Take()
    static member Take (_:Take, x:string        , _:string        ) = fun n -> x.[..n-1]
    static member Take (_:Take, x:StringBuilder , _:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[..n-1])
    static member Take (_:Take, x:'a []         , _:'a []         ) = fun n -> x.[n..] : 'a []
    static member Take (_:Take, x:'a ResizeArray, _:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.take n x)
    static member Take (_:Take, x:list<'a>      , _:list<'a>      ) = fun n -> Seq.take n x |> Seq.toList
    static member Take (_:Take, x:seq<'a>       , _:seq<'a>       ) = fun n -> Seq.take n x


type FromList() =
    static member val Instance = FromList()

#if NOTNET35
    static member FromList (_:FromList, _:string        ) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList)
    static member FromList (_:FromList, _:StringBuilder ) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList))
#else
    static member FromList (_:FromList, _:string        ) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList |> Array.map string)
    static member FromList (_:FromList, _:StringBuilder ) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList |> Array.map string))
#endif
    static member FromList (_:FromList, _:'a []         ) = Array.ofList<'a>
    static member FromList (_:FromList, _:'a ResizeArray) = fun (x:list<'a>)   -> ResizeArray x
    static member FromList (_:FromList, _:list<'a>      ) = id<list<'a>>
    static member FromList (_:FromList, _:Set<'a>       ) = Set.ofList<'a>
    static member FromList (_:FromList, _:seq<'a>       ) = Seq.ofList<'a>


type FromSeq() =
    inherit Typ1()
    static member val Instance = FromSeq()

#if NOTNET35
    static member FromSeq (_:FromSeq, _:string        ) = fun (x:seq<char>) -> String.Join("", Array.ofSeq x)
    static member FromSeq (_:FromSeq, _:StringBuilder ) = fun (x:seq<char>) -> new StringBuilder(String.Join("", Array.ofSeq x))
#else
    static member FromSeq (_:FromSeq, _:string        ) = fun (x:seq<char>) -> String.Join("",  x |> Array.ofSeq |> Array.map string)
    static member FromSeq (_:FromSeq, _:StringBuilder ) = fun (x:seq<char>) -> new StringBuilder(String.Join("", x |> Array.ofSeq |> Array.map string))
#endif
    static member FromSeq (_:FromSeq, _:'a []         ) = Array.ofSeq<'a>
    static member FromSeq (_:FromSeq, _:'a ResizeArray) = fun (x:seq<'a>)   -> ResizeArray x
    static member FromSeq (_:FromSeq, _:list<'a>      ) = List.ofSeq<'a>
    static member FromSeq (_:FromSeq, _:Set<'a>       ) = Set.ofSeq<'a>
    static member FromSeq (_:FromSeq, _:seq<'a>       ) = id<seq<'a>>
    static member FromSeq (_:FromSeq, _:'a Id         ) = fun (x:seq<'a>)   -> Id.create (Seq.head x)


type ToSeq() =
    static member val Instance = ToSeq()
    static member ToSeq (_:ToSeq, x:#seq<'T>  , _:seq<'T>) = fun () -> x :> seq<_>
    static member ToSeq (_:ToSeq, x:Id<'T>    , _:seq<'T>) = fun () -> Seq.singleton x.getValue
    static member ToSeq (_:ToSeq, x:option<'T>, _:seq<'T>) = fun () -> match x with Some x -> Seq.singleton x | None -> Seq.empty


type Choose() =
    static member val Instance = Choose()
    static member Choose (_:Choose, x:Id<'T>  , _:Id<'U>  ) = fun (f:_->'U option) -> invalidOp "Choose on ID" :Id<'U> 
    static member Choose (_:Choose, x:seq<'T> , _:seq<'U> ) = fun (f:_->'U option) -> Seq.choose   f x
    static member Choose (_:Choose, x:list<'T>, _:list<'U>) = fun (f:_->'U option) -> List.choose  f x
    static member Choose (_:Choose, x:'T []   , _:'U []   ) = fun (f:_->'U option) -> Array.choose f x


type Distinct() =
    static member val Instance = Distinct()
    static member Distinct (_:Distinct, x:Id<'T>   , _:Id<'T>  ) = fun () -> x
    static member Distinct (_:Distinct, x:seq<'T>  , _:seq<'T> ) = fun () -> Seq.distinct x
    static member Distinct (_:Distinct, x:list<'T> , _:list<'T>) = fun () -> Seq.distinct x |> Seq.toList
    static member Distinct (_:Distinct, x:'T []    , _:'T []   ) = fun () -> Seq.distinct x |> Seq.toArray


type DistinctBy() =
    static member val Instance = DistinctBy()
    static member DistinctBy (_:DistinctBy, x:Id<'T>   , _:Id<'T>  ) = fun f -> x
    static member DistinctBy (_:DistinctBy, x:seq<'T>  , _:seq<'T> ) = fun f -> Seq.distinctBy f x
    static member DistinctBy (_:DistinctBy, x:list<'T> , _:list<'T>) = fun f -> Seq.distinctBy f x |> Seq.toList
    static member DistinctBy (_:DistinctBy, x:'T []    , _:'T []   ) = fun f -> Seq.distinctBy f x |> Seq.toArray


type GroupBy() =
    static member val Instance = GroupBy()
    static member GroupBy (_:GroupBy, x:Id<'T>  , _:Id<'Key*Id<'T>>    ) = fun (f:'T->'Key) -> let a = Id.run x in Id.create (f a, x)
    static member GroupBy (_:GroupBy, x:seq<'T> , _:seq<'Key*seq<'T>>  ) = fun (f:'T->'Key) -> Seq.groupBy f x
    static member GroupBy (_:GroupBy, x:list<'T>, _:list<'Key*list<'T>>) = fun (f:'T->'Key) -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupBy (_:GroupBy, x:'T []   , _:('Key*('T [])) []  ) = fun (f:'T->'Key) -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray


type GroupAdjBy() =
    static member val Instance = GroupAdjBy()
    static member GroupAdjBy (_:GroupAdjBy, x:Id<'a>  , _) = fun f -> let a = Id.run x in Id.create (f a, x)
    static member GroupAdjBy (_:GroupAdjBy, x:seq<'a> , _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    static member GroupAdjBy (_:GroupAdjBy, x:list<'a>, _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupAdjBy (_:GroupAdjBy, x:'a []   , _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray


type Intersperse() =
    static member val Instance = Intersperse()

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    static member inline internal intersperse sep list = seq {
        let notFirst = ref false
        for element in list do 
            if !notFirst then yield sep
            yield element
            notFirst := true}

    static member Intersperse (_:Intersperse, x:Id<'T>  , _:Id<'T>  ) = fun (e:'T) -> x
    static member Intersperse (_:Intersperse, x:seq<'T> , _:seq<'T> ) = fun (e:'T) -> Intersperse.intersperse e x
    static member Intersperse (_:Intersperse, x:list<'T>, _:list<'T>) = fun (e:'T) -> x |> List.toSeq  |> Intersperse.intersperse e |> Seq.toList
    static member Intersperse (_:Intersperse, x:'T []   , _:'T []   ) = fun (e:'T) -> x |> Array.toSeq |> Intersperse.intersperse e |> Seq.toArray
    

type Iteri() =
    static member val Instance = Iteri()
    static member Iteri (_:Iteri, x:Id<'T>   , _:unit) = fun (f:int->'T->unit) -> f 0 x.getValue
    static member Iteri (_:Iteri, x:seq<'T>  , _:unit) = fun f -> Seq.iteri   f x
    static member Iteri (_:Iteri, x:list<'T> , _:unit) = fun f -> List.iteri  f x
    static member Iteri (_:Iteri, x:'T []    , _:unit) = fun f -> Array.iteri f x


type Length() =
    static member val Instance = Length()
    static member Length (_:Length, x:Id<'T>   , _:int) = fun () -> 1
    static member Length (_:Length, x:seq<'T>  , _:int) = fun () -> Seq.length   x
    static member Length (_:Length, x:list<'T> , _:int) = fun () -> List.length  x
    static member Length (_:Length, x:'T []    , _:int) = fun () -> Array.length x


type Mapi() =
    static member val Instance = Mapi()
    static member Mapi (_:Mapi, x:Id<'T>   , _:Id<'U>  ) = fun (f:int->'T->'U) -> f 0 x.getValue
    static member Mapi (_:Mapi, x:seq<'T>  , _:seq<'U> ) = fun f -> Seq.mapi   f x
    static member Mapi (_:Mapi, x:list<'T> , _:list<'U>) = fun f -> List.mapi  f x
    static member Mapi (_:Mapi, x:'T []    , _:'U []   ) = fun f -> Array.mapi f x


type Max() =
    static member val Instance = Max()
    static member Max (_:Max, x:Id<'T>  , _:'T) = fun () -> x.getValue
    static member Max (_:Max, x:seq<'T> , _:'T) = fun () -> Seq.max   x
    static member Max (_:Max, x:list<'T>, _:'T) = fun () -> List.max  x
    static member Max (_:Max, x:'T []   , _:'T) = fun () -> Array.max x


type MaxBy() =
    static member val Instance = MaxBy()
    static member MaxBy (_:MaxBy, x:Id<'T>  , _:'T) = fun (f:'T->'U) -> x.getValue
    static member MaxBy (_:MaxBy, x:seq<'T> , _:'T) = fun f -> Seq.maxBy   f x
    static member MaxBy (_:MaxBy, x:list<'T>, _:'T) = fun f -> List.maxBy  f x
    static member MaxBy (_:MaxBy, x:'T []   , _:'T) = fun f -> Array.maxBy f x


type Min() =
    static member val Instance = Min()
    static member Min (_:Min, x:Id<'T>  , _:'T) = fun () -> x.getValue
    static member Min (_:Min, x:seq<'T> , _:'T) = fun () -> Seq.min   x
    static member Min (_:Min, x:list<'T>, _:'T) = fun () -> List.min  x
    static member Min (_:Min, x:'T []   , _:'T) = fun () -> Array.min x


type MinBy() =
    static member val Instance = MinBy()
    static member MinBy (_:MinBy, x:Id<'T>  , _:'T) = fun f -> x.getValue
    static member MinBy (_:MinBy, x:seq<'T> , _:'T) = fun f -> Seq.minBy   f x
    static member MinBy (_:MinBy, x:list<'T>, _:'T) = fun f -> List.minBy  f x
    static member MinBy (_:MinBy, x:'T []   , _:'T) = fun f -> Array.minBy f x


type Rev() =
    static member val Instance = Rev()
    static member Rev (_:Rev, x:Id<'a>  , _:Id<'a>  ) = fun () -> x
    static member Rev (_:Rev, x:seq<'a> , _:seq<'a> ) = fun () -> x |> Seq.toArray |> Array.rev |> Array.toSeq
    static member Rev (_:Rev, x:list<'a>, _:list<'a>) = fun () -> List.rev  x
    static member Rev (_:Rev, x:'a []   , _:'a []   ) = fun () -> Array.rev x


type Scan() =
    static member val Instance = Scan()
    static member Scan (_:Scan, x:Id<'T>  , _:Id<'S>  ) = fun f (z:'S) -> Id.create (f z x.getValue)
    static member Scan (_:Scan, x:seq<'T> , _:seq<'S> ) = fun f (z:'S) -> Seq.scan   f z x
    static member Scan (_:Scan, x:list<'T>, _:list<'S>) = fun f (z:'S) -> List.scan  f z x
    static member Scan (_:Scan, x:'T []   , _:'S []   ) = fun f (z:'S) -> Array.scan f z x


type Sort() =
    static member val Instance = Sort()
    static member Sort (_:Sort, x:Id<'a>  , _:Id<'a>  ) = fun () -> x
    static member Sort (_:Sort, x:seq<'a> , _:seq<'a> ) = fun () -> Seq.sort   x
    static member Sort (_:Sort, x:list<'a>, _:list<'a>) = fun () -> List.sort  x
    static member Sort (_:Sort, x:'a []   , _:'a []   ) = fun () -> Array.sort x


type SortBy() =
    static member val Instance = SortBy()
    static member SortBy (_:SortBy, x:Id<'a>  , _) = fun (f:'a->_) -> x
    static member SortBy (_:SortBy, x:seq<'a> , _) = fun f -> Seq.sortBy   f x
    static member SortBy (_:SortBy, x:list<'a>, _) = fun f -> List.sortBy  f x
    static member SortBy (_:SortBy, x:'a []   , _) = fun f -> Array.sortBy f x


type Zip() =
    static member val Instance = Zip()
    static member Zip (_:Zip, x:Id<'T>  , y:Id<'U>  , _:Id<'T*'U>  ) = fun () -> Id.create(x.getValue,y.getValue)
    static member Zip (_:Zip, x:seq<'T> , y:seq<'U> , _:seq<'T*'U> ) = fun () -> Seq.zip   x y
    static member Zip (_:Zip, x:list<'T>, y:list<'U>, _:list<'T*'U>) = fun () -> List.zip  x y
    static member Zip (_:Zip, x:'T []   , y:'U []   , _:('T*'U) [] ) = fun () -> Array.zip x y