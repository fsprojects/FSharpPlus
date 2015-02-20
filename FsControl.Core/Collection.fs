namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Types
open System
open System.Text


type Skip() =
    static member val Instance = Skip()
    static member Skip (_:Skip, x:string        ) = fun n -> x.[n..]
    static member Skip (_:Skip, x:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[n..])
    static member Skip (_:Skip, x:'a []         ) = fun n -> x.[n..] : 'a []
    static member Skip (_:Skip, x:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.skip n x)
    static member Skip (_:Skip, x:list<'a>      ) = let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) in listSkip x
    static member Skip (_:Skip, x:seq<'a>       ) = fun n -> Seq.skip n x
    static member Skip (_:Skip, x:'a Id         ) = fun n -> x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Skip: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Skip.Instance, source) n


type Take() =
    static member val Instance = Take()
    static member Take (_:Take, x:string        ) = fun n -> x.[..n-1]
    static member Take (_:Take, x:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[..n-1])
    static member Take (_:Take, x:'a []         ) = fun n -> x.[n..] : 'a []
    static member Take (_:Take, x:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.take n x)
    static member Take (_:Take, x:list<'a>      ) = fun n -> Seq.take n x |> Seq.toList
    static member Take (_:Take, x:seq<'a>       ) = fun n -> Seq.take n x
    static member Take (_:Take, x:'a Id         ) = fun n -> x

    static member inline Invoke (n:int) (source:'Collection'T)  :'Collection'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Take: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Take.Instance, source) n


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

    static member inline Invoke (value :list<'t>) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromList: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromList.Instance value


type FromSeq() =
    inherit Default1()
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

    static member inline Invoke  (value :seq<'t> ) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromSeq: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call FromSeq.Instance value


type ToSeq() =
    static member val Instance = ToSeq()
    static member ToSeq (_:ToSeq, x:#seq<'T>  ) = x :> seq<_>
    static member ToSeq (_:ToSeq, x:Id<'T>    ) = Seq.singleton x.getValue
    static member ToSeq (_:ToSeq, x:option<'T>) = match x with Some x -> Seq.singleton x | None -> Seq.empty

    static member inline Invoke (source:'Collection'T)  : seq<'T>  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToSeq: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToSeq.Instance, source)   


type Choose() =
    static member val Instance = Choose()
    static member Choose (_:Choose, x:Id<'T>  , _:Id<'U>  ) = fun (f:_->'U option) -> invalidOp "Choose on ID" :Id<'U> 
    static member Choose (_:Choose, x:seq<'T> , _:seq<'U> ) = fun (f:_->'U option) -> Seq.choose   f x
    static member Choose (_:Choose, x:list<'T>, _:list<'U>) = fun (f:_->'U option) -> List.choose  f x
    static member Choose (_:Choose, x:'T []   , _:'U []   ) = fun (f:_->'U option) -> Array.choose f x

    static member inline Invoke (chooser:'T->'U option)   (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Choose: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Choose.Instance, source) chooser         :'Collection'U


type Distinct() =
    static member val Instance = Distinct()
    static member Distinct (_:Distinct, x:Id<'T>  ) = x
    static member Distinct (_:Distinct, x:seq<'T> ) = Seq.distinct x
    static member Distinct (_:Distinct, x:list<'T>) = Seq.distinct x |> Seq.toList
    static member Distinct (_:Distinct, x:'T []   ) = Seq.distinct x |> Seq.toArray

    static member inline Invoke                         (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Distinct: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Distinct.Instance, source)  :'Collection'T


type DistinctBy() =
    static member val Instance = DistinctBy()
    static member DistinctBy (_:DistinctBy, x:Id<'T>  ) = fun f -> x
    static member DistinctBy (_:DistinctBy, x:seq<'T> ) = fun f -> Seq.distinctBy f x
    static member DistinctBy (_:DistinctBy, x:list<'T>) = fun f -> Seq.distinctBy f x |> Seq.toList
    static member DistinctBy (_:DistinctBy, x:'T []   ) = fun f -> Seq.distinctBy f x |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member DistinctBy: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (DistinctBy.Instance, source) projection      :'Collection'T


type GroupBy() =
    static member val Instance = GroupBy()
    static member GroupBy (_:GroupBy, x:Id<'T>  , _:Id<'Key*Id<'T>>    ) = fun (f:'T->'Key) -> let a = Id.run x in Id.create (f a, x)
    static member GroupBy (_:GroupBy, x:seq<'T> , _:seq<'Key*seq<'T>>  ) = fun (f:'T->'Key) -> Seq.groupBy f x
    static member GroupBy (_:GroupBy, x:list<'T>, _:list<'Key*list<'T>>) = fun (f:'T->'Key) -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupBy (_:GroupBy, x:'T []   , _:('Key*('T [])) []  ) = fun (f:'T->'Key) -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke    (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member GroupBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (GroupBy.Instance, source) projection


type GroupAdjBy() =
    static member val Instance = GroupAdjBy()
    static member GroupAdjBy (_:GroupAdjBy, x:Id<'T>  , _:Id<'Key*Id<'T>>    ) = fun (f:'T->'Key) -> let a = Id.run x in Id.create (f a, x)
    static member GroupAdjBy (_:GroupAdjBy, x:seq<'T> , _:seq<'Key*seq<'T>>  ) = fun (f:'T->'Key) -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    static member GroupAdjBy (_:GroupAdjBy, x:list<'T>, _:list<'Key*list<'T>>) = fun (f:'T->'Key) -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupAdjBy (_:GroupAdjBy, x:'T []   , _:('Key*('T [])) []  ) = fun (f:'T->'Key) -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member GroupAdjBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (GroupAdjBy.Instance, source) projection


type Intersperse() =
    static member val Instance = Intersperse()

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    static member inline internal intersperse sep list = seq {
        let notFirst = ref false
        for element in list do 
            if !notFirst then yield sep
            yield element
            notFirst := true}

    static member Intersperse (_:Intersperse, x:Id<'T>  ) = fun (e:'T) -> x
    static member Intersperse (_:Intersperse, x:seq<'T> ) = fun (e:'T) -> Intersperse.intersperse e x
    static member Intersperse (_:Intersperse, x:list<'T>) = fun (e:'T) -> x |> List.toSeq  |> Intersperse.intersperse e |> Seq.toList
    static member Intersperse (_:Intersperse, x:'T []   ) = fun (e:'T) -> x |> Array.toSeq |> Intersperse.intersperse e |> Seq.toArray
 
    static member inline Invoke      (sep:'T)        (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Intersperse: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Intersperse.Instance, source) sep            :'Collection'T
    

type Iteri() =
    static member val Instance = Iteri()
    static member Iteri (_:Iteri, x:Id<'T>  ) = fun (f:int->'T->unit) -> f 0 x.getValue
    static member Iteri (_:Iteri, x:seq<'T> ) = fun f -> Seq.iteri   f x
    static member Iteri (_:Iteri, x:list<'T>) = fun f -> List.iteri  f x
    static member Iteri (_:Iteri, x:'T []   ) = fun f -> Array.iteri f x

    static member inline Invoke (action:int->'T->unit)     (source:'Collection'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Iteri: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Iteri.Instance,  source) action              :unit


type Length() =
    static member val Instance = Length()
    static member Length (_:Length, x:Id<'T>  ) = 1
    static member Length (_:Length, x:seq<'T> ) = Seq.length   x
    static member Length (_:Length, x:list<'T>) = List.length  x
    static member Length (_:Length, x:'T []   ) = Array.length x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Length: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Length.Instance, source)            :int


type Mapi() =
    static member val Instance = Mapi()
    static member Mapi (_:Mapi, x:Id<'T>   , _:Id<'U>  ) = fun (f:int->'T->'U) -> f 0 x.getValue
    static member Mapi (_:Mapi, x:seq<'T>  , _:seq<'U> ) = fun f -> Seq.mapi   f x
    static member Mapi (_:Mapi, x:list<'T> , _:list<'U>) = fun f -> List.mapi  f x
    static member Mapi (_:Mapi, x:'T []    , _:'U []   ) = fun f -> Array.mapi f x

    static member inline Invoke    (mapping:int->'T->'U)    (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mapi: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Mapi.Instance,   source) mapping             :'Collection'U


type Max() =
    static member val Instance = Max()
    static member Max (_:Max, x:Id<'T>  ) = x.getValue
    static member Max (_:Max, x:seq<'T> ) = Seq.max   x
    static member Max (_:Max, x:list<'T>) = List.max  x
    static member Max (_:Max, x:'T []   ) = Array.max x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Max: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Max.Instance, source)                  :'T


type MaxBy() =
    static member val Instance = MaxBy()
    static member MaxBy (_:MaxBy, x:Id<'T>  ) = fun (f:'T->'U) -> x.getValue
    static member MaxBy (_:MaxBy, x:seq<'T> ) = fun f -> Seq.maxBy   f x
    static member MaxBy (_:MaxBy, x:list<'T>) = fun f -> List.maxBy  f x
    static member MaxBy (_:MaxBy, x:'T []   ) = fun f -> Array.maxBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MaxBy: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (MaxBy.Instance, source) projection           :'T


type Min() =
    static member val Instance = Min()
    static member Min (_:Min, x:Id<'T>  ) = x.getValue
    static member Min (_:Min, x:seq<'T> ) = Seq.min   x
    static member Min (_:Min, x:list<'T>) = List.min  x
    static member Min (_:Min, x:'T []   ) = Array.min x

    static member inline Invoke (source:'Collection'T)                                  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Min: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Min.Instance, source)                  :'T


type MinBy() =
    static member val Instance = MinBy()
    static member MinBy (_:MinBy, x:Id<'T>  ) = fun (f:'T->'U) -> x.getValue
    static member MinBy (_:MinBy, x:seq<'T> ) = fun f -> Seq.minBy   f x
    static member MinBy (_:MinBy, x:list<'T>) = fun f -> List.minBy  f x
    static member MinBy (_:MinBy, x:'T []   ) = fun f -> Array.minBy f x

    static member inline Invoke (projection:'T->'U) (source:'Collection'T)               =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MinBy: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (MinBy.Instance, source) projection           :'T

type Rev() =
    static member val Instance = Rev()
    static member Rev (_:Rev, x:Id<'a>  ) = x
    static member Rev (_:Rev, x:seq<'a> ) = x |> Seq.toArray |> Array.rev |> Array.toSeq
    static member Rev (_:Rev, x:list<'a>) = List.rev  x
    static member Rev (_:Rev, x:'a []   ) = Array.rev x

    static member inline Invoke  (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Rev: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Rev.Instance, source)                   :'Collection'T


type Scan() =
    static member val Instance = Scan()
    static member Scan (_:Scan, x:Id<'T>  , _:Id<'S>  ) = fun f (z:'S) -> Id.create (f z x.getValue)
    static member Scan (_:Scan, x:seq<'T> , _:seq<'S> ) = fun f (z:'S) -> Seq.scan   f z x
    static member Scan (_:Scan, x:list<'T>, _:list<'S>) = fun f (z:'S) -> List.scan  f z x
    static member Scan (_:Scan, x:'T []   , _:'S []   ) = fun f (z:'S) -> Array.scan f z x

    static member inline Invoke (folder:'State'->'T->'State) state (source:'Collection'T) =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Scan: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Scan.Instance, source) folder (state:'State) :'Collection'State


type Sort() =
    static member val Instance = Sort()
    static member Sort (_:Sort, x:Id<'a>  ) = x
    static member Sort (_:Sort, x:seq<'a> ) = Seq.sort   x
    static member Sort (_:Sort, x:list<'a>) = List.sort  x
    static member Sort (_:Sort, x:'a []   ) = Array.sort x

    static member inline Invoke (source:'Collection'T)                                    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Sort: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Sort.Instance, source)                 :'Collection'T


type SortBy() =
    static member val Instance = SortBy()
    static member SortBy (_:SortBy, x:Id<'a>  ) = fun (f:'a->_) -> x
    static member SortBy (_:SortBy, x:seq<'a> ) = fun f -> Seq.sortBy   f x
    static member SortBy (_:SortBy, x:list<'a>) = fun f -> List.sortBy  f x
    static member SortBy (_:SortBy, x:'a []   ) = fun f -> Array.sortBy f x

    static member inline Invoke (projection:'T->'Key) (source:'Collection'T) : 'Collection'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member SortBy: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (SortBy.Instance, source) projection


type Zip() =
    static member val Instance = Zip()
    static member Zip (_:Zip, x:Id<'T>  , y:Id<'U>  , _:Id<'T*'U>  ) = Id.create(x.getValue,y.getValue)
    static member Zip (_:Zip, x:seq<'T> , y:seq<'U> , _:seq<'T*'U> ) = Seq.zip   x y
    static member Zip (_:Zip, x:list<'T>, y:list<'U>, _:list<'T*'U>) = List.zip  x y
    static member Zip (_:Zip, x:'T []   , y:'U []   , _:('T*'U) [] ) = Array.zip x y

    static member inline Invoke (source1:'Collection'T1) (source2:'Collection'T2)          =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member Zip: _*_*_*_ -> _) a, b, c, d)
        let inline call (a:'a, b:'b, c:'c) = call_4 (a, b, c, Unchecked.defaultof<'r>) :'r
        call (Zip.Instance, source1, source2)           :'Collection'T1'T2