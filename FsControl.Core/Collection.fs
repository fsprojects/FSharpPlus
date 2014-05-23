namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Types
open System
open System.Text

module Collection =

    type Skip = Skip with
        static member instance (Skip, x:string        , _:string        ) = fun n -> x.[n..]
        static member instance (Skip, x:StringBuilder , _:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[n..])
        static member instance (Skip, x:'a []         , _:'a []         ) = fun n -> x.[n..] : 'a []
        static member instance (Skip, x:'a ResizeArray, _:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.skip n x)
        static member instance (Skip, x:List<'a>      , _:List<'a>      ) =
            let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) 
            listSkip x
        static member instance (Skip, x:seq<'a>       , _:seq<'a>       ) = fun n -> Seq.skip n x

    let inline internal skip (n:int) x = Inline.instance (Skip, x) n


    type Take = Take with
        static member instance (Take, x:string        , _:string        ) = fun n -> x.[..n-1]
        static member instance (Take, x:StringBuilder , _:StringBuilder ) = fun n -> new StringBuilder(x.ToString().[..n-1])
        static member instance (Take, x:'a []         , _:'a []         ) = fun n -> x.[n..] : 'a []
        static member instance (Take, x:'a ResizeArray, _:'a ResizeArray) = fun n -> ResizeArray<'a> (Seq.take n x)
        static member instance (Take, x:List<'a>      , _:List<'a>      ) = fun n -> Seq.take n x |> Seq.toList
        static member instance (Take, x:seq<'a>       , _:seq<'a>       ) = fun n -> Seq.take n x

    let inline internal take (n:int) x = Inline.instance (Take, x) n


    type FromList = FromList with


#if NOTNET35
        static member instance (FromList, _:string        ) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList)
        static member instance (FromList, _:StringBuilder ) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList))
#else
        static member instance (FromList, _:string        ) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList |> Array.map string)
        static member instance (FromList, _:StringBuilder ) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList |> Array.map string))
#endif
        static member instance (FromList, _:'a []         ) = Array.ofList<'a>
        static member instance (FromList, _:'a ResizeArray) = fun (x:list<'a>)   -> ResizeArray x
        static member instance (FromList, _:List<'a>      ) = id<list<'a>>
        static member instance (FromList, _:Set<'a>       ) = Set.ofList<'a>
        static member instance (FromList, _:seq<'a>       ) = Seq.ofList<'a>

    let inline internal fromList (value:list<'t>)  = Inline.instance FromList value


    type GroupBy = GroupBy with
        static member instance (GroupBy, x:Id<'a>  , _) = fun f -> let a = Id.run x in Id (f a, x)
        static member instance (GroupBy, x:seq<'a> , _) = fun f -> Seq.groupBy f x
        static member instance (GroupBy, x:List<'a>, _) = fun f -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
        static member instance (GroupBy, x:'a []   , _) = fun f -> Seq.groupBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    type GroupAdjBy = GroupAdjBy with
        static member instance (GroupAdjBy, x:Id<'a>  , _) = fun f -> let a = Id.run x in Id (f a, x)
        static member instance (GroupAdjBy, x:seq<'a> , _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
        static member instance (GroupAdjBy, x:List<'a>, _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
        static member instance (GroupAdjBy, x:'a []   , _) = fun f -> Seq.groupAdjBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    type SortBy = SortBy with
        static member instance (SortBy, x:Id<'a>   , _) = fun (f:'a->_) -> x
        static member instance (SortBy, x:seq<'a>  , _) = fun f -> Seq.sortBy   f x
        static member instance (SortBy, x:List<'a> , _) = fun f -> List.sortBy  f x
        static member instance (SortBy, x:'a []    , _) = fun f -> Array.sortBy f x
