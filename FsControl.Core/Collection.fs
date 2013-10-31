namespace FsControl.Core.Abstractions

open System
open System.Collections
open System.Text

module Collection =

    type Skip = Skip with
        static member instance (Skip, x:string         , _:string         ) = fun n -> x.[n..]
        static member instance (Skip, x:StringBuilder  , _:StringBuilder  ) = fun n -> new StringBuilder(x.ToString().[n..])
        static member instance (Skip, x:'a []          , _:'a []          ) = fun n -> x.[n..] : 'a []
        static member instance (Skip, x:'a Generic.List, _:'a Generic.List) = fun n -> new Generic.List<'a>(Seq.skip n x)
        static member instance (Skip, x:List<'a>       , _:List<'a>       ) =
            let rec listSkip lst = function 0 -> lst | n -> listSkip (List.tail lst) (n-1) 
            listSkip x

    let inline internal skip (n:int) x = Inline.instance (Skip, x) n


    type Take = Take with
        static member instance (Take, x:string         , _:string         ) = fun n -> x.[..n-1]
        static member instance (Take, x:StringBuilder  , _:StringBuilder  ) = fun n -> new StringBuilder(x.ToString().[..n-1])
        static member instance (Take, x:'a []          , _:'a []          ) = fun n -> x.[n..] : 'a []
        static member instance (Take, x:'a Generic.List, _:'a Generic.List) = fun n -> new Generic.List<'a>(Seq.take n x)
        static member instance (Take, x:List<'a>       , _:List<'a>       ) = fun n -> Seq.take n x |> Seq.toList

    let inline internal take (n:int) x = Inline.instance (Take, x) n