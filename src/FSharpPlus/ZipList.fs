namespace FSharpPlus

open FsControl.Core.Abstractions
open FSharpPlus.Prelude

type ZipList<'s> = ZipList of 's seq with
    member this.Item n = let (ZipList s) = this in Seq.nth n s

[<RequireQualifiedAccess>]
module ZipList =
    let run   (ZipList x) = x
    let map f (ZipList x) = ZipList (Seq.map f x)
    let singleton x = ZipList (Seq.singleton x)

type ZipList with
    static member instance (_:Functor.Map      ,    ZipList x   , _) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member instance (_:Applicative.Pure , _:ZipList<'a>) = fun (x:'a)     -> ZipList (Seq.initInfinite (konst x))
    static member instance (_:Applicative.Apply,   ZipList (f:seq<'a->'b>), ZipList x ,_:ZipList<'b>) = fun () ->
        ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>
    static member instance (_:Monoid.Mempty  , _:ZipList<'a>  ) = fun () -> ZipList Seq.empty   : ZipList<'a>
    static member instance (_:Monoid.Mappend , ZipList(x) , _) = fun (ZipList(y)) -> ZipList (Seq.append x y)
    static member instance (_:Collection.Skip, (ZipList s):ZipList<'a> , _:ZipList<'a>) = fun n -> ZipList (Seq.skip n s) :ZipList<'a>
    static member instance (_:Collection.Take, (ZipList s):ZipList<'a> , _:ZipList<'a>) = fun n -> ZipList (Seq.take n s) :ZipList<'a>
    static member instance (_:Comonad.Extract, (ZipList s):ZipList<'a> , _:'a) = fun () -> Seq.head s
    member this.GetSlice = function
        | None  , None   -> this
        | Some a, None   -> this |> skip a 
        | None  , Some b -> this |> take b 
        | Some a, Some b -> this |> skip a |> take (b-a+1)