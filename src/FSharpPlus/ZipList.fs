namespace FSharpPlus.Data

open System.Text
open System.Runtime.InteropServices
open FSharpPlus
open FSharpPlus.Control

/// A sequence with an Applicative functor based on zipping.
[<NoComparison>]
type ZipList<'s> = ZipList of 's seq with
    member this.Item n = let (ZipList s) = this in Seq.item n s

/// Basic operations on ZipList
[<RequireQualifiedAccess>]
module ZipList =
    let run   (ZipList x) = x
    let map f (ZipList x) = ZipList (Seq.map f x)
    let singleton x = ZipList (Seq.singleton x)

type ZipList<'s> with
    static member Map (ZipList x, f:'a->'b) = ZipList (Seq.map f x)
    static member Return (x:'a)     = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList<'b>
    static member inline get_Zero() = result (getZero()) : ZipList<'a>
    static member inline (+) (x:ZipList<'a>, y:ZipList<'a>) = liftA2 plus x y : ZipList<'a>
    static member ToSeq (ZipList x) = x

    static member inline Traverse (ZipList (x:seq<'T>), f:'T->'``Functor<'U>``) =
        let lst = traverse f x : '``Functor<List<'U>>``
        ZipList <!> lst : '``Functor<ZipList<'U>>``