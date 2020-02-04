namespace FSharpPlus.Data

open System.Text
open System.Runtime.InteropServices
open FSharpPlus
open FSharpPlus.Control
open System.ComponentModel

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

    /// <summary>Combines the two lists into a list of pairs. The two lists need not have equal lengths:
    /// when one list is exhausted any remaining elements in the other
    /// list are ignored.</summary>
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <returns>A single list containing pairs of matching elements from the input lists.</returns>
    let zip (list1: ZipList<'T>) (list2: ZipList<'U>) = ZipList (Seq.zip (run list1) (run list2))

type ZipList<'s> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (ZipList x, f: 'a->'b) = ZipList (Seq.map f x)

    static member Return (x: 'a)     = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f: seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList<'b>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = ZipList.zip x y

    #if !FABLE_COMPILER
    static member inline get_Zero () = result (getZero ()) : ZipList<'a>
    #endif
    static member inline (+) (x: ZipList<'a>, y: ZipList<'a>) = lift2 plus x y : ZipList<'a>
    static member ToSeq (ZipList x) = x

    #if !FABLE_COMPILER
    static member inline Traverse (ZipList (x: seq<'T>), f: 'T->'``Functor<'U>``) =
        let lst = traverse f x : '``Functor<List<'U>>``
        ZipList <!> lst : '``Functor<ZipList<'U>>``
    #endif