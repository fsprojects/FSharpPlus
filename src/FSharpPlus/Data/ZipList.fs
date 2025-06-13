namespace FSharpPlus.Data

open System.ComponentModel
open FSharpPlus


/// A sequence with an Applicative functor based on zipping.
[<NoEquality;NoComparison>]
type ZipList<'s> = ZipList of 's seq with
    member this.Item n = let (ZipList s) = this in Seq.item n s

/// Basic operations on ZipList
[<RequireQualifiedAccess>]
module ZipList =
    let run   (ZipList x) = x
    let map f (ZipList x) = ZipList (Seq.map f x)
    let map2 (f:'T1->'T2->'U) (ZipList x) (ZipList y) = ZipList (Seq.map2 f x y)
    let map3 (f:'T1->'T2->'T3->'U) (ZipList x) (ZipList y) (ZipList z) = ZipList (Seq.map3 f x y z)
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
    static member Lift2 (f, x : ZipList<'T1>, y : ZipList<'T2>) = ZipList.map2 f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Lift3 (f, x : ZipList<'T1>, y : ZipList<'T2>, z : ZipList<'T3>) = ZipList.map3 f x y z

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member IsLeftZero (ZipList x) = Seq.isEmpty x
    
    static member get_Empty () = ZipList Seq.empty
    static member (<|>) (ZipList x, ZipList y) = ZipList <| seq {
        let mutable i = 0
        for e in x do i <- i + 1; yield e
        yield! Seq.drop i y }
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Zip (x, y) = ZipList.zip x y

    static member ToSeq (ZipList x) = x

#if !FABLE_COMPILER

    static member inline get_Zero () = result (getZero ()) : ZipList<'a>
    static member inline (+) (x: ZipList<'a>, y: ZipList<'a>) = lift2 plus x y : ZipList<'a>
    
    static member inline Traverse (ZipList (x: seq<'T>), f: 'T->'``Functor<'U>``) =
        let lst = traverse f x : '``Functor<List<'U>>``
        ZipList <!> lst : '``Functor<ZipList<'U>>``

#endif
