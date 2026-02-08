module FSharpPlus.Tests.Helpers

open System
open System.Collections
open NUnit.Framework
open FSharpPlus
open FSharpPlus.Control

let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)
let areStEqual x y = Assert.IsTrue( (x = y), sprintf "Expected %A to be structurally equal to %A" x y)
let areEquivalent (x:IEnumerable) (y:IEnumerable) = CollectionAssert.AreEquivalent (x, y)
let require (x: bool) (msg: string) = Assert.IsTrue (x, msg)

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset () = effects.Clear ()
    let add x = effects.Add (x)
    let get () = effects |> Seq.toList
    let are lst = areEquivalent lst (get ())


type WrappedListD<'s> = WrappedListD of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator () :> Collections.IEnumerator
    static member Return  (x) = SideEffects.add "Using WrappedListD's Return"; WrappedListD [x]
    static member (>>=) ((WrappedListD x):WrappedListD<'T>, f) = SideEffects.add "Using WrappedListD's Bind"; WrappedListD (List.collect (f >> (fun (WrappedListD x) -> x)) x)
    static member inline FoldMap (WrappedListD x, f) =
        SideEffects.add "Using optimized foldMap"
        Seq.fold (fun x y -> x ++ (f y)) zero x
    static member Zip (WrappedListD x, WrappedListD y) = SideEffects.add "Using WrappedListD's zip"; WrappedListD (List.zip x y)
    static member Exists (x, f) =
        SideEffects.add "Using WrappedListD's Exists"
        let (WrappedListD lst) = x
        List.exists f lst    
    static member Pick (x, f) =
        SideEffects.add "Using WrappedListD's Pick"
        let (WrappedListD lst) = x
        List.pick f lst
    static member Min x =
        SideEffects.add "Using WrappedListD's Min"
        let (WrappedListD lst) = x
        List.min lst
    static member MaxBy (x, f) =
        SideEffects.add "Using WrappedListD's MaxBy"
        let (WrappedListD lst) = x
        List.maxBy f lst
    static member MapIndexed (WrappedListD x, f) =
        SideEffects.add "Using WrappedListD's MapIndexed"
        WrappedListD (List.mapi f x)
    static member ChooseIndexed (WrappedListD x, f) =
        SideEffects.add "Using WrappedListD's ChooseIndexed"
        WrappedListD (List.choosei f x)
    static member Lift3 (f, WrappedListD x, WrappedListD y, WrappedListD z) =
        SideEffects.add "Using WrappedListD's Lift3"
        WrappedListD (List.lift3 f x y z)
    static member IterateIndexed (WrappedListD x, f) =
        SideEffects.add "Using WrappedListD's IterateIndexed"
        List.iteri f x
    static member inline FoldIndexed (WrappedListD x, f, z) =
        SideEffects.add "Using WrappedListD's FoldIndexed"
        foldi f z x
    static member inline TraverseIndexed (WrappedListD x, f) =
        SideEffects.add "Using WrappedListD's TraverseIndexed"
        WrappedListD <!> (traversei f x : ^r)
    static member FindIndex (WrappedListD x, y) =
        SideEffects.add "Using WrappedListD's FindIndex"
        findIndex y x
    static member FindSliceIndex (WrappedListD x, WrappedListD y) =
        SideEffects.add "Using WrappedListD's FindSliceIndex"
        findSliceIndex y x
    static member FindLastSliceIndex (WrappedListD x, WrappedListD y) =
        SideEffects.add "Using WrappedListD's FindLastSliceIndex"
        findLastSliceIndex y x
    member this.Length =
        SideEffects.add "Using WrappedListD's Length"
        let (WrappedListD lst) = this
        List.length lst