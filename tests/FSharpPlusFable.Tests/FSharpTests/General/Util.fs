namespace General.Util
open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open Testing
#if !FABLE_COMPILER || FABLE_COMPILER_3
type WrappedMapA<'K,'V when 'K : comparison> = WrappedMapA of Map<'K,'V> with
    static member ToMap (WrappedMapA m) = m
    static member inline TraverseIndexed (WrappedMapA m, f) =
        SideEffects.add "Using WrappedMapA's TraverseIndexed"
        WrappedMapA <!> (traversei f m : ^__)
module WrappedMapA=
    let inline ofList l = Map.ofList l |> WrappedMapA

type WrappedListA<'s> = WrappedListA of 's list with
    static member ToSeq (WrappedListA lst) = SideEffects.add "Using WrappedListA's ToSeq"; List.toSeq lst
    static member OfSeq lst = WrappedListA (Seq.toList lst)
    static member TryItem (i, WrappedListA x) = List.tryItem i x
    static member TryParse x =
        if x = "[1;2;3]" then Some (WrappedListA [1;2;3])
        else None
    static member Exists (x, f) =
        SideEffects.add "Using WrappedListA's Exists"
        let (WrappedListA lst) = x
        List.exists f lst
    static member Pick (x, f) =
        SideEffects.add "Using WrappedListA's Pick"
        let (WrappedListA lst) = x
        List.pick f lst
    static member Min x =
        SideEffects.add "Using WrappedListA's Min"
        let (WrappedListA lst) = x
        List.min lst
    static member MaxBy (x, f) =
        SideEffects.add "Using WrappedListA's MaxBy"
        let (WrappedListA lst) = x
        List.maxBy f lst
    member this.Length =
        SideEffects.add "Using WrappedListA's Length"
        let (WrappedListA lst) = this
        List.length lst
#endif

#if !FABLE_COMPILER || FABLE_COMPILER_3
type WrappedListB<'s> = WrappedListB of 's list with
    static member Return x = WrappedListB [x]
    static member (+) (WrappedListB l, WrappedListB x) = WrappedListB (l @ x)
    static member Zero = WrappedListB List.empty
    static member ToSeq (WrappedListB lst)     = List.toSeq lst
    static member FoldBack (WrappedListB x, f, z) = List.foldBack f x z

type WrappedListB'<'s> = WrappedListB' of 's list with // Same as B but without clean signatures
    static member Return   (_:WrappedListB'<'a>, _:Return ) = fun (x:'a)     -> WrappedListB' [x]
    static member (+)      (WrappedListB' l, WrappedListB' x) = WrappedListB' (l @ x)
    static member Zero     (_:WrappedListB'<'a>, _:Zero) = WrappedListB' List.empty
    static member ToSeq    (WrappedListB' lst)     = List.toSeq lst
    static member FoldBack (WrappedListB' x, f, z) = List.foldBack f x z
#endif

type WrappedListC<'s> = WrappedListC of 's list with
    static member (+) (WrappedListC l, WrappedListC x) = WrappedListC (l @ x)
    static member Zero = WrappedListC List.empty
    static member Sum (lst: seq<WrappedListC<_>>) = Seq.head lst

#if !FABLE_COMPILER || FABLE_COMPILER_3
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
        printfn "WrappedListD.FindIndex"
        findIndex y x
    #if !FABLE_COMPILER
    static member FindSliceIndex (WrappedListD x, WrappedListD y) =
        SideEffects.add "Using WrappedListD's FindSliceIndex"
        printfn "WrappedListD.FindSliceIndex"
        findSliceIndex y x
    #endif
    member this.Length =
        SideEffects.add "Using WrappedListD's Length"
        let (WrappedListD lst) = this
        List.length lst
#endif

type WrappedListE<'s> = WrappedListE of 's list with
    static member Return x = WrappedListE [x]
    static member (>>=)  (WrappedListE x: WrappedListE<'T>, f) = WrappedListE (List.collect (f >> (fun (WrappedListE x) -> x)) x)
    static member get_Empty () = WrappedListE List.empty
    static member (<|>) (WrappedListE l, WrappedListE x) = WrappedListE (l @ x)
    
type WrappedListF<'s> = WrappedListF of 's list with
    static member Return x = WrappedListF [x]
    static member (>>=) (WrappedListF x: WrappedListF<'T>, f) = WrappedListF (List.collect (f >> (fun (WrappedListF x) -> x)) x)
    static member Join  (WrappedListF wlst) = SideEffects.add "Join";  WrappedListF wlst >>= id
    static member get_Empty () = WrappedListF List.empty
    static member (<|>) (WrappedListF l, WrappedListF x) = WrappedListF (l @ x)

type WrappedListG<'s> = WrappedListG of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedListG x) = x in x :> _ seq).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedListG x) = x in x :> _ seq).GetEnumerator () :> Collections.IEnumerator
    static member Return x = WrappedListG [x]
    static member (>>=) (WrappedListG x: WrappedListG<'T>, f) = WrappedListG (List.collect (f >> (fun (WrappedListG x) -> x)) x)
    static member Join  (WrappedListG wlst) = (*SideEffects.add "Join";*)  WrappedListG wlst >>= id
    static member get_Empty () = WrappedListG List.empty
    static member (<|>) (WrappedListG l, WrappedListG x) = WrappedListG (l @ x)
    static member Delay (f: unit -> WrappedListG<_>) = SideEffects.add "Using WrappedListG's Delay"; f ()
    static member Using (resource, body)             = SideEffects.add "Using WrappedListG's Using"; using resource body

#if !FABLE_COMPILER || FABLE_COMPILER_3
type WrappedListH<'s> = WrappedListH of 's list with
    static member Map (WrappedListH lst, f) = WrappedListH (List.map f lst)
    static member inline Sequence (x: WrappedListH<'``Functor<'T>``>) =
        let (WrappedListH lst) = x
        let s = sequence lst : '``Functor<List<'T>>``
        map WrappedListH s : '``Functor<WrappedListH<'T>>``
#endif

type WrappedListI<'s> = WrappedListI of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedListI x) = x in x :> _ seq).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedListI x) = x in x :> _ seq).GetEnumerator () :> Collections.IEnumerator
    static member Return  (x) = SideEffects.add "Using WrappedListI's Return"; WrappedListI [x]
    static member Sum (lst: seq<WrappedListI<_>>) = Seq.head lst


type WrappedSeqA<'s> = WrappedSeqA of 's seq with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedSeqA x) = x in x).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedSeqA x) = x in x).GetEnumerator () :> Collections.IEnumerator
    static member Return x = WrappedSeqA [x]
    static member (>>=) (WrappedSeqA x: WrappedSeqA<'T>, f) = WrappedSeqA (Seq.collect (f >> (fun (WrappedSeqA x) -> x)) x)
    static member Join  (WrappedSeqA wlst) = WrappedSeqA wlst >>= id
    static member get_Empty () = WrappedSeqA List.empty
    static member (<|>) (WrappedSeqA l, WrappedSeqA x) = WrappedSeqA (Seq.append l x)
    static member Delay (f: unit -> WrappedSeqA<_>) =
                    let run (WrappedSeqA s) = s
                    WrappedSeqA (Seq.delay (f >> run))

type WrappedSeqB<'s> = WrappedSeqB of 's seq with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedSeqB x) = x in x).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedSeqB x) = x in x).GetEnumerator () :> Collections.IEnumerator
    static member Return x = WrappedSeqB [x]
    static member (>>=) (WrappedSeqB x: WrappedSeqB<'T>, f) = WrappedSeqB (Seq.collect (f >> (fun (WrappedSeqB x) -> x)) x)
    static member Join  (WrappedSeqB wlst) = WrappedSeqB wlst >>= id
    static member get_Empty () = WrappedSeqB List.empty
    static member (<|>) (WrappedSeqB l, WrappedSeqB x) = WrappedSeqB (Seq.append l x)
    static member Delay (f: unit -> WrappedSeqB<_>) =
                    let run (WrappedSeqB s) = s
                    WrappedSeqB (Seq.delay (f >> run))
    static member TryFinally (computation, compensation) =
                    SideEffects.add "Using WrappedSeqA's TryFinally"
                    try computation finally compensation ()
    static member Using (resource, body) = 
                    SideEffects.add "Using WrappedSeqB's Using"
                    using resource body

type WrappedSeqC<'s> = WrappedSeqC of 's seq with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator () = (let (WrappedSeqC x) = x in x).GetEnumerator ()
    interface Collections.IEnumerable             with member x.GetEnumerator () = (let (WrappedSeqC x) = x in x).GetEnumerator () :> Collections.IEnumerator
    static member Return x = WrappedSeqC [x]
    static member (>>=) (WrappedSeqC x: WrappedSeqC<'T>, f) = WrappedSeqC (Seq.collect (f >> (fun (WrappedSeqC x) -> x)) x)
    static member Join  (WrappedSeqC wlst) = WrappedSeqC wlst >>= id
    static member get_Empty () = WrappedSeqC List.empty
    static member (<|>) (WrappedSeqC l, WrappedSeqC x) = WrappedSeqC (Seq.append l x)
    static member Delay (f: unit -> WrappedSeqC<_>) =
                    let run (WrappedSeqC s) = s
                    WrappedSeqC (Seq.delay (f >> run))
    static member TryFinally (computation, compensation) =
                    SideEffects.add "Using WrappedSeqC's TryFinally"
                    try computation finally compensation ()

#if !FABLE_COMPILER || FABLE_COMPILER_3
type WrappedSeqD<'s> = WrappedSeqD of 's seq with
    static member Return x = SideEffects.add "Using WrappedSeqD's Return"; WrappedSeqD (Seq.singleton x)
    static member (<*>)  (WrappedSeqD f, WrappedSeqD x) = SideEffects.add "Using WrappedSeqD's Apply"; WrappedSeqD (f <*> x)
    static member ToList (WrappedSeqD x) = Seq.toList x
#endif

type WrappedSeqE<'s> = WrappedSeqE of 's seq with
    static member Reduce (WrappedSeqE x, reduction) = SideEffects.add "Using WrappedSeqE's Reduce"; Seq.reduce reduction x
    static member ToSeq  (WrappedSeqE x) = SideEffects.add "Using WrappedSeqE's ToSeq"; x

type TestNonEmptyCollection<'a> = private { Singleton: 'a } with
    interface NonEmptySeq<'a> with
        member this.First =
          SideEffects.add "Using TestNonEmptyCollection's Head"
          this.Singleton
        member this.GetEnumerator() =
          SideEffects.add "Using TestNonEmptyCollection's GetEnumerator<>"
          (Seq.singleton this.Singleton).GetEnumerator()
        member this.GetEnumerator() =
          SideEffects.add "Using TestNonEmptyCollection's GetEnumerator"
          (Seq.singleton this.Singleton).GetEnumerator() :> System.Collections.IEnumerator

module TestNonEmptyCollection =
    let Create x = { Singleton = x } :> NonEmptySeq<_>

open System.Collections.Generic
open System.Collections
open System.Threading.Tasks

type ListOnlyIndex<'s> (l: 's list) = 
    interface IList<'s> with 
        member __.Count = List.length l 
        member __.IsReadOnly with get () = true
        member __.Item with get index = List.item index l and set _ _ = failwith "set"
        member __.Add _ = failwith "Add"
        member __.Clear () = failwith "Clear"
        member __.Contains _ = failwith "Contains"
        member __.CopyTo (_, _) = failwith "CopyTo"
        member __.GetEnumerator () : IEnumerator<'s> = failwith "ListOnlyIndex.GetEnumerator"
        member __.GetEnumerator () : IEnumerator = failwith "ListOnlyIndex.GetEnumerator"
        member __.IndexOf _ = failwith "IndexOf"
        member __.Insert (index: int, item:'s) = failwithf "Insert %i %A" index item
        member __.Remove _ = failwith "Remove"
        member __.RemoveAt _ = failwith "RemoveAt"

type ReadOnlyListOnlyIndex<'s> (l: 's list) = 
    interface IReadOnlyList<'s> with 
        member __.Count = List.length l 
        member __.Item with get index = List.item index l
        member __.GetEnumerator () : IEnumerator<'s> = failwith "ReadOnlyListOnlyIndex.GetEnumerator"
        member __.GetEnumerator () : IEnumerator = failwith "ReadOnlyListOnlyIndex.GetEnumerator"

