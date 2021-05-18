namespace FSharpPlus.Tests

#nowarn "686"

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open FSharpPlus.Math.Applicative
open CSharpLib

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

type WrappedListC<'s> = WrappedListC of 's list with
    static member (+) (WrappedListC l, WrappedListC x) = WrappedListC (l @ x)
    static member Zero = WrappedListC List.empty
    static member Sum (lst: seq<WrappedListC<_>>) = Seq.head lst

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
        printfn "WrappedListD.FindIndex"
        findIndex y x
    static member FindSliceIndex (WrappedListD x, WrappedListD y) =
        SideEffects.add "Using WrappedListD's FindSliceIndex"
        printfn "WrappedListD.FindSliceIndex"
        findSliceIndex y x
    member this.Length =
        SideEffects.add "Using WrappedListD's Length"
        let (WrappedListD lst) = this
        List.length lst
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

type WrappedListH<'s> = WrappedListH of 's list with
    static member Map (WrappedListH lst, f) = WrappedListH (List.map f lst)
    static member inline Sequence (x: WrappedListH<'``Functor<'T>``>) =
        let (WrappedListH lst) = x
        let s = sequence lst : '``Functor<List<'T>>``
        map WrappedListH s : '``Functor<WrappedListH<'T>>``

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

type WrappedSeqD<'s> = WrappedSeqD of 's seq with
    static member Return x = SideEffects.add "Using WrappedSeqD's Return"; WrappedSeqD (Seq.singleton x)
    static member (<*>)  (WrappedSeqD f, WrappedSeqD x) = SideEffects.add "Using WrappedSeqD's Apply"; WrappedSeqD (f <*> x)
    static member ToList (WrappedSeqD x) = Seq.toList x
    static member ChooseIndexed (WrappedSeqD x, f) =
            SideEffects.add "Using WrappedSeqD's ChooseIndexed"
            WrappedSeqD (Seq.choosei f x)
    static member Lift3 (f, WrappedSeqD x, WrappedSeqD y, WrappedSeqD z) =
            SideEffects.add "Using WrappedSeqD's Lift3"
            WrappedSeqD (Seq.lift3 f x y z)

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

module Monoid =

    type ZipList<'s> = ZipList of 's seq with
        static member Return (x:'a)                               = ZipList (Seq.initInfinite (konst x))
        static member Map   (ZipList x, f: 'a->'b)                = ZipList (Seq.map f x)
        static member (<*>) (ZipList (f: seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) : ZipList<'b>
        static member inline get_Zero () = result zero            : ZipList<'a>
        static member inline (+) (x:ZipList<'a>, y:ZipList<'a>) = lift2 plus x y :ZipList<'a>
        static member ToSeq    (ZipList lst)     = lst

    type ZipList'<'s> = ZipList' of 's seq with
        static member Return (x: 'a)                                = ZipList' (Seq.initInfinite (konst x))
        static member Map   (ZipList' x, f: 'a->'b)                 = ZipList' (Seq.map f x)
        static member (<*>) (ZipList' (f: seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) : ZipList'<'b>
        static member inline get_Zero () = result zero              : ZipList'<'a>
        static member inline (+) (x: ZipList'<'a>, y: ZipList'<'a>) = lift2 plus x y :ZipList'<'a>
        static member inline Sum (x: seq<ZipList'<'a>>) = SideEffects.add "Using optimized Sum"; List.foldBack plus (Seq.toList x) zero : ZipList'<'a>
        static member ToSeq    (ZipList' lst)     = lst

    type MyList<'t> = MyList of list<'t> with
        static member get_Empty () = MyList []
        static member (<|>) (MyList x, MyList y) = MyList (x @ y)

    type MyNum = MyNum of int with
        static member get_Empty () = MyNum 0
        static member FromInt32 x = MyNum x


    let testCompile =
        let _res1n2 = MyList [1] ++ MyList [2] ++ zero
        let _res0 : MyNum = zero 

        let _asQuotation = plus    <@ ResizeArray (["1"]) @> <@ ResizeArray (["2;3"]) @>
        let _quot123     = plus    <@ ResizeArray ([1])   @> <@ ResizeArray ([2;3])   @>
        let _quot1       = plus    <@ ResizeArray ([1])   @>      (zero)
        let _quot23      = plus       (zero)         <@ ResizeArray ([2;3])   @>
        let _quot13      = plus       (zero)         <@ ("1","3") @>
        let lzy1 = plus (lazy [1]) (lazy [2;3])
        let _lzy = plus (zero) lzy1
        let asy1 = plus (async.Return [1]) (async.Return [2;3])
        let _asy = plus (zero) asy1
        let _bigNestedTuple1 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (2, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (3, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20))
        let _bigNestedTuple2 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (zero, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ zero

        let _nes : NonEmptySeq<_> = plus (NonEmptySeq.singleton 1) (NonEmptySeq.singleton 2)

        let mapA = Map.empty 
                    |> Map.add 1 (async.Return "Hey")
                    |> Map.add 2 (async.Return "Hello")

        let mapB = Map.empty 
                    |> Map.add 3 (async.Return " You")
                    |> Map.add 2 (async.Return " World")

        let mapAB = plus mapA mapB
        let _greeting1 = Async.RunSynchronously mapAB.[2]
        let _greeting2 = Async.RunSynchronously (Seq.sum [mapA; zero; mapB]).[2]

        let dicA = new Dictionary<string,Task<string>> ()
        dicA.["keya"] <- (result "Hey"  : Task<_>)
        dicA.["keyb"] <- (result "Hello": Task<_>)

        let dicB = new Dictionary<string,Task<string>> ()
        dicB.["keyc"] <- (result " You"  : Task<_>)
        dicB.["keyb"] <- (result " World": Task<_>)

        let dicAB = plus dicA dicB
        let _iDicAb   = plus (dicA :> IDictionary<_,_>)         (dicB :> IDictionary<_,_>)
        let _iroDicAb = plus (dicA :> IReadOnlyDictionary<_,_>) (dicB :> IReadOnlyDictionary<_,_>)

        let _greeting3 = extract dicAB.["keyb"]
        let _greeting4 = extract (Seq.sum [dicA; zero; dicB]).["keyb"]

        let _res2   = Seq.sum [async {return Endo ((+) 2)}; async {return Endo ((*) 10)}; async {return Endo id}; async {return Endo ((%) 3)}; async {return zero} ] |> Async.RunSynchronously |> Endo.run <| 3
        let _res330 = Seq.sum [async {return (fun (x:int) -> string x)}; async {return (fun (x:int) -> string (x*10))}; async {return zero } ] </Async.RunSynchronously/>  3
        ()


    [<Test>]
    let seqSumDefaultCustom () =
        let (WrappedListB x) = Seq.sum [WrappedListB [10]; WrappedListB [15]]
        let (WrappedListC y) = Seq.sum [WrappedListC [10]; WrappedListC [15]]
        Assert.AreEqual ([10;15], x)
        Assert.AreEqual ([10], y)

        let x = [ ("a", 1); ("b", 2); ("a", 3) ]
        let y = x |> map (Seq.singleton >> (ofSeq : seq<_*_> -> Dictionary<_,_>) >> map List.singleton) |> Seq.sum
        let z = x |> map (Seq.singleton >>             dict                      >> map List.singleton) |> Seq.sum
        Assert.IsInstanceOf<Option< Dictionary<string,int list>>> (Some y)
        Assert.IsInstanceOf<Option<IDictionary<string,int list>>> (Some z)

        SideEffects.reset ()

        let quotLst123  = plus zero (ZipList [ [1];[2];[3] ])

        Assert.AreEqual ([[1]; [2]; [3]], quotLst123 |> toList)
        Assert.AreEqual ([], SideEffects.get ())

        let quotLst123' = Seq.sum [zero; zero; ZipList' [ [1];[2];[3] ]]

        Assert.AreEqual ([[1]; [2]; [3]], quotLst123' |> toList)
        Assert.AreEqual (["Using optimized Sum"], SideEffects.get ())

        let _wl = WrappedListB  [2..10]

        let _arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
        let _listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
        let _seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

        let _arrayGroupAdj   = chunkBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]

        ()






module Functor =
    [<Test>]
    let mapDefaultCustom () = 

        SideEffects.reset ()

        // NonEmptyList<_> has Map but at the same time is a seq<_>
        let testVal1 = map ((+) 1) (nelist {10; 20; 30})
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal1)

        let testVal2 = map ((+) 1) ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some testVal2)

        let testVal3 = map ((+) 1) (dict (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some testVal3)

        let testVal4 = map ((+) 1) (NonEmptySet.Create(10, 20, 30))
        Assert.IsInstanceOf<Option<NonEmptySet<int>>> (Some testVal4)

        let testVal5 = map ((+) 1) (NonEmptyMap.Create(("a", 1), ("b", 2)))
        Assert.IsInstanceOf<Option<NonEmptyMap<string,int>>> (Some testVal5)

        let testVal6 = map ((+) 1) (TestNonEmptyCollection.Create 1)
        Assert.IsInstanceOf<Option<NonEmptySeq<int>>> (Some testVal6)

        // WrappedSeqD is Applicative. Applicatives are Functors => map should work
        Assert.AreEqual ([], SideEffects.get ())
        let testVal4 = map ((+) 1) (WrappedSeqD [1..3])
        Assert.IsInstanceOf<Option<WrappedSeqD<int>>> (Some testVal4)
        Assert.AreEqual (["Using WrappedSeqD's Return"; "Using WrappedSeqD's Apply"], SideEffects.get ())
        SideEffects.reset ()
        
        // WrappedListE is a Monad. Monads are Functors => map should work
        let testVal5 = map ((+) 1) (WrappedListE [1..3])
        Assert.IsInstanceOf<Option<WrappedListE<int>>> (Some testVal5)

        // Same with WrappedListD but WrappedListD is also IEnumerable<_>
        Assert.AreEqual ([], SideEffects.get ())
        let testVal6 = map ((+) 1) (WrappedListD [1..3])
        Assert.IsInstanceOf<Option<WrappedListD<int>>> (Some testVal6)
        Assert.AreEqual (["Using WrappedListD's Bind"; "Using WrappedListD's Return"; "Using WrappedListD's Return"; "Using WrappedListD's Return"], SideEffects.get ())
        SideEffects.reset ()

        let testVal7 = TestNonEmptyCollection.Create 42
        head testVal7 |> ignore
        Assert.AreEqual (["Using TestNonEmptyCollection's Head"], SideEffects.get ())
        let testVal8 = testVal7 >>= fun i -> result (string i)
        Assert.IsInstanceOf<Option<NonEmptySeq<string>>> (Some testVal8)
        
        let testVal9 = map ((+) 1) (IReadOnlyCollection.ofList [1..3])
        Assert.IsInstanceOf<Option<IReadOnlyCollection<int>>> (Some testVal9)

        let testVal10 = map ((+) 1) (async { return 1})
        Assert.IsInstanceOf<Option<Async<int>>> (Some testVal10)
        areEqual 2 (testVal10 |> Async.RunSynchronously)

    [<Test>]
    let unzip () = 
        let testVal = unzip {Head = (1, 'a'); Tail = [(2, 'b');(3, 'b')]}
        Assert.IsInstanceOf<Option<NonEmptyList<int> * NonEmptyList<char>>> (Some testVal)

        let testVal2 = unzip (NonEmptyMap.Create((1,(true, 'a')), (2, (false, 'b'))))
        Assert.IsInstanceOf<Option<NonEmptyMap<int, bool> * NonEmptyMap<int, char>>> (Some testVal2)

    [<Test>]
    let zipTest () =

        SideEffects.reset ()
        let _a = zip (seq [1;2;3]) (seq [1. .. 3. ])
        Assert.AreEqual ([], SideEffects.get ())

        let _b = zip (WrappedListD [1;2;3]) (WrappedListD [1. .. 3. ])
        Assert.AreEqual (["Using WrappedListD's zip"], SideEffects.get ())

        let _c = zip (dict [1,'1' ; 2,'2' ; 4,'4']) (dict [1,'1' ; 2,'2' ; 3,'3'])
        let _d = zip [ 1;2;3 ] [ 1. .. 3. ]
        let _e = zip [|1;2;3|] [|1. .. 3.|]
        let _g = zip ((seq [1;2;3]).GetEnumerator ()) ((seq [1. .. 3. ]).GetEnumerator ())
        let _h = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let _i = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let _j = zip (async {return 1}) (async {return '2'})
        let _h = zip (Task.FromResult 1) (Task.FromResult '2')
        let _i = zip List.singleton<int> Array.singleton<int>
        let _k = zip (TestNonEmptyCollection.Create 1) (result 2)

        let _fa a = zip a (seq [1. .. 3. ])
        let _fb a = zip a (WrappedListD [1. .. 3. ])
        let _fc a = zip a (dict [1,'1' ; 2,'2' ; 3,'3'])
        let _fd a = zip a [ 1. .. 3. ]
        let _fe a = zip a [|1. .. 3.|]
        let _fg a = zip a ((seq [1. .. 3. ]).GetEnumerator ())
        let _fh a = zip a (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let _fi a = zip a (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let _fj a = zip a (async {return '2'})
        let _fh a = zip a (Task.FromResult '2')
        let _fi a = zip a Array.singleton<int>
        let _fj a = zip a (TestNonEmptyCollection.Create 2)

        let _ga b = zip (seq [1;2;3]) b
        let _gb b = zip (WrappedListD [1;2;3]) b
        let _gc b = zip (dict [1,'1' ; 2,'2' ; 4,'4']) b
        let _gd b = zip  [ 1;2;3 ] b
        let _ge b = zip  [|1;2;3|] b
        let _gg b = zip ((seq [1;2;3]).GetEnumerator ()) b
        let _gh b = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) b
        let _gi b = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) b
        let _gj b = zip (async {return 1}) b
        let _gh b = zip (Task.FromResult 1) b
        let _gh b = zip List.singleton<int> b
        let _gj b = zip (TestNonEmptyCollection.Create 1) b

        let _ha : _ -> _ -> _ seq            = zip
        let _hb : _ -> _ -> _ WrappedListD   = zip
        let _hc : _ -> _ -> IDictionary<_,_> = zip
        let _hd : _ -> _ -> _ list           = zip
        let _he : _ -> _ -> _ []             = zip
        let _hg : _ -> _ -> _ IEnumerator    = zip
        let _hh : _ -> _ -> Map<_,_>         = zip
        let _hi : _ -> _ -> Dictionary<_,_>  = zip
        let _hj : _ -> _ -> Async<_>         = zip
        let _hh : _ -> _ -> Task<_>          = zip
        let _hi : _ -> _ -> (int -> _ )      = zip
        let _hj : _ -> _ -> NonEmptySeq<_>   = zip

        ()

    [<Test>]
    let genericZipShortest () =
        let a = zip [|1; 2; 3|]  [|"a"; "b"|]
        CollectionAssert.AreEqual ([|1,"a"; 2,"b"|], a)
        
        let l = zip [1; 2]  ["a"; "b"; "c"]
        CollectionAssert.AreEqual ([1,"a"; 2,"b"], l)
        
        let e = zip (ResizeArray [1; 2]) (ResizeArray ["a"; "b"; "c"])
        CollectionAssert.AreEqual (ResizeArray [1,"a"; 2,"b"], e)
        
        let nel = zip (NonEmptyList.ofList [1; 2]) (NonEmptyList.ofList ["a"; "b"; "c"])
        CollectionAssert.AreEqual (NonEmptyList.ofList [1,"a"; 2,"b"], nel)

module Collections =

    open System.Collections.Concurrent

    let testCollections =
        let bigSeq = seq {1..10000000}
        let bigLst = [ 1..10000000 ]
        let bigArr = [|1..10000000|]
        let bigMut = ResizeArray(seq {1..10000000})

        let _ = head bigSeq
        let _ = head bigLst
        let _ = head bigArr

        let _ = skip 1000 bigSeq
        let _ = skip 1000 bigLst
        let _ = skip 1000 bigArr
        let _ = skip 1000 bigMut
        let _ = "hello world" |> skip 6 |> toList
        let _ = ofList ['h';'e';'l';'l';'o';' '] + "world"
        let _ = item 2 "hello"
        ()

    let testSeqConversions =

        let sk: Generic.Stack<_>          = ofSeq { 1 .. 3 }
        let sg: string                    = ofSeq {'1'..'3'}  // but it will come back as seq<char>
        let sb: Text.StringBuilder        = ofSeq {'1'..'3'}  // but it will come back as seq<char>
        let sq1:_ seq                     = ofSeq { 1 .. 3 }
        let sq2:_ seq                     = ofSeq (seq [(1, "One"); (2, "Two")])
        let sq3:_ seq                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        let sq4:_ seq                     = ofSeq (seq [(1, "One", '1', 1M); (2, "Two", '2', 2M)])
        let ls1:_ list                    = ofSeq {'1'..'3'}
        let ls2:_ list                    = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        let st1:_ Set                     = ofSeq {'1'..'3'}
        let st2:_ Set                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        let ss: Generic.SortedSet<_>      = ofSeq (seq [3..6])
        let ra: Generic.List<_>           = ofSeq (seq [1..3])
        let sl: Generic.SortedList<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _sl:Generic.SortedList<_,_>   = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _mp:Map<_,_>                  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let d : Generic.IDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let _d: Generic.IDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let r : IReadOnlyDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let _r: IReadOnlyDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let rc: IReadOnlyCollection<_>    = ofSeq (seq [2..7])
        let ut: Hashtable                 = ofSeq (seq [1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofSeq (seq ["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofSeq (seq [4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofSeq {'1'..'3'}                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _cd:ConcurrentDictionary<_,_> = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofSeq {'1'..'3'}

        // now go back
        let _sk'  = toSeq sk
        let _sg'  = toSeq sg
        let _sb'  = toSeq sb
        let _sq1' = toSeq sq1
        let _sq2' = toSeq sq2
        let _sq3' = toSeq sq3
        let _sq4' = toSeq sq4
        let _ls1' = toSeq ls1
        let _ls2' = toSeq ls2
        let _st1' = toSeq st1
        let _st2' = toSeq st2
        let _ss'  = toSeq ss 
        let _ra'  = toSeq ra 
        let _sl'  = toSeq sl 
        let _dc'  = toSeq dc 
        let _mp'  = toSeq mp 
        let _d'   = toSeq d  
        let _r'   = toSeq r
        let _rc'  = toSeq rc
        let _ut'  = toSeq ut 
        let _al'  = toSeq al 
        let _us'  = toSeq us 
        let _cc'  = toSeq cc 
        let _cd'  = toSeq cd 
        let _cb'  = toSeq cb 

        // there are some 'one-way' collections that can only be converted toSeq

        let columns = 
            let d = new Data.DataTable () 
            [|new Data.DataColumn "id";new Data.DataColumn "column1";new Data.DataColumn "column2"|] |> d.Columns.AddRange
            d.Columns
        let _col1 = columns |> find (fun x -> x.ColumnName = "column1")
        let _cols = columns |> toList |> map  (fun x -> x.ColumnName)

        // Defaults

        let _12: WrappedListI<_> = seq [1;2] |> ofSeq


        ()

    let testListConversions =

        // From sequence

        let sk: Generic.Stack<_>          = ofList [ 1 .. 3 ]
        let sg: string                    = ofList ['1'..'3']  // but it will come back as seq<char>
        let sb: Text.StringBuilder        = ofList ['1'..'3']  // but it will come back as seq<char>
        let sq1:_ seq                     = ofList [ 1 .. 3 ]
        let sq2:_ seq                     = ofList ([(1, "One"); (2, "Two")])
        let sq3:_ seq                     = ofList ([(1, "One", '1'); (2, "Two", '2')])
        let sq4:_ seq                     = ofList ([(1, "One", '1', 1M); (2, "Two", '2', 2M)])
        let ls1:_ list                    = ofList ['1'..'3']
        let ls2:_ list                    = ofList ([(1, "One", '1'); (2, "Two", '2')])
        let st1:_ Set                     = ofList ['1'..'3']
        let st2:_ Set                     = ofList ([(1, "One", '1'); (2, "Two", '2')])
        let ss: Generic.SortedSet<_>      = ofList ([3..6])
        let ra: Generic.List<_>           = ofList ([1..3])
        let sl: Generic.SortedList<_,_>   = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _sl:Generic.SortedList<_,_>   = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofList ([(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _mp:Map<_,_>                  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let d : Generic.IDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let _d: Generic.IDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let r : IReadOnlyDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let _r: IReadOnlyDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let rc: IReadOnlyCollection<_>    = ofList ([2..5])
        let ut: Hashtable                 = ofList ([1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofList (["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofList ([4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofList ['1'..'3']                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _cd:ConcurrentDictionary<_,_> = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofList ['1'..'3']

        // now go back
        let _sk'  = toList sk
        let _sg'  = toList sg
        let _sb'  = toList sb
        let _sq1' = toList sq1
        let _sq2' = toList sq2
        let _sq3' = toList sq3
        let _sq4' = toList sq4
        let _ls1' = toList ls1
        let _ls2' = toList ls2
        let _st1' = toList st1
        let _st2' = toList st2
        let _ss'  = toList ss
        let _ra'  = toList ra
        let _sl'  = toList sl
        let _dc'  = toList dc
        let _mp'  = toList mp
        let _d'   = toList d
        let _r'   = toList r
        let _rc'  = toList rc
        let _ut'  = toList ut
        let _al'  = toList al
        let _us'  = toList us
        let _cc'  = toList cc
        let _cd'  = toList cd
        let _cb'  = toList cb

        ()

    let testSorts =
        let _r1 = [4..1] |> sort
        let _r2 = [4..1] |> sortBy string
        let _r3 = seq [4..1] |> sort
        let _r4 = seq [4..1] |> sortBy string
        let _r5 = ResizeArray [4..1] |> sort
        let _r6 = ResizeArray [4..1] |> sortBy string

        ()

    let testGeneralizableValues () =
        let a: list<_> = empty
        let _ =  0 ::a
        let _ = '0'::a

        let b: WrappedSeqA<_> = empty
        let _ = WrappedSeqA [ 0 ] <|> b
        let _ = WrappedSeqA ['0'] <|> b
         
        ()

    [<Test>]
    let readOnlyNth () =
        let readOnlyCollection = ReadOnlyCollection [|1..10|]
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        Assert.AreEqual (2, nth 1 [1..10])
        Assert.AreEqual (2, nth 1 readOnlyCollection)
        Assert.AreEqual (2, nth 1 iReadOnlyList)

    [<Test>]
    let readOnlyNthIndex () =
        let l = ListOnlyIndex [1..10]
        Assert.AreEqual (2, nth 1 l)
        let rl = ReadOnlyListOnlyIndex [1..10]
        Assert.AreEqual (2, nth 1 rl)

    [<Test>]
    let choose () = 
        let d = choose Some ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some d)

        let d' = choose Some ((ofSeq :seq<_*_> -> IDictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some d')
        
        let rd = choose Some ((ofSeq :seq<_*_> -> IReadOnlyDictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IReadOnlyDictionary<string,int>>> (Some rd)
        
        let m = choose Some ((ofSeq :seq<_*_> -> Map<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Map<string,int>>> (Some m)
        
module Foldable =

    let foldables =
        let _10  = foldBack (+) (seq [1;2;3;4]) 0
        let _323 = toList (seq [3;2;3])
        let _03  = filter ((=) 3) (seq [1;2;3])
        ()

    [<Test>]
    let foldMapDefaultCustom () =
        SideEffects.reset ()
        let x = foldMap ((+) 10) (WrappedListD [1..4]) //= 50 w side effect
        Assert.AreEqual (50, x)
        Assert.AreEqual (["Using optimized foldMap"], SideEffects.get ())

        SideEffects.reset ()
        let _ = foldMap ((+) 10) {1..4}  //= 50 w/o side effect
        Assert.AreEqual (50, x)
        Assert.AreEqual ([], SideEffects.get ())

    [<Test>]
    let filterDefaultCustom () = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (WrappedListA [2], testVal)
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

        let _twos   = filter ((=) (box 2)) (([1;2;3;4;3;2;1;2;3] |> ofSeq) : Collections.ArrayList)
        let _five   = filter ((=) 5) (WrappedListB' [1;2;3;4;5;6])   // <- Uses the default method for filter.
        let _optionFilter = filter ((=) 3) (Some 4)

        ()

    [<Test>]
    let foldAternatives () = 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        Assert.AreEqual (Some 3, x)
        Assert.AreEqual ([3; 4], y)

    [<Test>]
    let fromToSeq () =
        let s = seq [Collections.Generic.KeyValuePair(1, "One"); Collections.Generic.KeyValuePair(2, "Two")]
        let t = {'a'..'d'}

        let dc2:Collections.Generic.Dictionary<_,_> = ofSeq s
        let s' = toSeq dc2

        let arr:_ [] = ofSeq s
        let s'' = toSeq arr

        let str:string = ofSeq t
        let t' = toSeq str 

        Assert.AreEqual (toList s, toList s')
        Assert.AreEqual (toList s, toList s'')
        Assert.AreEqual (toList t, toList t')

        Assert.IsInstanceOf ((Some s' ).GetType (), Some s)
        Assert.IsInstanceOf ((Some s'').GetType (), Some s)
        Assert.IsInstanceOf ((Some t' ).GetType (), Some t)


    [<Test>]
    let sortBy () =
        let l  = [10;4;6;89]
        let l' = sortBy id l
        let s  = WrappedListB [10;4;6;89]
        let s' = sortBy id s
        Assert.AreEqual ([4;6;10;89], l')
        Assert.AreEqual (WrappedListB [4;6;10;89], s')

        let sortedList = sortBy string     [ 11;2;3;9;5;6;7;8;9;10 ]
        let sortedSeq  = sortBy string (seq [11;2;3;9;5;6;7;8;9;10])
        Assert.IsInstanceOf<Option<list<int>>> (Some sortedList)
        Assert.IsInstanceOf<Option<seq<int>>> (Some sortedSeq)

    [<Test>]
    let intersperse () =
        Assert.AreEqual ("a,b,c,d,e", intersperse ',' "abcde")
        Assert.AreEqual (["a";",";"b";",";"c";",";"d";",";"e"], intersperse "," ["a";"b";"c";"d";"e"])

    [<Test>]
    let readOnlyIntercalate () =
        Assert.AreEqual ("Lorem, ipsum, dolor", intercalate ", " ["Lorem"; "ipsum"; "dolor"])
        Assert.AreEqual ("Lorem, ipsum, dolor", intercalate ", " (ReadOnlyCollection( [|"Lorem"; "ipsum"; "dolor"|] )))


    [<Test>]
    let readOnlyTryPick () =
        let readOnlyCollection = ReadOnlyCollection( [|1..10|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>        
        let picker i = if i % 3 = 0 then Some i else None
        Assert.AreEqual (Some 3, tryPick picker [1..10])
        Assert.AreEqual (Some 3, tryPick picker readOnlyCollection)
        Assert.AreEqual (Some 3, tryPick picker iReadOnlyList)

    [<Test>]
    let readOnlyTryFind () =
        let predicate i = i % 3 = 0
        let readOnlyCollection = ReadOnlyCollection( [|1..10|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        Assert.AreEqual (Some 3, tryFind predicate [1..10])
        Assert.AreEqual (Some 3, tryFind predicate readOnlyCollection)
        Assert.AreEqual (Some 3, tryFind predicate iReadOnlyList)

    [<Test>]
    let readOnlyfoldMap () =
        let readOnlyCollection = ReadOnlyCollection( [|1..4|] )
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        Assert.AreEqual (50, foldMap ((+) 10) readOnlyCollection)
        Assert.AreEqual (50, foldMap ((+) 10) iReadOnlyList)

    [<Test>]
    let exists () =
        SideEffects.reset ()
        let _ = exists ((=) 2) [1..3]
        let _ = exists ((=) '2') (System.Text.StringBuilder "abc")
        let _ = exists ((=) 2) (NonEmptySet.Create(1,2,3))
        let _ = exists ((=) 2) (WrappedListA [1..3])
        let _ = exists ((=) 2) (WrappedListD [1..3])
        areEqual ["Using WrappedListA's Exists"; "Using WrappedListD's Exists"] (SideEffects.get ())
        ()

    [<Test>]
    let pick () =
        SideEffects.reset ()
        let _ = pick Some [1..3]
        let _ = pick Some (System.Text.StringBuilder "abc")
        let _ = pick Some (NonEmptySet.Create(1,2,3))
        let _ = pick Some (WrappedListA [1..3])
        let _ = pick Some (WrappedListD [1..3])
        areEqual ["Using WrappedListA's Pick"; "Using WrappedListD's Pick"] (SideEffects.get ())
        ()

    [<Test>]
    let minimum () =
        SideEffects.reset ()
        let _ = minimum [1..3]
        let _ = minimum (System.Text.StringBuilder "abc")
        let _ = minimum (NonEmptySet.Create(1,2,3))
        let _ = minimum (WrappedListA [1..3])
        let _ = minimum (WrappedListD [1..3])
        areEqual ["Using WrappedListA's Min"; "Using WrappedListD's Min"] (SideEffects.get ())
        ()

    [<Test>]
    let maxBy () =
        SideEffects.reset ()
        let _ = maxBy id [1..3]
        let _ = maxBy id (System.Text.StringBuilder "abc")
        let _ = maxBy id (NonEmptySet.Create(1,2,3))
        let _ = maxBy id (WrappedListA [1..3])
        let _ = maxBy id (WrappedListD [1..3])
        areEqual ["Using WrappedListA's MaxBy"; "Using WrappedListD's MaxBy"] (SideEffects.get ())
        ()

    [<Test>]
    let length () =
        SideEffects.reset ()
        let _ = length [1..3]
        let _ = length (System.Text.StringBuilder "abc")
        let _ = length (NonEmptySet.Create(1,2,3))
        let _ = length (WrappedListA [1..3])
        let _ = length (WrappedListD [1..3])
        areEqual ["Using WrappedListA's Length"; "Using WrappedListD's Length"] (SideEffects.get ())
        ()

    [<Test>]
    let tryHead () =
        let s                = tryHead <| seq [1;2]
        let s': int option   = tryHead <| seq []
        areEqual s (Some 1)
        areEqual s' None

        let l                = tryHead [1;2;3]
        let l': int option   = tryHead []
        areEqual l (Some 1)
        areEqual l' None

        let a                = tryHead [|1|]
        let a': int option   = tryHead [||]
        areEqual a (Some 1)
        areEqual a' None

        let nes              = tryHead <| NonEmptySeq.ofList [1;2]
        areEqual nes (Some 1)

        let str                = tryHead "string"
        let str': char option  = tryHead ""
        areEqual str (Some 's')
        areEqual str' None

        let sb               = tryHead (System.Text.StringBuilder("string"))
        let sb'              = tryHead (System.Text.StringBuilder())
        areEqual sb (Some 's')
        areEqual sb' None
        ()

    [<Test>]
    let tryLast () =
        let s                = tryLast <| seq [1;2]
        let s': int option   = tryLast <| seq []
        areEqual s (Some 2)
        areEqual s' None

        let l                = tryLast [1;2;3]
        let l': int option   = tryLast []
        areEqual l (Some 3)
        areEqual l' None

        let a                = tryLast [|1|]
        let a': int option   = tryLast [||]
        areEqual a (Some 1)
        areEqual a' None

        let nes              = tryLast <| NonEmptySeq.ofList [1;2]
        areEqual nes (Some 2)

        let str                = tryLast "string"
        let str': char option  = tryLast ""
        areEqual str (Some 'g')
        areEqual str' None

        let sb               = tryLast (System.Text.StringBuilder("string"))
        let sb'              = tryLast (System.Text.StringBuilder())
        areEqual sb (Some 'g')
        areEqual sb' None
        ()

module Indexable = 
    [<Test>]
    let testCompileAndExecuteItem () =

        let a = Map.ofSeq [1, "one"; 2, "two"]
        let _ = item 1 a

        let b = dict [1, "one"; 2, "two"]
        let _ = item 1 b

        let c = "two"
        let _ = item 1 c

        let d = System.Text.StringBuilder "one"
        let _ = item 1 d

        let e = array2D [[1;2];[3;4];[5;6]]
        let _ = item (1, 1) e

        let f = [1, "one"; 2, "two"]
        let _ = item 1 f

        let g = [|1, "one"; 2, "two"|]
        let _ = item 1 g

        let h = ResizeArray [1, "one"; 2, "two"]
        let _ = item 1 h

        let i = Array3D.create 3 2 2 0
        let _ = item (1, 1, 1) i

        let j = Array4D.create 3 2 2 3 0
        let _ = item (1, 1, 1, 1) j

        let k = NonEmptyMap.Create (("a", 1), ("b", 2))
        let _ = item "b" k

        // This doesn't intentionally compile: seq is not Indexable. Not all foldables are Indexable, for example a Set is foldable but not Indexable. For seq use nth instead.
        // let f = seq [1, "one"; 2, "two"]
        // let _ = item 1 f

        ()

    [<Test>]
    let testCompileAndExecuteTryItem () =

        let a = Map.ofSeq [1, "one"; 2, "two"]
        let _ = tryItem 1 a

        let b = dict [1, "one"; 2, "two"]
        let _ = tryItem 1 b

        let c = "two"
        let _ = tryItem 1 c

        let d = System.Text.StringBuilder "one"
        let _ = tryItem 1 d

        let e = array2D [[1;2];[3;4];[5;6]]
        let _ = tryItem (1, 1) e

        let f = [1, "one"; 2, "two"]
        let _ = tryItem 1 f

        let g = [|1, "one"; 2, "two"|]
        let _ = tryItem 1 g

        let h = ResizeArray [1, "one"; 2, "two"]
        let _ = tryItem 1 h

        let i = Array3D.create 3 2 2 0
        let _ = tryItem (1, 1, 1) i

        let j = Array4D.create 3 2 2 3 0
        let _ = tryItem (1, 1, 1, 1) j

        let k = NonEmptyMap.Create (("a", 1), ("b", 2))
        let _ = tryItem "b" k

        let w = WrappedListA [1, "one"; 2, "two"]
        let _ = tryItem 1 w

        ()

    [<Test>]
    let testCompileAndExecuteTraverseIndexed () =
        let nem = NonEmptyMap.Create (("a", Some 1), ("b", Some 2), ("c", Some 3))
        let rs1 = traversei (fun _ v -> v) nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs1

    [<Test>]
    let tryItemReadonly () =
        let d = ReadOnlyDictionary (dict [1, "one"; 2, "two"])
        let iReadOnlyDict = d :> IReadOnlyDictionary<_,_>
        let l = ReadOnlyCollection [|1..10|]
        let iReadOnlyList = l :> IReadOnlyList<_>
        let rarr = ResizeArray [|1..10|]
        Assert.AreEqual (Some "one", tryItem 1 d)
        Assert.AreEqual (Some "one", tryItem 1 iReadOnlyDict)
        Assert.AreEqual ("one", item 1 d)
        Assert.AreEqual ("one", item 1 iReadOnlyDict)
        Assert.AreEqual (2, item 1 l)
        Assert.AreEqual (2, item 1 rarr)
        Assert.AreEqual (2, item 1 iReadOnlyList)
        Assert.AreEqual (Some 2, tryItem 1 l)
        Assert.AreEqual (Some 2, tryItem 1 iReadOnlyList)
        Assert.AreEqual (Some 2, tryItem 1 rarr)

    [<Test>]
    let mapiUsage () =
        let m = Map.ofList [1, "one"; 2, "two"]
        let l = ReadOnlyCollection [|1..2|]
        let iReadOnlyList = l :> IReadOnlyList<_>
        let rarr = ResizeArray [|1..2|]
        let mapDS = sprintf "%d-%s"
        areEquivalent [KeyValuePair(1,"1-one"); KeyValuePair(2,"2-two")] (mapi mapDS m)
        let mapDD = sprintf "%d-%d"
        areEquivalent ["0-1";"1-2"] (mapi mapDD l)
        areEquivalent ["0-1";"1-2"] (mapi mapDD iReadOnlyList)
        areEquivalent ["0-1";"1-2"] (mapi mapDD rarr)

        // correct overload:
        SideEffects.reset ()
        areEquivalent ["0-1";"1-2"] (mapi mapDD (WrappedListD [1..2]))
        areEqual ["Using WrappedListD's MapIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        areEquivalent ["0-1";"1-2"] (MapIndexed.InvokeOnInstance mapDD (WrappedListD [1..2]))
        areEqual ["Using WrappedListD's MapIndexed"] (SideEffects.get ())

    [<Test>]
    let iteriUsage () =
        let m = Map.ofList [1, "one"; 2, "two"]
        SideEffects.reset ()
        iteri (fun i v -> SideEffects.add <| sprintf "Got %d-%s" i v) m
        areEquivalent ["Got 1-one";"Got 2-two"] (SideEffects.get ())

        SideEffects.reset ()
        let onIteration i v= ()
        iteri onIteration (WrappedListD [1..2])
        areEqual ["Using WrappedListD's IterateIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        IterateIndexed.InvokeOnInstance onIteration (WrappedListD [1..2])
        areEqual ["Using WrappedListD's IterateIndexed"] (SideEffects.get ())

    [<Test>]
    let foldiUsage () =
        SideEffects.reset ()
        let folder (s:int) (i:int) (t:int) = t * s - i
        let wlist = WrappedListD [1..2]
        let res = foldi folder 10 wlist
        areEquivalent ["Using WrappedListD's FoldIndexed"] (SideEffects.get ())
        areEqual 19 res
        SideEffects.reset ()
        let res1 = FoldIndexed.InvokeOnInstance folder 10 wlist
        areEquivalent ["Using WrappedListD's FoldIndexed"] (SideEffects.get ())
        areEqual 19 res1

    [<Test>]
    let traverseiUsage () =
        let m1 = WrappedMapA.ofList [(1, [1;1;1]); (2, [2;2;2])]

        SideEffects.reset ()
        let r1 = m1 |> TraverseIndexed.InvokeOnInstance (fun _ _ -> None)
        Assert.AreEqual(None, r1)
        areEquivalent ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r1 = m1 |> traversei (fun _ _ -> None)
        Assert.AreEqual(None, r1)
        areEquivalent ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r2 = m1 |> TraverseIndexed.InvokeOnInstance (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        areEqual (WrappedMapA.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])]) r2.Value
        areEquivalent ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

        SideEffects.reset ()
        let r3 = m1 |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        areEqual (WrappedMapA.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])]) r3.Value
        areEquivalent ["Using WrappedMapA's TraverseIndexed"] (SideEffects.get ())

    [<Test>]
    let findIndexUsage () =
        let m1 = WrappedListD [0..4]
        SideEffects.reset ()
        let i1 = findIndex ((=) 2) m1
        areEquivalent ["Using WrappedListD's FindIndex"] (SideEffects.get ())
        areEqual i1 2

    [<Test>]
    let findSliceIndexUsage () =
        let m1 = WrappedListD [0..4]
        let m2 = WrappedListD [1..3]
        SideEffects.reset ()
        let i1 = findSliceIndex m2 m1
        areEquivalent ["Using WrappedListD's FindSliceIndex"] (SideEffects.get ())
        areEqual i1 1

module Monad = 
    [<Test>]
    let joinDefaultCustom () = 
        let x = join [[1];[2]]
        Assert.AreEqual ([1;2], x)
        let y: WrappedListE<_> = join (WrappedListE [WrappedListE [1];WrappedListE [2]])
        Assert.AreEqual (WrappedListE [1;2], y)
        SideEffects.reset ()
        let z = join (WrappedListF [WrappedListF [1];WrappedListF [2]])
        Assert.AreEqual (WrappedListF [1;2], z)
        Assert.AreEqual (["Join"], SideEffects.get ())

    [<Test>]
    let workFlow () =       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        Assert.IsInstanceOf<WrappedListD<int>> (testVal)

    [<Test>]
    let DelayForCont () = 
        // If Delay is not properly implemented this will stack-overflow
        // See http://stackoverflow.com/questions/11188779/stackoverflow-in-continuation-monad
#if MONO
        Assert.Ignore ()
#else
        let map f xs =
            let rec loop xs =
                monad {
                    match xs with
                    | [] -> return []
                    | x :: xs ->
                        let! xs = loop xs
                        return f x :: xs }
            Cont.run (loop xs) id
        let _ = [1..100000] |> map ((+) 1)
        Assert.Pass ()
#endif


module Traversable =

    type Either<'l,'r> = Left of 'l | Right of 'r with
        static member Return x = Right x
        static member inline get_Empty () = Left empty
        static member Map (x, f) = match x with Right a -> Right (f a) | Left a -> Left a
        static member (<*>) (f, x) =
            SideEffects.add ("f(x) <*> " + string x)
            match f, x with Right a, Right b -> Right (a b) | Left e, _ | _, Left e -> Left e
        static member IsLeftZero x = match x with Left _ -> true | _ -> false

    let traverseTest =
        let _None = sequence (seq [Some 3;None ;Some 1])
        let _None2 = sequence (TestNonEmptyCollection.Create (Some 42))
        ()

    [<Test>]
    let sequence_Default_Primitive () = 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    [<Test>]
    let traverseDerivedFromSequence () = 
        let testVal = traverse (fun x -> [int16 x..int16 (x+2)]) (WrappedListH [1; 4])
        Assert.AreEqual (
            [
                WrappedListH [1s; 4s]; WrappedListH [1s; 5s]; WrappedListH [1s; 6s];
                WrappedListH [2s; 4s]; WrappedListH [2s; 5s]; WrappedListH [2s; 6s];
                WrappedListH [3s; 4s]; WrappedListH [3s; 5s]; WrappedListH [3s; 6s]
            ] , testVal)
        Assert.IsInstanceOf<list<WrappedListH<int16>>> testVal

    [<Test>]
    let sequence_Specialization () =
        
        let inline seqSeq (x:_ seq ) = sequence x
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x

        let a : list<_> = seqSeq (seq [[1];[3]])
        CollectionAssert.AreEqual ([seq [1; 3]], a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        CollectionAssert.AreEqual ([[|1; 3|]], b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        CollectionAssert.AreEqual ([[1; 3]], c)
        Assert.IsInstanceOf<list<list<int>>> c

    [<Test>]
    let traverse_Specialization () =
        let _ = Seq.traverse id [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.sequence    [WrappedSeqD [1]; WrappedSeqD [2]]
        let _ = Seq.traverse id [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        let _ = Seq.sequence    [ZipList [1]; ZipList []; ZipList (seq {failwith "sholdn't get here"})] |> toList
        ()

    [<Test>]
    let traverse_Order () =
        SideEffects.reset()
        let mapper v = SideEffects.add <| sprintf "mapping %d" v
        let _ = traverse (Option.map mapper) [Some 1; Some 2]
        SideEffects.are ["mapping 1"; "mapping 2"]


    [<Test>]
    let traversableForNonPrimitive () =
        let nel = nelist { Some 1 }
        let rs1 = traverse id nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
        let rs2 = sequence nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2
        let nem = NonEmptyMap.Create (("a", Some 1), ("b", Some 2), ("c", Some 3))
        let rs3 = traverse id nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs3
        let rs4 = sequence nem
        Assert.IsInstanceOf<option<NonEmptyMap<string, int>>> rs4
        let rs5 = traverse id (TestNonEmptyCollection.Create (Some 42))
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs5
        let nes = neseq { Some 1 }
        let rs6 = traverse id nes
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs6
        let rs7 = sequence nes
        Assert.IsInstanceOf<option<NonEmptySeq<int>>> rs7

    let toOptions x = if x <> 4 then Some x       else None
    let toChoices x = if x <> 4 then Choice1Of2 x else Choice2Of2 "This is a failure"
    let toLists   x = if x <> 4 then [x; x]       else []
    let toEithers x =
        if x > 4 then failwithf "Shouldn't be mapping for %i" x
        if x = 4 then Left ["This is a failure"] else Right x

    let expectedEffects =
        [
            """f(x) <*> Right 0"""
            """f(x) <*> Right 1"""
            """f(x) <*> Right 2"""
            """f(x) <*> Right 3"""
            """f(x) <*> Left ["This is a failure"]"""
        ]

    [<Test>]
    let traverseInfiniteApplicatives () =

        SideEffects.reset ()

        let a = sequence (Seq.initInfinite toOptions)
        let b = sequence (Seq.initInfinite toOptions)
        let c = sequence (Seq.initInfinite toChoices)
        let d = sequence (Seq.initInfinite toLists)
        let e = sequence (Seq.initInfinite toEithers)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions (Seq.initInfinite id)
        let _b = traverse toOptions (Seq.initInfinite id)
        let _c = traverse toChoices (Seq.initInfinite id)
        let _d = traverse toLists   (Seq.initInfinite id)
        let _e = traverse toEithers (Seq.initInfinite id)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<seq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.AreEqual ([], d)
        Assert.AreEqual (Either<string list,seq<int>>.Left ["This is a failure"], e)

        SideEffects.reset ()

        let a = sequence (NonEmptySeq.initInfinite toOptions)
        let b = sequence (NonEmptySeq.initInfinite toOptions)
        let c = sequence (NonEmptySeq.initInfinite toChoices)
        let d = sequence (NonEmptySeq.initInfinite toLists)
        let e = sequence (NonEmptySeq.initInfinite toEithers)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions (NonEmptySeq.initInfinite id)
        let _b = traverse toOptions (NonEmptySeq.initInfinite id)
        let _c = traverse toChoices (NonEmptySeq.initInfinite id)
        let _d = traverse toLists   (NonEmptySeq.initInfinite id)
        let _e = traverse toEithers (NonEmptySeq.initInfinite id)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<NonEmptySeq<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.AreEqual ([], d)
        Assert.AreEqual (Either<string list,NonEmptySeq<int>>.Left ["This is a failure"], e)
        

    let toEithersStrict x =
        if x = 4 then Left ["This is a failure"] else Right x

    [<Test>]
    let traverseFiniteApplicatives () =

        SideEffects.reset ()

        let a = sequence (Seq.initInfinite toOptions       |> Seq.take 20 |> Seq.toList)
        let b = sequence (Seq.initInfinite toOptions       |> Seq.take 20 |> Seq.toList)
        let c = sequence (Seq.initInfinite toChoices       |> Seq.take 20 |> Seq.toList)
        let d = sequence (Seq.initInfinite toLists         |> Seq.take 20 |> Seq.toList)
        let e = sequence (Seq.initInfinite toEithersStrict |> Seq.take 20 |> Seq.toList)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let f = sequence (Seq.initInfinite toEithersStrict |> Seq.take 20 |> Seq.toArray)

        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _a = traverse toOptions       [1..20]
        let _b = traverse toOptions       [1..20]
        let _c = traverse toChoices       [1..20]
        let _d = traverse toLists         [1..20]
        let _e = traverse toEithersStrict [1..20]

        CollectionAssert.AreNotEqual (expectedEffects, SideEffects.get ())
        SideEffects.reset ()

        let _f = traverse toEithersStrict [|1..20|]

        CollectionAssert.AreNotEqual (expectedEffects, SideEffects.get ())
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.AreEqual (Choice<list<int>,string>.Choice2Of2 "This is a failure", c)
        Assert.AreEqual ([], d)
        Assert.AreEqual (Either<string list,list<int>>.Left ["This is a failure"], e)
        Assert.AreEqual (Either<string list,array<int>>.Left ["This is a failure"], f)
        ()

    [<Test>]
    let traverseAsyncSequences =
        SideEffects.reset ()

        let doSomething v =
            SideEffects.add (sprintf "doSomething: %A" v)
            sprintf "some: %A" v
            |> async.Return
        
        seq [1..10] 
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        CollectionAssert.AreEqual (["doSomething: 1"], SideEffects.get ())

        SideEffects.reset ()
        NonEmptySeq.create 1 [2..10]
        |> traverse doSomething
        |> map  (head >> printfn "%A")
        |> Async.RunSynchronously
        CollectionAssert.AreEqual (["doSomething: 1"], SideEffects.get ())

    [<Test>]
    let traverseInfiniteAsyncSequences =
        let s = Seq.initInfinite async.Return
        let s' = sequence s
        let l = s' |> Async.RunSynchronously |> Seq.take 10 |> Seq.toList
        CollectionAssert.AreEqual ([0;1;2;3;4;5;6;7;8;9], l)

    [<Test>]
    let traverseTask () =
        let a = traverse Task.FromResult [1;2]
        CollectionAssert.AreEqual ([1;2], a.Result)
        Assert.IsInstanceOf<Option<list<int>>> (Some a.Result)
        let b = map Task.FromResult [1;2] |> sequence
        CollectionAssert.AreEqual ([1;2], b.Result)
        Assert.IsInstanceOf<Option<list<int>>> (Some b.Result)
        let c = traverse Task.FromResult [|1;2|]
        CollectionAssert.AreEqual ([|1;2|], c.Result)
        Assert.IsInstanceOf<Option<array<int>>> (Some c.Result)
        let d = map Task.FromResult [|1;2|] |> sequence
        CollectionAssert.AreEqual ([|1;2|], d.Result)
        Assert.IsInstanceOf<Option<array<int>>> (Some d.Result)

    [<Test>]
    let traverseMap () =
        let m = Map.ofList [("a", 1); ("b", 2); ("c", 3)]
        let r1 = traverse (fun i -> if i = 2 then None else Some i) m
        let r2 = traverse Some m
        Assert.AreEqual(None, r1)
        CollectionAssert.AreEqual (r2.Value, m)

        let m1 = Map.ofList [(1, [1;1;1]); (2, [2;2;2])]
        let r1 = m1 |> traversei (fun _ _ -> None)
        let r2 = m1 |> traversei (fun i v -> if List.forall ((=) i) v then Some (i :: v) else None)
        Assert.AreEqual(None, r1)
        CollectionAssert.AreEqual (Map.ofList [(1, [1;1;1;1]); (2, [2;2;2;2])], r2.Value)

        let expected = [Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)];
                        Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]; Map.ofList [(1, 1); (2, 2)]]
        let actual = sequence m1
        CollectionAssert.AreEqual (expected, actual)

    [<Test>]
    let traverseResults () =
        let a = sequence (if true then Ok [1] else Error "no")
        let b = traverse id (if true then Ok [1] else Error "no")
        let expected: Result<int, string> list = [Ok 1]
        CollectionAssert.AreEqual (expected, a)
        CollectionAssert.AreEqual (expected, b)


module Bitraversable =

    type Either<'left,'right> = Left of 'left | Right of 'right with
        static member        Bimap (x, f, g) = match x with Right a -> Right (f a) | Left e -> Left (g e)
        static member inline Bisequence x = match x with Right a -> map Either<'Left,'Right>.Right a | Left e -> map Either<'Left,'Right>.Left e

    let _ErrorBad: Result<int,string> list = bitraverse id id (Error ["Bad"])
    let _FailureBad: Validation<string, int> list = bitraverse id id (Failure ["Bad"])
    let _Some42x = bitraverse (Option.map string) Some (Some 42, 'x')
    let _LeftBad: Either<string, int> list = bitraverse id id (Left ["Bad"])  // works through Bisequence and Bimap

    type Either2<'left,'right> = Left of 'left | Right of 'right with
        static member inline Bitraverse (x, f, g) = match x with | Right a -> Either2<'Error2,'T2>.Right <!> g a | Left e -> Either2<'Error2,'T2>.Left <!> f e

    let _Right42: Either2<string, int> list = bisequence (Right [42])  // works through Bitraverse

    let c: Const<int list, string list> = Const [1]
    let d: Const<_, bool> = Const 2
    let e: Const<_, bool> = Const 3

    let _Const1 = bisequence c    
    let _Const2 = bitraverse List.singleton List.singleton d    
    let _Const3 = bitraverse NonEmptyList.singleton NonEmptyList.singleton e

    ()



type ZipList<'s> = ZipList of 's seq with
    static member Map    (ZipList x, f:'a->'b)               = ZipList (Seq.map f x)
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList<'b>
    
type ZipList'<'s> = ZipList' of 's seq with
    static member Return (x:'a)                                = ZipList' (Seq.initInfinite (konst x))
    static member (<*>) (ZipList' (f:seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) : ZipList'<'b>

module Applicative = 
    [<Test>]
    let applicativeMath () = 
        let inline (+) (a: 'T) (b: 'T) : 'T = a + b
        let inline ( .+  ) (x: 'Functor't)     (y: 't)             = map ((+)/> y) x : 'Functor't
        let inline (  +. ) (x: 't)             (y: 'Functor't)     = map ((+)   x) y : 'Functor't
        let inline ( .+. ) (x: 'Applicative't) (y: 'Applicative't) = (+) <!> x <*> y : 'Applicative't

        let testVal = [1;2] .+. [10;20] .+. [100;200] .+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOf<Option<list<int>>> (Some testVal)

        let testVal2 = NonEmptySeq.create 1 [2] .+. NonEmptySeq.create 10 [20] .+. NonEmptySeq.create 100 [200] .+ 2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], Seq.toList testVal2)
        Assert.IsInstanceOf<Option<NonEmptySeq<int>>> (Some testVal2)

        let testLTE1 = Some 1 .<=. Some 2
        Assert.AreEqual (Some true, testLTE1)
        Assert.IsInstanceOf<Option<bool>> testLTE1

        let testLTE2 = Some 1 .<= 2
        Assert.AreEqual (Some true, testLTE2)
        Assert.IsInstanceOf<Option<bool>> testLTE2
        
        let testLTE3 = 3 <=. Some 1
        Assert.AreEqual (Some false, testLTE3)
        Assert.IsInstanceOf<Option<bool>> testLTE3

        let testLTE4 = Some 3 .<=. Some 3
        Assert.AreEqual (Some true, testLTE4)
        Assert.IsInstanceOf<Option<bool>> testLTE4

        let testGTE1 = Some 1 .>=. Some 2
        Assert.AreEqual (Some false, testGTE1)
        Assert.IsInstanceOf<Option<bool>> testGTE1

        let testGTE2 = Some 1 .>= 2
        Assert.AreEqual (Some false, testGTE2)
        Assert.IsInstanceOf<Option<bool>> testGTE2
        
        let testGTE3 = 3 >=. Some 1
        Assert.AreEqual (Some true, testGTE3)
        Assert.IsInstanceOf<Option<bool>> testGTE3

        let testGTE4 = Some 3 .>=. Some 3
        Assert.AreEqual (Some true, testGTE4)
        Assert.IsInstanceOf<Option<bool>> testGTE4

    [<Test>]
    let applicatives () = 

        let run (ZipList x) = x
        let run' (ZipList' x) = x


        // Test Applicative (functions)
        let fn = 
            let mutable x = 0
            fun () -> 
                x <- x + 1
                x
        let res607 = map (+) ( (*) 100 ) 6 7
        let res606 = ( (+) <*>  (*) 100 ) 6
        let res508 = (map (+) ((+) 3 ) <*> (*) 100) 5
        let res123 = (map tuple3 fn <*> fn <*> fn) ()
        let res456 = (sequence [fn; fn; fn] : unit -> int list) ()

        // Test Applicative (ZipList)
        let res9n5  = map ((+) 1) (ZipList [8;4])
        let _20n30  = result (+) <*> result 10 <*> ZipList [10;20]
        let _18n14  = result (+) <*> ZipList [8;4] <*> result 10
        let res9n5' = map ((+) 1) (ZipList' [8;4])

        Assert.AreEqual (607, res607)
        Assert.AreEqual (606, res606)
        Assert.AreEqual (508, res508)
        Assert.AreEqual ((1,2,3), res123)
        Assert.AreEqual ([4;5;6], res456)
        Assert.AreEqual (toList (run res9n5), toList (run' res9n5'))

    let testLift2 () =
        let expectedEffects = ["Using WrappedSeqD's Return"; "Using WrappedSeqD's Apply"; "Using WrappedSeqD's Apply"]
        SideEffects.reset ()
        let _ = (WrappedSeqD [1] , WrappedSeqD [2]) ||> lift2 (+)
        CollectionAssert.AreEqual (expectedEffects, SideEffects.get ())

        let a1 = StateT <| fun x -> (async { return 1, "Here's the state " +  x})
        let b1 = StateT <| fun x -> (async { return 5, "Here's the other state " +  x})
        let r1 = lift2 (+) a1 b1
        Assert.AreEqual ((6, "Here's the other state Here's the state S"), (StateT.run r1 "S" |> Async.RunSynchronously))

        let a2 = WriterT <| (async { return 1, "Here's the state "})
        let b2 = WriterT <| (async { return 5, "Here's the other state "})
        let r2 = lift2 (+) a2 b2
        Assert.AreEqual ((6, "Here's the state Here's the other state "), (WriterT.run r2 |> Async.RunSynchronously))

        let a3 = ReaderT <| fun x -> (async { return "Here's the state " +  x})
        let b3 = ReaderT <| fun x -> (async { return "Here's the other state " +  x})
        let r3 = lift2 (+) a3 b3
        Assert.AreEqual ("Here's the state SHere's the other state S", (ReaderT.run r3 "S" |> Async.RunSynchronously))

// Idiom brackets from http://www.haskell.org/haskellwiki/Idiom_brackets
type Ii = Ii
type Ji = Ji
type J = J
type Idiomatic = Idiomatic with
    static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
    static member        ($) (Idiomatic, Ii) = id

module IdiomBrackets =
    [<Test>]
    let idiomBrackets () =    
        let inline idiomatic a b = (Idiomatic $ b) a
        let inline iI x = (idiomatic << result) x

        let res3n4''  = iI ((+) 2) [1;2] Ii
        let res3n4''' = iI (+) (result 2) [1;2] Ii   // fails to compile when constraints are not properly defined
        Assert.AreEqual ([3;4], res3n4'' )
        Assert.AreEqual ([3;4], res3n4''')


        let output = System.Text.StringBuilder ()
        let append (x: string) = output.Append x |> ignore

        let v5: Lazy<_> = lazy (append "5"; 5)
        Assert.AreEqual (0, output.Length)
        let fPlus10 x   = lazy (append " + 10"; x + 10)
        Assert.AreEqual (0, output.Length)
        let v5plus10    = v5 >>= fPlus10
        Assert.AreEqual (0, output.Length)
        let v15 = v5plus10.Force ()
        Assert.AreEqual ("5 + 10", string output)
        Assert.AreEqual (15, v15)

        output.Clear () |> ignore

        let v4ll: Lazy<_> = lazy (append "outer"; lazy (append "inner"; 4))
        Assert.AreEqual (0, output.Length)
        let v4l = join v4ll
        Assert.AreEqual (0, output.Length)
        let v4  = v4l.Force()
        Assert.AreEqual ("outerinner", string output)
        Assert.AreEqual (4, v4)
 

module Alternative =
    
    let testEmpty () =
        let _: WrappedListE<int> = empty
        let _: list<int>         = empty
        let _: WrappedListG<int> = empty
        let _: seq<int>          = empty
        
        // shoud not compile. 
        // Although WrappedListD implements IEnumerable, it should explicitely implement Empty. Not all IEnumerables have empty.
        // let (z: WrappedListD<int>) = empty
        ()

    let testAppend () =
        let _ = WrappedListE [1;2] <|> WrappedListE [3;4]
        let _ = [1;2] <|> [3;4]
        let _ = WrappedListG [1;2] <|> WrappedListG [3;4]
        let _ = seq [1;2] <|> seq [3;4]
        let _ = NonEmptySeq.create 1 [2] <|> NonEmptySeq.create 3 [4]

        // shoud not compile. 
        // Although WrappedListD implements IEnumerable, it should explicitely implement (<|>). Not all IEnumerables have (<|>).
        // let z = WrappedListD [1;2] ++ WrappedListD [3;4]
        ()

    [<Test>]
    let testEmptyAndAppendForCustomType () =
        let u = WrappedListE [1;2]
        let v = WrappedListG [1;2]
        let w = u <|> empty
        let x = empty <|> u
        let y = v <|> empty
        let z = empty <|> v
        Assert.AreEqual (u, w)
        Assert.AreEqual (u, x)
        Assert.AreEqual (v, y)
        Assert.AreEqual (v, z)

    [<Test>]
    let testOptionTAppliesFunctionOnce () =
        SideEffects.reset ()
        let x = OptionT <| async { SideEffects.add "hello"; return Some 1 }
        let y = OptionT <| async { SideEffects.add "good bye"; return Some 2 }

        let z = (x <|> y) |> OptionT.run |> Async.RunSynchronously

        Assert.AreEqual (["hello"], SideEffects.get ())
        Assert.AreEqual (Some 1, z)

    [<Test>]
    let testChoice () =
        let s = seq { 
            yield (SideEffects.add "a"; None)
            yield (SideEffects.add "b"; None)
            yield (SideEffects.add "c"; Some 'c')
            yield (SideEffects.add "d"; Some 'd')
            yield (SideEffects.add "e"; None)
            yield (SideEffects.add "f"; Some 'f')
            }

        let t = seq { 
            yield (SideEffects.add "a"; Error "a")
            yield (SideEffects.add "b"; Error "b")
            yield (SideEffects.add "c"; Ok 'c')
            yield (SideEffects.add "d"; Ok 'd')
            yield (SideEffects.add "e"; Error "e")
            yield (SideEffects.add "f"; Ok 'f')
            }


        let v = seq { 
            yield (SideEffects.add "a"; Failure ["a"])
            yield (SideEffects.add "b"; Failure ["b"])
            yield (SideEffects.add "c"; Success 'c')
            yield (SideEffects.add "d"; Success 'd')
            yield (SideEffects.add "e"; Failure ["e"])
            yield (SideEffects.add "f"; Success 'f')
            }

        let shortList, fullList = ["a"; "b"; "c"; "d"], ["a"; "b"; "c"; "d"; "e"; "f"]

        SideEffects.reset ()
        let _ = choice s                               // uses specific overload for seqs
        Assert.AreEqual (shortList, SideEffects.get ()) // short-circuits

        SideEffects.reset ()
        let _ = choice (toList s)                     // uses specific overload for lists
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to list forces all side-effects

        SideEffects.reset ()
        let _ = choice (toArray s)                    // uses specific overload for arrays
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to array forces all side-effects

        SideEffects.reset ()
        let _ = choice (NonEmptyList.ofList (toList s)) // uses Default1 (Choice defined on NonEmptyList)
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to list forces all side-effects

        SideEffects.reset ()
        let _ = choice (ofSeq s: Set<_>)              // use Default3: choice of an alternative
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects

        SideEffects.reset ()
        let _ = choice (NonEmptyList.ofList (toList t)) // uses Default1 (Choice defined on NonEmptyList)
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects

        SideEffects.reset ()
        let _ = choice (WrappedSeqE t)                // uses Default2
        Assert.AreEqual ("Using WrappedSeqE's ToSeq"::shortList, SideEffects.get ()) // short-circuits

        SideEffects.reset ()
        let _ = choice (toList v)                    // uses specific overload for lists
        Assert.AreEqual (fullList, SideEffects.get ()) // short-circuits but the conversion to set forces all side-effects


module MonadTransformers =
    [<Test>]
    let testCompileResultT () =
        // Test MonadError
        let _err1Layers = catch (Error "Invalid Value") (fun s -> Error ["the error was: " + s]) : Result<int, _>


        let someResultFunction foo = if foo = "foo" then Result.Ok foo else Result.Error "not good"

        let doSomeOperation x = ResultT <| async {
            if x < 10 then return Result.Ok 10
            else return Result.Error "failure" }

        let okFoo10Comp: ResultT<_> =
            monad {
                let! resFoo = ResultT.hoist <| someResultFunction "foo"
                let! res10  = doSomeOperation 0
                return (resFoo, res10) } 
            </catch/> (fun s -> throw ("The error was: " + s))

        let _okFoo10 = okFoo10Comp |> ResultT.run |> Async.RunSynchronously

        ()

    [<Test>]
    let testCompileChoiceT () =
        // Test MonadError
        let _err1Layers = catch (Choice2Of2 "Invalid Value") (fun s -> Choice2Of2 ["the error was: " + s]) : Choice<int, _>


        let someErrorFunction foo = if foo = "foo" then Choice1Of2 foo else Choice2Of2 "not good"

        let doSomeOperation x = ChoiceT <| async {
            if x < 10 then return Choice1Of2 10
            else return Choice2Of2 "failure"   }

        let okFoo10Comp: ChoiceT<_> =
            monad {
                let! resFoo = ChoiceT.hoist <| someErrorFunction "foo"
                let! res10  = doSomeOperation 0
                return (resFoo, res10) }
            </catch/> (fun s -> throw ("The error was: " + s))

        let _okFoo10 = okFoo10Comp |> ChoiceT.run |> Async.RunSynchronously

        // test generic put (no unknown(1,1): error FS0073: internal error: Undefined or unsolved type variable:  ^_?51242)
        let initialState = -1
        let _ = put initialState : ListT<State<int, unit list>>
        let _ = put initialState : ChoiceT<State<int, Choice<unit,string>>>

        ()

    [<Test>]
    let testStateT () =
        let lst1: StateT<string,_> = StateT.lift [1;2]
        let lst2: StateT<string,_> = StateT.lift [4;5]

        let m = monad { 
            let! x =  lst1
            let! y =  lst2
            do! modify String.toUpper
            let! st = gets String.length
            return (x, y +  st)
            }

        CollectionAssert.AreEqual ([((1, 6), "OK"); ((1, 7), "OK"); ((2, 6), "OK"); ((2, 7), "OK")], StateT.run m "ok")

        ()

    type RErrors = | NegativeValue
    [<Test>]
    let testCompilationMT1 () =

        let fn : ResultT<Reader<int,Result<_,RErrors>>> = 
            monad {
               let! x1 = lift ask
               let! x2 = 
                   if x1 > 0 then result 1
                   else ResultT (result (Error NegativeValue)) 
               return x1 + x2
            }

        let x = (fn |> ResultT.run |> Reader.run) 10
        areEqual (Ok 11) x
        let y = (fn |> ResultT.run |> Reader.run) -1
        areEqual (Error NegativeValue) y

module ProfunctorDefaults =
    type Fun<'T,'U> = Fun of ('T -> 'U) with
        static member Dimap ((Fun f): Fun<'B,'C>, g: 'A->'B, h:'C->'D) = Fun (g >> f >> h)

    let a = lmap id (Fun int)
    let b = rmap id (Fun float)
    let b' = map id (Fun float)
    ()

module BifunctorDefaults =
    type Tup<'a,'b> = Tup of ('a * 'b) with
        static member Bimap (Tup (a, b), f, g) = Tup (f a, g b)

    let a = first  string (Tup (1, '2'))
    let b = second string (Tup (1, '2'))
    let b' =  map  string (Tup (1, '2'))
    ()

module Invariant =

    type StringCodec<'t> = StringCodec of ReaderT<string, Result<'t,string>> * ('t -> Const<string, unit>) with
        static member Invmap (StringCodec (d, e), f: 'T -> 'U, g: 'U -> 'T) = StringCodec (map f d, contramap g e)
    module StringCodec =
        let decode (StringCodec (d,_)) x = ReaderT.run d x
        let encode (StringCodec (_,e)) x = Const.run (e x)

    [<Test>]
    let testStringToIntDerivedFromFloat() =
        let floatCodec = StringCodec (ReaderT (tryParse >> Option.toResultWith "Parse error"), string<float> >> Const)
        let floatParsed  = StringCodec.decode floatCodec "1.8"
        let floatEncoded = StringCodec.encode floatCodec 1.5
        Assert.AreEqual (Result<float, string>.Ok 1.8, floatParsed)
        Assert.AreEqual ("1.5", floatEncoded)

        let intCodec = invmap int<float> float<int> floatCodec
        let oneParsed  = StringCodec.decode intCodec "1"
        let tenEncoded = StringCodec.encode intCodec 10
        Assert.AreEqual (Result<int, string>.Ok 1, oneParsed)
        Assert.AreEqual ("10", tenEncoded)

module Categories =

    // Kleisli (slightly different definition)

    open FSharpPlus.Control

    type Kleisli<'t, '``monad<'u>``> = Kleisli of ('t -> '``monad<'u>``) with

        // Profunctor
        static member inline Dimap (Kleisli bmc :Kleisli<'B,'``Monad<'C>``>, ab:'A->'B, cd:'C->'D) = let cmd = map cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
        static member        Contramap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, k:'A->'B            ) = Kleisli (k >> f)       : Kleisli<'A,'``Monad<'C>``>
        static member inline Map (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, cd:'C->'D           ) = Kleisli (map cd << f) : Kleisli<'B,'``Monad<'D>``>
    
        // Category
        static member inline get_Id () = Kleisli result :Kleisli<'a,'b>
        static member inline (<<<) (Kleisli f, Kleisli g) = Kleisli (g >=> f)

        // Arrow
        static member inline Arr f = Kleisli ((<<) result f)
        static member inline First  (Kleisli f) = Kleisli (fun (b, d) -> f b >>= fun c -> result (c, d))
        static member inline Second (Kleisli f) = Kleisli (fun (d, b) -> f b >>= fun c -> result (d, c))
        static member inline (|||) (Kleisli f, Kleisli g) = Kleisli (FSharpPlus.Choice.either g f)

        static member inline (+++) (Kleisli (f:'T->'u), Kleisli (g:'v->'w)) =
            Fanin.InvokeOnInstance (Kleisli (f >=> ((<<) result Choice2Of2))) (Kleisli (g >=> ((<<) result Choice1Of2))) : Kleisli<Choice<'v,'T>,'z>

        static member inline Left (Kleisli f) =
            let inline (+++) a b = AcMerge.Invoke a b
            AcMerge.Invoke (Kleisli f) (Arr.Invoke (Id.Invoke ()))
        static member inline Right (Kleisli f) =
            let inline (+++) a b = AcMerge.Invoke a b
            (+++) (Arr.Invoke (Id.Invoke ())) (Kleisli f)
        static member get_App () = Kleisli (fun (Kleisli f, x) -> f x)
    
        // ArrowPlus
        static member inline Empty (output: Kleisli<'T,'``Monad<'U>``>, mthd: Empty) = Kleisli (fun _ -> Empty.Invoke ())
        static member inline ``<|>`` (Kleisli f, Kleisli g, mthd:Append) = Kleisli (fun x -> Append.Invoke (f x) (g x))

    let runKleisli (Kleisli f) = f
    let runFunc (f: System.Func<_,_>) = f.Invoke


    type MapTuple = MapTuple with
        static member inline (?<-) (MapTuple, f, (x,y))   = (Invoke.Invoke (f, x), Invoke.Invoke (f, y)) 
        static member inline (?<-) (MapTuple, f, (x,y,z)) = (Invoke.Invoke (f, x), Invoke.Invoke (f, y), Invoke.Invoke (f, z))
    let inline mapTuple f t = (?<-) MapTuple f t
    

    let testCompile () =

        // Arrows

        let inline id' () = FSharpPlus.Operators.getCatId ()
        let inline (<<<) f g = FSharpPlus.Operators.catComp f g
        let inline (>>>) f g = FSharpPlus.Operators.catComp g f
        let inline ( *** ) f g = FSharpPlus.Operators.( *** ) f g
        let inline ( &&& ) f g = FSharpPlus.Operators.fanout f g
        let inline (|||) f g = FSharpPlus.Operators.fanin f g
        let inline (+++) f g = FSharpPlus.Operators.(+++) f g
        let inline app () = FSharpPlus.Operators.getApp ()
        let inline zeroArrow () = FSharpPlus.Operators.getEmpty ()
        let inline (<+>) f g = FSharpPlus.Operators.(<|>) f g

        // Test Categories
        let r5: List<_> = (runKleisli (id' ())) 5
        let k = Kleisli (fun y -> [y; y * 2 ; y * 3]) <<< Kleisli (fun x -> [x + 3; x * 2])
        let _8n16n24n10n20n30 = runKleisli k  5

        let _1 = (System.Func<_,_> string >>> System.Func<_,_> int).Invoke '1'



        let _tupInt5nInt5  = mapTuple (      List.max           >>>      List.min           ) ([[7;5;8]; [4;5;3]], [   [7;5;8]   ;    [4;5;3]]   )
        let _tupInt5nChar5 = mapTuple (Unchecked.defaultof<Max> >>> Unchecked.defaultof<Min>) ([[7;5;8]; [4;5;3]], [['7';'5';'8']; ['4';'5';'3']])


        // Test Arrows
        let _20n5n30n5   = runKleisli (arrFirst  <| Kleisli (fun y -> [y * 2; y * 3])) (10,5) 
        let _10n10n10n15 = runKleisli (arrSecond <| Kleisli (fun y -> [y * 2; y * 3])) (10,5)

        let _Str6 =          arr (fun x -> string (x * 2 ))  3
        let _Str8 = runFunc (arr (fun x -> string (x * 2 ))) 4
        let _Some2n4n6:option<_> = runKleisli (arr (fun y -> [y; y * 2 ; y * 3])) 2

        let _500n19 = ( (*) 100) *** ((+) 9)  <| (5,10)
        let _500n14 = ( (*) 100) &&& ((+) 9)  <| 5
        let _10x13n10x20n15x13n15x20: list<_> = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) *** Kleisli (fun x -> [x + 3; x *  2] )) (5,10)
        let _10x8n10x10n15x8n15x10  : list<_> = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) &&& Kleisli (fun x -> [x + 3; x *  2] )) 5

        // Test Arrow Choice
        let _Left7       = ( (+) 2) +++ ( (*) 10)   <| Choice2Of2 5
        let _7n50        = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) ||| Kleisli (fun x -> [x + 2; x * 10] )) (Choice1Of2 5)
        let _Left5n10n15 = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) +++ Kleisli (fun x -> [x + 3; x *  2] )) (Choice2Of2 5)

        // Test Arrow Apply
        let _7      = app () ( (+) 3 , 4)
        let _4n8n12 = runKleisli (app ()) (Kleisli (fun y -> [y; y * 2 ; y * 3]) , 4)

        // Test Arrow Plus
        let resSomeX = Kleisli (fun x -> Some x)
        let _SomeXPlusZero: option<_> = runKleisli (resSomeX <+> zeroArrow ()) 10

        ()

module NumericLiteralG =
    
    let inline FromZero () = Zero.Invoke ()
    let inline FromOne  () = One.Invoke  ()
    let inline FromInt32  (i: int   ) = FromInt32.Invoke i
    let inline FromInt64  (i: int64 ) = FromInt64.Invoke i
    let inline FromString (i: string) = fromBigInt <| System.Numerics.BigInteger.Parse i

open MathNet.Numerics

module Numerics = 
    [<Test>]
    let genericMath () = 
        let argUint        : uint32      =              42G
        let argInt         :    int      =         -424242G
        let argBigInt      : bigint      = -42424242424242G
        let argFloat       : float       = -(42G + (42G/100G))  // -42.42
        let argFloat32     : float32     = -(42G + (42G/100G))  // -42.4199982f
        let argDecimal     : decimal     = -(42G + (42G/100G))
        let argComplex                   = Complex.mkRect (-42.42, 24.24)
        let argComplex32                 = Complex32.mkRect (-42.42f, 24.24f)
        let argBigRational : BigRational = -42424242424242G / 42424G

        let _01 = signum' argUint
        let _02 = signum' argInt
        let _03 = signum' argBigInt
        let _04 = signum' argFloat
        let _05 = signum' argFloat32
        let _06 = signum' argDecimal
        let _07 = signum' argComplex
        let _08 = signum' argComplex32
        let r09 = signum' argBigRational

        let _11 = abs' argUint
        let _12 = abs' argInt
        let _13 = abs' argBigInt
        let _14 = abs' argFloat
        let _15 = abs' argFloat32
        let _16 = abs' argDecimal
        let _17 = abs' argComplex
        let _18 = abs' argComplex32
        let r19 = abs' argBigRational

        let (_20: int * char * int * char * int * char * int * char * int * char) = maxValue

        Assert.AreEqual (argBigRational, r09 * r19)



type Sum<'a> = Sum of 'a with
    static member inline get_Zero () = Sum 0G
    static member inline (+) (Sum (x:'n), Sum (y:'n)) = Sum (x + y)


module Splits = 
    [<Test>]
    let splitArraysAndStrings () = 
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((toList a1 = toList a2))
        Assert.IsTrue((toList b1 = toList b2))
        Assert.IsInstanceOf<Option<string []>> (Some a1)

    [<Test>]
    let replaceArraysAndStrings () = 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B |> System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B |> System.Text.Encoding.ASCII.GetString

        Assert.IsTrue ((a1 = a2))
        Assert.IsTrue ((b1 = b2))

    [<Test>]
    let intercalateArraysAndStrings () = 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B |> System.Text.Encoding.ASCII.GetString

        let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        let _c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        let d  = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)
        let _e = intercalate 10 (seq [1; 2; 3])

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        Assert.IsTrue((d = Sum 13))


module Parsing =
    let (|Int32|_|) : _-> Int32 option = tryParse
    type ProductId = { Value:int }
    with
        static member TryParse(value:string) : ProductId option=
            match value.Split('_') |> List.ofArray with
            | "P" :: Int32 v :: [] -> Some { Value = v }
            | _ -> None

    [<Test>]
    let parseDateTime () =
#if MONO
        let v1 : DateTime = parse "2011-03-04T15:42:19+03:00"
        Assert.IsTrue((v1 = DateTime(2011,3,4,12,42,19)))
#else
        Assert.Ignore ("Depends on how it's executed...")
#endif

    [<Test>]
    let parse () = 
        let v2 : DateTimeOffset = parse "2011-03-04T15:42:19+03:00"

        Assert.IsTrue((v2 = DateTimeOffset(2011,3,4,15,42,19, TimeSpan.FromHours 3.)))

        let _101 = tryParse "10.1.0.1" : Net.IPAddress option
        let _102 = tryParse "102" : string option
        let _MTS = [tryParse "Monday" ; Some DayOfWeek.Thursday; Some DayOfWeek.Saturday]
        let _103 = tryParse "103" : Text.StringBuilder option

        let _109 = parse "10.0.9.1" : Net.IPAddress
        let _111 = parse "true" && true
        let _MTF = [parse "Monday" ; DayOfWeek.Thursday; DayOfWeek.Friday]
        let _110 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.
        let _120 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;|]                + 100
        let _121 = parse "121" : string
        let _122 = parse "122" : Text.StringBuilder
        
        let r66: float option = tryParse "66.0"
        areStEqual r66 (Some 66.0)

        let r123: WrappedListA<int> option = tryParse "[1;2;3]"
        areStEqual r123 (Some (WrappedListA [1; 2; 3]))

    [<Test>]
    let parseCustomType () = 
        let v1 : CustomerId option = tryParse "C_1"
        Assert.IsTrue((v1.Value.Value = 1L))
        let v2 : CustomerId option = tryParse "C_X"
        Assert.IsTrue(Option.isNone v2)
        let v3 : ProductId option = tryParse "P_1"
        Assert.IsTrue((v3.Value.Value = 1))
        let v4 : ProductId option = tryParse "P_X"
        Assert.IsTrue(Option.isNone v4)
#if NETSTANDARD3_0
        let v5 : ICustomerId option = tryParse "C_1"
        Assert.IsTrue((v5.Value.Value = 1L))
        let v6 : ICustomerId option = tryParse "C_X"
        Assert.IsTrue(Option.isNone v6)
#endif

    [<Test>]
    let scanfParsing () =
        let _ccx: int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int = parseArray [|"34"; "24"; "34"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"|]
        
        let _t = sscanf "(%i-%i-%f-%i-%i-%i-%i-%i-%i)" "(32-66-888-4-5-6-7-8-9)"
        let (_a,_b) = sscanf "(%%%s,%M)" "(%hello, 4.53)"
        let (_x,_y,_z) = sscanf "%s-%s-%s" "test-this-string"
        let (_j,_k,_l,_m,_n,_o,_p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"
        
        let (_r1,_r2,_r3,_r4,_r5,_r6,_r7,_r8)          = sscanf "%f %F %g %G %e %E %c %c"    "1 2.1 3.4 .3 43.2e32 0 f f"
        let (_s1,_s2,_s3,_s4,_s5,_s6,_s7,_s8,_s9)      = sscanf "%f %F %g %G %e %E %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f"
        let (_t1,_t2,_t3,_t4,_t5,_t6,_t7,_t8,_t9,_t10) = sscanf "%f %F %g %G %e %E %c %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f f"
        let (_u1,_u2,_u3,_u4,_u5,_u6,_u7,_u8,_u9,_u10,_u11,_u12,_u13,_u14,_u15)           = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c"       "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f"
        let (_v1,_v2,_v3,_v4,_v5,_v6,_v7,_v8,_v9,_v10,_v11,_v12,_v13,_v14,_v15,_v16)      = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i"    "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16"
        let (_w1,_w2,_w3,_w4,_w5,_w6,_w7,_w8,_w9,_w10,_w11,_w12,_w13,_w14,_w15,_w16,_w17) = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i %f" "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16 17"
        
        
        let _zzz = sscanf "(%%%s)" "(%hello)"
        let (_x1,_y1,_z1) = sscanf "%s--%s-%s" "test--this-string"
        
        
        let _f1 = trySscanf "(%%%s)" "(%hello)"
        let _f2 = trySscanf "%s--%s-%s" "test--this-gg"
        let _f3 = trySscanf "%f %F %g %G %e %E %c %c"    "1 2.1 3.4 .3 43.2e32 0 f f"
        let _f4 = trySscanf "%f %F %g %G %e %E %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f"
        let _f5 = trySscanf "%f %F %g %G %e %E %c %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f f"
        let _f6 = trySscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c"       "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f"
        let _f7 = trySscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i"    "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16"
        let _f8 = trySscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i %f" "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16 17"
        
        let _date: (DayOfWeek * string * uint16 * int) option = trySscanf "%A %A %A %A" "Saturday March 25 1989"
        
        let x = trySscanf "%X %x" "13 43"
        let o = trySscanf "%o" "10"
        
        areEqual (Some 8) x
        areEqual (Some (19, 67)) o


module Conversions =
    let test =
        // Generic op_Explicit
        let _302: float  = explicit 302
        let _303: float  = explicit "303"
        let _304: char   = explicit "F"
        let _214: unativeint = explicit 2147483648I
        let inline _f () : 't = if true then explicit 42M else explicit 42.0f
        ()

module BitConverter =
    [<Test>]
    let roundtrips () =
        let a0 = -79800210978L
        let b0 = -798021978
        let c0 = -79872.8
        let d0 = -79872.7969f
        let e0 = 43u
        let f0 = -45s
        let g0 = "this is just a String"
        let h0 = true
        let i0 = 'h'

        Assert.IsTrue ((a0 = (a0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((b0 = (b0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((c0 = (c0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((d0 = (d0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((e0 = (e0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((f0 = (f0 |> toBytes |> ofBytes)))
        Assert.IsTrue (("74-68-69-73-20-69-73-20-6A-75-73-74-20-61-20-53-74-72-69-6E-67" = (g0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((h0 = (h0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((i0 = (i0 |> toBytes |> ofBytes)))

module Sequences =

    let test =
        let stack = new Collections.Generic.Stack<_> ([1;2;3])

        let _twoSeqs = plus (seq [1;2;3]) (seq [4;5;6])
        let _sameSeq = plus (getZero () ) (seq [4;5;6])

        let _seqFromLst: _ seq = ofList [1;2;3;4]
        let _seqFromLst' = toSeq [1;2;3;4]
        let _seqFromOpt  = toSeq (Some 1)

        let _singletonList: _ list = result 1
        let _singletonSeq : _ seq  = result 1

        // This stopped compiling, but actually it's the right thing, before was returning a seq<_>
        (*
        let mappedstack = map string stack
        *)
        
        let _stackGroup  = groupBy ((%)/> 2) stack

        let _r03 = filter ((=) 3) stack

        // Test Seq Monad
                        
        let _rseq =
            monad {
                let! x1 = seq [1;2]
                let! x2 = seq [10;20]
                return ((+) x1 x2) }


        // Test Seq Comonad

        let lst    = seq [1;2;3;4;5]
        let _elem1 = head       lst
        let _tails = duplicate  lst
        let _lst  = extend head lst

        // Test MonadPlus
        let getLine    = async { return System.Console.ReadLine () }
        let putStrLn x = async { printfn "%s" x}

        let inline sequence ms =
            let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
            Array.foldBack k (Seq.toArray ms) ((result :seq<'a> -> 'M) (Seq.empty))

        let inline mapM f as' = sequence (Seq.map f as')

        let _nameAndAddress = mapM (fun x -> putStrLn x >>= fun _ -> getLine) (seq ["name";"address"])

        let _pythags = monad.plus {
          let! z = seq [1..50]
          let! x = seq [1..z]
          let! y = seq [x..z]
          if (x*x + y*y = z*z) then return (x, y, z) else () }

        let _ = monad.plus {
          let! z = seq [1..50]
          for x in seq [1..z]  do
          for y in seq [x..z]  do
          where (x*x + y*y = z*z)
          yield (x, y, z)}

        let _res123123 = (seq [1;2;3]) <|> (seq [1;2;3])
        let _allCombinations = sequence (seq [seq ['a';'b';'c']; seq ['1';'2']])
        ()



module ShouldNotCompile =
    let stack = new Collections.Generic.Stack<_> ([1;2;3])

    // This should not compile. TODO find out how to test that it keeps failing
    (*
    let twoStacks = plus stack stack
    let twoSeqs'  = plus (seq [1;2;3]) [4;5;6]
    let twoSeqs'' = plus [1;2;3] (seq [4;5;6])
    let (stackFromLst:_ Collections.Generic.Stack) = ofList [1;2;3;4]
    let tails' = duplicate stack
    let stk'  = extend head stack
    *)

    // This should not compile, it started failing, at least at f80ad7e6, now unfortunately it compiles again, todo find out since when
    let sortedStack = sortBy  string    stack

    // This should not compile ??? (but it does)
    let r10' = foldBack (+) stack 0
    let r123 = toList stack

    // This should not compile (but it does)
    let resNone'' = sequence (new Collections.Generic.Stack<_>([Some 3; None; Some 1]))

    // this compiles but it requires a type annotation, but
    // thanks to https://github.com/Microsoft/visualfsharp/pull/4170
    // in F# 4.1 versions released as of 2018 it won't be required
    let pythags = monad {
      let! z = seq [1..50]
      let! x = seq [1..z]
      let! y = seq [x..z]
      do! (guard (x*x + y*y = z*z) : _ seq)
      return (x, y, z)}




module ApplicativeInference =

    // test applicative from monad
    let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline ap     x y     = liftM2 id x y
    let res2n4n8 = result pown </ap/> result 2. <*> [1;2;3]

    let res9n5   = map ((+) 1) (ZipList(seq [8;4]))
    let res18n24 = result (+) <*> ZipList(seq [8;4]) <*> ZipList(seq [10;20])

    open FSharpPlus.Math.Generic

    let res6n7n8 = result (+) <*> result 5G <*> ZipList [1;2;3]
    let res18n14 = result (+) <*> ZipList(seq [8;4]) <*> result 10

    open FSharpPlus.GenericBuilders // for applicative brackets

    let res3n4''  = iI ((+) 2) [1;2] Ii
    let res3n4''' = iI (+) (result 2) [1;2] Ii                              // *1
    let res18n24' = iI (+) (ZipList(seq [8;4])) (ZipList(seq [10;20])) Ii
    // let res6n7n8' = iI (+) (result 5G      ) (ZipList [1;2;3]     ) Ii   // *1, *2
    let res18n14' = iI (+) (ZipList(seq [8;4])) (result 10           ) Ii

    let safeDiv x y = if y = 0 then None else Some (x </div/> y)
    let resSome3    = join (iI safeDiv (Some 6) (Some 2) Ii)
    let resSome3'   =       iI safeDiv (Some 6) (Some 2) Ji

    let safeDivBy y = if y = 0 then None else Some (fun x -> x </div/> y)
    let resSome2  = join (result safeDivBy  <*> Some 4G) <*> Some 8G
    let resSome2' = join (   iI safeDivBy (Some 4G) Ii) <*> Some 8G

    let resSome2'' = iI safeDivBy (Some 4G) J (Some 8G) Ii
    let resNone = iI safeDivBy (Some 0G) J (Some 8G) Ii
    let res16n17   = iI (+) (iI (+) (result 4) [2;3] Ii ) (result 10: _ list) Ii   // *1

    // *1 These lines fails when Apply.Invoke has no 'or ^'``Applicative<'U>`` ' (output) constraint.
    // *2 F# 4.1 regression



module Curry =

    [<Test>]
    let curryTest () =
        let f1  (x: Tuple<_>) = [x.Item1]
        let f2  (x, y)    = [x + y]
        let f3  (x, y, z) = [x + y + z]
        let f7  (t1, t2, t3, t4, t5, t6, t7) = [t1+t2+t3+t4+t5+t6+t7]
        let f8  (t1, t2, t3, t4, t5, t6, t7: float, t8: char) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8]
        let f9  (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9]
        let f15 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15]
        let f16 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15, t16) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16]
        let f17 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15, t16, t17) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16+t17]

        let _x1  = curryN f1 100
        let _x2  = curryN f2 1 2
        let _x3  = curryN f3 1 2 3
        let _x7  = curryN f7 1 2 3 4 5 6 7
        let _x8  = curryN f8 1 2 3 4 5 6 7. '8'
        let _x9  = curryN f9 1 2 3 4 5 6 7. '8' 9M
        let _x15 = curryN f15 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15
        let _x16 = curryN f16 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15 16
        let _x17 = curryN f17 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15 16 17

        Assert.Pass ()

    [<Test>]
    let uncurryTest () =
        let g2  x y   = [x + y]
        let g3  x y z = [x + y + z]
        let g7  a b c d e f g = [a + b + c + d + e + f + g]
        let g8  t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8]
        let g9  t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal)  = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9]
        let g12 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12]
        let g15 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 t13 t14 t15 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15]
        let g16 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 t13 t14 t15 t16 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16]

        let _y1  = uncurryN string (Tuple<_> 1)
        let _y2  = uncurryN g2 (1, 2)
        let _y3  = uncurryN g3 (1, 2, 3)
        let _y7  = uncurryN g7 (1, 2, 3, 4, 5, 6, 7)
        let _y8  = uncurryN g8 (1, 2, 3, 4, 5, 6, 7. , '8')
        let _y9  = uncurryN g9 (1, 2, 3, 4, 5, 6, 7. , '8', 9M)
        let _y12 = uncurryN g12 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12)
        let _y15 = uncurryN g15 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12, 13, 14, 15)
        let _y16 = uncurryN g16 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12, 13, 14, 15, 16)

        Assert.Pass ()



module Memoization =

    [<Test>]
    let memoization () =
        let effs = ResizeArray ()

        let f x                       = printfn "calculating"; effs.Add "f"; string x
        let g x (y:string) z : uint32 = printfn "calculating"; effs.Add "g"; uint32 (x * int y + int z)
        let h x y z                   = printfn "calculating"; effs.Add "h"; new System.DateTime (x, y, z)
        let sum2 (a:int)       = printfn "calculating"; effs.Add "sum2"; (+) a
        let sum3 a (b:int) c   = printfn "calculating"; effs.Add "sum3"; a + b + c
        let sum4 a b c d : int = printfn "calculating"; effs.Add "sum4"; a + b + c + d

        // memoize them
        let msum2 = memoizeN sum2
        let msum3 = memoizeN sum3
        let msum4 = memoizeN sum4
        let mf    = memoizeN f
        let mg    = memoizeN g
        let mh    = memoizeN h

        // check memoization really happens
        let _v1  = msum2 1 1
        let _v2  = msum2 1 1
        let _v3  = msum2 2 1
        let _v4  = msum3 1 2 3
        let _v5  = msum3 1 2 3
        let _v6  = msum4 3 1 2 3
        let _v7  = msum4 3 1 2 3
        let _v8  = msum4 3 5 2 3
        let _v9  = mf 3M
        let _v10 = mf 3M
        let _v11 = mg 4 "2" 3M
        let _v12 = mg 4 "2" 3M
        let _v13 = mh 2010 1 1
        let _v14 = mh 2010 1 1

        Assert.AreEqual ([|"sum2"; "sum2"; "sum3"; "sum4"; "sum4"; "f"; "g"; "h"|], effs.ToArray ())


    [<Test>]
    let memoizeAcceptsNullArgument () =
        let f x y = ""
        let mf = memoizeN f
        let _ = mf null null  // should not throw
        ()



// Old code, no longer used but still interesting to see if it still compiles

module Ratio =
    
    // Strict version of math operators
    let inline internal ( +.) (a: 'Num) (b: 'Num) : 'Num = a + b
    let inline internal ( -.) (a: 'Num) (b: 'Num) : 'Num = a - b
    let inline internal ( *.) (a: 'Num) (b: 'Num) : 'Num = a * b

    let inline whenIntegral a = let _ = if false then toBigInt a else 0I in ()

    let inline internal gcd x y : 'Integral =
        let zero = getZero ()
        let rec loop a b =
            if b = zero then a
            else loop b (a % b)
        if (x, y) = (zero, zero) then failwith "gcd 0 0 is undefined"
        else loop (Abs.Invoke x) (Abs.Invoke y)

    type Ratio<'Integral> =
        struct
            val Numerator   : 'Integral
            val Denominator : 'Integral
            new (numerator: 'Integral, denominator: 'Integral) = {Numerator = numerator; Denominator = denominator}
        end
        override this.ToString () = this.Numerator.ToString () + " % " + this.Denominator.ToString ()

    let inline internal ratio (a: 'Integral) (b: 'Integral) : Ratio<'Integral> =
        whenIntegral a
        let zero = getZero ()
        if b = zero then failwith "Ratio.%: zero denominator"
        let (a, b) = if b < zero then (-a, -b) else (a, b)
        let gcd = gcd a b
        Ratio (a / gcd, b / gcd)

    let inline internal Ratio (x, y) = x </ratio/> y

    let inline internal numerator   (r: Ratio<_>) = r.Numerator
    let inline internal denominator (r: Ratio<_>) = r.Denominator

    type Ratio<'Integral> with
        static member inline (/) (a: Ratio<_>, b: Ratio<_>) = (a.Numerator *. b.Denominator) </ratio/> (a.Denominator *. b.Numerator)                                              
        static member inline (+) (a: Ratio<_>, b: Ratio<_>) = (a.Numerator *. b.Denominator +. b.Numerator *. a.Denominator) </ratio/> (a.Denominator *. b.Denominator)
        static member inline (-) (a: Ratio<_>, b: Ratio<_>) = (a.Numerator *. b.Denominator -. b.Numerator *. a.Denominator) </ratio/> (a.Denominator *. b.Denominator)
        static member inline (*) (a: Ratio<_>, b: Ratio<_>) = (a.Numerator *. b.Numerator) </ratio/> (a.Denominator *. b.Denominator)

        static member inline Abs        (r: Ratio<_>) = (Abs.Invoke    (numerator r)) </ratio/> (denominator r)
        static member inline Signum     (r: Ratio<_>) = (Signum.Invoke (numerator r)) </ratio/> (One.Invoke ())
        static member inline FromBigInt (x: bigint) = FromBigInt.Invoke x </ratio/> (One.Invoke ())
        static member inline (~-)       (r: Ratio<_>) = -(numerator r) </ratio/> (denominator r)


    let (|Ratio|) (ratio:Ratio<_>) = (ratio.Numerator, ratio.Denominator)

type Rational = Ratio.Ratio<bigint>

module testCompileOldCode =
    open Ratio

    open FSharpPlus.Math.Generic

    let inline negate (x:'Num) :'Num = FSharpPlus.Operators.negate x
    let inline (~-)   (x:'Num) :'Num = FSharpPlus.Operators.negate x


    let inline div (a:'Integral) b :'Integral =
        whenIntegral a
        let (a,b) = if b < 0G then (-a,-b) else (a,b)
        (if a < 0G then (a - b + 1G) else a) / b

    let inline quot (a: 'Integral) (b: 'Integral) : 'Integral = whenIntegral a; a / b
    let inline rem  (a: 'Integral) (b: 'Integral) : 'Integral = whenIntegral a; a % b
    let inline quotRem a b : 'Integral * 'Integral = whenIntegral a; FSharpPlus.Operators.divRem a b
    let inline mod'   a b : 'Integral = whenIntegral a; ((a % b) + b) % b  
    let inline divMod D d : 'Integral * 'Integral =
        let q, r = quotRem D d
        if (r < 0G) then
            if (d > 0G) then (q - 1G, r + d)
            else             (q + 1G, r - d)
        else (q, r)

    
    // let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> = a </ratio/> b
    // let inline fromRational (x:Rational) :'Fractional = FSharpPlus.Operators.fromRational x
    // let inline whenFractional a = let _ = if false then fromRational (1I % 1I) else a in ()
    let inline (/) (a:'Fractional) (b:'Fractional) :'Fractional = (* whenFractional a;*) a / b
    let inline recip x :'Fractional = 1G / x

    // Exp functions
    let inline ( **^ ) (x: 'Num) (n: 'Integral)  = 
        whenIntegral n
        let rec f a b n = if n == 0G then a else f (b * a) b (n - 1G)
        if (n < 0G) then failwith "Negative exponent" else f 1G x n
    let inline ( **^^ ) (x:'Fractional) (n:'Integral) = if n >= 0G then x**^n else recip (x**^(negate n))

module Choosei =
    [<Test>]
    let choosei () =
        let someIfIndexEven i x =
            if i % 2 = 0 then Some x
            else None
        let l = (choosei someIfIndexEven [1;2;3;4;5]) 
        areEqual [1;3;5] l
        Assert.IsInstanceOf<Option<Microsoft.FSharp.Collections.List<int>>> (Some l)
        
        let a = (choosei someIfIndexEven [|1;2;3;4;5|]) 
        areEqual [|1;3;5|] a
        Assert.IsInstanceOf<Option<int[]>> (Some a)
        
        let s =(choosei someIfIndexEven (seq [1;2;3;4;5])) 
        areEqual (seq [1;3;5]) s
        Assert.IsInstanceOf<Option<Microsoft.FSharp.Collections.seq<int>>> (Some s)
        
        let m = (choosei someIfIndexEven (Map [1,2;2,3;3,4])) 
        areEqual (Map [2,3]) m
        Assert.IsInstanceOf<Option<Microsoft.FSharp.Collections.Map<int, int>>> (Some m)
        
        // correct overload:
        SideEffects.reset ()
        areEquivalent [1;3;5] (ChooseIndexed.InvokeOnInstance someIfIndexEven (WrappedListD [1..5]))
        areEqual ["Using WrappedListD's ChooseIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        areEquivalent [1;3;5] (choosei someIfIndexEven (WrappedListD [1..5]))
        areEqual ["Using WrappedListD's ChooseIndexed"] (SideEffects.get ())
        
        SideEffects.reset ()
        (ChooseIndexed.InvokeOnInstance someIfIndexEven (WrappedSeqD [1..5])) |> ignore
        areEqual ["Using WrappedSeqD's ChooseIndexed"] (SideEffects.get ())
        SideEffects.reset ()
        (choosei someIfIndexEven (WrappedSeqD [1..5])) |> ignore
        areEqual ["Using WrappedSeqD's ChooseIndexed"] (SideEffects.get ())

module lift3 = 
    [<Test>]
    let NonEmptySeqLift3 () =
        // NonEmptySeq
        NonEmptySeq.lift3 (fun x y z -> x + y + z) (NonEmptySeq.ofList[1;2]) (NonEmptySeq.ofList[7;11]) (NonEmptySeq.ofList[22;33]) 
        |> areEqual (NonEmptySeq.ofList[30; 41; 34; 45; 31; 42; 35; 46])
    
    [<Test>]
    let CorrectOverload () =
        let sumOfThree x y z = x + y + z
        
        // correct overload:
        SideEffects.reset ()
        areEquivalent [3] (Lift3.InvokeOnInstance sumOfThree (WrappedListD [1]) (WrappedListD [1]) (WrappedListD [1]))
        areEqual ["Using WrappedListD's Lift3"] (SideEffects.get ())
        
        SideEffects.reset ()
        (Lift3.InvokeOnInstance sumOfThree (WrappedSeqD [1]) (WrappedSeqD [1]) (WrappedSeqD [1])) |> ignore
        areEqual ["Using WrappedSeqD's Lift3"] (SideEffects.get ())
        
