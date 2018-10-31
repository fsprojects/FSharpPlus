namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset () = effects.Clear ()
    let add x = effects.Add (x)
    let get () = effects |> Seq.toList

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
    static member Delay (f: unit -> WrappedListD<_>) = SideEffects.add "Using WrappedListG's Delay"; f ()
    static member Using (resource, body)             = SideEffects.add "Using WrappedListG's Using"; using resource body


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
    static member (<*>)  (WrappedSeqD f, WrappedSeqD x) = SideEffects.add "Using WrappedSeqD's Return"; WrappedSeqD (f <*> x)
    static member ToList (WrappedSeqD x) = Seq.toList x

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
        static member inline (+) (x:ZipList<'a>, y:ZipList<'a>) = liftA2 plus x y :ZipList<'a>
        static member ToSeq    (ZipList lst)     = lst

    type ZipList'<'s> = ZipList' of 's seq with
        static member Return (x: 'a)                                = ZipList' (Seq.initInfinite (konst x))
        static member Map   (ZipList' x, f: 'a->'b)                 = ZipList' (Seq.map f x)
        static member (<*>) (ZipList' (f: seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) : ZipList'<'b>
        static member inline get_Zero () = result zero              : ZipList'<'a>
        static member inline (+) (x: ZipList'<'a>, y: ZipList'<'a>) = liftA2 plus x y :ZipList'<'a>
        static member inline Sum (x: seq<ZipList'<'a>>) = SideEffects.add "Using optimized Sum"; List.foldBack plus (Seq.toList x) zero : ZipList'<'a>
        static member ToSeq    (ZipList' lst)     = lst

    type MyList<'t> = MyList of list<'t> with
        static member get_Empty () = MyList []
        static member (<|>) (MyList x, MyList y) = MyList (x @ y)

    type MyNum = MyNum of int with
        static member get_Empty () = MyNum 0
        static member FromInt32 x = MyNum x


    let testCompile =
        let res1n2 = MyList [1] ++ MyList [2] ++ zero
        let res0 : MyNum = zero 

        let asQuotation = plus    <@ ResizeArray (["1"]) @> <@ ResizeArray (["2;3"]) @>
        let quot123     = plus    <@ ResizeArray ([1])   @> <@ ResizeArray ([2;3])   @>
        let quot1       = plus    <@ ResizeArray ([1])   @>      (zero)
        let quot23      = plus       (zero)         <@ ResizeArray ([2;3])   @>
        let quot13      = plus       (zero)         <@ ("1","3") @>
        let lzy1 = plus (lazy [1]) (lazy [2;3])
        let lzy2 = plus (zero) lzy1
        let asy1 = plus (async.Return [1]) (async.Return [2;3])
        let asy2 = plus (zero) asy1
        let bigNestedTuple1 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (2, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (3, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20))
        let bigNestedTuple2 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (zero, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ zero

        let mapA = Map.empty 
                    |> Map.add 1 (async.Return "Hey")
                    |> Map.add 2 (async.Return "Hello")

        let mapB = Map.empty 
                    |> Map.add 3 (async.Return " You")
                    |> Map.add 2 (async.Return " World")

        let mapAB = plus mapA mapB
        let greeting1 = Async.RunSynchronously mapAB.[2]
        let greeting2 = Async.RunSynchronously (Seq.sum [mapA; zero; mapB]).[2]

        let dicA = new Dictionary<string,Task<string>> ()
        dicA.["keya"] <- (result "Hey"  : Task<_>)
        dicA.["keyb"] <- (result "Hello": Task<_>)

        let dicB = new Dictionary<string,Task<string>> ()
        dicB.["keyc"] <- (result " You"  : Task<_>)
        dicB.["keyb"] <- (result " World": Task<_>)

        let dicAB = plus dicA dicB
        let iDicAb   = plus (dicA :> IDictionary<_,_>)         (dicB :> IDictionary<_,_>)
        let iroDicAb = plus (dicA :> IReadOnlyDictionary<_,_>) (dicB :> IReadOnlyDictionary<_,_>)

        let greeting3 = extract dicAB.["keyb"]
        let greeting4 = extract (Seq.sum [dicA; zero; dicB]).["keyb"]

        let res2   = Seq.sum [ async {return Endo ((+) 2)} ; async {return Endo ((*) 10)} ; async {return Endo id } ;  async {return Endo ((%) 3)} ; async {return zero } ] |> Async.RunSynchronously |> Endo.run <| 3
        let res330 = Seq.sum [ async {return (fun (x:int) -> string x)} ; async {return (fun (x:int) -> string (x*10))} ; async {return zero } ] </Async.RunSynchronously/>  3
        ()


    [<Test>]
    let seqSumDefaultCustom () =
        let (WrappedListB x) = Seq.sum [WrappedListB [10] ;WrappedListB [15]]
        let (WrappedListC y) = Seq.sum [WrappedListC [10] ;WrappedListC [15]]
        Assert.AreEqual (x, [10;15])
        Assert.AreEqual (y, [10])

        let x = [ ("a", 1); ("b", 2); ("a", 3) ]
        let y = x |> map (Seq.singleton >> (ofSeq : seq<_*_> -> Dictionary<_,_>) >> map List.singleton) |> Seq.sum
        let z = x |> map (Seq.singleton >>             dict                      >> map List.singleton) |> Seq.sum
        Assert.IsInstanceOf<Option< Dictionary<string,int list>>> (Some y)
        Assert.IsInstanceOf<Option<IDictionary<string,int list>>> (Some z)

        SideEffects.reset ()

        let quotLst123  = plus zero (ZipList [ [1];[2];[3] ])

        Assert.AreEqual (quotLst123 |> toList, [[1]; [2]; [3]])
        Assert.AreEqual (SideEffects.get (), [])

        let quotLst123' = Seq.sum [zero; zero; ZipList' [ [1];[2];[3] ]]

        Assert.AreEqual (quotLst123' |> toList, [[1]; [2]; [3]])
        Assert.AreEqual (SideEffects.get (), ["Using optimized Sum"])

        let wl = WrappedListB  [2..10]

        let arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
        let listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
        let seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

        let arrayGroupAdj   = chunkBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]

        ()






module Functor =
    [<Test>]
    let mapDefaultCustom () = 

        SideEffects.reset ()

        // NonEmptyList<_> has Map but at the same time is a seq<_>
        let testVal1 = map ((+) 1) {Head = 10; Tail = [20;30]}
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal1)

        let testVal2 = map ((+) 1) ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some testVal2)

        let testVal3 = map ((+) 1) (dict (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some testVal3)

        // WrappedSeqD is Applicative. Applicatives are Functors => map should work
        Assert.AreEqual (SideEffects.get (), [])
        let testVal4 = map ((+) 1) (WrappedSeqD [1..3])
        Assert.IsInstanceOf<Option<WrappedSeqD<int>>> (Some testVal4)
        Assert.AreEqual (SideEffects.get (), ["Using WrappedSeqD's Return"; "Using WrappedSeqD's Return"])
        SideEffects.reset ()
        
        // WrappedListE is a Monad. Monads are Functors => map should work
        let testVal5 = map ((+) 1) (WrappedListE [1..3])
        Assert.IsInstanceOf<Option<WrappedListE<int>>> (Some testVal5)

        // Same with WrappedListD but WrappedListD is also IEnumerable<_>
        Assert.AreEqual (SideEffects.get (), [])
        let testVal6 = map ((+) 1) (WrappedListD [1..3])
        Assert.IsInstanceOf<Option<WrappedListD<int>>> (Some testVal6)
        Assert.AreEqual (SideEffects.get (), ["Using WrappedListD's Bind"; "Using WrappedListD's Return"; "Using WrappedListD's Return"; "Using WrappedListD's Return"])

    [<Test>]
    let unzip () = 
        let testVal = unzip {Head = (1, 'a'); Tail = [(2, 'b');(3, 'b')]}
        Assert.IsInstanceOf<Option<NonEmptyList<int> * NonEmptyList<char>>> (Some testVal)

    [<Test>]
    let zipTest () =

        SideEffects.reset ()
        let a = zip (seq [1;2;3]) (seq [1. .. 3. ])
        Assert.AreEqual (SideEffects.get (), [])

        let b = zip (WrappedListD [1;2;3]) (WrappedListD [1. .. 3. ])
        Assert.AreEqual (SideEffects.get (), ["Using WrappedListD's zip"])

        let c = zip (dict [1,'1' ; 2,'2' ; 4,'4']) (dict [1,'1' ; 2,'2' ; 3,'3'])
        let d = zip [ 1;2;3 ] [ 1. .. 3. ]
        let e = zip [|1;2;3|] [|1. .. 3.|]
        let g = zip ((seq [1;2;3]).GetEnumerator ()) ((seq [1. .. 3. ]).GetEnumerator ())
        let h = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let i = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let j = zip (async {return 1}) (async {return '2'})

        let fa a = zip a (seq [1. .. 3. ])
        let fb a = zip a (WrappedListD [1. .. 3. ])
        let fc a = zip a (dict [1,'1' ; 2,'2' ; 3,'3'])
        let fd a = zip a [ 1. .. 3. ]
        let fe a = zip a [|1. .. 3.|]
        let fg a = zip a ((seq [1. .. 3. ]).GetEnumerator ())
        let fh a = zip a (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let fi a = zip a (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let fj a = zip a (async {return '2'})

        let ga b = zip (seq [1;2;3]) b
        let gb b = zip (WrappedListD [1;2;3]) b
        let gc b = zip (dict [1,'1' ; 2,'2' ; 4,'4']) b
        let gd b = zip  [ 1;2;3 ] b
        let ge b = zip  [|1;2;3|] b
        let gg b = zip ((seq [1;2;3]).GetEnumerator ()) b
        let gh b = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) b
        let gi b = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) b
        let gj b = zip (async {return 1}) b

        let ha : _ -> _ -> _ seq            = zip
        let hb : _ -> _ -> _ WrappedListD   = zip
        let hc : _ -> _ -> IDictionary<_,_> = zip
        let hd : _ -> _ -> _ list           = zip
        let he : _ -> _ -> _ []             = zip
        let hg : _ -> _ -> _ IEnumerator    = zip
        let hh : _ -> _ -> Map<_,_>         = zip
        let hi : _ -> _ -> Dictionary<_,_>  = zip
        let hj : _ -> _ -> Async<_>         = zip

        ()


module Collections =

    open System.Collections
    open System.Collections.Concurrent
    open System.Collections.Generic

    let testCollections =
        let bigSeq = seq {1..10000000}
        let bigLst = [ 1..10000000 ]
        let bigArr = [|1..10000000|]
        let bigMut = ResizeArray(seq {1..10000000})

        let x = head bigSeq
        let y = head bigLst
        let z = head bigArr

        let a = skip 1000 bigSeq
        let b = skip 1000 bigLst
        let c = skip 1000 bigArr
        let d = skip 1000 bigMut
        let e = "hello world" |> skip 6 |> toList
        let h = ofList ['h';'e';'l';'l';'o';' '] + "world"
        let j = item 2 "hello"
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
        let sl2:Generic.SortedList<_,_>   = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let mp2:Map<_,_>                  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let d : Generic.IDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let d2: Generic.IDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let r : IReadOnlyDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let r2: IReadOnlyDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let ut: Hashtable                 = ofSeq (seq [1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofSeq (seq ["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofSeq (seq [4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofSeq {'1'..'3'}                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let cd2:ConcurrentDictionary<_,_> = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofSeq {'1'..'3'}

        // now go back
        let sk'  = toSeq sk
        let sg'  = toSeq sg
        let sb'  = toSeq sb
        let sq1' = toSeq sq1
        let sq2' = toSeq sq2
        let sq3' = toSeq sq3
        let sq4' = toSeq sq4
        let ls1' = toSeq ls1
        let ls2' = toSeq ls2
        let st1' = toSeq st1
        let st2' = toSeq st2
        let ss'  = toSeq ss 
        let ra'  = toSeq ra 
        let sl'  = toSeq sl 
        let dc'  = toSeq dc 
        let mp'  = toSeq mp 
        let d'   = toSeq d  
        let r'   = toSeq r
        let ut'  = toSeq ut 
        let al'  = toSeq al 
        let us'  = toSeq us 
        let cc'  = toSeq cc 
        let cd'  = toSeq cd 
        let cb'  = toSeq cb 

        // there are some 'one-way' collections that can only be converted toSeq

        let columns = 
            let d = new Data.DataTable () 
            [|new Data.DataColumn "id";new Data.DataColumn "column1";new Data.DataColumn "column2"|] |> d.Columns.AddRange
            d.Columns
        let col1 = columns |> find (fun x -> x.ColumnName = "column1")
        let cols = columns |> toList |> map  (fun x -> x.ColumnName)

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
        let sl2:Generic.SortedList<_,_>   = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofList ([(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let mp2:Map<_,_>                  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let d : Generic.IDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let d2: Generic.IDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let r : IReadOnlyDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let r2: IReadOnlyDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let ut: Hashtable                 = ofList ([1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofList (["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofList ([4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofList ['1'..'3']                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let cd2:ConcurrentDictionary<_,_> = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofList ['1'..'3']

        // now go back
        let sk'  = toList sk
        let sg'  = toList sg
        let sb'  = toList sb
        let sq1' = toList sq1
        let sq2' = toList sq2
        let sq3' = toList sq3
        let sq4' = toList sq4
        let ls1' = toList ls1
        let ls2' = toList ls2
        let st1' = toList st1
        let st2' = toList st2
        let ss'  = toList ss
        let ra'  = toList ra
        let sl'  = toList sl
        let dc'  = toList dc
        let mp'  = toList mp
        let d'   = toList d
        let r'   = toList r
        let ut'  = toList ut
        let al'  = toList al
        let us'  = toList us
        let cc'  = toList cc
        let cd'  = toList cd
        let cb'  = toList cb

        ()

    let testSorts =
        let r1 = [4..1] |> sort
        let r2 = [4..1] |> sortBy string
        let r3 = seq [4..1] |> sort
        let r4 = seq [4..1] |> sortBy string
        let r5 = ResizeArray [4..1] |> sort
        let r6 = ResizeArray [4..1] |> sortBy string

        ()

    let testGeneralizableValues () =
        let a:list<_> = empty
        let b =  0 ::a
        let c = '0'::a

        let d:WrappedSeqA<_> = empty
        let e = WrappedSeqA [ 0 ] <|> d
        let f = WrappedSeqA ['0'] <|> d
         
        ()
    [<Test>]
    let readOnlyNth () =
        let readOnlyCollection = ReadOnlyCollection( [|1..10|] )
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

module Foldable =

    let foldables =
        let r10  = foldBack (+) (seq [1;2;3;4]) 0
        let r323 = toList (seq [3;2;3])
        let r03  = filter ((=) 3) (seq [1;2;3])
        ()

    [<Test>]
    let foldMapDefaultCustom () =
        SideEffects.reset ()
        let x = foldMap ((+) 10) (WrappedListD [1..4]) //= 50 w side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get (), ["Using optimized foldMap"])

        SideEffects.reset ()
        let y = foldMap ((+) 10) {1..4}  //= 50 w/o side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get (), [])

    [<Test>]
    let filterDefaultCustom () = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (testVal, WrappedListA [2])
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

        let twos   = filter ((=) (box 2)) (([1;2;3;4;3;2;1;2;3] |> ofSeq) : Collections.ArrayList)
        let five   = filter ((=) 5) (WrappedListB' [1;2;3;4;5;6])   // <- Uses the default method for filter.
        let optionFilter = filter ((=) 3) (Some 4)

        ()

    [<Test>]
    let foldAternatives () = 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        Assert.AreEqual (x, Some 3)
        Assert.AreEqual (y, [3;4])

    [<Test>]
    let fromToSeq () =
        let s = (seq [Collections.Generic.KeyValuePair(1, "One"); Collections.Generic.KeyValuePair(2, "Two")])
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
        Assert.AreEqual (l', [4;6;10;89])
        Assert.AreEqual (s', WrappedListB [4;6;10;89])

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
        let a = exists ((=) 2) [1..3]
        let b = exists ((=) '2') (System.Text.StringBuilder "abc")
        let c = exists ((=) 2) (WrappedListA [1..3])
        let d = exists ((=) 2) (WrappedListD [1..3])
        areEqual (SideEffects.get ()) ["Using WrappedListA's Exists"; "Using WrappedListD's Exists"]
        ()

    [<Test>]
    let pick () =
        SideEffects.reset ()
        let a = pick Some [1..3]
        let b = pick Some (System.Text.StringBuilder "abc")
        let c = pick Some (WrappedListA [1..3])
        let d = pick Some (WrappedListD [1..3])
        areEqual (SideEffects.get ()) ["Using WrappedListA's Pick"; "Using WrappedListD's Pick"]
        ()

    [<Test>]
    let minimum () =
        SideEffects.reset ()
        let a = minimum [1..3]
        let b = minimum (System.Text.StringBuilder "abc")
        let c = minimum (WrappedListA [1..3])
        let d = minimum (WrappedListD [1..3])
        areEqual (SideEffects.get ()) ["Using WrappedListA's Min"; "Using WrappedListD's Min"]
        ()

    [<Test>]
    let maxBy () =
        SideEffects.reset ()
        let a = maxBy id [1..3]
        let b = maxBy id (System.Text.StringBuilder "abc")
        let c = maxBy id (WrappedListA [1..3])
        let d = maxBy id (WrappedListD [1..3])
        areEqual (SideEffects.get ()) ["Using WrappedListA's MaxBy"; "Using WrappedListD's MaxBy"]
        ()

    [<Test>]
    let length () =
        SideEffects.reset ()
        let a = length [1..3]
        let b = length (System.Text.StringBuilder "abc")
        let c = length (WrappedListA [1..3])
        let d = length (WrappedListD [1..3])
        areEqual (SideEffects.get ()) ["Using WrappedListA's Length"; "Using WrappedListD's Length"]
        ()


module Indexable = 
    [<Test>]
    let testCompileAndExecuteItem () =

        let a = Map.ofSeq [1, "one"; 2, "two"]
        let a1 = item 1 a

        let b = Map.ofSeq [1, "one"; 2, "two"] :> IDictionary<_,_>
        let b1 = item 1 b

        let c = "two"
        let c1 = item 1 c

        let d = System.Text.StringBuilder "one"
        let d1 = item 1 d

        let e = array2D [[1;2];[3;4];[5;6]]
        let e1 = item (1, 1) e

        let f = [1, "one"; 2, "two"]
        let f1 = item 1 f

        let g = [|1, "one"; 2, "two"|]
        let g1 = item 1 g

        let h = ResizeArray [1, "one"; 2, "two"]
        let h1 = item 1 h

        let i = Array3D.create 3 2 2 0
        let i1 = item (1, 1, 1) i

        let j = Array4D.create 3 2 2 3 0
        let j1 = item (1, 1, 1, 1) j

        // This doesn't intetionally compile: seq is not Indexable. Not all foldables are Indexable, for example a Set is foldable but not Indexable. For seq use nth instead.
        // let f = seq [1, "one"; 2, "two"]
        // let f1 = item 1 f

        ()

    [<Test>]
    let testCompileAndExecuteTryItem () =

        let a = Map.ofSeq [1, "one"; 2, "two"]
        let a1 = tryItem 1 a

        let b = Map.ofSeq [1, "one"; 2, "two"] :> IDictionary<_,_>
        let b1 = tryItem 1 b

        let c = "two"
        let c1 = tryItem 1 c

        let d = System.Text.StringBuilder "one"
        let d1 = tryItem 1 d

        let e = array2D [[1;2];[3;4];[5;6]]
        let e1 = tryItem (1, 1) e


        let f = [1, "one"; 2, "two"]
        let f1 = tryItem 1 f

        let g = [|1, "one"; 2, "two"|]
        let g1 = tryItem 1 g

        let h = ResizeArray [1, "one"; 2, "two"]
        let h1 = tryItem 1 h

        let i = Array3D.create 3 2 2 0
        let i1 = tryItem (1, 1, 1) i

        let j = Array4D.create 3 2 2 3 0
        let j1 = tryItem (1, 1, 1, 1) j

        let w = WrappedListA [1, "one"; 2, "two"]
        let w1 = tryItem 1 w

        ()

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

module Monad = 
    [<Test>]
    let joinDefaultCustom () = 
        let x = join [[1];[2]]
        Assert.AreEqual (x, [1;2])
        let y : WrappedListE<_> = join (WrappedListE [WrappedListE [1];WrappedListE [2]])
        Assert.AreEqual (y, WrappedListE [1;2])
        SideEffects.reset ()
        let z = join (WrappedListF [WrappedListF [1];WrappedListF [2]])
        Assert.AreEqual (z, WrappedListF [1;2])
        Assert.AreEqual (SideEffects.get (), ["Join"])

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
        let q = [1..100000] |> map ((+) 1)
        Assert.Pass ()
#endif


module Traversable = 
    let traverseTest =
        let resNone = sequence (seq [Some 3;None ;Some 1])
        ()

    [<Test>]
    let sequence_Default_Primitive () = 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    [<Test>]
    let sequence_Specialization () =
        
        let inline seqSeq (x:_ seq ) = sequence x
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x

        let a : list<_> = seqSeq (seq [[1];[3]])
        Assert.AreEqual ([seq [1; 3]], a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        Assert.AreEqual ([[|1; 3|]], b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        Assert.AreEqual ([[1; 3]], c)
        Assert.IsInstanceOf<list<list<int>>> c

    [<Test>]
    let traversableForNonPrimitive () =
        let nel = NonEmptyList.create (Some 1) [Some 2]
        let rs1  = traverse id nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
        let rs2  = sequence nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2

    [<Test>]
    let traverseInfiniteOptions () =
        let toOptions x = if x <> 4 then Some x       else None
        let toChoices x = if x <> 4 then Choice1Of2 x else Choice2Of2 "This is a failure"
        let toLists   x = if x <> 4 then [x; x]       else []
        let a = traverse toOptions (Seq.initInfinite id)
        let b = sequence  (Seq.initInfinite toOptions)
        let c = sequence  (Seq.initInfinite toChoices)
        let d = sequence  (Seq.initInfinite toLists)
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.True ((Choice2Of2 "This is a failure" = c))
        Assert.AreEqual ([], d)
        let resNone   = traverse (fun x -> if x > 4 then Some x else None) (Seq.initInfinite id) // optimized method, otherwise it doesn't end
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


    [<Test>]
    let applicatives () = 

        let run (ZipList x) = x
        let run' (ZipList' x) = x

        // Test Applicative (functions)
        let res607 = map (+) ( (*) 100 ) 6 7
        let res606 = ( (+) <*>  (*) 100 ) 6
        let res508 = (map (+) ((+) 3 ) <*> (*) 100) 5

        // Test Applicative (ZipList)
        let res9n5   = map ((+) 1) (ZipList [8;4])
        let res20n30 = result (+) <*> result 10 <*> ZipList [10;20]
        let res18n14 = result (+) <*> ZipList [8;4] <*> result 10
        let res9n5'  = map ((+) 1) (ZipList' [8;4])

        Assert.AreEqual (607, res607)
        Assert.AreEqual (606, res606)
        Assert.AreEqual (508, res508)
        Assert.AreEqual (toList (run res9n5), toList (run' res9n5'))


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
        let (v: WrappedListE<int>) = empty
        let (w: list<int>)         = empty
        let (x: WrappedListG<int>) = empty
        let (y: seq<int>)          = empty
        
        // shoud not compile. 
        // Although WrappedListD implements IEnumerable, it should explicitely implement Empty. Not all IEnumerables have empty.
        // let (z: WrappedListD<int>) = empty
        ()

    let testAppend () =
        let v = WrappedListE [1;2] <|> WrappedListE [3;4]
        let w = [1;2] <|> [3;4]
        let x = WrappedListG [1;2] <|> WrappedListG [3;4]
        let y = seq [1;2] <|> seq [3;4]

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

        Assert.AreEqual (SideEffects.get (), ["hello"])
        Assert.AreEqual (z, Some 1)


module MonadTransformers =
    let testCompileResultT () =
        // Test MonadError
        let err1Layers   = catch (Error "Invalid Value") (fun s -> Error ["the error was: " + s]) : Result<int, _>


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

        let okFoo10 = okFoo10Comp |> ResultT.run |> Async.RunSynchronously

        ()
    let testCompileChoiceT () =
        // Test MonadError
        let err1Layers   = catch (Choice2Of2 "Invalid Value") (fun s -> Choice2Of2 ["the error was: " + s]) : Choice<int, _>


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

        let okFoo10 = okFoo10Comp |> ChoiceT.run |> Async.RunSynchronously

        ()

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
        Assert.AreEqual (floatParsed, Result<float, string>.Ok 1.8)
        Assert.AreEqual (floatEncoded, "1.5")

        let intCodec = invmap int<float> float<int> floatCodec
        let oneParsed  = StringCodec.decode intCodec "1"
        let tenEncoded = StringCodec.encode intCodec 10
        Assert.AreEqual (oneParsed, Result<int, string>.Ok 1)
        Assert.AreEqual (tenEncoded, "10")

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
        let r8n16n24n10n20n30 = runKleisli k  5

        let res1 = (System.Func<_,_> string >>> System.Func<_,_> int).Invoke '1'



        let tupInt5nInt5  = mapTuple (      List.max           >>>      List.min           ) ([[7;5;8]; [4;5;3]], [   [7;5;8]   ;    [4;5;3]]   )
        let tupInt5nChar5 = mapTuple (Unchecked.defaultof<Max> >>> Unchecked.defaultof<Min>) ([[7;5;8]; [4;5;3]], [['7';'5';'8']; ['4';'5';'3']])


        // Test Arrows
        let r20n5n30n5   = runKleisli (arrFirst  <| Kleisli (fun y -> [y * 2; y * 3])) (10,5) 
        let r10n10n10n15 = runKleisli (arrSecond <| Kleisli (fun y -> [y * 2; y * 3])) (10,5)

        let resStr6 =          arr (fun x -> string (x * 2 ))  3
        let resStr8 = runFunc (arr (fun x -> string (x * 2 ))) 4
        let resSome2n4n6:option<_> = runKleisli (arr (fun y -> [y; y * 2 ; y * 3])) 2

        let res500n19 = ( (*) 100) *** ((+) 9)  <| (5,10)
        let res500n14 = ( (*) 100) &&& ((+) 9)  <| 5
        let (res10x13n10x20n15x13n15x20:list<_>) = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) *** Kleisli (fun x -> [x + 3; x *  2] )) (5,10)
        let (res10x8n10x10n15x8n15x10  :list<_>) = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) &&& Kleisli (fun x -> [x + 3; x *  2] )) 5

        // Test Arrow Choice
        let resLeft7       = ( (+) 2) +++ ( (*) 10)   <| Choice2Of2 5
        let res7n50        = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) ||| Kleisli (fun x -> [x + 2; x * 10] )) (Choice1Of2 5)
        let resLeft5n10n15 = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) +++ Kleisli (fun x -> [x + 3; x *  2] )) (Choice2Of2 5)

        // Test Arrow Apply
        let res7      = app () ( (+) 3 , 4)
        let res4n8n12 = runKleisli (app ()) (Kleisli (fun y -> [y; y * 2 ; y * 3]) , 4)

        // Test Arrow Plus
        let resSomeX = Kleisli (fun x -> Some x)
        let (resSomeXPlusZero: option<_>) = runKleisli (resSomeX <+> zeroArrow ()) 10

        ()

module NumericLiteralG =
    open FSharpPlus.Control
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

        let res01 = signum' argUint
        let res02 = signum' argInt
        let res03 = signum' argBigInt
        let res04 = signum' argFloat
        let res05 = signum' argFloat32
        let res06 = signum' argDecimal
        let res07 = signum' argComplex
        let res08 = signum' argComplex32
        let res09 = signum' argBigRational

        let res11 = abs' argUint
        let res12 = abs' argInt
        let res13 = abs' argBigInt
        let res14 = abs' argFloat
        let res15 = abs' argFloat32
        let res16 = abs' argDecimal
        let res17 = abs' argComplex
        let res18 = abs' argComplex32
        let res19 = abs' argBigRational

        let (res20: int * char * int * char * int * char * int * char * int * char) = maxValue

        Assert.AreEqual (res09 * res19, argBigRational)



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

        // Fails to compile but works in F#4.1
        // let c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        // 

        let d = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        Assert.IsTrue((d = Sum 13))


module Parsing = 
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

        let r101 = tryParse "10.1.0.1" : Net.IPAddress option
        let r102 = tryParse "102" : string option
        let rMTS = [tryParse "Monday" ; Some DayOfWeek.Thursday; Some DayOfWeek.Saturday]
        let r103 = tryParse "103" : Text.StringBuilder option

        let r109 = parse "10.0.9.1" : Net.IPAddress
        let r111 = parse "true" && true
        let rMTF = [parse "Monday" ; DayOfWeek.Thursday; DayOfWeek.Friday]
        let r110 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.
        let r120 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;|]                + 100
        let r121 = parse "121" : string
        let r122 = parse "122" : Text.StringBuilder
        
        let (r66: float option) = tryParse "66.0"
        Assert.IsTrue ((r66 = Some 66.0))

        let (r123: WrappedListA<int> option) = tryParse "[1;2;3]"
        Assert.IsTrue ((r123 = Some (WrappedListA [1; 2; 3])))


module Conversions =
    let test =
        // Generic op_Explicit
        let r302:float  = explicit 302
        let r303:float  = explicit "303"
        let r304:char   = explicit "F"
        let a : unativeint = explicit 2147483648I
        let inline h () : 't = if true then explicit 42M else explicit 42.0f
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
    open FSharpPlus.Builders
    let test =
        let stack = new Collections.Generic.Stack<_> ([1;2;3])

        let twoSeqs = plus (seq [1;2;3]) (seq [4;5;6])
        let sameSeq = plus (getZero () ) (seq [4;5;6])

        let seqFromLst: _ seq = ofList [1;2;3;4]
        let seqFromLst' = toSeq [1;2;3;4]
        let seqFromOpt  = toSeq (Some 1)

        let singletonList: _ list = result 1
        let singletonSeq : _ seq  = result 1

        // This stopped compiling, but actually it's the right thing, before was returning a seq<_>
        (*
        let mappedstack = map string stack
        *)
        
        let stackGroup  = groupBy ((%)/> 2) stack

        let r03' = filter ((=) 3) stack

        // Test Seq Monad
                        
        let rseq =
            monad {
                let! x1 = seq [1;2]
                let! x2 = seq [10;20]
                return ((+) x1 x2) }


        // Test Seq Comonad

        let lst   = seq [1;2;3;4;5]
        let elem1 = head        lst
        let tails = duplicate   lst
        let lst'  = extend head lst

        // Test MonadPlus
        let getLine    = async { return System.Console.ReadLine () }
        let putStrLn x = async { printfn "%s" x}

        let inline sequence ms =
            let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
            Array.foldBack k (Seq.toArray ms) ((result :seq<'a> -> 'M) (Seq.empty))

        let inline mapM f as' = sequence (Seq.map f as')

        let nameAndAddress = mapM (fun x -> putStrLn x >>= fun _ -> getLine) (seq ["name";"address"])

        let pythags' = monad.plus {
          let! z = seq [1..50]
          let! x = seq [1..z]
          let! y = seq [x..z]
          if (x*x + y*y = z*z) then return (x, y, z)}

        let pythags'' = monad.plus {
          let! z = seq [1..50]
          for x in seq [1..z]  do
          for y in seq [x..z]  do
          where (x*x + y*y = z*z)
          yield (x, y, z)}

        let res123123 = (seq [1;2;3]) <|> (seq [1;2;3])
        let allCombinations = sequence (seq [seq ['a';'b';'c']; seq ['1';'2']])
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

    open FSharpPlus.Builders

    let res3n4''  = iI ((+) 2) [1;2] Ii
    let res3n4''' = iI (+) (result 2) [1;2] Ii                               // *1
    let res18n24' = iI (+) (ZipList(seq [8;4])) (ZipList(seq [10;20])) Ii
    // let res6n7n8' = iI (+) (result 5G          ) (ZipList [1;2;3]     ) Ii   // *1, *2
    let res18n14' = iI (+) (ZipList(seq [8;4])) (result 10            ) Ii

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
        let msum2 = memoize sum2
        let msum3 = memoize sum3
        let msum4 = memoize sum4
        let mf    = memoize f
        let mg    = memoize g
        let mh    = memoize h

        // check memoization really happens
        let v1  = msum2 1 1
        let v2  = msum2 1 1
        let v3  = msum2 2 1
        let v4  = msum3 1 2 3
        let v5  = msum3 1 2 3
        let v6  = msum4 3 1 2 3
        let v7  = msum4 3 1 2 3
        let v8  = msum4 3 5 2 3
        let v9  = mf 3M
        let v10 = mf 3M
        let v11 = mg 4 "2" 3M
        let v12 = mg 4 "2" 3M
        let v13 = mh 2010 1 1
        let v14 = mh 2010 1 1

        Assert.AreEqual(effs.ToArray(), [|"sum2"; "sum2"; "sum3"; "sum4"; "sum4"; "f"; "g"; "h"|])



// Old code, no longer used but still interesting to see if it still compiles

module Ratio =
    open FSharpPlus.Control
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
