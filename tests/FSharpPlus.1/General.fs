module FSharpPlus.One.General


open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework

module Helpers =
    let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset() = effects.Clear()
    let add x = effects.Add(x)
    let get() = effects |> Seq.toList

type WrappedListA<'s> = WrappedListA of 's list with
    static member ToSeq    (WrappedListA lst) = List.toSeq lst
    static member OfSeq  lst = WrappedListA (Seq.toList lst)
    static member TryItem (i, WrappedListA x) = List.tryItem i x

type WrappedListB<'s> = WrappedListB of 's list with
    static member Return   (x) = WrappedListB [x]
    static member (+)  (WrappedListB l, WrappedListB x) = WrappedListB (l @ x)
    static member Zero   = WrappedListB List.empty
    static member ToSeq    (WrappedListB lst)     = List.toSeq lst
    static member FoldBack (WrappedListB x, f, z) = List.foldBack f x z

type WrappedListB'<'s> = WrappedListB' of 's list with // Same as B but without clean signatures
    static member Return   (_:WrappedListB'<'a>, _:Return ) = fun (x:'a)     -> WrappedListB' [x]
    static member (+)      (WrappedListB' l, WrappedListB' x) = WrappedListB' (l @ x)
    static member Zero     (_:WrappedListB'<'a>, _:Zero) = WrappedListB' List.empty
    static member ToSeq    (WrappedListB' lst)     = List.toSeq lst
    static member FoldBack (WrappedListB' x, f, z) = List.foldBack f x z

type WrappedListC<'s> = WrappedListC of 's list with
    static member (+)  (WrappedListC l, WrappedListC x) = WrappedListC (l @ x)
    static member Zero   = WrappedListC List.empty
    static member Sum  (lst: seq<WrappedListC<_>>)  = Seq.head lst

type WrappedListD<'s> = WrappedListD of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = SideEffects.add "Using WrappedListD's Return"; WrappedListD [x]
    static member (>>=) ((WrappedListD x):WrappedListD<'T>, f) = SideEffects.add "Using WrappedListD's Bind"; WrappedListD (List.collect (f >> (fun (WrappedListD x) -> x)) x)
    static member inline FoldMap (WrappedListD x, f) =
        SideEffects.add "Using optimized foldMap"
        Seq.fold (fun x y -> x ++ (f y)) zero x
    static member Zip (WrappedListD x, WrappedListD y) = SideEffects.add "Using WrappedListD's zip"; WrappedListD (List.zip x y)

type WrappedListE<'s> = WrappedListE of 's list with
    static member Return  (x) = WrappedListE [x]
    static member (>>=)  (WrappedListE x: WrappedListE<'T>, f) = WrappedListE (List.collect (f >> (fun (WrappedListE x) -> x)) x)
    static member get_Empty() = WrappedListE List.empty
    static member (<|>) (WrappedListE l, WrappedListE x) = WrappedListE (l @ x)
    
type WrappedListF<'s> = WrappedListF of 's list with
    static member Return  (x) = WrappedListF [x]
    static member (>>=)  (WrappedListF x: WrappedListF<'T>, f) = WrappedListF (List.collect (f >> (fun (WrappedListF x) -> x)) x)
    static member Join  (WrappedListF wlst) = SideEffects.add "Join";  WrappedListF wlst >>= id
    static member get_Empty() = WrappedListF List.empty
    static member (<|>) (WrappedListF l, WrappedListF x) = WrappedListF (l @ x)

type WrappedListG<'s> = WrappedListG of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedListG x) = x in x :> _ seq).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedListG x) = x in x :> _ seq).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedListG [x]
    static member (>>=)  (WrappedListG x: WrappedListG<'T>, f) = WrappedListG (List.collect (f >> (fun (WrappedListG x) -> x)) x)
    static member Join  (WrappedListG wlst) = (*SideEffects.add "Join";*)  WrappedListG wlst >>= id
    static member get_Empty() = WrappedListG List.empty
    static member (<|>) (WrappedListG l, WrappedListG x) = WrappedListG (l @ x)
    static member Delay (f: unit -> WrappedListD<_>) = SideEffects.add "Using WrappedListG's Delay"; f()
    static member Using (resource, body)             = SideEffects.add "Using WrappedListG's Using"; using resource body


type WrappedSeqA<'s> = WrappedSeqA of 's seq with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedSeqA x) = x in x).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedSeqA x) = x in x).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedSeqA [x]
    static member (>>=)  (WrappedSeqA x: WrappedSeqA<'T>, f) = WrappedSeqA (Seq.collect (f >> (fun (WrappedSeqA x) -> x)) x)
    static member Join  (WrappedSeqA wlst) = WrappedSeqA wlst >>= id
    static member get_Empty() = WrappedSeqA List.empty
    static member (<|>) (WrappedSeqA l, WrappedSeqA x) = WrappedSeqA (Seq.append l x)
    static member Delay (f: unit -> WrappedSeqA<_>) =
                    let run (WrappedSeqA s) = s
                    WrappedSeqA (Seq.delay (f >> run))

type WrappedSeqB<'s> = WrappedSeqB of 's seq with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedSeqB x) = x in x).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedSeqB x) = x in x).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedSeqB [x]
    static member (>>=)  (WrappedSeqB x: WrappedSeqB<'T>, f) = WrappedSeqB (Seq.collect (f >> (fun (WrappedSeqB x) -> x)) x)
    static member Join  (WrappedSeqB wlst) = WrappedSeqB wlst >>= id
    static member get_Empty() = WrappedSeqB List.empty
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
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedSeqC x) = x in x).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedSeqC x) = x in x).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedSeqC [x]
    static member (>>=)  (WrappedSeqC x: WrappedSeqC<'T>, f) = WrappedSeqC (Seq.collect (f >> (fun (WrappedSeqC x) -> x)) x)
    static member Join  (WrappedSeqC wlst) = WrappedSeqC wlst >>= id
    static member get_Empty() = WrappedSeqC List.empty
    static member (<|>) (WrappedSeqC l, WrappedSeqC x) = WrappedSeqC (Seq.append l x)
    static member Delay (f: unit -> WrappedSeqC<_>) =
                    let run (WrappedSeqC s) = s
                    WrappedSeqC (Seq.delay (f >> run))
    static member TryFinally (computation, compensation) =
                    SideEffects.add "Using WrappedSeqC's TryFinally"
                    try computation finally compensation ()

type WrappedSeqD<'s> = WrappedSeqD of 's seq with
    static member Return  (x) = SideEffects.add "Using WrappedSeqD's Return"; WrappedSeqD (Seq.singleton x)
    static member (<*>)  (WrappedSeqD f, WrappedSeqD x) = SideEffects.add "Using WrappedSeqD's Return"; WrappedSeqD (f <*> x)
    static member ToList (WrappedSeqD x) = Seq.toList x

open System.Collections.Generic
open System.Threading.Tasks
open FSharpPlus.One.Helpers
type ZipList<'s> = ZipList of 's seq with
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member Map   (ZipList x, f:'a->'b)                = ZipList (Seq.map f x)
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>
    static member inline get_Zero() = result zero                      :ZipList<'a>
    static member inline (+) (x:ZipList<'a>, y:ZipList<'a>) = liftA2 plus x y :ZipList<'a>
    static member ToSeq    (ZipList lst)     = lst

type ZipList'<'s> = ZipList' of 's seq with
    static member Return (x:'a)                              = ZipList' (Seq.initInfinite (konst x))
    static member Map   (ZipList' x, f:'a->'b)                = ZipList' (Seq.map f x)
    static member (<*>) (ZipList' (f:seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList'<'b>
    static member inline get_Zero() = result zero                      :ZipList'<'a>
    static member inline (+) (x:ZipList'<'a>, y:ZipList'<'a>) = liftA2 plus x y :ZipList'<'a>
    static member inline Sum (x:seq<ZipList'<'a>>) = SideEffects.add "Using optimized Sum"; List.foldBack plus (Seq.toList x) zero:ZipList'<'a>
    static member ToSeq    (ZipList' lst)     = lst

type MyList<'t> = MyList of list<'t> with
    static member get_Empty () = MyList []
    static member (<|>) (MyList x, MyList y) = MyList (x @ y)

type MyNum = MyNum of int with
    static member get_Empty () = MyNum 0
    static member FromInt32 x = MyNum x

[<AbstractClass>]
type Monoid() =


    [<Test>]
    member __.testCompile() =
        let res1n2 = MyList [1] ++ MyList [2] ++ zero
        let res0 : MyNum = zero 

        let asQuotation = plus    <@ ResizeArray(["1"]) @> <@ ResizeArray(["2;3"]) @>
        let quot123     = plus    <@ ResizeArray([1])   @> <@ ResizeArray([2;3])   @>
        let quot1       = plus    <@ ResizeArray([1])   @>      (zero)
        let quot23      = plus       (zero)         <@ ResizeArray([2;3])   @>
        let quot13      = plus       (zero)         <@ ("1","3") @>
        let lzy1 = plus (lazy [1]) (lazy [2;3])
        let lzy2 = plus (zero) lzy1
        let asy1 = plus (async.Return [1]) (async.Return [2;3])
        let asy2 = plus (zero) asy1

        let mapA = Map.empty 
                    |> Map.add 1 (async.Return "Hey")
                    |> Map.add 2 (async.Return "Hello")

        let mapB = Map.empty 
                    |> Map.add 3 (async.Return " You")
                    |> Map.add 2 (async.Return " World")

        let mapAB = plus mapA mapB
        let greeting1 = Async.RunSynchronously mapAB.[2]
        let greeting2 = Async.RunSynchronously (Seq.sum [mapA; zero; mapB]).[2]

        let dicA = new Dictionary<string,Task<string>>()
        dicA.["keya"] <- (result "Hey"  : Task<_>)
        dicA.["keyb"] <- (result "Hello": Task<_>)

        let dicB = new Dictionary<string,Task<string>>()
        dicB.["keyc"] <- (result " You"  : Task<_>)
        dicB.["keyb"] <- (result " World": Task<_>)

        let dicAB = plus dicA dicB

        let greeting3 = extract dicAB.["keyb"]
        let greeting4 = extract (Seq.sum [dicA; zero; dicB]).["keyb"]

        let res2   = Seq.sum [ async {return Endo ((+) 2)} ; async {return Endo ((*) 10)} ; async {return Endo id } ;  async {return Endo ((%) 3)} ; async {return zero } ] |> Async.RunSynchronously |> Endo.run <| 3
        let res330 = Seq.sum [ async {return (fun (x:int) -> string x)} ; async {return (fun (x:int) -> string (x*10))} ; async {return zero } ] </Async.RunSynchronously/>  3
        ()


    [<Test>]
    member __.seqSumDefaultCustom() =
        let (WrappedListB x) = Seq.sum [WrappedListB [10] ;WrappedListB [15]]
        let (WrappedListC y) = Seq.sum [WrappedListC [10] ;WrappedListC [15]]
        Assert.AreEqual (x, [10;15])
        Assert.AreEqual (y, [10])

        let x = [ ("a", 1); ("b", 2); ("a", 3) ]
        let y = x |> map (Seq.singleton >> (ofSeq : seq<_*_> -> Dictionary<_,_>) >> map List.singleton) |> Seq.sum
        let z = x |> map (Seq.singleton >>             dict                      >> map List.singleton) |> Seq.sum
        Assert.IsInstanceOf<Option< Dictionary<string,int list>>> (Some y)
        Assert.IsInstanceOf<Option<IDictionary<string,int list>>> (Some z)

        SideEffects.reset()

        let quotLst123  = plus zero (ZipList [ [1];[2];[3] ])

        Assert.AreEqual (quotLst123 |> toList, [[1]; [2]; [3]])
        Assert.AreEqual (SideEffects.get(), [])

        let quotLst123' = Seq.sum [zero; zero; ZipList' [ [1];[2];[3] ]]

        Assert.AreEqual (quotLst123' |> toList, [[1]; [2]; [3]])
        Assert.AreEqual (SideEffects.get(), ["Using optimized Sum"])

        let wl = WrappedListB  [2..10]

        let arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
        let listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
        let seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

        let arrayGroupAdj   = chunkBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]

        ()


[<AbstractClass>]
type Functor() =
    [<Test>]
    member __.mapDefaultCustom() = 

        // NonEmptyList<_> has Map but at the same time is a seq<_>
        let testVal1 = map ((+) 1) {Head = 10; Tail = [20;30]}
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal1)

        let testVal2 = map ((+) 1) ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some testVal2)

        let testVal3 = map ((+) 1) (dict (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some testVal3)

        // WrappedSeqD is Applicative. Applicatives are Functors => map should work
        Assert.AreEqual (SideEffects.get(), [])
        let testVal4 = map ((+) 1) (WrappedSeqD [1..3])
        Assert.IsInstanceOf<Option<WrappedSeqD<int>>> (Some testVal4)
        Assert.AreEqual (SideEffects.get(), ["Using WrappedSeqD's Return"; "Using WrappedSeqD's Return"])
        SideEffects.reset()
        
        // WrappedListE is a Monad. Monads are Functors => map should work
        let testVal5 = map ((+) 1) (WrappedListE [1..3])
        Assert.IsInstanceOf<Option<WrappedListE<int>>> (Some testVal5)

        // Same with WrappedListD but WrappedListD is also IEnumerable<_>
        Assert.AreEqual (SideEffects.get(), [])
        let testVal6 = map ((+) 1) (WrappedListD [1..3])
        Assert.IsInstanceOf<Option<WrappedListD<int>>> (Some testVal6)
        Assert.AreEqual (SideEffects.get(), ["Using WrappedListD's Bind"; "Using WrappedListD's Return"; "Using WrappedListD's Return"; "Using WrappedListD's Return"])

    [<Test>]
    member __.unzip() = 
        let testVal = unzip {Head = (1, 'a'); Tail = [(2, 'b');(3, 'b')]}
        Assert.IsInstanceOf<Option<NonEmptyList<int> * NonEmptyList<char>>> (Some testVal)

    [<Test>]
    member __.zipTest() =

        SideEffects.reset()
        let a = zip (seq [1;2;3]) (seq [1. .. 3. ])
        Assert.AreEqual (SideEffects.get(), [])

        let b = zip (WrappedListD [1;2;3]) (WrappedListD [1. .. 3. ])
        Assert.AreEqual (SideEffects.get(), ["Using WrappedListD's zip"])

        let c = zip (dict [1,'1' ; 2,'2' ; 4,'4']) (dict [1,'1' ; 2,'2' ; 3,'3'])
        let d = zip [ 1;2;3 ] [ 1. .. 3. ]
        let e = zip [|1;2;3|] [|1. .. 3.|]
        let g = zip ((seq [1;2;3]).GetEnumerator()) ((seq [1. .. 3. ]).GetEnumerator())

        let fa a = zip a (seq [1. .. 3. ])
        let fb a = zip a (WrappedListD [1. .. 3. ])
        let fc a = zip a (dict [1,'1' ; 2,'2' ; 3,'3'])
        let fd a = zip a [ 1. .. 3. ]
        let fe a = zip a [|1. .. 3.|]
        let fg a = zip a ((seq [1. .. 3. ]).GetEnumerator())

        let ga b = zip (seq [1;2;3]) b
        let gb b = zip (WrappedListD [1;2;3]) b
        let gc b = zip (dict [1,'1' ; 2,'2' ; 4,'4']) b
        let gd b = zip  [ 1;2;3 ] b
        let ge b = zip  [|1;2;3|] b
        let gg b = zip ((seq [1;2;3]).GetEnumerator()) b

        let ha : _ -> _ -> _ seq            = zip
        let hb : _ -> _ -> _ WrappedListD   = zip
        let hc : _ -> _ -> IDictionary<_,_> = zip
        let hd : _ -> _ -> _ list           = zip
        let he : _ -> _ -> _ []             = zip
        let hg : _ -> _ -> _ IEnumerator    = zip

        ()

[<AbstractClass>]
type Foldable() =

    let foldables =
        let r10  = foldBack (+) (seq [1;2;3;4]) 0
        let r323 = toList (seq [3;2;3])
        let r03  = filter ((=) 3) (seq [1;2;3])
        ()

    [<Test>]
    member __.foldMapDefaultCustom() =
        SideEffects.reset()
        let x = foldMap ((+) 10) (WrappedListD [1..4]) //= 50 w side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get(), ["Using optimized foldMap"])

        SideEffects.reset()
        let y = foldMap ((+) 10) {1..4}  //= 50 w/o side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get(), [])

    [<Test>]
    member __.filterDefaultCustom() = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (testVal, WrappedListA [2])
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

        let twos   = filter ((=) (box 2)) (([1;2;3;4;3;2;1;2;3] |> ofSeq) : Collections.ArrayList)
        let five   = filter ((=) 5) (WrappedListB' [1;2;3;4;5;6])   // <- Uses the default method for filter.
        let optionFilter = filter ((=) 3) (Some 4)

        ()

    [<Test>]
    member __.foldAternatives() = 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        Assert.AreEqual (x, Some 3)
        Assert.AreEqual (y, [3;4])

    [<Test>]
    member __.fromToSeq() =
        let s = (seq [Collections.Generic.KeyValuePair(1, "One"); Collections.Generic.KeyValuePair(2, "Two")])
        let t = {'a'..'d'}

        let dc2:Collections.Generic.Dictionary<_,_> = ofSeq s
        let s' = toSeq dc2

        let arr:_ [] = ofSeq s
        let s'' = toSeq arr

        let str:string = ofSeq t
        let t' = toSeq str 

        Assert.AreEqual (toList s, toList s')
        Assert.AreEqual (toList s , toList s'')
        Assert.AreEqual (toList t , toList t')

        Assert.IsInstanceOf ((Some s').GetType(), Some s)
        Assert.IsInstanceOf ((Some s'').GetType(), Some s)
        Assert.IsInstanceOf ((Some t'  ).GetType(), Some t)


    [<Test>]
    member __.sortBy() =
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

[<AbstractClass>]
type Indexable() = 
    [<Test>]
    member __.testCompileAndExecuteItem() =

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
    member __.testCompileAndExecuteTryItem() =

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
    member __.tryItemReadonly () =
        let d = ReadOnlyDictionary (dict [1, "one"; 2, "two"])
        let iReadOnlyDict = d :> IReadOnlyDictionary<_,_>
        let l = ReadOnlyCollection [|1..10|]
        let rarr = ResizeArray [|1..10|]
        Assert.AreEqual (Some "one", tryItem 1 d)
        //Assert.AreEqual (Some "one", tryItem 1 iReadOnlyDict)
        Assert.AreEqual ("one", item 1 d)
        Assert.AreEqual ("one", item 1 iReadOnlyDict)
        Assert.AreEqual (2, item 1 l)
        Assert.AreEqual (2, item 1 rarr)
        Assert.AreEqual (Some 2, tryItem 1 rarr)

    [<Test>]
    member __.mapiUsage () =
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


    [<Test>]
    member __.iteriUsage () =
        let m = Map.ofList [1, "one"; 2, "two"]
        SideEffects.reset ()
        iteri (fun i v -> SideEffects.add <| sprintf "Got %d-%s" i v) m
        areEquivalent ["Got 1-one";"Got 2-two"] (SideEffects.get ())

        SideEffects.reset ()
        let onIteration i v= ()
        iteri onIteration (WrappedListD [1..2])
        areEqual ["Using WrappedListD's IterateIndexed"] (SideEffects.get ())

    [<Test>]
    member __.foldiUsage () =
        SideEffects.reset ()
        let folder (s:int) (i:int) (t:int) = t * s - i
        let wlist = WrappedListD [1..2]
        let res = foldi folder 10 wlist
        areEquivalent ["Using WrappedListD's FoldIndexed"] (SideEffects.get ())
        areEqual 19 res

    [<Test>]
    member __.traverseiUsage () =
        let resSomeId20 = traversei (fun k t -> Some (10 + t)) (Tuple 10)
        ()

[<AbstractClass>]
type Monad() = 
    [<Test>]
    member __.joinDefaultCustom() = 
        let x = join [[1];[2]]
        Assert.AreEqual (x, [1;2])
        let y : WrappedListE<_> = join (WrappedListE [WrappedListE [1];WrappedListE [2]])
        Assert.AreEqual (y, WrappedListE [1;2])
        SideEffects.reset()
        let z = join (WrappedListF [WrappedListF [1];WrappedListF [2]])
        Assert.AreEqual (z, WrappedListF [1;2])
        Assert.AreEqual (SideEffects.get(), ["Join"])

    [<Test>]
    member __.workFlow() =       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        Assert.IsInstanceOf<WrappedListD<int>>(testVal)

    [<Test>]
    member __.DelayForCont() = 
        // If Delay is not properly implemented this will stack-overflow
        // See http://stackoverflow.com/questions/11188779/stackoverflow-in-continuation-monad
#if MONO
        Assert.Ignore()
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
        Assert.Pass()
#endif


[<AbstractClass>]
type Traversable() = 
    let traverseTest =
        let resNone = sequence (seq [Some 3;None ;Some 1])
        ()

    [<Test>]
    member __.sequence_Default_Primitive() = 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    [<Test>]
    member __.sequence_Specialization() =
        
        let inline seqSeq (x:_ seq ) = sequence x
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x

        let a : list<_> = seqSeq (seq [[1];[3]])
        //Assert.AreEqual ([seq [1; 3]], a, sprintf "A: %A" a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        Assert.AreEqual ([[|1; 3|]], b, sprintf "B: %A" b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        Assert.AreEqual ([[1; 3]], c, sprintf "C: %A" c)
        Assert.IsInstanceOf<list<list<int>>> c

    [<Test>]
    member __.traversableForNonPrimitive() =
        let nel = NonEmptyList.create (Some 1) [Some 2]
        let rs1  = traverse id nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
        let rs2  = sequence nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2

    [<Test>]
    member __.traverseInfiniteOptions() =
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
        

[<AbstractClass>]
type Applicative() = 
    [<Test>]
    member __.applicativeMath() = 
        let inline (+) (a:'T) (b:'T) :'T = a + b
        let inline ( .+  ) (x :'Functor't)     (y :'t)             = map ((+)/> y) x :'Functor't
        let inline (  +. ) (x :'t)             (y :'Functor't)     = map ((+)   x) y :'Functor't
        let inline ( .+. ) (x :'Applicative't) (y :'Applicative't) = (+) <!> x <*> y :'Applicative't

        let testVal = [1;2] .+. [10;20] .+. [100;200] .+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOf<Option<list<int>>> (Some testVal)


    [<Test>]
    member __.applicatives() = 

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
[<AbstractClass>]
type IdiomBrackets() =
    [<Test>]
    member __.idiomBrackets() =    
        let inline idiomatic a b = (Idiomatic $ b) a
        let inline iI x = (idiomatic << result) x

        let res3n4''  = iI ((+) 2) [1;2] Ii
        let res3n4''' = iI (+) (result 2) [1;2] Ii   // fails to compile when constraints are not properly defined
        Assert.AreEqual ([3;4], res3n4'' )
        Assert.AreEqual ([3;4], res3n4''')


        let output = System.Text.StringBuilder()
        let append (x:string) = output.Append x |> ignore

        let v5: Lazy<_> = lazy (append "5"; 5)
        Assert.AreEqual (0, output.Length)
        let fPlus10 x   = lazy (append " + 10"; x + 10)
        Assert.AreEqual (0, output.Length)
        let v5plus10    = v5 >>= fPlus10
        Assert.AreEqual (0, output.Length)
        let v15 = v5plus10.Force()
        Assert.AreEqual ("5 + 10", output.ToString())
        Assert.AreEqual (15, v15)

        output.Clear() |> ignore

        let v4ll: Lazy<_> = lazy (append "outer"; lazy (append "inner"; 4))
        Assert.AreEqual (0, output.Length)
        let v4l = join v4ll
        Assert.AreEqual (0, output.Length)
        let v4  = v4l.Force()
        Assert.AreEqual ("outerinner", output.ToString())
        Assert.AreEqual (4, v4)
 
[<AbstractClass>]
type Alternative() =
    
    let testEmpty() =
        let (v: WrappedListE<int>) = empty
        let (w: list<int>)         = empty
        let (x: WrappedListG<int>) = empty
        let (y: seq<int>)          = empty
        
        // shoud not compile. 
        // Although WrappedListD implements IEnumerable, it should explicitely implement Empty. Not all IEnumerables have empty.
        // let (z: WrappedListD<int>) = empty
        ()

    let testAppend() =
        let v = WrappedListE [1;2] <|> WrappedListE [3;4]
        let w = [1;2] <|> [3;4]
        let x = WrappedListG [1;2] <|> WrappedListG [3;4]
        let y = seq [1;2] <|> seq [3;4]

        // shoud not compile. 
        // Although WrappedListD implements IEnumerable, it should explicitely implement (<|>). Not all IEnumerables have (<|>).
        // let z = WrappedListD [1;2] ++ WrappedListD [3;4]
        ()

    [<Test>]
    member __.testEmptyAndAppendForCustomType() =
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
    member __.testOptionTAppliesFunctionOnce() =
        SideEffects.reset()
        let x = OptionT <| async { SideEffects.add "hello"; return Some 1 }
        let y = OptionT <| async { SideEffects.add "good bye"; return Some 2 }

        let z = (x <|> y) |> OptionT.run |> Async.RunSynchronously

        Assert.AreEqual (SideEffects.get(), ["hello"])
        Assert.AreEqual (z, Some 1)


[<AbstractClass>]
type Splits() = 
    [<Test>]
    member __.splitArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|]  |> Seq.map System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((toList a1 = toList a2))
        Assert.IsTrue((toList b1 = toList b2))
        Assert.IsInstanceOf<Option<string []>> (Some a1)

    [<Test>]
    member __.replaceArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B  |> System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B |> System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b1 = b2))

    [<Test>]
    member __.intercalateArraysAndStrings() = 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B  |> System.Text.Encoding.ASCII.GetString

        let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        // Fails to compile but works in F#4.1
        // let c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        // 

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))

[<AbstractClass>]
type Parsing() = 
    [<Test>]
    member __.parseDateTime() =
#if MONO
        let v1 : DateTime = parse "2011-03-04T15:42:19+03:00"
        Assert.IsTrue((v1 = DateTime(2011,3,4,12,42,19)))
#else
        Assert.Ignore("Depends on how it's executed...")
#endif

    [<Test>]
    member __.parse() = 
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
        ()

[<AbstractClass>]
type Conversions() =
    [<Test>]
    member __.test() =
        // Generic op_Explicit
        let r302:float  = explicit 302
        let r303:float  = explicit "303"
        let r304:char   = explicit "F"
        let a : unativeint = explicit 2147483648I
        ()

[<AbstractClass>]
type BitConverter() =
    [<Test>]
    member __.roundtrips () =
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
open FSharpPlus.Builders
[<AbstractClass>]
type Sequences() =
    [<Test>]
    member __.test() =
        let stack = new Collections.Generic.Stack<_>([1;2;3])

        let twoSeqs = plus (seq [1;2;3]) (seq [4;5;6])
        let sameSeq = plus (getZero()  ) (seq [4;5;6])

        let seqFromLst:_ seq = ofList [1;2;3;4]
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
        let getLine    = async { return System.Console.ReadLine() }
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


