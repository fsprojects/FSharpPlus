module General.MonoidCompile
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"
#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

#if !FABLE_COMPILER
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
#endif

type MyList<'t> = MyList of list<'t> with
    static member get_Empty () = MyList []
    static member (<|>) (MyList x, MyList y) = MyList (x @ y)

type MyNum = MyNum of int with
    static member get_Empty () = MyNum 0
    static member FromInt32 x = MyNum x


let testCompile =
    #if !FABLE_COMPILER

    let _res1n2 = MyList [1] ++ MyList [2] ++ zero
    let _res0 : MyNum = zero 
    let _asQuotation = plus    <@ ResizeArray (["1"]) @> <@ ResizeArray (["2;3"]) @>
    let _quot123     = plus    <@ ResizeArray ([1])   @> <@ ResizeArray ([2;3])   @>
    let _quot1       = plus    <@ ResizeArray ([1])   @>      (zero)
    let _quot23      = plus       (zero)         <@ ResizeArray ([2;3])   @>
    let _quot13      = plus       (zero)         <@ ("1","3") @>
    #endif
    #if !FABLE_COMPILER
    let lzy1 = plus (lazy [1]) (lazy [2;3])
    #endif
    #if !FABLE_COMPILER
    let _lzy = plus (zero) lzy1
    #endif
    #if !FABLE_COMPILER
    let asy1 = plus (async.Return [1]) (async.Return [2;3])
    #endif
    #if !FABLE_COMPILER
    let _asy = plus (zero) asy1
    let _bigNestedTuple1 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (2, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (3, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20))
    let _bigNestedTuple2 = (1, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ (zero, System.Tuple (8, "ff",3,4,5,6,7,8,9,10,11,12,(),14,15,16,17,18,19,20)) ++ zero
    #endif

    #if !FABLE_COMPILER
    let _nes : NonEmptySeq<_> = plus (NonEmptySeq.singleton 1) (NonEmptySeq.singleton 2)
    #endif

    let mapA = Map.empty 
                |> Map.add 1 (async.Return "Hey")
                |> Map.add 2 (async.Return "Hello")

    let mapB = Map.empty 
                |> Map.add 3 (async.Return " You")
                |> Map.add 2 (async.Return " World")

    #if !FABLE_COMPILER
    let mapAB = plus mapA mapB
    #endif
    #if !FABLE_COMPILER
    let _greeting1 = Async.RunSynchronously mapAB.[2]
    let _greeting2 = Async.RunSynchronously (Seq.sum [mapA; zero; mapB]).[2]
    #endif

    #if !FABLE_COMPILER
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
    #endif
    ()
let monoidCompile = testList "monoidCompile" [
    testCase "MonoidCompile" (fun () ->
        ())
    ]