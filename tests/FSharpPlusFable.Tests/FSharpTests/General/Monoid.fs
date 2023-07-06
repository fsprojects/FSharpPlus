module General.Monoid
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"
#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
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



let monoid = testList "Monoid" [
    testCase "seqSumDefaultCustom" (fun () ->
        #if !FABLE_COMPILER
        let (WrappedListB x) = Seq.sum [WrappedListB [10]; WrappedListB [15]]
        #endif
        #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
        //equalSeq [10;15] x // fails to infer type?
        let (WrappedListC y) = Seq.sum [WrappedListC [10]; WrappedListC [15]]
        equalSeq [10] y
        #endif

        let x = [ ("a", 1); ("b", 2); ("a", 3) ]
        #if !FABLE_COMPILER
        let y = x |> map (Seq.singleton >> (ofSeq : seq<_*_> -> Dictionary<_,_>) >> map List.singleton) |> Seq.sum
        let z = x |> map (Seq.singleton >>             dict                      >> map List.singleton) |> Seq.sum
        Assert.IsInstanceOf<Option< Dictionary<string,int list>>> (Some y)
        Assert.IsInstanceOf<Option<IDictionary<string,int list>>> (Some z)
        #endif
        SideEffects.reset ()

        #if !FABLE_COMPILER
        let quotLst123  = plus zero (ZipList [ [1];[2];[3] ])

        equal [[1]; [2]; [3]] (quotLst123 |> toList)
        equal [] (SideEffects.get ())

        let quotLst123' = Seq.sum [zero; zero; ZipList' [ [1];[2];[3] ]]

        equal [[1]; [2]; [3]] (quotLst123' |> toList)
        equal ["Using optimized Sum"] (SideEffects.get ())
        #endif

        #if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
        let _wl = WrappedListB  [2..10]

        let _arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
        let _listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
        let _seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

        let _arrayGroupAdj   = chunkBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]
        #endif

        ())
    ]