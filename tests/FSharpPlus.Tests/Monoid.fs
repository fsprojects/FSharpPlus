namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open CSharpLib

module Monoid =
    open System.Collections
    open System.Collections.Generic

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
        Assert.AreEqual (list<string>.Empty, SideEffects.get ())

        let quotLst123' = Seq.sum [zero; zero; ZipList' [ [1];[2];[3] ]]

        Assert.AreEqual ([[1]; [2]; [3]], quotLst123' |> toList)
        Assert.AreEqual (["Using optimized Sum"], SideEffects.get ())

        let _wl = WrappedListB  [2..10]

        let _arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
        let _listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
        let _seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

        let _arrayGroupAdj   = chunkBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]
        
        ()

    [<Test>]
    let writerMonad () =
        let struct (str, num) = monad {
            let! x = struct ("Four", 4)
            let! y = struct ("Ten", 10)
            return y - x }
        Assert.AreEqual (str, "FourTen")
        Assert.AreEqual (num, 6)

    #if NET6_0_OR_GREATER
    [<Test>]
    let testDateAndTimes =
        let d1 = DateOnly(2020, 1, 1)
        let d2 = d1 ++ zero ++ one
        Assert.AreEqual (DateOnly(2020, 1, 2), d2)

        let t1 = TimeOnly (2, 10, 30)
        let t2 = t1 ++ zero
        Assert.AreEqual (t1, t2)
    #endif