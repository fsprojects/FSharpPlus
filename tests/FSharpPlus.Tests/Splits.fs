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


module Parsing =
    let (|Int32|_|) : _-> Int32 option = tryParse
    type ProductId = { Value:int }
    with
        static member TryParse(value:string) : ProductId option =
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
        let b = trySscanf (PrintfFormat<int -> string, unit, string, string, int> "%B") "101"
        let a = trySscanf (PrintfFormat<int -> int -> int -> int -> string, unit, string, string, int * int * int * int> "%B %o %x %X") "100 100 100 100"
        
        areEqual (Some (19, 67)) x
        areEqual (Some 8) o
        areEqual (Some 5) b
        areEqual (Some (4, 64, 256, 256)) a