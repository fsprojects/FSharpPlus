namespace FSharpPlus.Tests

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open NUnit.Framework
open Helpers
open CSharpLib

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

        let t  = DateTime(2011,3,14,12,42,19)
        let u  = DateTimeOffset(2011,3,14,15,42,19, TimeSpan.FromHours 3.)
        let u0 = DateTimeOffset(2011,3,14,15,42,19, TimeSpan.FromHours 0.)

        let t1 = parse<DateTime>    "2011-03-14T15:42:19+03:00" in Assert.AreEqual(     t, t1, nameof t1)
        let t2 = tryParse<DateTime> "2011-03-14T15:42:19+03:00" in Assert.AreEqual(Some t, t2, nameof t2)

        let u1 = parse<DateTimeOffset>    "2011-03-14T15:42:19+03:00" in Assert.AreEqual(     u, u1, nameof u1)
        let u2 = tryParse<DateTimeOffset> "2011-03-14T15:42:19+03:00" in Assert.AreEqual(Some u, u2, nameof u2)

        let t3  = parse<DateTime>    "Mon, 14 Mar 2011 12:42:19 GMT" in Assert.AreEqual(     t, t3, nameof t3)
        let t4  = tryParse<DateTime> "Mon, 14 Mar 2011 12:42:19 GMT" in Assert.AreEqual(Some t, t4, nameof t4)

        let u3  = parse<DateTimeOffset>    "Mon, 14 Mar 2011 15:42:19 GMT" in Assert.AreEqual(     u0, u3, nameof u3)
        let u4  = tryParse<DateTimeOffset> "Mon, 14 Mar 2011 15:42:19 GMT" in Assert.AreEqual(Some u0, u4, nameof u4)

        let u5 = parse<DateTimeOffset>    "2011-03-14T15:42:19" in Assert.AreEqual(     u0, u5, nameof u5)
        let u6 = tryParse<DateTimeOffset> "2011-03-14T15:42:19" in Assert.AreEqual(Some u0, u6, nameof u6)

        let u7 = parse<DateTimeOffset>    "2011-03-14T15:42:19Z" in Assert.AreEqual(     u0, u7, nameof u7)
        let u8 = tryParse<DateTimeOffset> "2011-03-14T15:42:19Z" in Assert.AreEqual(Some u0, u8, nameof u8)



    [<Test>]
    let parse () = 

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
        let v5 : ICustomerId option = tryParse "C_1"
        Assert.IsTrue((v5.Value.Value = 1L))
        let v6 : ICustomerId option = tryParse "C_X"
        Assert.IsTrue(Option.isNone v6)

    [<Test>]
    let scanfParsing () =
        let _ccx: int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int = parseArray [|"34"; "24"; "34"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"|]
        
        let _t = sscanf "(%i-%i-%f-%i-%i-%i-%i-%i-%i)" "(32-66-888-4-5-6-7-8-9)"
        let (_a,_b) = sscanf "(%%%s,%M)" "(%hello,4.53)"
        let (_a1,_b1) = sscanf "(%%%s,% M)" "(%hello, 4.53)"
        let (_a2,_b2) = sscanf "(%%%s,%-M)" "(%hello,4.53 )"
        let (_a3,_b3) = sscanf "(%%%s,% -M)" "(%hello, 4.53 )"
        let (_a4,_b4,_ab) = sscanf "(%%%s,% d%-M)" "(%hello, 4.53 )"
        let (_x,_y,_z) = sscanf "%s-%s-%s" "test-this-string"
        let (_j,_k,_l,_m,_n,_o,_p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"
        
        let (_r1,_r2,_r3,_r4,_r5,_r6,_r7,_r8)          = sscanf "%f %F %g %G %e %E %c %c"    "1 2.1 3.4 .3 43.2e32 0 f f"
        let (_s1,_s2,_s3,_s4,_s5,_s6,_s7,_s8,_s9)      = sscanf "%f %F %g %G %e %E %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f"
        let (_t1,_t2,_t3,_t4,_t5,_t6,_t7,_t8,_t9,_t10) = sscanf "%f %F %g %G %e %E %c %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f f"
        let (_u1,_u2,_u3,_u4,_u5,_u6,_u7,_u8,_u9,_u10,_u11,_u12,_u13,_u14,_u15)           = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c"       "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f"
        let (_v1,_v2,_v3,_v4,_v5,_v6,_v7,_v8,_v9,_v10,_v11,_v12,_v13,_v14,_v15,_v16)      = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i"    "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16"
        let (_w1,_w2,_w3,_w4,_w5,_w6,_w7,_w8,_w9,_w10,_w11,_w12,_w13,_w14,_w15,_w16,_w17) = sscanf "%f %F %g %G %e %E %c %c %c %c %c %c %c %c %c %i %f" "1 2.1 3.4 .3 43.2e32 0 f f f f f f f f f 16 17"
        
        
        let _zzz = sscanf "(%%%s)" "(%hello)"
        let _zzz1 = sscanf "%%(%s)" "%(hello)"
        let (_x1,_y1,_z1) = sscanf "%s--%s-%s" "test--this-string"
        
        match "ab" with Parsedf "%c" _ -> failwith "wrong match" | Parsedf "%c%c" ('a', 'b') -> () | _ -> failwith "didn't match"
        match "abc" with Parsedf "%c%c" ('a', 'b') -> failwith "wrong match" | Parsedf "%c%c%c%s" ('a', 'b', 'c', "") -> () | _ -> failwith "didn't match"
        match "(%hello)" with
        | Parsedf "%d" _ | Parsedf "%f" _ | Parsedf "%x" _ -> failwith "wrong match"
        | Parsedf "%%(%%%s)" _ | Parsedf "(%%%sa" _ | Parsedf "(%%hel%c" _ | Parsedf "%%h%cllo)" _ -> failwith "wrong match"
        | Parsedf "(%%%s)" "hello" -> ()
        | _ -> failwith "didn't match"
        match " 3" with Parsedf "% d" 3 -> () | _ -> failwith "didn't match"
        match "  3" with Parsedf "% d" 3 -> () | _ -> failwith "didn't match"
        match " 3" with Parsedf "% d" 3 -> () | _ -> failwith "didn't match" // em space
        match "3 " with Parsedf "%-d" 3 -> () | _ -> failwith "didn't match"
        match "3  " with Parsedf "%-d" 3 -> () | _ -> failwith "didn't match"
        match "3 " with Parsedf "%-d" 3 -> () | _ -> failwith "didn't match" // em space
        match " 3 " with Parsedf "% -d" 3 -> () | _ -> failwith "didn't match"
        match "  3  " with Parsedf "% -d" 3 -> () | _ -> failwith "didn't match"
        match " 3 " with Parsedf "% -d" 3 -> () | _ -> failwith "didn't match" // em space
        match "test--this-gg" with Parsedf "%s--%s-%s" ("test", "this", "gg") -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4 .3 43.2e32 0 f f" with Parsedf "%f %F %g %G %e %E %c %c" (1f, 2.1, 3.4m, 0.3, 43.2e32, 0., 'f', 'f') -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4 .3 43.2e32 0 f f f" with Parsedf "%f% F %g %G %e %E %c %c %c" (1m, 2.1, 3.4, 0.3m, 43.2e32, 0., 'f', 'f', 'f') -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4.3 43.2e32 0 f f ff" with Parsedf "%B %F %-g%G %e %E %c %c %c%c" (1, 2.1, 3.4, 0.3, 43.2e32, 0., 'f', 'f', 'f', 'f') -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4.3 43.2e32 0 f f fff" with Parsedf "%o %F % g%-G %e %E %c %c %c%c%c" (1y, 2.1, 3.4, 0.3, 43.2e32, 0., 'f', 'f', 'f', 'f', 'f') -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4.3 43.2e32 0 f f fff16" with Parsedf "%x %F %- g%- G %e %E %c %c %c%c%c%i" (1us, 2.1, 3.4, 0.3, 43.2e32, 0., 'f', 'f', 'f', 'f', 'f', 16) -> () | _ -> failwith "didn't match"
        match "1 2.1 3.4.3 43.2e32 0 f f fff16 17" with Parsedf "%X %F %g% G %e %E %c %c %c%c%c%i %f" (1s, 2.1, 3.4, 0.3, 43.2e32, 0., 'f', 'f', 'f', 'f', 'f', 16L, 17.) -> () | _ -> failwith "didn't match"
        match "13 43 AA 77A" with Parsedf "%x %X %x %o%X" (0x13, 0x43, 0xAA, 0o77, 0xA) -> () | _ -> failwith "didn't match"
        match "13 43 AA 77A" with Parsedf "%B%x %X %x %o%X" (0b1, 0x3, 0x43, 0xAA, 0o77, 0xA) -> () | _ -> failwith "didn't match"
        match "111AAA" with Parsedf "%B%s" (0b111, "AAA") -> () | _ -> failwith "didn't match"
        match "100700 100 100" with Parsedf "%B%o %x %X" (0b100, 0o700, 0x100, 0x100) -> () | _ -> failwith "didn't match"

        match "1+1-2+2-8+8" with
        | Parsedf "%s%o" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+u%+u" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+d%o" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%-d%u" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%u%+d" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+o%+u" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+B%+u" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+x%+X" _ -> failwith "wrong match"
        | Parsedf "%+u%+u%+d%+u%+d%+X" (a, b, c, d, e, f) ->
            areEqual (a |> box |> unbox<int>) 1
            areEqual (b |> box |> unbox<int>) 1
            areEqual (c |> box |> unbox<int>) -2
            areEqual (d |> box |> unbox<int>) 2
            areEqual (e |> box |> unbox<int>) -8
            areEqual (f |> box |> unbox<int>) 8
        | _ -> failwith "didn't match"
        match "1+1-2+2-8+8" with Parsedf "%+-d%+d%+-d%+d%+-d%+d" (1,1,-2,2,-8,8) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8" with Parsedf "%d+%d%d%+d%d%+d" (1,1,-2,2,-8,8) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8" with Parsedf "%+B%+B-%+o%+o-%+X%+X" (1,1,2,2,8,8) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8e" with Parsedf "%+f%+F%+e%+E%+g%+G%+X" (1f,1.,-2m,2f,-8.,8M,0xE) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8e1a" with Parsedf "%+f%+F%+e%+E%+g%+G%+X" (1f,1.,-2m,2f,-8.,80M,0xA) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8e-1a" with Parsedf "%+f%+F%+e%+E%+g%+G%+X" (1f,1.,-2m,2f,-8.,0.8M,0xA) -> () | _ -> failwith "didn't match"
        match "1+1-2+2-8+8ea" with Parsedf "%+-f%+-F%+-e%+-E%+-g%+-G%+-X" (1f,1.,-2m,2f,-8.,8M,0xEA) -> () | _ -> failwith "didn't match"

        let _date: (DayOfWeek * string * uint16 * int) option = trySscanf "%A %A %A %A" "Saturday March 25 1989"
        let _date1: DateTime option = trySscanf "%A" "Saturday March 25 1989"
        
        match "12:34" with Parsedf "%A" (x: TimeSpan) -> areEqual (TimeSpan(12, 34, 0)) x | _ -> failwith "Pattern match failed"
        match "12:34:56" with Parsedf "%O" (x: TimeSpan) -> areEqual (TimeSpan(12, 34, 56)) x | _ -> failwith "Pattern match failed"
        match "9876-5-4 3:2:1" with Parsedf "%A" (x: DateTime) -> areEqual (DateTime(9876,5,4,3,2,1)) x | _ -> failwith "Pattern match failed"
        match "9876-5-4 3:2:1 a" with Parsedf "%O %x" (x: DateTime, y) -> areEqual (DateTime(9876,5,4,3,2,1)) x; areEqual 0xA y | _ -> failwith "Pattern match failed"
        
        let x = trySscanf "%X %x" "13 43"
        let o = trySscanf "%o" "10"
        let b = trySscanf (PrintfFormat<int -> string, unit, string, string, int> "%B") "101"
        let a = trySscanf (PrintfFormat<int -> int -> int -> int -> string, unit, string, string, int * int * int * int> "%B %o %x %X") "100 100 100 100"
        
        areEqual (Some (19, 67)) x
        areEqual (Some 8) o
        areEqual (Some 5) b
        areEqual (Some (4, 64, 256, 256)) a
