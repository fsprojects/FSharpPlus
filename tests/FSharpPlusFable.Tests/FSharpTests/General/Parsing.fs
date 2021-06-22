module General.Parsing
open Testing
open General.Util
open FSharpPlus
open FSharpPlus.Data
#nowarn "686"
open System

#if !FABLE_COMPILER || FABLE_COMPILER_3
let (|Int32|_|) : _-> Int32 option = tryParse
type ProductId = { Value:int }
with
    static member TryParse(value:string) : ProductId option=
        match value.Split('_') |> List.ofArray with
        | "P" :: Int32 v :: [] -> Some { Value = v }
        | _ -> None
#endif

let parsing = testList "Parsing" [
    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "parse" (fun () -> 
        #if !FABLE_COMPILER
        let v2 : DateTimeOffset = parse "2011-03-04T15:42:19+03:00"

        Assert.IsTrue((v2 = DateTimeOffset(2011,3,4,15,42,19, TimeSpan.FromHours 3.)))
        #endif
        #if !FABLE_COMPILER
        let _101 = tryParse "10.1.0.1" : Net.IPAddress option
        #endif
        let _102 = tryParse "102" : string option
        #if !FABLE_COMPILER
        let _MTS = [tryParse "Monday" ; Some DayOfWeek.Thursday; Some DayOfWeek.Saturday]
        #endif
        let _103 = tryParse "103" : Text.StringBuilder option
        
        #if !FABLE_COMPILER
        let _109 = parse "10.0.9.1" : Net.IPAddress
        #endif
        
        let _111 = parse "true" && true        
        let _MTF = [parse "Monday" ; DayOfWeek.Thursday; DayOfWeek.Friday]
        
        #if !FABLE_COMPILER
        let _110 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.
        let _120 = parse "10" + ofBytes [|10uy;0uy;0uy;0uy;|]                + 100
        #endif
        
        let _121 = parse "121" : string
        let _122 = parse "122" : Text.StringBuilder
        
        let r66: float option = tryParse "66.0"
        equal r66 (Some 66.0)

        let r123: WrappedListA<int> option = tryParse "[1;2;3]"
        equal r123 (Some (WrappedListA [1; 2; 3])))

    testCase "scanfParsing" (fun () ->
        let _ccx: int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int * uint32 * float * float32 * int = parseArray [|"34"; "24"; "34"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"|]
        
        let _t = sscanf "(%i-%i-%f-%i-%i-%i-%i-%i-%i)" "(32-66-888-4-5-6-7-8-9)"
        let (_a,_b) = sscanf "(%%%s,%M)" "(%hello, 4.53)"
        let (_x,_y,_z) = sscanf "%s-%s-%s" "test-this-string"
        let (_j,_k,_l,_m,_n,_o,_p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"
        
        // let (_r1,_r2,_r3,_r4,_r5,_r6,_r7,_r8)          = sscanf "%f %F %g %G %e %E %c %c"    "1 2.1 3.4 .3 43.2e32 0 f f"
        let (_s1,_s2,_s3,_s4,_s5,_s6,_s7,_s8,_s9)      = sscanf "%f %F %g %G %e %E %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f"
        // let (_t1,_t2,_t3,_t4,_t5,_t6,_t7,_t8,_t9,_t10) = sscanf "%f %F %g %G %e %E %c %c %c %c" "1 2.1 3.4 .3 43.2e32 0 f f f f"
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

        ())

    #endif
]