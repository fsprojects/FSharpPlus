module FSharpPlus.One.Usage
open System
open System.Collections
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Data

let seqS = seq { 
    yield (None)
    yield (None)
    yield (Some 'c')
    yield (Some 'd')
    yield (None)
    yield (Some 'f')
}
let toListSeqS ()= choice (toList seqS)
let ofSeqS ()= choice (ofSeq seqS: Set<_>)
let toArraySeqS ()= choice (toArray seqS)


module testSeqConversions =

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
    let ut: Hashtable                 = ofSeq (seq [1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
    let al: ArrayList                 = ofSeq (seq ["1";"2";"3"])            // but it will come back as seq<obj>
    let us: SortedList                = ofSeq (seq [4,'2';3,'4'])            // but it will come back as seq<obj>

    // now go back
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
    let _ut'  = toSeq ut 
    let _al'  = toSeq al 
    let _us'  = toSeq us 

    // there are some 'one-way' collections that can only be converted toSeq

    let columns = 
        let d = new Data.DataTable () 
        [|new Data.DataColumn "id";new Data.DataColumn "column1";new Data.DataColumn "column2"|] |> d.Columns.AddRange
        d.Columns
    let _col1 = columns |> find (fun x -> x.ColumnName = "column1")
    let _cols = columns |> toList |> map  (fun x -> x.ColumnName)



module testListConversions =

    // From sequence

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
    let ra: Generic.List<_>           = ofList ([1..3])

    // now go back
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
    let _ra'  = toList ra

module applicatives = 
    // Test Applicative (functions)
    let res607 = map (+) ( (*) 100 ) 6 7
    let res606 = ( (+) <*>  (*) 100 ) 6
    let res508 = (map (+) ((+) 3 ) <*> (*) 100) 5

    // Test Applicative (ZipList)
    let res9n5  = map ((+) 1) (ZipList [8;4])
    let _20n30  = result (+) <*> result 10 <*> ZipList [10;20]
    let _18n14  = result (+) <*> ZipList [8;4] <*> result 10


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

    open FSharpPlus.Builders // for applicative brackets

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
