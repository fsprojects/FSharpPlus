#nowarn "3186"
#r @"..\..\build\FSharpPlus.dll"

open System
open FSharpPlus.Operators

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

let r123 = toString [1;2;3]
let r140 = toString (1,4,0)
let r150 = toString (Some 150)
let r160 = toString ([1;6;0] :> _ seq)
let r170 = toString (ResizeArray([1;7;0]))
let r180 = toString (Set [1;8;0])
let r190 = toString [|1;9;0|]
let r200 = toString [|{1..3};{4..6};{7..9}|]
let r210 = toString (Map  ['a',2; 'b',1; 'c',0])
let r220 = toString (dict ['a',2; 'b',2; 'c',0])


// Generic op_Explicit
let r302:float  = explicit 302
let r303:float  = explicit "303"
let r304:char   = explicit "F"


// From sequence
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic

let sk :Generic.Stack<_>          = ofSeq { 1 .. 3 }
let sg :string                    = ofSeq {'1'..'3'}  // but it will come back as seq<char>
let sb :Text.StringBuilder        = ofSeq {'1'..'3'}  // but it will come back as seq<char>
let sq1:_ seq                     = ofSeq { 1 .. 3 }
let sq2:_ seq                     = ofSeq (seq [(1, "One"); (2, "Two")])
let sq3:_ seq                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let sq4:_ seq                     = ofSeq (seq [(1, "One", '1', 1M); (2, "Two", '2', 2M)])
let ls1:_ list                    = ofSeq {'1'..'3'}
let ls2:_ list                    = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let st1:_ Set                     = ofSeq {'1'..'3'}
let st2:_ Set                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let ss :Generic.SortedSet<_>      = ofSeq (seq [3..6])
let ra :Generic.List<_>           = ofSeq (seq [1..3])
let sl :Generic.SortedList<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
let sl2:Generic.SortedList<_,_>   = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
let dc :Generic.Dictionary<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as kKeyValuePair
let mp :Map<_,_>                  = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
let mp2:Map<_,_>                  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
let d  :Generic.IDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
let d2 :Generic.IDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
let ut :Hashtable                 = ofSeq (seq [1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
let al :ArrayList                 = ofSeq (seq ["1";"2";"3"])            // but it will come back as seq<obj>
let us :SortedList                = ofSeq (seq [4,'2';3,'4'])            // but it will come back as seq<obj>
let cc :BlockingCollection<_>     = ofSeq {'1'..'3'}                     // but it will come back as seq<obj>
let cd :ConcurrentDictionary<_,_> = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
let cd2:ConcurrentDictionary<_,_> = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
let cb :ConcurrentBag<_>          = ofSeq {'1'..'3'}

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
let ut'  = toSeq ut 
let al'  = toSeq al 
let us'  = toSeq us 
let cc'  = toSeq cc 
let cd'  = toSeq cd 
let cb'  = toSeq cb 

// there are some 'one-way' collections that can only be converted toSeq

let columns = 
    let d = new Data.DataTable() 
    [|new Data.DataColumn "id";new Data.DataColumn "column1";new Data.DataColumn "column2"|] |> d.Columns.AddRange
    d.Columns
let col1 = columns |> find (fun x -> x.ColumnName = "column1")
let cols = columns |> toList |> map  (fun x -> x.ColumnName)