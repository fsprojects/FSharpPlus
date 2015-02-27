#r @"..\bin\Release\FsControl.Core.dll"

open System
open FsControl.Operators

let r101 = tryParse "10.1.0.1" : Net.IPAddress option
let r102 = tryParse "102" : string option
let rMTS = [tryParse "Monday" ; Some DayOfWeek.Thursday; Some DayOfWeek.Saturday]
let r103 = tryParse "103" : Text.StringBuilder option

let r109 = parse "10.0.9.1" : Net.IPAddress
let r111 = parse "true" && true
let rMTF = [parse "Monday" ; DayOfWeek.Thursday; DayOfWeek.Friday]
let r110 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.
let r120 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;|]                + 100
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
let r301:string = convert 301
let r302:float  = convert 302
let r303:float  = convert "303"
let r304:char   = convert "F"


// From sequence
open System.Collections
open System.Collections.Concurrent

let sk :Generic.Stack<_>          = fromSeq { 1 .. 3 }
let sg :string                    = fromSeq {'1'..'3'}
let sb :Text.StringBuilder        = fromSeq {'1'..'3'}
let sq1:_ seq                     = fromSeq { 1 .. 3 }
let sq2:_ seq                     = fromSeq (seq [(1, "One"); (2, "Two")])
let sq3:_ seq                     = fromSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let sq4:_ seq                     = fromSeq (seq [(1, "One", '1', 1M); (2, "Two", '2', 2M)])
let ls1:_ list                    = fromSeq {'1'..'3'}
let ls2:_ list                    = fromSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let st1:_ Set                     = fromSeq {'1'..'3'}
let st2:_ Set                     = fromSeq (seq [(1, "One", '1'); (2, "Two", '2')])
let ss :Generic.SortedSet<_>      = fromSeq (seq [3..6])
let ra :Generic.List<_>           = fromSeq (seq [1..3])
let sl :Generic.SortedList<_,_>   = fromSeq (seq [(1, "One"); (2, "Two")])
let dc :Generic.Dictionary<_,_>   = fromSeq (seq [(1, "One"); (2, "Two")])
let mp :Map<_,_>                  = fromSeq (seq [(1, "One"); (2, "Two")])
let d  :Generic.IDictionary<_,_>  = fromSeq (seq [("One", 1)])
let ut :Hashtable                 = fromSeq (seq [1,'1';2, '2';3,'3'])
let al :ArrayList                 = fromSeq (seq ["1";"2";"3"])
let us :SortedList                = fromSeq (seq [4,'2';3,'4'])
let cc :BlockingCollection<_>     = fromSeq {'1'..'3'}
let cd :ConcurrentDictionary<_,_> = fromSeq (seq [(1, "One"); (2, "Two")])
let cb :ConcurrentBag<_>          = fromSeq {'1'..'3'}