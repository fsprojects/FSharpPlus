#r @"..\bin\Release\FsControl.Core.dll"

open System
open FsControl.Core.TypeMethods.Converter

let inline internal fromBytesWithOffset (startIndex:int) (value:byte[]) = Inline.instance FromBytes (value, startIndex)
let inline internal fromBytes                            (value:byte[]) = Inline.instance FromBytes (value, 0         )
let inline internal toBytes value :byte[] = Inline.instance (ToBytes, value) ()
let inline internal parse  (value:string) = Inline.instance Parse value
let inline internal toString value : string = Inline.instance (ToString, value) ()

let rMTF = [parse "Monday" ; DayOfWeek.Thursday; DayOfWeek.Friday]
let r110 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy|] + 100.
let r120 = parse "10" + fromBytes [|10uy;0uy;0uy;0uy;|]                + 100
let r123 = toString [1;2;3]
let r140 = toString (1,4,0)
let r150 = toString (Some 150)
let r160 = toString ([1;6;0] :> _ seq)
let r170 = toString (new ResizeArray<_>([1;7;0] :> _ seq))
let r180 = toString (Set [1;8;0])
let r190 = toString [|1;9;0|]
let r200 = toString [|([1;2;3] :> _ seq);([4;5;6] :> _ seq);([7;8;9] :> _ seq)|]
let r210 = toString (Map  ['a',2; 'b',1; 'c',0])
let r220 = toString (dict ['a',2; 'b',2; 'c',0])