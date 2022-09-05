(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

(**
DList
=========================

DList is an ordered linear structure implementing the List signature (head, tail, cons), 
end-insertion (add), and O(1) append. Ordering is by insertion history.
DList is an implementation of [John Hughes' append list](http://dl.acm.org/citation.cfm?id=8475).


Examples
--------
*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open FSharpPlus
open FSharpPlus.Data

(**
### Constructing DLists
*)
// you can construct a DList by using ofSeq
let list123 = DList.ofSeq [ 1; 2; 3 ]

let listEmpty = DList.empty
// cons
let list2 = DList.cons 100 list123 
// append two DLists
let list3 = DList.append list2 (DList.singleton 200)
// this can be written as (since list2 is a DList):
let list3' = plus list2 (result 200)
// in order to get back to a regular list you can then use toList:
let list4 = toList list3'

(**
### Operations on DList
*)

let lengthOfList3 = DList.length list3
let lengthOfList3' = length list3

let headOf3 = DList.head list3 
let headOf3' = head list3 
