(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
open FSharpPlus

(**
Extension Methods
=================

*)

(**

Some methods are also exposed as extensions. This makes possible some uses from C#

Here are some examples:

*)
(**
```f#
#r @"nuget: FSharpPlus"
```
*)
open FSharpPlus.Extensions

let opt  = Option.Sequential [Some 1; Some 2]
let asn = Async.Sequential [| async {return 1}; async {return 2} |]