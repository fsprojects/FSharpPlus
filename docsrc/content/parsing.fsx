(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
open System
#r @"../../src/FSharpPlus/bin/Release/net46/FSharpPlus.dll"
open FSharpPlus
(**
# Parsing

F#+ provides several helper methods in order to simplify building parsers and parsing like tasks. 
*)

(**
## Parse

Parse allows you to use `parse` generic method for standard types and types that implement a static Parse method with the correct signature.


### Minimal definition

*)

(**
```f#
static member Parse (x:'r) :'T
```
or

```f#
static member Parse (x:'r, c:CultureInfo) :'T
```

*)

(**
## TryParse

TryParse allows you to use `tryParse` generic method for standard types and types that implement a static TryParse method with the correct signature.

### Minimal definition

In order to use `tryParse` together with a type the type needs to implement a TryParse like static method.

*)

(**
You can use F# style TryParse:
```f#
static member TryParse(value:'r) : 'T option
```
or C# style TryParse:
```f#
static member TryParse (x:'r, [<Out>] result: 'T byref) :bool
```
expressed in C# that would be:
```c#
public static bool TryParse (string x, out T result) 
```

A neat thing when you have types that implement the above definition is that it's simple to define active patterns:
*)

let (|Port|_|) : _-> UInt16 option = tryParse
let (|IPAddress|_|) :_->System.Net.IPAddress option = tryParse

(**
## sscanf, trySscanf and friends

In F# you have some nice utility functions for creating printf style string writer function. In F#+ we find the inverse: sscanf and trySscanf.

For instance if you want to parse based on known format of a url:
*)

let route1 x = trySscanf "/api/resource/%d" x
let parsed : int option = route1 "/api/resource/1"
