(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FSharpPlus
======================
F#+ is a base library that aims to take F# to the next level of functional
programming. 

*What if we imagine F# as more than it is?*

F#+ builds upon FSharp, using generic programming techniques to help
you avoid boiler plate code.  However, by naming conventions and signatures
it can be seen to 'enhance' rather than 'replace' existing patterns as much
as possible.

The additions can be summarised as:

 * [Extensions](extensions.html) to core types, such as [`String.toLower`](reference/fsharpplus-string.html)

 * [Generic Functions and Operators](generic-doc.html) like `map`, which can be extended to support other types

 * Generic and customizable [Computation Expressions](computation-expressions.html),
   like `monad`

 * A generic [Math Module](numerics.html)

 * [Abstractions](abstractions.html) that capture common FP patterns, such as
   the standard monads Cont, Reader, Writer, State and their Monad Transformers

 * Some new types that work well with the abstractions, such as NonEmptyList,
   DList and Validation

 * A polymorphic [Lenses/Optics](tutorial.html#Lens) to easily read and update
   parts of immutable data

Note, however, that F#+ does not go into solving a specific thing for a specific
technology, such as JSON parsing.

Some functions are available as [extension methods](extension-methods.html)
so are callable from C#. Note that this is not complete, or currently considered high priority.

Getting started is easy since you can start with enjoying some of the extensions
and generic functions, but you will find other parts of F#+ unfold before you
and become useful the deeper in you get.

Example 1
---------

This example demonstrates using an extension function defined in this library.

*)
#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus

let x = String.replace "old" "new" "Good old days"
// val x : string = "Good new days"

(**

Example 2
---------

This example demonstrates using a generic function defined in this library.

*)

map string [|2;3;4;5|]
// val it : string [] = [|"2"; "3"; "4"; "5"|]

map ((+) 9) (Some 3)
// val it : int option = Some 12

open FSharpPlus.Data

map string (NonEmptyList.create 2 [3;4;5])
// val it : NonEmptyList<string> = {Head = "2"; Tail = ["3"; "4"; "5"];}

(**

For a more hands on run through F#+ we recommend following the tutorial:

 * [Tutorial](tutorial.html) contains a further explanation of this library.


Reference Documentation
-----------------------

 * [Types](types.html) contains detailed information about all the types provided in this library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.

Samples
-----------------------

This documentation is automatically generated from `*.fsx` files in [the content folder][content]. 
It can be useful to clone a local copy to review.

The [API reference](reference/index.html) is automatically generated from
Markdown comments in the library implementation.
 
Also of note is the [Sample folder][samples]
which contains sample scripts showing how to use F#+ in your code.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

If you're adding a new public API, please also consider adding [documentation][content].
You might also want to read the [library design notes][design] to understand how it works.

The library is available under Apache License, Version 2.0, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharpPlus/tree/master/docsrc/content
  [samples]: https://github.com/fsprojects/FSharpPlus/tree/master/src/FSharpPlus.Docs/Samples
  [gh]: https://github.com/fsprojects/FSharpPlus
  [issues]: https://github.com/fsprojects/FSharpPlus/issues
  [readme]: https://github.com/fsprojects/FSharpPlus/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharpPlus/blob/master/LICENSE.txt
  [design]: https://github.com/fsprojects/FSharpPlus/blob/master/DESIGN_GUIDELINES.md
*)
