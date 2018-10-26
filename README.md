FSharpPlus [![Build Status](https://api.travis-ci.org/fsprojects/FSharpPlus.svg?branch=master)](https://travis-ci.org/fsprojects/FSharpPlus) [![Build status](https://ci.appveyor.com/api/projects/status/25ukpc0lamyf7pdx/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/fsharpplus/branch/master)
==========

A complete and extensible base library for F#.

It contains the most requested additions to the F# core library, including:

 - Common FP combinators, generic functions and operators.
 - Extension methods for F# types with consistent names and signatures.
 - Standard Monads: Cont, Reader, Writer, State and their Monad Transformers.
 - Other popular [FP abstractions](//fsprojects.github.io/FSharpPlus/abstractions.html).
 - [Generic Functions and Operators](//fsprojects.github.io/FSharpPlus/reference/fsharpplus-operators.html) which may be further extended to support other types.
 - Generic and customizable [Computation Expressions](//fsprojects.github.io/FSharpPlus/computation-expressions.html).
 - A [generic Math](//fsprojects.github.io/FSharpPlus/numerics.html) module.
 - A true polymorphic [Lens/Optics module](//fsprojects.github.io/FSharpPlus/tutorial.html#Lens).

Users of this library have the option to use their functions in different styles:
 - F# Standard module + function style: [module].[function] [arg]
 - As [generic functions](//fsprojects.github.io/FSharpPlus/generic-doc.html) [function] [arg]
 - As [extension methods](//fsprojects.github.io/FSharpPlus/extension-methods.html) [type].[function] [arg]

In the [Sample folder](//github.com/fsprojects/FSharpPlus/tree/master/src/FSharpPlus/Samples) you can find scripts showing how to use F#+ in your code.

See the [documentation](//fsprojects.github.io/FSharpPlus) for more details.
