FSharpPlus [![Build Status](https://api.travis-ci.org/gusty/FSharpPlus.svg?branch=master)](https://travis-ci.org/gusty/FSharpPlus)
==========

A complete and extensible base library for F#.

It contains the most requested additions to the F# core library, including:

 - Common FP combinators, generic functions and operators.
 - Extension methods for F# types with consistent names and signatures.
 - Standard Monads: Cont, Reader, Writer, State and their Monad Transformers.
 - Other popular [FP abstractions](//gusty.github.io/FSharpPlus/abstractions.html).
 - [Generic Functions and Operators](//gusty.github.io/FSharpPlus/reference/fsharpplus-operators.html) which may be further extended to support other types.
 - Generic and customizable Computation Expressions.
 - A generic Math module.
 - A true polymorphic [Lens/Optics module](//gusty.github.io/FSharpPlus/tutorial.html#Lens).
 - A Haskell compatibility module.

Users of this library have the option to use their functions in different styles:
 - F# Standard module + function style: [module].[function] [arg]
 - As extension methods [arg].[function]
 - As generic functions [function] [arg]

In the [Sample folder](//github.com/gusty/FSharpPlus/tree/master/src/FSharpPlus/Samples) you can find scripts showing how to use F#+ in your code.

See the [documentation](//gusty.github.io/FSharpPlus) for more details.
