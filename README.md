FSharpPlus [![Build Status](https://api.travis-ci.org/gusty/FSharpPlus.svg?branch=master)](https://travis-ci.org/gusty/FSharpPlus)
==========

This project contains some extensions for F#.

 - Common FP combinators, generic functions and operators.- Extension methods for common F# types.
 - Collections.
 - Common FP abstration types: Cont, Reader, Writer, State and their Monad Transformers.
 - Generic Computation Expressions and Linq Builders.
 - A generic Math module.
 - A full Lens/Optics module.
 - A Haskell compatibility module.

F#+ types contains methods which are added as extensions and at the same time are made instances of the existing 'Invokables' defined in FsControl Namespace, integrating them with existing .NET types.

It also provides [Generic Functions and Operators](http://gusty.github.io/FSharpPlus/reference/fsharpplus-operators.html) which may be further extended to support other types.

As an example, there is a <code>NonEmptyList</code> type which can act as a Functor (instance for Map), an Applicative (instances for Return and Apply) and a Semigroup (Append). So code like ``map f lst`` or ``lst1 ++ lst2`` will work on primitive types (list, seq) but also on ``NonEmptyList``.

In the [Sample folder](https://github.com/gusty/FSharpPlus/tree/master/src/FSharpPlus/Samples) you can find scripts showing how to use F#+ in your code.

See the [documentation](http://gusty.github.io/FSharpPlus) for more details.
