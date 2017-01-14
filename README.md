FSharpPlus [![Build Status](https://api.travis-ci.org/gmpl/FSharpPlus.svg?branch=master)](https://travis-ci.org/gmpl/FSharpPlus)
==========

This project contains some extensions for F#.

 - Extension methods for common F# types.
 - Collections.
 - Common FP abstrations: Cont, Reader, Writer, State and their Monad Transformers.
 - Generic Computation Expressions and Linq Builders.
 - Common FP combinators, generic functions and operators.
 - A generic Math module.
 - A Lens module.
 - A Haskell compatibility module.

F#+ types contains methods which are added as extensions and at the same time are made instances of the existing Type/Methods defined in [FsControl](https://github.com/gmpl/FsControl) integrating them with existing .NET types.

As an example, there is a <code>ParallelArray</code> type which can act as a Functor (instance for Map), an Applicative (instances for Return and Apply) and a Monoid (Empty and Append).

Based on FsControl, F#+ provides Generic Functions and Operators which may be further extended to support other types.

In the [Sample folder](https://github.com/gmpl/FSharpPlus/tree/master/FSharpPlus/Samples) you can find scripts showing how to use F#+ in your code.
