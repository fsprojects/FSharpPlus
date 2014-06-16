FSharpPlus [![Build Status](https://api.travis-ci.org/gmpl/FSharpPlus.svg?branch=master)](https://travis-ci.org/gmpl/FSharpPlus)
==========

This project contains some extensions for F#.

 - Extension methods for existing types.
 - New types.
 - Generic Computation Expressions and Linq Builders.
 - Generic Functions and Operators.
 - A generic Math module.

FSharpPlus (F#+) makes the new types instances of the existing TypeMethods defined in [FsControl](https://github.com/gmpl/FsControl) integrating them with existing .NET types.

As an example, there is a <code>ParallelArray</code> type which can act as a Functor (instance for Map), an Applicative (instances for Pure and Apply) and a Monoid (Mempty and Mappend).

At the same time, F#+ provides Generic Functions and Operators, most of them based in direct calls to FsControl's TypeMethods or common operations for the abstractions represented there.

In the Sample folder you can find examples about how to use F#+ in your code.



