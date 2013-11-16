FsControl
=========

This is basically an overload library.

The goal is to create overloads of standard F# types, organized into 'type methods' to be used from other projects/libraries.

Type methods are similar to type classes but with a single method.

It is also possible to create libraries (like [FSharpPlus](https://github.com/gmpl/FSharpPlus)) that define their own custom types and add overloads for those types, making them instances of the type methods defined in FsControl.

You may find hard to understand how to use FsControl, there is no documentation at the moment because the systax is not 100% final, so the best is to have a look at the samples, if you just want to use the generic functions use Functions.fsx or [FSharpPlus](https://github.com/gmpl/FSharpPlus).