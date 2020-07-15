# DEVELOPER GUIDE



### Definition of abstraction

The abstractions represented in the Control namespace of this library are a set of types that in fact represents generic functions, we call these types and we'll refer to as "Invokables".

These Invokables are organized in such a way that resemble typeclasses, concepts or traits in other languages where they have a type system construct enabling higher kinds and ad-hoc polymorphism.

In F#+, due to the constraint of targeting CIL which doesn't have first class support for Higher Kinds, we can't enforce that a specific type parameter belongs to a specific abstraction. So generic type parameters are normally given the name of the abstraction, in the various type signatures that forms the idioms of this library implementation.

Due to that, they don't carry any effect on implementation as they are just arbitrary labels for the generic type parameters, useful to communicate the intention to the developers and users of the library. Still F# type system will normally show some constraints at the method level, but these constraints are sometimes not very helpful in communicating the related abstraction.

The concrete implementation of these abstractions on the other hand is done, minimally, by defining a concrete type (an Invokable) per function.

For example, one of the simplest abstractions `Monoid` has its core implementation split under two types `Plus` and `Zero`.

Those Invokers contains static methods of several kinds:

* concrete overloads, for example Monoid's `Plus` will have overloads of a `Plus` method (named the same as the type defining those) for each of the BCL and FSharp.Core types that ought to support the `Monoid` type class, e.g. one to concatenate two strings, one to concatenate two lists, one to concatenate two arrays, and so on and so forth.
* an _invokable_ method, that should always be defined as `inline`, which embeds the SRTP dispatcher machinery that is used to pick the right overload (whether defined in F#+ or outside of it)
* a default or set of default implementations, those may need to be defined in an instrinsic type extension due to F# compiler implementation details to enable being picked up correctly for types that end up supporting the abstraction when such abstraction comes with a default implementation expressed in terms of composing several functions, those are always defined as `inline`.

### Implementation idioms

#### Concrete implementations

We should add as much concrete implementations for primitive types as possible, since the end user won't be able to add them later.

#### Defaults

The goal of defaults is to allow users of the library to write less code, but their are not intended to be used by developers of F#+ as we should afford writing more code in order to maximize usability of the library.

#### Invoker

The signature of the Invoker should be ideally the same as the end function. This will allow Invokers to be used in lieu of functions in scenarios where passing a type leads to more generic code than passing a function.
