# DEVELOPER GUIDE



### Definition of abstraction

The abstractions represented in this library are a set of types that in fact represents generic functions, we call these types and we'll refer to as "Invokables".

This Invokables are organized in such a way that resemble typeclasses, concepts or traits in other languages where they have a type system construct enabling higher kinds and ad-hoc polymorphism.

In F#+, due to the constraint of targetting CIL, which doesn't have first class support for Higher Kinds, we can't enforce that a specific type parameter belongs to a specific abstraction. So generic type parameters are normally given the name of the abstraction, in the various type signatures that forms the idioms of this library implementation.

Due to that, they don't carry any effect on implementation as they are just arbitrary labels for the generic type parameters, useful to communicate the intention to the developers and users of the library. Still F# type system will normall show some constraints at the method level, but these constraints are sometimes not very helpful in communicating the related abstraction.

The concrete implementation of these abstractions on the other hand is done, minimally, by defining a concrete type (an Invokable) per function.

For example, one of the simplest abstractions `Monoid` has it's core implementation split under two types `Plus` and `Zero`, called _Type Class Implementation Function Types_ in this document.

Those _Type Class Implementation Function Types_ define static methods of several kinds:

* concrete overloads, for exemple Monoid's `Plus` will have overloads of a `Plus` method (named the same as the type defining those) for each of the BCL and FSharp.Core types that ought to support the `Monoid` type class, e.g. one to concatenate two strings, one to concatenate two lists, one to concatenate two arrays, and so on and so forth.
* an _invokable_ method, that should always be defined as `inline`, which embeds the SRTP machinery that is used to pick the right overload (whether defined in F#+ or outside of it for implementing type class)
* a default or set of default implementations, those may need to be defined in an instrinsec type extension due to F# compiler implementation details to enable being picked up correctly for types that end up supporting the type class when such type class come with default implementation expressed in terms of composing several functions, those are also always defined as `inline`.

### Implementation idioms

#### Concrete implementations

#### Defaults

#### Invoker
