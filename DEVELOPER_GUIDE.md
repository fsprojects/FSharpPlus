# DEVELOPER GUIDE



### Definition of a type class

The concept of type class is a type system construct enabling ad-hoc polymorphism.

in F#+, due to the constraint of targetting CIL, which doesn't have first class support for type class definition and implementation, those are not defined as F# types; instead, they are referred to in generic type parameters in the various type signatures that forms the idioms of this library implementation.

Due to that, they don't carry any effect on implementation as they are just arbitrary labels for the generic type parameters, useful to the developers and users of the library.

The concrete implementation of them on the other hand is done, minimally, by defining a concrete type per function.

For example, one of the simplest type classes `Monoid` has it's core implementation split under two types `Plus` and `Zero`, called _Type Class Implementation Function Types_ in this document.

Those _Type Class Implementation Function Types_ define static methods of several kinds:

* concrete overloads, for exemple Monoid's `Plus` will have overloads of a `Plus` method (named the same as the type defining those) for each of the BCL and FSharp.Core types that ought to support the `Monoid` type class, e.g. one to concatenate two strings, one to concatenate two lists, one to concatenate two arrays, and so on and so forth.
* an _invoker_ method, that should always be defined as `inline`, which embeds the SRTP machinery that is used to pick the right overload (whether defined in F#+ or outside of it for implementing type class)
* a default or set of default implementations, those may need to be defined in a type extension due to F# compiler implementation details to enable being picked up correctly for types that end up supporting the type class when such type class come with default implementation expressed in terms of composing several functions, those are also always defined as `inline`.

### Implementation idioms

#### Concrete implementations

#### Defaults

#### Invoker
