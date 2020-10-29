# DEVELOPER GUIDE

### Extensions

This library defines many extensions for different types.
Some of these functions are conceptually connected, in that they represent an abstraction and sometimes they have indeed a generic version of the function which doesn't require typing the type as module prefix.
Note that in these cases, normally the names are the same but this is not a rule, there are cases where the name differs to avoid collisions or confusion with other functions, for example:

 - `map` is a generic function, and we have many types implementing it, but for dictionaries this correspond to `mapValues`
 - `min` is a non-generic function operating in collections, but its generic counterpart is `minimum` to avoid collision with the built-in `min` function (minimum between two values).
 
So, what this means is, we're not providing generic functions based on names, although in many cases names are the same, we need to take into account:
 
 - F# core has some inconsistencies as it's not a type classes based library. A typeclasses based library is not necessarily something that implments a trick for typeclasses, but something that is designed as if we had support for them, I mean capturing some generic concepts and making them clear in the names choosen for every functions.

 - But here we're not attempting to fix F# core, we want to build on top in a non intrusive way, and try to re-use all the consistent concepts, idioms and de-facto naming conventions from F# core as much as possible without increasing the inconsistency level already there.

So the solution sometimes require some creativity like thinking new names that make it clear what the function does without departing too far from existing naming conventions. 
 
Another interesting case is the `zip` related functions:

 - For collections like types, in F# core it's normal to find `zip` / `map2` functions, which acts pairwise. But another possible implementation is the applicative zip which works cross product.
 - Here we define 2 generic functions, `lift2` which corresponds always to the applicative instance, so normally in non-collections it will correspond to the non-generic `map2` but since collections already have (or at least it's expetcted to) a `map2` acting pairwise, in those cases only we provide a `lift2` extension and that's what's used for its generic counterpart.
 
 - The other generic function we provide is `zip` which is available mainly for collection like types and although they match the non-generic name, note that the behavior is not exactly the same, because F# core throws errors for list and arrays when the number of elements are different. So we could say that in fact it matches `.zipShortest` when defined, otherwise it matchs `.zip`.


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



#### Concrete implementations

We should add as much concrete implementations for primitive types as possible, since the end user won't be able to add them later.

#### Defaults

The goal of defaults is to allow users of the library to write less code, but their are not intended to be used by developers of F#+ as we should afford writing more code in order to maximize usability of the library.

#### Invoker

The signature of the Invoker should be ideally the same as the end function. This will allow Invokers to be used in lieu of functions in scenarios where passing a type leads to more generic code than passing a function.
