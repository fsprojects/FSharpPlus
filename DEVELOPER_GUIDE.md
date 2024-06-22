# DEVELOPER GUIDE

### General Goals / Philosophy:

- Consistency: the design of this library should maximize consistency, this is relation between group of functions, argument types, types and abstractions they represent. By doing this, it's easier for the user to guess what are the expectations instead of relaying all the time to check docs and/or source code. Therefore naming should be carefully considered in order achieve this goal.
- Non intrusive: all additions to this library should not change existing functionality of F# language and FSharp.Core. In the cases when it is needed, it must be done in a specific module/namespace that forces to be `open`.
- Non opinionated: as much as possible F#+ is not trying to force the user to solve problems in a specific way, instead it tries to give generic ways to approach a problem from different angles. Specific ways of using this library should be eventually defined by the Organization/Team/User that use F#+ to solve problems in a specific way and in order to achieve that goal. A small set of functions, type aliases can be added on top of F#+ for that purpose, but the library must be as neutral as possible.
- Trust users: F#+ doesn't apply the principle of "removing functions that could be used in a wrong way", it is assumed the user has enough skills to decide what's a good use and what is a bad use of a specific function. It is not the intent of F#+ maintainers and contributors to educate users by hiding stuff, that's not the goal of this library, though there's a lot to learn, but for that there are general guidelines and normally there should be also specific ones for each Organization/Team/User. This doesn't mean that documentation will be poorly maintain.
- Coding style: There is no specific set of rules, but it should stick to general coding conventions with some relaxation in order be able to align code in places where this is convenient. For instance when there's a big set of overloads, this makes it easier to read but also easier to edit the code with multiline editor support. The best advice is to try to copy existing style. For signatures, try to follow [FSharp.Core](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/) design, i.e. `'T` not `'a`.
- Naming commits: Use a very short and descriptive sentence in imperative mood, as if completing the sentence "If applied, this commit will ...". For more details [use this](https://cbea.ms/git-commit) as a reference.
- PR's: try to keep them as atomic as possible, if a PR is found that touches many unrelated areas it will be asked to split in different PRs. A typical example is a PR that adds a specific functionality but also fix bugs, or typos in the docs. If we eventually revert that functionality, the other changes will be reverted as well which might not be desired. Draft PRs are encouraged and feel free to ask for advice while in the writing process. Use the same naming convention as for commits.
- This is F#: although F#+ contains some abstractions inspired in libraries used in other languages, those concepts are translated to F# standards and eventually adapted to fit better in F#.

### Extensions

This library defines many extensions for different types.
Some of these functions are conceptually connected, in that they represent an abstraction and sometimes they have indeed a generic version of the function which doesn't require typing the type as module prefix.
Note that in these cases, normally the names are the same but this is not a rule, there are cases where the name differs to avoid collisions or confusion with other functions, for example:

- `map` is a generic function, and F#+ has many types implementing it, but for dictionaries this correspond to `mapValues`
- `min` is a non-generic function operating in collections, but its generic counterpart is `minimum` to avoid collision with the built-in `min` function (minimum between two values).

So, what this means is, F#+ doesn't provide generic functions based on names, although in many cases names are the same, we need to take into account:

1. F# core has some inconsistencies as it's not a type classes based library. A type-class based library is not necessarily something that implements a trick for type-class, but something that is designed as if we had support for them, it means capturing some generic concepts and making them clear in the names chosen for every function.
2. But F#+ is not attempting to fix F# core but to build on top in a non-intrusive way, and try to re-use all the consistent concepts, idioms and de-facto naming conventions from F# core as much as possible without increasing the inconsistency level already there. Keep in mind, in F#+ **we try to expand FSharp.Core functionalities, but we are trying not to be intrusive**, as stated before: F#+ builds upon FSharp, using generic programming techniques to help avoid boiler plate code. However, by naming conventions and signatures it can be seen to 'enhance' rather than 'replace' existing patterns as much as possible.

So the solution sometimes require some creativity like thinking new names that make it clear what the function does without departing too far from existing naming conventions.

Another interesting case is the `zip` related functions:

- For collections like types, in F# core it's normal to find `zip` / `map2` functions, which acts pairwise. But another possible implementation is the applicative zip which works cross product.
- Here we define 2 generic functions, `lift2` which corresponds always to the applicative instance, so normally in non-collections it will correspond to the non-generic `map2` but since collections already have (or at least it's expected to) a `map2` acting pairwise, in those cases only F#+ provides a `lift2` extension and that's what's used for its generic counterpart.
- The other provided generic function is `zip` which is available mainly for collection like types and although they match the non-generic name, note that the behaviour is not exactly the same, because F# core throws errors for list and arrays when the number of elements are different. In that sense, in fact, it matches `.zipShortest` when defined, otherwise it matches `.zip`.

So these zip related functions are a good example of how F#+ does its best to fix FSharp.Core inconsistencies in a non-intrusive way.


### Abstractions

The abstractions represented in the Control namespace of this library are a set of types that in fact represents generic functions, these types are referred to as Invokables.

These Invokables are organized in such a way that resemble `static interfaces`, `type-classes`, `concepts` or `traits` in other languages where they have a type system construct enabling higher kinds and ad-hoc polymorphism.

In F#+, due to the constraint of targeting CIL which doesn't have first class support for Higher Kinds, we can't enforce that a specific type parameter belongs to a specific abstraction. This way generic type parameters are normally given the name of the abstraction, in the various type signatures that forms the idioms of this library implementation. As an example, the signature for generic `map` is written as `('T -'U) -> '``Functor<'T>``-> '``Functor<'U>`` ` which will be rendered as `('T -'U) -> 'Functor<'T> -> 'Functor<'U>`  where `Functor` is the name of the abstraction and all types which align with a Functor can go there.

Due to that, Invokables don't carry any effect on implementation as they are just arbitrary labels for the generic type parameters, useful to communicate the intention to the developers and users of the library. Still F# type system will normally show some constraints at the method level, but these constraints are sometimes not very helpful in communicating the related abstraction.

The concrete implementation of these abstractions on the other hand is done, minimally, by defining a concrete type (an Invokable) per function.

For example, one of the simplest abstractions `Monoid` has its core implementation split under two types `Plus` and `Zero`.

Those Invokers contains static methods of several kinds:

* concrete overloads, for example Monoid's `Plus` will have overloads of a `Plus` method (named the same as the type defining those) for each of the BCL and FSharp.Core types that ought to support the `Monoid` type class, e.g. one to concatenate two strings, one to concatenate two lists, one to concatenate two arrays, and so on and so forth.
* an _invokable_ method, that should always be defined as `inline`, which embeds the SRTP dispatcher machinery that is used to pick the right overload (whether defined in F#+ or outside of it)
* a default or set of default implementations, those may need to be defined in an intrinsic type extension due to F# compiler implementation details to enable being picked up correctly for types that end up supporting the abstraction when such abstraction comes with a default implementation expressed in terms of composing several functions, those are always defined as `inline`.


#### Concrete implementations

F#+ should add as much concrete implementations for primitive types (types coming from F#+ dependencies which are mainly BCL types and FSharp.Core types) as possible, since the end user won't be able to add them later.

#### Defaults

The goal of default implementations is to allow users of the library to write less code, as an example F#+ expects users to add `Bind` , `Return` to their specific monad types, but not force them to add `Join` although if they do, the code might be more efficient.
Defaults are not intended to be used by developers of F#+, as F#+ should afford writing more code in order to maximize usability of the library. You can read this as "the principle is to allow user to write less boilerplate, and because of that we have to put some boilerplate inside F#+".

#### Invoker

The signature of the Invoker should ideally be the same as the generic function. This will allow Invokers to be used in lieu of functions in scenarios where passing a type leads to more generic code than passing a function.
