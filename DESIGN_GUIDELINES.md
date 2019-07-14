# DESIGN GUIDELINES


F#+ is an F# base library intended for production use, so the design of this library should be adjusted to the following guidelines:

### Considerations before adding
-new functionality

 - Does it belong to this F#+ or to a separate library, dependent or not on F#+?
 Most likely if it's something technology specific it will not have a place here, whereas if it's something related to a language extension this might be a good place. There are situations that fall in the middle. Parsing is one example, at the moment we consider Json or other format parsing not part of this library but we're dealing with F# values parsing.
 
 - Should we add experimental, exotic code to this library?
 This is a trick question.
 On one side, this library is aimed at production code, but also should be taken into account that we rely on users understanding what are they putting into production. Code qualities like performance, readability and scalability are not being enforced here by hiding functions. This in conjunction with the fact that the concept of "production code" is a bit vague in terms of different scenarios, for instance there are production code scenarios where performance is not a goal, nor scalability.

### Naming

 - General F# guidelines and naming conventions apply here.
 - For new functions or operators, try to match first existing de-facto conventions in F# (ie: `map2` instead of `unzip`).
 - If there is no possibility to relate the name with an existing F# function name, we look in other languages, prefereably FP first languages.
 - Generic functions should be named the same as module specific ones, ie: `map`, not `fmap`.
 
### Addition of new functions

 - In contrast to F# core, we don't apply the principle of encouraging good practices by hiding away stuff, namely useful functions that are at evil at the same time, ie: `curry`. The end user is responsibe for good use of the functions.
 - Generic functions should ideally have a corresponding non-generic counterparts, ie `map3` ==> `Option.map3` - `Result.map3`.
 - Generic functions should ideally relate to rules, not neccesary Category Theory.
 - Remember to design related functions in a consitent way across different types, even if that lead to a function that is not as efficient. Performance is a priority but we don't assure performance by hiding away stuff.
 
### Modules

 - Generic operators and functions should go in the FSharpPlus.Operators module unless they conflict each other or with existing F# functionality, in which case they should go in a module that requires explicit opening.
 - New types/collections should be added in a separate file, ideally having a module with let-bound functions as it's in general more F# idiomatic. Some static members, required to satistfy static constraints for generic abstractions should be added when the type implement that abstraction, but if it expose a let-bound function with the same functionality, the static member can be hidden from the tooling with an attribute.
