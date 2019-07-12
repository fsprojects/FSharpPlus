# DESIGN GUIDELINES


F#+ is an F# base library intended for production use, so the design of this library should be adjusted to the following guidelines:

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
