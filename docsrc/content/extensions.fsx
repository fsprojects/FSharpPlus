(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

(**
Extensions
=======================
Extensions are what you probably expect: helper functions for existing types.
 
They are defined as modules with the same name as the types they operate on
under the FSharpPlus namespace, so can be accessed via:
*)
open FSharpPlus

(**
Some functions are common across foldable types such as `intercalate` on
List, Array and Seq, and others are common across wrapping containers,
such as `map`, `bind` and `apply` on List, Array, and Seq, but also Option and Result.

Construction:
=============
The `singleton` function is already defined for Seq, Array and List, but F#+ adds it for Enumerator:

 * Enumerator.singleton - construct a container with the given value inside it

To construct MonadError instances (Result or Choice) you can use result/throw:

 * Result.result / Choice.result - construct with the given value (as Ok or Choice1Of2)
 * Result.throw / Choice.throw - construct an error from the given value (as Error or Choice2of2)

It's also possible to construct by wrapping exception producing functions:

 * Option.protect - returns None on exception
 * Result.protect - returns Error with exception value on exception
 * Choice.protect - returns Choice2Of2 with exception value on exception
*)
// throws "ArgumentException: The input sequence was empty."
let expectedSingleItem1 : int = List.exactlyOne []

// returns a Result.Error holding the exception as its value:
let expectedSingleItem2 : Result<int,exn> = Result.protect List.exactlyOne []

// ...or like typical try prefixed functions, treat exception as None
let expectedSingleItem3 : Option<int> = Option.protect List.exactlyOne []

// which might look like this:
let inline tryExactlyOne xs = Option.protect List.exactlyOne xs

(**
Deconstruction (unwrapping):
============================
Some extensions on Result are designed to behave like Option:

 * Result.get - unwraps the value when it is an 'ok, otherwise throws an exception
 * Result.defaultValue - return the 'ok value if present, otherwise the default value
 * Result.defaultWith - return the 'ok value if present, otherwise apply the given function
   to the 'error value

To deconstruct MonadError instances (Result or Choice) use:

 * Result.either - unwraps the result by applying the given `ok` or `err` function as appropriate
 * Choice.either - unwraps the choice by applying the given `choice1` or `choice2` function as appropriate

Note that there is also the generic `either` operator function that works
exactly the same as `Result.either`.

Also, see the generic function [`option`](reference/fsharpplus-operators.html) that
unwraps an Option in a similar way to `either`.

On Foldables
============
Foldables are the class of data structures that can be folded to a summary value.
Most collections, or specifically 'foldable' instances implement these:

 * intersperse - takes an element and `intersperses' that element between the elements

*)
let a = ["Bob"; "Jane"] |> List.intersperse "and"
// vat a : string list = ["Bob"; "and"; "Jane"]

let b = "WooHoo" |> String.intersperse '-'
// val b : string = "W-o-o-H-o-o"

(**
 * intercalate - insert a list of elements between each element and flattens
*)
let c = [[1;2]; [3;4]] |> List.intercalate [-1;-2];;
// val c : int list = [1; 2; -1; -2; 3; 4]

let d = ["Woo"; "Hoo"] |> String.intercalate "--o.o--";;
// val d : string = "Woo--o.o--Hoo"

(**
 * zip/unzip - tuple together values inside two containers, or untuble tupled values

On Monad/Functor/Applicatives
=============================
Types that implement these will (typically) have these functions defined:

 * map - apply a mapping function to the value inside a container
 * bind - take a contained value, and apply a function that produces another contained value
 * apply - like map but where the mapping function is also inside a container

These can also be invoked from the generic functions without module prefix as per
[generic functions & operators](reference/fsharpplus-operators.html).

Flatten:
========
Flatten can be used when a container has another container inside it:

 * Choice.flatten
 * Result.flatten
 * Option.flatten (already defined in FSharp Core)

Note that on traversable types like List, Array and Seq, FSharp Core uses the
more common `concat` for flatten and so this naming is continued for Enumerable:

 * Enumerable.concat

Partitioning:
=============
Partitioning can be done by applying a separating function that produces a Choice:

 * Array.partitionMap
 * List.partitionMap
*)
let isEven x = (x % 2) = 0
let chooseEven x = if isEven x then Choice1Of2 x else Choice2Of2 x

let e = [1; 2; 3; 4] |> List.partitionMap chooseEven
// val e : int list * int list = ([2; 4], [1; 3])

(**
Conversion functions:
=====================
F#+ adds functions to convert between Result, Choice and Option types.

These should be self explanatory, but be aware that sometimes they are 'lossy'
usually when converting to Option:
*)
(**
// Convert a `Result` to an `Option` - effectively throws away error value
// when present, by replacing with `None`
```f#
request |> validateRequest |> Option.ofResult
```
*)

(**
Going the other way is similar, but a value needs to be filled in for None:
*)
let xs = ["some value"]
let firstElementOption = xs |> List.tryHead

// Convert an `Option` to a `Result` will use unit as the Error:
firstElementOption |> Option.toResult

// ...but you can specify an error value with Option.toResultWith:
firstElementOption |> Option.toResultWith "No Element"

(**
Converting between `Choice` and `Result` is often useful:
```f#
let asyncChoice = anAsyncValue |> Async.Catch |> Async.map Result.ofChoice
```
*)

(**
The String type:
================

 * [ String ](reference/fsharpplus-string.html)
   * intercalate, intersperse, 
   * split, replace
   * isSubString, startsWith, endsWith, contains
   * toUpper, toLower
   * trimWhiteSpaces
   * normalize
   * removeDiacritics
   * padLeft, padLeftWith, padRight, padRightWith
   * trim, trimStart, trimEnd
   * item, tryItem
   * rev
   * take, skip takeWhile, skipWhile
   * truncate, drop
   * findIndex, tryFindIndex
   * findSliceIndex, tryFindSliceIndex
   * toArray, ofArray, toList, ofList, toSeq, ofSeq, toCodePoints, ofCodePoints
   * getBytes

Collections / Traversable types:
=================================
 * [Array](reference/fsharpplus-array.html)
   * intercalate, intersperse,
   * split, replace,
   * findSliceIndex, trySliceIndex,
   * partitionMap
 * [IList](reference/fsharpplus-ilist.html)
   * toIReadOnlyList
 * [List](reference/fsharpplus-list.html)
   * singleton,
   * cons,
   * apply,
   * tails, take, skip, drop,
   * intercalate, intersperse, 
   * split, replace,
   * toIReadOnlyList,
   * findSliceIndex, tryFindSliceIndex,
   * partitionMap
   * setAt, removeAt
 * [Enumerator](reference/fsharpplus-enumerator.html)
   * EmptyEnumerator
      * Empty - create an empty enumerator
   * ConcatEnumerator
      * concat
   * MapEnumerators
      * map, mapi, map2, mapi2, map3
   * singleton
   * tryItem, nth
   * choose
   * filter
   * unfold
   * upto
   * zip, zip3
 * [ Seq ](reference/fsharpplus-seq.html)
    * bind, apply, foldback
    * chunkBy
    * intersperse, intercalate,
    * split, replace
    * drop
    * replicate
    * toIReadOnlyList
    * findSliceIndex, tryFindSliceIndex
 * [ IReadOnlyCollection ](reference/fsharpplus-ireadonlycollection.html)
     * ofArray, ofList, ofSeq
     * map
 * [ IReadOnlyList ](reference/fsharpplus-ireadonlylist.html)
   * ofArray, toArray
   * trySetItem, tryItem
 * [ Map ](reference/fsharpplus-map.html)
   * keys, values
   * mapValues, mapValues2
   * zip, unzip
   * unionWith, union, intersectWith, intersect
 * [ Dict ](reference/fsharpplus-dict.html)
   * toIReadOnlyDictionary
   * tryGetValue
   * containsKey
   * keys, values
   * map, map2
   * zip, unzip
   * unionWith, union, intersectWith, intersect
 * [ IReadOnlyDictionary ](reference/fsharpplus-ireadonlydictionary.html)
   * add, 
   * tryGetValue, containsKey,
   * keys, values,
   * map, map2, 
   * zip, unzip, 
   * unionWith, union, intersectWith, intersect

Async and Tasks:
================
 * [ Task ](reference/fsharpplus-task.html)
   * map, map2, map3
   * apply
   * zip
   * join
   * ignore
 * [ Async ](reference/fsharpplus-async.html)
   * map, map2
   * zip
   * join
   * apply
   * raise

Option, Choice and Result types:
================================
 * [Option](reference/fsharpplus-option.html)
   * apply, 
   * unzip, zip,
   * toResult, toResultWith, ofResult, 
   * protect
 * [Choice](reference/fsharpplus-choice.html)
   * result, throw - construct a Choice
   * bind, apply, flatten,
   * map,
   * catch, - deprecated
   * bindChoice2Of2,
   * either,
   * protect
 * [ Result ](reference/fsharpplus-result.html)
   * result, throw - construct a Result
   * apply, (map, bind already defined)
   * flatten,
   * bindError,
   * either,
   * protect,
   * get,
   * defaultValue, defaultWith,
   * toChoice, ofChoice,
   * partition

Extensions Methods (on existing types):
=======================================
These are usable from C#

 * [ Extension Methods ](extension-methods.html)
   * IEnumerable<'T'>.GetSlice
   * List<'T>.GetSlice
   * Task<'T>.WhenAll
   * Async.Sequence - of seq, list or array
   * Option.Sequence - of seq

*)
