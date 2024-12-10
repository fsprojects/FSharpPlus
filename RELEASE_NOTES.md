#### 1.7.0  - December 15 2024
 - ZipApplicative docs
 - Add missing iter functions to Extensions
 - Fix issue with iteration of dictionaries
 - Removed String.startsWith !FABLE_COMPILER condition
 - Add NonEmptyList and NonEmptySeq Helper Functions
 - Add pick related functions to SeqT
 - Monad instance for Vector and Matrix

#### 1.6.1  - February 13 2024
 - Add Non-sequential-Applicative operators and computation expressions
 - Add proper SeqT implementation
 - Matrix and Vector types implement IEnumerable's and IReadOnly's interfaces
 - Add limited applicative support to dictionaries
 - Add (forward) tee operator (|-)
 - Add "mapsquared" pipe operators (|>>>) and (<<<|)
 - Add some functions, interfaces and optimizations to DList<'T>
 - Add findLastSliceIndex and tryFindLastSliceIndex
 - Add Result.Sequence
 - Add Result.iterError
 - Add Validation.ofOptionWith
 - Add List.chunkBy
 - Rename Sequence overloads to Sequential
 - Add Async.Await Async.AsTask and more overloads to Async.Sequential
 - Add empty and isEmpty to IReadOnlyCollection and Exception module
 - Fix bug in lift3 for List and Array
 - Type inference for generic traversals slightly improved

#### 1.5.0  - October 15 2023
 - Support for Fable 4 (some functions had to be removed from Fable in order to it)
 - More IList and IReadOnlyList functions
 - Bug fixes in parse, tryParse, seq's TryFinally and (=>>) for ValueTask
 - Add Free.hoist
 - Add distinct for NonEmptySeq and NonEmptyList
 - Add ValueOption.ofOption and Task.result for Task and ValueTask

#### 1.4.0  - February 22 2023
 - Additional Alternatives available (functions, error monads)
 - More IReadOnlyDictionary functions
 - Bug fixes in Map as FoldIndexable and missing <*> for IDictionary and IReadOnlyDictionary
 - Deprecate IReadOnlyDictionary.map
 - Guid to/from bytes conversion

#### 1.3.3  - February 5 2023
 - Fix missing zero overload for voption
 - Add (>>=) and (>=>) to ReaderT
 - Add ValueOption.toOption
 - Deprecate (<**>)

#### 1.3.2  - December 2 2022
 - Applicative Computation Expressions
 - Support for ValueOption, ValueTask and ValueTuple
 - Possibility to use explicit type parameters (plus, Seq.sum, guard, ofBytes, ofBytesWithOptions, ofBytesBE, parse, tryParse)
 - Use InlineIfLambda attribute in CEs
 - Small fixes (add lift3 for tuples, fix join for ref tuple and Free.map3)
 - Small improvements in type inference
 - Use F# Core 6.0.6
 - Speed up List and Array extensions using Collectors
 - Use FSharp.Core implementations for old functions that were adopted there
 - Add some missing Option, Result zip functions
 - Add explicit type parameters
 - Use InlineIfLambda attribute in CEs
 - Some missing Option, Result zip functions
 - Add NonEmptyList.sequence
 - Improve null handling for Array extensions

#### 1.2.4  - May 5 2022
- Fix: Fable regressions
- Add additional applicative operators directly to types

#### 1.2.3  - March 31 2022
- Fix: upcoming problem with new .NET6 overloads
- Add Choice method and reduce function for NonEmptySeq

#### 1.2.2  - October 7 2021
- Bugfix: Seq.drop returns an empty sequence instead of intended output

#### 1.2.1  - July 16 2021
- Bump Fable to 3.2.6
- Fable compatibility for un/curry functions and CE expressions
- Add missing Lift2, Lift3, lift3 and use MergeSources3
- Add bindError function
- Bug fixes: tryParseArray and Result.map3 signature

#### 1.2.0  - June 6 2021
- Update to compile with Fable 3
- Allow specialized builders for generic CEs
- Add new types: NonEmptySeq, NonEmptySet, NonEmptyMap and MultiMap
- Integrate Task in strict CEs
- Add Mapi Support in NonEmpty List
- Add Cont.eval / ContT.eval
- Builder for NonEmptySeq
- Add choose function to Map and Dictionaries modules (in Extensions)
- Add choosei to several modules
- Add zipShortest function and use that to make generic Zip safe for collections
- Add map2Shortest, a safe map2 variant to several modules
- Add missing map3/lift3 extensions
- Add TryLast to Foldable and introduce operator tryLast
- Add tryHead and tryLast to String module (in the Extensions namespace)
- Add yield! to monad.plus
- Add SequenceBiApply and partition to Validation
- Add indexer optic for List and Array
- Rename Parse active pattern to Parsed
- Include internal error in Result.get
- Add support for %o %x %X and %B to scan functions
- Bugfix: Evaluate traverse left to right
- Use list instead of array for the generic implementation of sequence
- Better hiding of internals in parsing

#### 1.1.7  - January 7 2021
- Fix short-circuiting Traverse

#### 1.1.6  - December 7 2020
- Fix signature of Free.fold
- Update Task extensions so they can handle short-circuit, exceptions and cancellations
- Fix String.drop function

#### 1.1.5  - November 22 2020
- Use list internally instead of array for the generic implementation of sequence
- Make `<*>` for functions evaluate side-effects left-to-right
- Remove BindReturn from monad computation expression in order to avoid type inference issues in F#5

#### 1.1.4  - October 7 2020
- Fix: `TryWith` for State and Reader, `Filter`, `DistinctBy`, `SortBy` and `SortByAscending` for user defined types.
- Compile-time safety for try-blocks and while-loops in generic computation expressions.
- Reduce allocations in NonEmptyList builder implementation.
- Add support for clean signatures to all Indexables.
- Improved, fixed and additional documentation in the extensions namespace.

#### 1.1.3  - July 19 2020
* Add map2/lift2 for all monad transformers
* Add Tuple2 and Tuple3 extensions
* Add non-generic <!> operator for Validation, mainly intended to be used for applicative validation in Fable

#### 1.1.1  - May 10 2020
* Fix: issue with generic intercalate function
* Fix: traverse for seq of asyncs

#### 1.1.0  - April 14 2020
* Fix: issue with equality for DLists in Fable
* Add Nullable module and map and bind overloads for Nullable
* Add <= and >= Applicative Math operators

#### 1.1.0-RC4  - March 31 2020
* Fix: issue with Fable consumption of the library
* Bitraversable abstraction
* Fix: compatibility issue with 1.0 in a separate branch (from which it will be released)
* Generic lift2 function made extensible and usable from F#5 preview's applicative CEs
* Polymorphic itemX and mapItemX function working in Fable
* More Traverse/Sequence overloads for extensions
* Add Dictionary/Lazy extensions and some additionals for ResizeArray
* Convert NonEmptyList builder empty list error in a compile-time error
* ZipList as Alternative
* ZipList without Equality
* Conversion functions for NonEmptyList, replacing Unchecked module from previous RC
* Fix: zero and empty for DList
* Fix: defaults for zero function
* Fix: ofSeq/ofList for IEnumerables
* Rename LeftZero for Traversable/Alternative, fixed some issues and add some instances

#### 1.1.0-RC3  - March 5 2020
* Retarget Fable compilation to Extensions, some types from Data and abstractions Comonad, Invariant and Bind
* Lens new functions: choosed and non
* Fix: _item (from Lens)
* Generic choice function made extensible and already working with semigroups
* More operations on NonEmptyList and nel builder
* Added missing operations for Computation Expressions to some types
* Fix: signature for curry, uncurry and State.modify
* Fix: traverse operation when derived from sequence operation
* Short-circuit mechanism for Traversable and Alternatives
* More extension functions: Result.bindError, Choice.bindChoice2Of2 and Array.apply
* Fix and rename optional function to opt
* Rename liftA2 to lift2
* More ZipFunctor instances and better Unzip internals

#### 1.1.0-RC2  - January 25 2020
* Some tweaks to Free (internals)
* Add functions gets and modify for stateful computations
* Better type inference for lift
* Some functions deprecated in Validation and Extensions
* Fix: swap type parameteres of Bifunctor and Bifoldable for Choice / Result
* Default Monoid for bool (xor)
* Some minor bug fixes
* Better signatures and xml descriptions

#### 1.1.0-RC1  - January 13 2020
* More Fable support
* Free Monad
* Functor's Coproduct
* Bifoldable abstraction: http://fsprojects.github.io/FSharpPlus/abstraction-bifoldable.html
* deprecating `Validation.biFoldBack`
* Some Choice and Result functions
* tap function
* Rename IReadOnlyList.add to trySetItem
* Improved lift for Monad Transformers

#### 1.1.0-CI00272  - September 14 2019
* Initial Fable support for Extensions
* Add generic bind, try/findIndex and try/findSliceIndex functions
* Traverse and TraverseIndexed for Map<_,_>
* Lens: fix foldOf and add maximumOf/minimumOf

#### 1.1.0-CI00271  - August 15 2019
* Add intersect functions for Map/Dictionaries
* Add polyvariadic memoizationN, curryN/uncurryN functions
* Add polyvariadic parsing functions (scanf family)
* Add Dict.containsKey, IReadOnlyDictionary.containsKey
* More lens for Map and Set
* Extend String and Task modules
* Task as ZipFunctor and Applicative
* Improve performace for union on dictionaries and maps
* Add unary negation operator for applicative maths
* Fix: add missing CE members to Monad Transformers
* Fix: infinite seq of asyncs can now be traversed
* Fix: generic sum function works with any monoid
* Fix 'use' for strict builders

#### 1.1.0-CI00252  - October 8 2018
* Add Invariant Abstraction
* Some convenient conversion functions between Option and Result
* OfSeq for IReadOnlyDictionary
* OfList for all same instances as OfSeq
* Add some ReadOnly Collections related functions

#### 1.1.0-CI00245  - September 22 2018
* Fix some type inference problems
* More Xml docs and type annotations

#### 1.1.0-CI00240  - September 15 2018
* Lens with less constraints (speed up compile time)
* More generic Foldable functions
* More extensions and abstractions with IReadOnlyDictionary

#### 1.1.0-CI00230  - August 30 2018
* Option as ZipFunctor
* More Async extensions
* More clean signatures supported
* Removed tuple size limitations
* Add generic functions 'maximum' and 'minimum' for Foldable
* Fix: generic 'scan' signature
* Fix: bug in '<|>' for Choice
* Optimized Foldable/Collection generic functions by adding specific overloads

#### 1.1.0-CI00208  - June 15 2018
* Async as ZipFunctor
* More Async extensions

#### 1.1.0-CI00204  - June 5 2018
* Fix culture neutrality of TryParse functions
* More integration with ReadOnly Collections
* More extensions for Map, IDictionary and ReadOnly collections
* Map and Dictionary as ZipFunctor
* Result and Choice as Semigroup and Alt
* Exceptions and AggregateExceptions as Semigroup

#### 1.0.0  - April 27 2018
* Public 1.0.0 release that includes a better architected API and numerous bug fixes

#### 1.0.0-RC3  - April 13 2018
* Re-introduce Choice based monad transformers with ChoiceT
* Generic split function is now bigeneric
* Bug fix in OptionT execution

#### 1.0.0-RC2  - March 31 2018
* A hoist function was added to some Monad Transformers
* Bug Fixes

#### 1.0.0-RC1  - March 26 2018
* Accumulative Validations
* DList<'T> implementation changed
* C# Extension Methods removed
* Require operators instead of named methods: 
	* (>>=) instead of Bind
	* (=>>) instead of Extend
	* (<|>) instead of Append
* Custom operations:	
 	* sortBy renamed to orderBy
 	* first and nth removed
 	* top operator added
* Unify Second with Map
* Defaults improved
* Identity<'T> is now in FSharpPlus.Data

#### 1.0.0-CI00148  - February 17 2018
* More flexibility in Computation Expressions
* Dictionary extensions
* Bug fixes

#### 1.0.0-CI00145  - February 6 2018
* Wrappers compiled as structs
* New tryItem operation for Indexable types
* Better default methods
* Bug fixes

#### 1.0.0-CI00136  - January 28 2018
* Rename ErrorT to ResultT
* Removed Haskell Compatibility

#### 1.0.0-CI00134  - January 25 2018
* IEnumerator functions
* Applicative Math: new syntax, more operators
* Function sequenceA renamed to sequence
* More types: DList, Multiplication monoid
* Changes in either function (args swapped)
* Namespaces renamed
* Lens functions renamed
* Removed function toString

#### 1.0.0-CI00099  - December 31 2017
* Some Delayed Monad Transformers
* Bug fixes

#### 1.0.0-CI00092  - December 26 2017
* New execution model for Computation Expressions
* Use F# new Result Type to model failures
* Functions minValue and maxValue are now generic constants
* Many String and other extensions

#### 1.0.0-CI00091  - November 30 2017
* Targeting Netstandard 2.0
* (0, +) as default monoid for numerics
* Rename: 
	* Empty, Append and Concat to MEmpty, MAppend and MConcat
	* MZero and MPlus to Empty and Append.
	* mempty and mappend to zero and plus
	* mconcat and mfold to Seq.sum and sum
* Generic constants (empty, zero, app, ...)
* Compose for applicative functors
* Unify RMap with Map and LMap with Contramap

#### 1.0.0-CI00089  - March 9 2017
* Functions create, singleton and traverse (Traversable) for NonEmptyList
* Reader<'R, 'T> and Writer<'Monad<'T * 'Monoid>> as Comonads
* IDictionary<'Key,'Value> as Functor and Monoid
* Standard signature for Join
* Enhancements in Traversable, including bug fixed and more infinite seqs cases
* Adapt code to be usable (no regressions) and able to be compiled from F# 4.1
* Extensive documentation, including tutorials and a classepedia (abstractions) diagram (WIP)

#### 1.0.0-CI00078  - January 21 2017
* Builders support MonadPlus and FX computations
* Bin path for sample files corrected
* Added Delay implementation for Cont
* Added Invokables for builders: TryWith, TryFinally and Using
* Fix overloads for Task
* Improve functor's map resolution in presence of interfaces
* Generalize Unzip to any functor

#### 1.0.0-CI00063  - December 26 2016
* Initial release using ProjectScaffold
