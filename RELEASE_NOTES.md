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
