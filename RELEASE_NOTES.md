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
