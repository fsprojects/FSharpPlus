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
