namespace FSharpPlus.TypeLevel

#if !FABLE_COMPILER

/// Marker interface for type-level literals.
///
/// #### Members
///
///     static member inline Singleton (defaultValue: T) = (* unique value-level representation of T *)
///
/// `Singleton` will be called with `Unchecked.defaultof<T>` so it should not use/deconstruct its value.
///
///     static member inline RuntimeValue (t: T) = (* value-level counterpart of T *)
///
/// `RuntimeValue` should return value-level representation of `T`. i.e. If `T` is type-level boolean `True`, then
/// the method should return value-level boolean `true`.
type ITypeLiteral = interface end

/// Marker interface for getting the corresponding type-level error class of a type-level literal.
type IErrorLiftable<'a> = interface end

/// Base class for type-level errors.
///
/// For every type (kind) of type-level literals, the corresponding type-error class should be created
/// and it should provide all the methods the literals have to offer.
///
/// TypeError should not implement `ITypeLiteral` but should provide `Singleton` and `RuntimeValue`.
/// This is important for making type-level error-handling work correctly.
///
/// TypeError should implement the same `IErrorLiftable<TypeErrorLifter>` as the corresponding kind do.
/// For example, if `TypeFoo` implements `IErrorLiftable<FooTypeErrorLifter>`, `FooTypeError` should also implement it.
/// So, the implementation will look like below:
///
///     type FooTypeError<'a> =
///       inherit TypeError<'a>
///       interface IErrorLiftable<FooTypeErrorLifter>
///       (* members related to Foo *)
///
///     and AggregatedFooTypeError<'a>() =
///       inherit FooTypeError<AggregatedFooTypeError<'a>>()
///    
///     and FooTypeErrorLifter =
///       static member inline Lift (_: 'Error) = Unchecked.defaultof<AggregatedFooTypeError<'Error>>
///
///     type ITypeFoo =
///       inherit ITypeLiteral
///       inherit IErrorLiftable<FooTypeErrorLifter>
type TypeError<'a>() =
  static member inline Singleton _ = Unchecked.defaultof<'a>
  static member inline RuntimeValue _ = failwithf "type error: %A" typeof<'a>

type TryWithImpl =
  static member inline TryWith (x: 'a when 'a :> ITypeLiteral, _, f) = f x
  static member inline TryWith (_x: 'a when 'a :> TypeError<_>, y, _) = y
  static member inline Invoke (x: ^X, y, f) =
    let inline call_2 (_a: ^a, _b: ^b) = ((^a or ^b): (static member TryWith:_*_*_->_) x,y,f)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<TryWithImpl>, x)

[<AutoOpen>]
module TypeOp =
  let inline ( =^ ) (x: ^X) y = (^X: (static member ( =^ ):_*_->_) x,y)

module TypeLevelOperators =
  /// Gets a singleton value of given type-level literal.
  let inline Singleton< ^X when ^X: (static member Singleton: ^X -> ^X) > =
    (^X: (static member Singleton: _ -> _) Unchecked.defaultof< ^X >)

  /// Gets a value-level counterpart of given type-level literal.
  let inline RuntimeValue (x: ^X) = (^X: (static member RuntimeValue: ^X -> _) x)

  /// If `x` is a type-level error, returns `onError`. Otherwise, returns `x`.
  let inline TryWith (x: ^X) onError = TryWithImpl.Invoke (x, onError, id)
  
  /// If `x` is a type-level error, returns `onError`. Otherwise, returns `f x`.
  let inline TryWithCont (x: ^X) (f: _ -> _) onError = TryWithImpl.Invoke (x, onError, f)

#endif