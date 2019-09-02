namespace FSharpPlus.TypeLits

/// Marker interface for type-level literals.
///
/// # Members
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

/// Base class for type-level errors.
///
/// For every type (kind) of type-level literals, the corresponding type-error class should be created
/// and it should provide all the methods the literals have to offer.
///
/// TyITypeLiteral should not implement `ITypeLits` but should provide `Singleton` and `RuntimeValue`.
/// This is important for making type-level error-handling work correctly.
type TypeError<'a>() =
  static member inline Singleton _ = Unchecked.defaultof<'a>
  static member inline RuntimeValue _ = failwithf "type error: %A" typeof<'a>

type TryWithImpl =
  static member inline TryWith (x: 'a when 'a :> ITypeLiteral, _) = x
  static member inline TryWith (x: 'a when 'a :> TypeError<_>, y) = y
  static member inline Invoke (x: ^X, y) =
    let inline call_2 (a: ^a, b: ^b) = ((^a or ^b): (static member TryWith:_*_->_) x,y)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<TryWithImpl>, x)

/// Marker interface for getting the corresponding type-level error class of a type-level literal.
type IErrorLiftable<'a> = interface end

module TypeLevelOperators =
  /// Gets a singleton value of given type-level literal.
  let inline Singleton< ^X when ^X: (static member Singleton: ^X -> ^X) > =
    (^X: (static member Singleton: _ -> _) Unchecked.defaultof< ^X >)

  /// Gets a value-level counterpart of given type-level literal.
  let inline RuntimeValue (x: ^X) = (^X: (static member RuntimeValue: ^X -> _) x)

  /// If `x` is a type-level error, returns `onError`. Otherwise, returns `x`.
  let inline TryWith (x: ^X) onError = TryWithImpl.Invoke (x, onError)
