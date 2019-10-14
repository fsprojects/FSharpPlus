namespace FSharpPlus.TypeLits


type NatTypeError<'a>() =
  inherit TypeError<'a>()
  static member inline ( +^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( -^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( *^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( /^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( %^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( =^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( >^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( <^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( >=^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( <=^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline Match (_, _, _: _ -> 'result when 'result :> IErrorLiftable< ^Lifter >) =
    (^Lifter: (static member Lift: _ -> _) Unchecked.defaultof<'a>)

type AggregatedNatTypeError<'a>() =
  inherit NatTypeError<AggregatedNatTypeError<'a>>()

type OverflowError() =
  inherit NatTypeError<OverflowError>()
type DividedByZeroError() =
  inherit NatTypeError<DividedByZeroError>()

type NatTypeErrorLifter =
  static member inline Lift (_: 'Error) = Unchecked.defaultof<AggregatedNatTypeError<'Error>>

type ITypeNat =
  inherit ITypeLiteral
  inherit IErrorLiftable<NatTypeErrorLifter>

type S< 'n > = S of 'n with
  interface ITypeNat
  static member inline RuntimeValue (S x: S< ^X >) = 1 + (^X: (static member RuntimeValue: ^X -> int) x)
  static member inline Singleton (_: S< ^X >) =
    S (^X: (static member Singleton: ^X -> ^X) Unchecked.defaultof< ^X >)
  static member inline ( +^ ) (S x: S< ^X >, y) = S (x +^ y)
  static member inline ( -^ ) (S x, S y) = x -^ y
  static member inline ( *^ ) (S x, y) = x *^ y +^ y
  static member inline ( =^ ) (S x, S y) = x =^ y
  static member inline ( >^ ) (S x, S y) = x >^ y
  static member inline ( <^ ) (S x, S y) = x <^ y
  static member inline ( <=^ ) (S x, S y) = x <=^ y
  static member inline ( >=^ ) (S x, S y) = x >=^ y
  static member inline ( /^ ) (x: S<_>, y: S<_>) =
    TypeBool.IfThenElse (x <^ y) Z (
      let x': ^X = x -^ y
      S (^X: (static member ( /^ ): _*_->_) x', y)
    )
  static member inline ( %^ ) (x: S<_>, y: S<_>) =
    TypeBool.IfThenElse (x <^ y) x (
      let x': ^X = x -^ y
      (^X: (static member ( %^ ): _*_->_) x', y)
    )
  static member inline Match (x: S<_>, _caseZ, caseSn) = caseSn x

and  Z = Z with
  interface ITypeNat
  static member inline RuntimeValue Z = 0
  static member inline Singleton (_: Z) = Z
  static member inline ( +^ ) (Z, y) = y
  static member inline ( -^ ) (x, Z) = x
  static member inline ( -^ ) (Z, S _) = Unchecked.defaultof<OverflowError>
  static member inline ( *^ ) (Z, _) = Z
  static member inline ( /^ ) (Z, S _) = Z
  static member inline ( /^ ) (_, Z) = Unchecked.defaultof<DividedByZeroError>
  static member inline ( %^ ) (Z, S _) = Z
  static member inline ( %^ ) (_, Z) = Unchecked.defaultof<DividedByZeroError>
  static member inline ( =^ ) (Z, Z) = True
  static member inline ( =^ ) (S _, Z) = False
  static member inline ( =^ ) (Z, S _) = False
  static member inline ( >^ ) (Z, _)   = False
  static member inline ( >^ ) (S _, Z) = True
  static member inline ( <^ ) (_, Z)   = False
  static member inline ( <^ ) (Z, S _) = True
  static member inline ( <=^ ) (Z, _)   = True
  static member inline ( <=^ ) (S _, Z) = False
  static member inline ( >=^ ) (_, Z)   = True
  static member inline ( >=^ ) (Z, S _) = False
  static member inline Match (Z, caseZ, _caseSn) = caseZ Z

module TypeNat =
  let inline Match (caseZ: Z -> _) (caseSn: S<'a> -> _) (n: ^Nat) =
    (^Nat: (static member Match: _*_*_->_) n, caseZ, caseSn)
