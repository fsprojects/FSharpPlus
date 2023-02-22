namespace FSharpPlus.TypeLevel

#if !FABLE_COMPILER

type NatTypeError<'a>() =
  inherit TypeError<'a>()
  interface IErrorLiftable<NatTypeErrorLifter>
  static member inline Succ _ = Unchecked.defaultof<'a>
  static member inline Pred _ = Unchecked.defaultof<'a>
  static member inline IsZero = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( +^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( -^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline MultImpl (_,_,_) = Unchecked.defaultof<'a>
  static member inline ( /^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( %^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( =^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( <^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline ( <=^ ) (_, _) = Unchecked.defaultof<AggregatedBoolTypeError<'a>>
  static member inline Match (_, _, _: _ -> 'result when 'result :> IErrorLiftable< ^Lifter >) =
    (^Lifter: (static member Lift: _ -> _) Unchecked.defaultof<'a>)

and AggregatedNatTypeError<'a>() =
  inherit NatTypeError<AggregatedNatTypeError<'a>>()

and NatTypeErrorLifter =
  static member inline Lift (_: 'Error) = Unchecked.defaultof<AggregatedNatTypeError<'Error>>

type OverflowError() =
  inherit NatTypeError<OverflowError>()
type DividedByZeroError() =
  inherit NatTypeError<DividedByZeroError>()

type ITypeNat =
  inherit ITypeLiteral
  inherit IErrorLiftable<NatTypeErrorLifter>

type S< 'n > = S of 'n with
  interface ITypeNat
  static member inline RuntimeValue (S x: S< ^X >) = 1 + (^X: (static member RuntimeValue: ^X -> int) x)
  static member inline Singleton (_: S< ^X >) =
    S (^X: (static member Singleton: ^X -> ^X) Unchecked.defaultof< ^X >)
  static member inline Succ (S n) = S (S n)
  static member inline Pred (S n) = n
  static member inline IsZero = False
  static member inline ( +^ ) (S x: S< ^X >, y) = S (^X: (static member ( +^ ):_*_->_) x,y)
  static member inline ( -^ ) (x: ^X, S y: S< ^Y >) =
    (^Y: (static member ( -^ ):_*_->_) (^X: (static member Pred:_->_) x),y)
  static member inline MultImpl (S x: S< ^X >, y, sum: ^Sum) =
    (^X: (static member MultImpl:_*_*_->_) x, y, (^Sum: (static member ( +^ ):_*_->_) sum, y))
  static member inline ( =^ ) (S x: S< ^X >, y: ^Y) =
    TypeBool.IfThenElse (^Y: (static member IsZero: _) ())
      False
      (^X: (static member (=^):_*_->_)x,(^Y:(static member Pred:_->_)y))
  static member inline ( <^ ) (S x: S< ^X >, y: ^Y) =
    TypeBool.IfThenElse (^Y: (static member IsZero: _) ())
      False
      (^X: (static member (<^):_*_->_)x,(^Y:(static member Pred:_->_)y))
  static member inline ( <=^ ) (S x: S< ^X >, y: ^Y) =
    TypeBool.IfThenElse (^Y: (static member IsZero: _) ())
      False
      (^X: (static member (<=^):_*_->_)x,(^Y:(static member Pred:_->_)y))
  static member inline ( /^ ) (x, y: S<_>) =
    TypeBool.IfThenElse (TypeBool.Not (y <=^ x)) Z (
      let x': ^X' = x -^ y
      S (^X': (static member ( /^ ): _*_->_) x', y)
    )
  static member inline ( %^ ) (x, y: S<_>) =
    TypeBool.IfThenElse (TypeBool.Not (y <=^ x)) x (
      let x': ^X' = x -^ y
      (^X': (static member ( %^ ): _*_->_) x', y)
    )
  static member inline Match (x: S<_>, _caseZ, caseSn) = caseSn x

and  Z = Z with
  interface ITypeNat
  static member inline RuntimeValue Z = 0
  static member inline Singleton (_: Z) = Z
  static member inline Succ Z = S Z
  static member inline Pred Z = Unchecked.defaultof<OverflowError>
  static member inline IsZero = True
  static member inline ( +^ ) (Z, y) = y
  static member inline ( -^ ) (x, Z) = x
  static member inline MultImpl (Z, _, sum) = sum
  static member inline ( /^ ) (Z, S _) = Z
  static member inline ( /^ ) (_, Z) = Unchecked.defaultof<DividedByZeroError>
  static member inline ( %^ ) (Z, S _) = Z
  static member inline ( %^ ) (_, Z) = Unchecked.defaultof<DividedByZeroError>
  static member inline ( =^ ) (Z, _: ^X) = (^X: (static member IsZero: _) ())
  static member inline ( <^ ) (Z, _: ^X) = TypeBool.Not (^X: (static member IsZero: _) ())
  static member inline ( <=^ ) (Z, _) = True
  static member inline Match (Z, caseZ, _caseSn) = caseZ Z

[<AutoOpen>]
module NatOp =
  let inline ( +^ ) (x: ^X) (y: ^Y) = (^X: (static member ( +^ ): _*_->_) x,y)
  let inline ( -^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( -^ ):_*_->_) x,y)
  let inline ( *^ ) (x: ^X) (y: ^Y) = (^X: (static member MultImpl: _*_*_->_) x,y,Z)
  let inline ( /^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( /^ ):_*_->_) x,y)
  let inline ( %^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( %^ ):_*_->_) x,y)
  let inline ( <^ ) (x: ^X) (y: ^Y) = (^X: (static member ( <^ ):_*_->_) x,y)
  let inline ( >^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( <^ ):_*_->_) y,x)
  let inline ( <=^ ) (x: ^X) (y: ^Y) = (^X: (static member ( <=^ ):_*_->_) x,y)
  let inline ( >=^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( <=^ ):_*_->_) y,x)

module TypeNat =
  let inline IsZero (_: ^n) = (^n: (static member IsZero: _) ())

  let inline Pred (n: ^n) = (^n: (static member Pred: _ -> _) n)

  let inline Match (caseZ: Z -> _) (caseSn: S<'a> -> _) (n: ^Nat) =
    (^Nat: (static member Match: _*_*_->_) n, caseZ, caseSn)

#endif