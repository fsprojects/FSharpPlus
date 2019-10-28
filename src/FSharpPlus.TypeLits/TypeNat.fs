namespace FSharpPlus.TypeLits


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
  static member inline ( /^ ) (x, y) =
    TypeBool.IfThenElse (x <^ y) Z (
      let x': ^X' = x -^ y
      S (^X': (static member ( /^ ): _*_->_) x', y)
    )
  static member inline ( %^ ) (x, y) =
    TypeBool.IfThenElse (x <^ y) x (
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
  let inline ( /^ ) (x: ^X) (y: ^Y) = (^X: (static member ( /^ ):_*_->_) x,y)
  let inline ( %^ ) (x: ^X) (y: ^Y) = (^X: (static member ( %^ ):_*_->_) x,y)
  let inline ( <^ ) (x: ^X) (y: ^Y) = (^X: (static member ( <^ ):_*_->_) x,y)
  let inline ( >^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( <^ ):_*_->_) y,x)
  let inline ( <=^ ) (x: ^X) (y: ^Y) = (^X: (static member ( <=^ ):_*_->_) x,y)
  let inline ( >=^ ) (x: ^X) (y: ^Y) = (^Y: (static member ( <=^ ):_*_->_) y,x)

module TypeNat =
  let inline IsZero (_: ^n) = (^n: (static member IsZero: _) ())

  let inline Match (caseZ: Z -> _) (caseSn: S<'a> -> _) (n: ^Nat) =
    (^Nat: (static member Match: _*_*_->_) n, caseZ, caseSn)

  type ``0`` = Z
  type ``1`` = S<``0``>
  type ``2`` = S<``1``>
  type ``3`` = S<``2``>
  type ``4`` = S<``3``>
  type ``5`` = S<``4``>
  type ``6`` = S<``5``>
  type ``7`` = S<``6``>
  type ``8`` = S<``7``>
  type ``9`` = S<``8``>
  type ``10`` = S<``9``>

#if DEBUG
module private NatTests =
  open TypeLevelOperators
  open TypeBool
  let two = S (S Z)
  let inline f1 x = x +^ two
  Assert (f1 Z =^ two); Assert (f1 (S Z) =^ S two); Assert (f1 (S (S Z)) =^ S (S two))
  let inline f2 x = S (S Z) +^ x
  Assert (f2 Z =^ two); Assert (f2 (S Z) =^ S two); Assert (f2 (S (S Z)) =^ S (S two))
  
  let inline f3 x = x -^ two
  Assert (TryWith (f3 Z) True); Assert (TryWith (f3 (S Z)) True)
  Assert (f3 two =^ Z); Assert (f3 (S two) =^ S Z); Assert (f3 (S (S two)) =^ two)
  let inline f4 x = two -^ x
  Assert (f4 Z =^ two); Assert (f4 (S Z) =^ S Z); Assert (f4 (S (S Z)) =^ Z)
  Assert (TryWith (f4 (S (S (S Z)))) True)
  
  let inline f5 x = x *^ two
  Assert (f5 Z =^ Z); Assert (f5 (S Z) =^ two); Assert (f5 two =^ S (S two))
  let inline f6 x = two *^ x
  Assert (f6 Z =^ Z); Assert (f6 (S Z) =^ two); Assert (f6 two =^ S (S two))

  let inline f71  x = x =^ two
  Assert (Not (f71 Z)); Assert (Not (f71 (S Z))); Assert (f71 two); Assert (Not (f71 (S two)))
  let inline f72 x = x =^ Z
  Assert (f72 Z); Assert (Not (f72 (S Z))); Assert (Not (f72 two))
  let inline f73  x = two =^ x
  Assert (Not (f73 Z))
  Assert (Not (f73 (S Z)))
  Assert (f73 two)
  Assert (Not (f73 (S two)))
  let inline f74 x = Z =^ x
  Assert (f74 Z); Assert (Not (f74 (S Z))); Assert (Not (f74 two))

  let inline f81  x = x <^ two
  Assert (f81 Z)
  Assert (f81 (S Z))
  Assert (Not (f81 two))
  Assert (Not (f81 (S two)))
  let inline f82 x = x <^ Z
  Assert (Not (f82 Z)); Assert (Not (f82 (S Z)))
  let inline f83 x = two <^ x
  Assert (Not (f83 Z))
  Assert (Not (f83 (S Z)))
  Assert (Not (f83 two))
  Assert (f83 (S two))
  let inline f84 x = Z <^ x
  Assert (Not (f84 Z)); Assert (f84 (S Z)); Assert (f84 two)

  let inline f91  x = x <=^ two
  Assert (f91 Z)
  Assert (f91 (S Z))
  Assert (f91 two)
  Assert (Not (f91 (S two)))
  let inline f92 x = x <=^ Z
  Assert (f92 Z); Assert (Not (f92 (S Z)))
  let inline f93 x = two <=^ x
  Assert (Not (f93 Z))
  Assert (Not (f93 (S Z)))
  Assert (f93 two)
  Assert (f93 (S two))
  let inline f94 x = Z <=^ x
  Assert (f94 Z); Assert (f94 (S Z)); Assert (f94 two)

  let inline fa x = x /^ two
  Assert (fa Z =^ Z)
  Assert (fa (S Z) =^ Z)
  Assert (fa two =^ S Z)
  let inline fb x = S (S Z) /^ x
  // TODO: make this compile
  // Assert (TryWith (fb Z) True)
  Assert (fb (S Z) =^ two)
  Assert (fb two =^ (S Z))
  
  let inline fc x = x %^ S (S Z)
  Assert (fc Z =^ Z)
  Assert (fc (S Z) =^ (S Z))
  Assert (fc two =^ Z)
  let inline fd x = S (S Z) %^ x
  // TODO: make this compile
  // Assert (TryWith (fd Z) True)
  Assert (fd (S Z) =^ Z)
  Assert (fd two =^ Z)
  Assert (fd (S two) =^ two)
#endif
