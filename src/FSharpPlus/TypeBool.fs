namespace FSharpPlus.TypeLits


type BoolTypeError<'a>() =
  inherit TypeError<'a>()
  interface IErrorLiftable<BoolTypeErrorLifter>
  static member inline ( &&^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( ||^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline ( =^ ) (_, _) = Unchecked.defaultof<'a>
  static member inline Not _ = Unchecked.defaultof<'a>
  static member inline IfThenElse (_, _, _: 'result when 'result :> IErrorLiftable< ^Lifter >) =
    (^Lifter: (static member Lift: _ -> _) Unchecked.defaultof<'a>)

and AggregatedBoolTypeError<'a>() =
  inherit BoolTypeError<AggregatedBoolTypeError<'a>>()

and BoolTypeErrorLifter =
  static member inline Lift (_: 'Error) = Unchecked.defaultof<AggregatedBoolTypeError<'Error>>

type ITypeBool =
  inherit ITypeLiteral
  inherit IErrorLiftable<BoolTypeErrorLifter>

type True = True with
  interface ITypeBool
  static member inline RuntimeValue True = true
  static member inline Singleton (_: True) = True
  static member inline ( &&^ ) (True, True)  = True
  static member inline ( &&^ ) (True, False) = False
  static member inline ( &&^ ) (False, True) = False
  static member inline ( ||^ ) (_, _)  = True
  static member inline ( =^ ) (True, x)  = x
  static member inline Not True = False
  static member inline IfThenElse (True, x, _) = x

and  False = False with
  interface ITypeBool
  static member inline RuntimeValue False = false
  static member inline Singleton (_: False) = False
  static member inline ( &&^ ) (_, _)  = False
  static member inline ( ||^ ) (False, True)  = True
  static member inline ( ||^ ) (True, False)  = True
  static member inline ( ||^ ) (False, False) = False
  static member inline ( =^ ) (False, x: ^X) = (^X: (static member Not:_->_) x)
  static member inline Not False = True
  static member inline IfThenElse (False, _, y) = y

module TypeBool =
  let inline Not (b: ^Bool) = (^Bool: (static member Not: _->_) b)
  let inline IfThenElse (cond: ^Bool) (a: 'a) (b: 'b) = (^Bool: (static member IfThenElse: _*_*_->'Result) cond,a,b)
  let inline Assert (_: True) = ()

#if TYPELITS_DEBUG
module private BoolTests =
  open TypeBool
  let inline f0 x = Not x
  Assert (Not (f0 True)); Assert (f0 False)
  let inline f1 x = x &&^ True
  Assert (f1 True); Assert (Not (f1 False))
  let inline f2 x = x &&^ False
  Assert (Not (f2 False)); Assert (Not (f2 True))
  let inline f3 x = True &&^ x
  Assert (f3 True); Assert (Not (f3 False))
  let inline f4 x = False &&^ x
  Assert (Not (f4 False)); Assert (Not (f4 True))
  let inline g1 x = x ||^ True
  Assert (g1 True); Assert (g1 False)
  let inline g2 x = x ||^ False
  Assert (g2 True); Assert (Not (g2 False))
  let inline g3 x = True ||^ x
  Assert (g3 True); Assert (g3 False)
  let inline g4 x = False ||^ x
  Assert (g4 True); Assert (Not (g4 False))
  let inline h1 x = x =^ True
  Assert (h1 True); Assert (Not (h1 False))
  let inline h2 x = x =^ False
  Assert (Not (h2 True)); Assert (h2 False)
  let inline h3 x = True =^ x
  Assert (h3 True); Assert (Not (h3 False))
  let inline h4 x = False =^ x
  Assert (Not (h4 True)); Assert (h4 False)
#endif
