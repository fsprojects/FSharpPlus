namespace FSharpPlus.TypeLevel

#if !FABLE_COMPILER


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

#endif