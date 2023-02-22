namespace FSharpPlus.Control
open System
open FSharpPlus.Internals

open FSharpPlus.TypeLevel
#if !FABLE_COMPILER

type CountTuple =
  static member inline Invoke xs : 'n =
    let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member CountTuple: _*_ -> _) b, a)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<CountTuple>, xs)

  static member inline CountTuple (t: 't, ct: ^CountTuple) =
    let _,_,_,_,_,_,_,tr : _*_*_*_*_*_*_* ^TR = Constraints.whenNestedTuple t
    ((^TR or ^CountTuple): (static member CountTuple: _*_->_) tr,ct)
    |> S |> S |> S |> S |> S |> S |> S

  static member inline CountTuple (_: Tuple<_>, _: CountTuple) = S Z
  static member inline CountTuple ((_, _), _: CountTuple) = S Z |> S
  static member inline CountTuple ((_, _, _), _: CountTuple) = S Z |> S |> S
  static member inline CountTuple ((_, _, _, _), _: CountTuple) = S Z |> S |> S |> S
  static member inline CountTuple ((_, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S
  static member inline CountTuple ((_, _, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S |> S
  static member inline CountTuple ((_, _, _, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S |> S |> S
  
type TupleToList =
  static member inline Invoke xs : 'x list =
    let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TupleToList: _*_ -> _) b, a)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<TupleToList>, xs)

  static member inline TupleToList (t: 't, ct: ^TupleToList) =
    let t1,t2,t3,t4,t5,t6,t7,tr : _*_*_*_*_*_*_* ^TR = Constraints.whenNestedTuple t
    t1::t2::t3::t4::t5::t6::t7::((^TR or ^TupleToList): (static member TupleToList: _*_->_) tr,ct)

  static member inline TupleToList (x: Tuple<_>, _: TupleToList) = [x.Item1]
  static member inline TupleToList ((x1,x2), _: TupleToList) = [x1;x2]
  static member inline TupleToList ((x1,x2,x3), _: TupleToList) = [x1;x2;x3]
  static member inline TupleToList ((x1,x2,x3,x4), _: TupleToList) = [x1;x2;x3;x4]
  static member inline TupleToList ((x1,x2,x3,x4,x5), _: TupleToList) = [x1;x2;x3;x4;x5]
  static member inline TupleToList ((x1,x2,x3,x4,x5,x6), _: TupleToList) = [x1;x2;x3;x4;x5;x6]
  static member inline TupleToList ((x1,x2,x3,x4,x5,x6,x7), _: TupleToList) = [x1;x2;x3;x4;x5;x6;x7]

type AssertTupleType =
  static member inline Invoke (xs, ty, n) : unit =
    let inline call_2 (_a: ^a, b: ^b) = ((^a or ^b) : (static member AssertTupleType: _*_*_ -> _) b,ty,n)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<AssertTupleType>, xs)

  static member inline AssertTupleType (xs: 't, x: 'x, S(S(S(S(S(S(S(S(n))))))))) =
    let _,_,_,_,_,_,_,tr : 'x*'x*'x*'x*'x*'x*'x*_ = Constraints.whenNestedTuple xs
    AssertTupleType.Invoke (tr, x, S n)

  static member inline AssertTupleType (_: Tuple<'x>, _:'x,S Z) = ()
  static member inline AssertTupleType ((_:'x,_:'x),_:'x,S (S Z)) = ()
  static member inline AssertTupleType ((_:'x,_:'x,_:'x),_:'x, S(S(S Z))) = ()
  static member inline AssertTupleType ((_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S Z)))) = ()
  static member inline AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(Z)))))) = ()
  static member inline AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(S(Z))))))) = ()
  static member inline AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(S(S(Z)))))))) = ()

type ArrayToTuple =
  static member inline Invoke (xs: 'x[], n, ?index) =
    let inline call_2 (_a: ^a, b: ^b) = ((^a or ^b): (static member ArrayToTuple:_*_*_->_) xs,b,defaultArg index 0)
    let inline call (a: 'a, b: 'b) = call_2 (a, b)
    call (Unchecked.defaultof<ArrayToTuple>, n)

  static member inline ArrayToTuple (xs:_[],S(S(S(S(S(S(S(n))))))), i) =
    TypeBool.Assert(TypeBool.Not (TypeNat.IsZero n))
    Tuple<_,_,_,_,_,_,_,_>(
      xs.[i],xs.[i+1],xs.[i+2],xs.[i+3],xs.[i+4],xs.[i+5],xs.[i+6],
      ArrayToTuple.Invoke(xs,n,i+7)
    )

  static member inline ArrayToTuple (xs:_[], S Z, i) = Tuple<_>(xs.[i])
  static member inline ArrayToTuple (xs:_[], S (S Z), i) = (xs.[i], xs.[i+1])
  static member inline ArrayToTuple (xs:_[], S (S (S Z)), i) = (xs.[i], xs.[i+1], xs.[i+2])
  static member inline ArrayToTuple (xs:_[], S (S (S (S Z))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3])
  static member inline ArrayToTuple (xs:_[], S (S (S (S (S Z)))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4])
  static member inline ArrayToTuple (xs:_[], S (S (S (S (S (S Z))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5])
  static member inline ArrayToTuple (xs:_[], S (S (S (S (S (S (S Z)))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5], xs.[i+6])
#endif
