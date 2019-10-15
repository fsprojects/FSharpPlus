namespace FSharpPlus.TypeLits

open TypeLevelOperators
open System.Runtime.CompilerServices

#nowarn "0042" // retype

module MatrixHelpers =
  open System
  let inline internal retype (x: 'T) : 'U = (# "" x: 'U #)

  type NatToArguments =
    static member inline Invoke (x: ^X, f: _ list -> _) =
      let inline call_2 (a: ^a, b: ^b) = ((^a or ^b): (static member Ctor: _*_*_*_->_) a,b,[],f)
      let inline call (a: 'a, b: 'b) = call_2 (a, b)
      call (Unchecked.defaultof<NatToArguments>, x)

    static member inline Ctor (_: ^C, Z, xs, f) = f xs
    static member inline Ctor (c: ^C, S n: S< ^n >, xs, f) =
      fun x -> ((^C or ^n): (static member Ctor: _*_*_*_->_)c,n,x::xs,f)

  module Constraints =
    /// Constrain 't to be a nested tuple of <'t1,'t2,'t3,'t4,'t5,'t6,'t7,'tr>
    let inline whenNestedTuple (t: 't) = 
      (^t: (member Item1: 't1) t), (^t: (member Item2: 't2) t), (^t: (member Item3: 't3) t), (^t: (member Item4: 't4) t), (^t: (member Item5: 't5) t), (^t: (member Item6: 't6) t), (^t: (member Item7: 't7) t), (^t: (member Rest: 'tr) t)

  type CountTuple =
    static member inline Invoke xs : 'n =
      let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member CountTuple: _*_ -> _) b, a)
      let inline call (a: 'a, b: 'b) = call_2 (a, b)
      call (Unchecked.defaultof<CountTuple>, xs)

    static member inline CountTuple (t: 't, ct: ^CountTuple) =
      let _,_,_,_,_,_,_,tr : _*_*_*_*_*_*_* ^TR = Constraints.whenNestedTuple t
      ((^TR or ^CountTuple): (static member CountTuple: _*_->_) tr,ct)
      |> S |> S |> S |> S |> S |> S |> S

    static member CountTuple (_: Tuple<_>, _: CountTuple) = S Z
    static member CountTuple ((_, _), _: CountTuple) = S Z |> S
    static member CountTuple ((_, _, _), _: CountTuple) = S Z |> S |> S
    static member CountTuple ((_, _, _, _), _: CountTuple) = S Z |> S |> S |> S
    static member CountTuple ((_, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S
    static member CountTuple ((_, _, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S |> S
    static member CountTuple ((_, _, _, _, _, _, _), _: CountTuple) = S Z |> S |> S |> S |> S |> S |> S
    
  type TupleToList =
    static member inline Invoke xs : 'x list =
      let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TupleToList: _*_ -> _) b, a)
      let inline call (a: 'a, b: 'b) = call_2 (a, b)
      call (Unchecked.defaultof<TupleToList>, xs)

    static member inline TupleToList (t: 't, ct: ^TupleToList) =
      let t1,t2,t3,t4,t5,t6,t7,tr : _*_*_*_*_*_*_* ^TR = Constraints.whenNestedTuple t
      t1::t2::t3::t4::t5::t6::t7::((^TR or ^TupleToList): (static member TupleToList: _*_->_) tr,ct)

    static member TupleToList (x: Tuple<_>, _: TupleToList) = [x.Item1]
    static member TupleToList ((x1,x2), _: TupleToList) = [x1;x2]
    static member TupleToList ((x1,x2,x3), _: TupleToList) = [x1;x2;x3]
    static member TupleToList ((x1,x2,x3,x4), _: TupleToList) = [x1;x2;x3;x4]
    static member TupleToList ((x1,x2,x3,x4,x5), _: TupleToList) = [x1;x2;x3;x4;x5]
    static member TupleToList ((x1,x2,x3,x4,x5,x6), _: TupleToList) = [x1;x2;x3;x4;x5;x6]
    static member TupleToList ((x1,x2,x3,x4,x5,x6,x7), _: TupleToList) = [x1;x2;x3;x4;x5;x6;x7]

  type AssertTupleType =
    static member inline Invoke (xs, ty, n) : unit =
      let inline call_2 (_a: ^a, b: ^b) = ((^a or ^b) : (static member AssertTupleType: _*_*_ -> _) b,ty,n)
      let inline call (a: 'a, b: 'b) = call_2 (a, b)
      call (Unchecked.defaultof<AssertTupleType>, xs)

    static member inline AssertTupleType (xs: 't, x: 'x, S(S(S(S(S(S(S(S(n))))))))) =
      let _,_,_,_,_,_,_,tr : 'x*'x*'x*'x*'x*'x*'x*_ = Constraints.whenNestedTuple xs
      AssertTupleType.Invoke (tr, x, S n)

    static member AssertTupleType (_: Tuple<'x>, _:'x,S Z) = ()
    static member AssertTupleType ((_:'x,_:'x),_:'x,S (S Z)) = ()
    static member AssertTupleType ((_:'x,_:'x,_:'x),_:'x, S(S(S Z))) = ()
    static member AssertTupleType ((_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S Z)))) = ()
    static member AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(Z)))))) = ()
    static member AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(S(Z))))))) = ()
    static member AssertTupleType ((_:'x,_:'x,_:'x,_:'x,_:'x,_:'x,_:'x),_:'x,S(S(S(S(S(S(S(Z)))))))) = ()

  type ArrayToTuple =
    static member inline Invoke (xs: 'x[], n, ?index) =
      let inline call_2 (_a: ^a, b: ^b) = ((^a or ^b): (static member ArrayToTuple:_*_*_->_) xs,b,defaultArg index 0)
      let inline call (a: 'a, b: 'b) = call_2 (a, b)
      call (Unchecked.defaultof<ArrayToTuple>, n)

    static member inline ArrayToTuple (xs:_[],S(S(S(S(S(S(S(n))))))), i) =
      TypeBool.Assert(n >^ Z)
      Tuple<_,_,_,_,_,_,_,_>(
        xs.[i],xs.[i+1],xs.[i+2],xs.[i+3],xs.[i+4],xs.[i+5],xs.[i+6],
        ArrayToTuple.Invoke(xs,n,i+7)
      )

    static member ArrayToTuple (xs:_[], S Z, i) = Tuple<_>(xs.[i])
    static member ArrayToTuple (xs:_[], S (S Z), i) = (xs.[i], xs.[i+1])
    static member ArrayToTuple (xs:_[], S (S (S Z)), i) = (xs.[i], xs.[i+1], xs.[i+2])
    static member ArrayToTuple (xs:_[], S (S (S (S Z))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3])
    static member ArrayToTuple (xs:_[], S (S (S (S (S Z)))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4])
    static member ArrayToTuple (xs:_[], S (S (S (S (S (S Z))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5])
    static member ArrayToTuple (xs:_[], S (S (S (S (S (S (S Z)))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5], xs.[i+6])

open MatrixHelpers

// Items : 'Item[ 'Column, 'Row ]
[<Struct; StructuredFormatDisplay("{Items}")>]
type Matrix< 'Item, 'Row, 'Column > = private { Items: 'Item[,] } with
  member this.UnsafeGet (i, j) = this.Items.[i, j]

[<Struct; StructuredFormatDisplayAttribute("{Items}")>]
type Vector<'Item, 'Length> = private { Items: 'Item[] } with
  member this.UnsafeGet i = this.Items.[i]
  interface System.Collections.Generic.IReadOnlyList<'Item> with
    member this.Count = this.Items.Length
    member this.Item with get i = this.Items.[i]
    member this.GetEnumerator() = this.Items.GetEnumerator()
    member this.GetEnumerator() = (this.Items :> seq<_>).GetEnumerator()

module Matrix =
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map (f: 'a -> 'b) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> =
    { Items = Array2D.map f m.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi (f: int -> int -> 'a -> 'b) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> =
    { Items = Array2D.mapi (fun i j -> f i j) m.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter (f: 'a -> unit) (m: Matrix<'a, 'm, 'n>) : unit =
    Array2D.iter f m.Items

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri (f: int -> int -> 'a -> unit) (m: Matrix<'a, 'm, 'n>) : unit =
    Array2D.iteri (fun i j -> f i j) m.Items

  let inline length1 (_: Matrix<'a, 'm, 'n>) : 'm = Singleton<'m>
  let inline length2 (_: Matrix<'a, 'm, 'n>) : 'n = Singleton<'n>
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length1' (mtx: Matrix<'a, 'm, 'n>) = mtx.Items |> Array2D.length1
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length2' (mtx: Matrix<'a, 'm, 'n>) = mtx.Items |> Array2D.length2

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toArray2D (m: Matrix<'a, 'm, 'n>) = m.Items

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeCreate (_row: 'm) (_column: 'n) (items: _[,]) : Matrix<_, 'm, 'n> =
    { Items = items }

  let inline create (definition: '``('a * .. * 'a) * .. * ('a * .. * 'a)``) : Matrix<'a, 'm, 'n> =
    let rowLength = CountTuple.Invoke definition
    let columns : 'row array = TupleToList.Invoke definition |> Array.ofList
    let columnLength = CountTuple.Invoke columns.[0]
    let xs = columns |> Array.map (TupleToList.Invoke >> Array.ofList)
    let m, n = RuntimeValue rowLength, RuntimeValue columnLength
    let ys = Array2D.zeroCreate m n
    for i = 0 to m-1 do
      for j = 0 to n-1 do
        ys.[i, j] <- xs.[i].[j]
    unsafeCreate rowLength columnLength ys

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeGet (i, j) (m: Matrix<'a, 'm, 'n>) = m.Items.[i,j]

  let inline ofVectors (xs: Vector<Vector<'a, 'm>, 'n>) : Matrix<'a, 'm, 'n> =
    let m = Singleton<'m> |> RuntimeValue
    let n = Singleton<'n> |> RuntimeValue
    let ys = Array2D.zeroCreate m n
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        ys.[i, j] <- xs.UnsafeGet(j).UnsafeGet(i)
    unsafeCreate Singleton Singleton ys

  let inline get (row: ^``i when ^i < ^m``) (column: ^``j when ^j < ^n``) (mat: Matrix<'a, ^m, ^n>) : 'a =
    let m = Singleton<'m>
    let n = Singleton<'n>
    TypeBool.Assert (row <^ m)
    TypeBool.Assert (column <^ n)
    unsafeGet (RuntimeValue row, RuntimeValue column) mat

  let transpose (mtx: Matrix<'t, 'm, 'n>) : Matrix<'t, 'n, 'm> =
    let m = mtx |> length1'
    let n = mtx |> length2'
    let ys = Array2D.zeroCreate n m
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        ys.[j, i] <- mtx.Items.[i, j]
    { Items = ys }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let matrixProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'n, 'p>) : Matrix<'t, 'm, 'p> =
    { Items = failwith "TODO" }

  let inline tensorProduct (m1: Matrix<'t, ^m1, ^n1>) (m2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 * ^m2``, ^``n1 * ^n2``> =
    let m1m2 = Singleton< ^m1 > *^ Singleton< ^m2 >
    let n1n2 = Singleton< ^n1 > *^ Singleton< ^n2 >
    unsafeCreate m1m2 n1n2 <| failwith "TODO"

  let inline directSum (m1: Matrix<'t, ^m1, ^n1>) (m2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 + ^m2``, ^``n1 + ^n2``> =
    let m1m2 = Singleton< ^m1 > +^ Singleton< ^m2 >
    let n1n2 = Singleton< ^n1 > +^ Singleton< ^n2 >
    unsafeCreate m1m2 n1n2 <| failwith "TODO"

  let inline verticalSum (m1: Matrix<'t, ^m1, ^n>) (m2: Matrix<'t, ^m2, ^n>) : Matrix<'t, ^``m1 + ^m2``, ^n> =
    let m1m2 = Singleton< ^m1 > +^ Singleton< ^m2 >
    let n = Singleton< ^n >
    unsafeCreate m1m2 n <| failwith "TODO"

  let inline horizontalSum (m1: Matrix<'t, ^m, ^n1>) (m2: Matrix<'t, ^m, ^n2>) : Matrix<'t, ^m, ^``n1 + ^n2``> =
    let m = Singleton< ^m >
    let n1n2 = Singleton< ^n1 > +^ Singleton< ^n2 >
    unsafeCreate m n1n2 <| failwith "TODO"

  let inline hadamardProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'm, 'n>) : Matrix<'t, 'm, 'n> =
    unsafeCreate Singleton Singleton <| failwith "TODO"
 
module Vector =
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map (f: 'a -> 'b) (vec: Vector<'a, 'n>) : Vector<'b, 'n> =
    { Items = Array.map f vec.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi (f: int -> 'a -> 'b) (vec: Vector<'a, 'n>) : Vector<'b, 'n> =
    { Items = Array.mapi f vec.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map2 (f: 'a -> 'b -> 'c) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'c, 'n> =
    { Items = Array.map2 f vec1.Items vec2.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi2 (f: int -> 'a -> 'b -> 'c) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'c, 'n> =
    { Items = Array.mapi2 f vec1.Items vec2.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map3 (f: 'a -> 'b -> 'c -> 'd) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) (vec3: Vector<'c, 'n>) : Vector<'d, 'n> =
    { Items = Array.map3 f vec1.Items vec2.Items vec3.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter (f: 'a -> unit) (vec: Vector<'a, 'n>) : unit =
    vec.Items |> Array.iter f

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri (f: int -> 'a -> unit) (vec: Vector<'a, 'n>) : unit =
    vec.Items |> Array.iteri f

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter2 (f: 'a -> 'b -> unit) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : unit =
    Array.iter2 f vec1.Items vec2.Items

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri2 (f: int -> 'a -> 'b -> unit) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : unit =
    Array.iteri2 f vec1.Items vec2.Items

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let zip (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'a * 'b, 'n> =
    { Items = Array.zip vec1.Items vec2.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let zip3 (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) (vec3: Vector<'c, 'n>) : Vector<'a*'b*'c, 'n> =
    { Items = Array.zip3 vec1.Items vec2.Items vec3.Items }

  let inline length (_vec: Vector<'a, 'n>) : 'n = Singleton<'n>
  
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length' (xs: Vector<'a, 'n>) = xs.Items.Length

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toArray (vec: Vector<'a, 'n>) : 'a[] = vec.Items

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeCreate (_length: 'n) (xs: _[]) : Vector<_, 'n> =
    { Items = xs }

  let inline create (definition: '``a * 'a * .. * 'a``) : Vector<'a, 'n> =
    unsafeCreate (CountTuple.Invoke definition) (TupleToList.Invoke definition |> Array.ofList)

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeGet index (vec: Vector<'a, 'n>) = vec.Items.[index]

  let inline get (index: ^``i when ^i < ^n``) (vec: Vector<'a, ^n>) =
    let n = Singleton< ^n >
    TypeBool.Assert (index <^ n)
    vec |> unsafeGet (RuntimeValue index)

  let inline zeroCreate (n: 'n) : Vector<'a, 'n> =
    Array.zeroCreate (RuntimeValue n) |> unsafeCreate n

  let inline replicate (n: 'n) (value: 'a) : Vector<'a, 'n> =
    Array.replicate (RuntimeValue n) value |> unsafeCreate n

  let inline init (n: 'n) (f: int -> 'a) : Vector<'a, 'n > =
    Array.init (RuntimeValue n) f |> unsafeCreate n

  let inline append (v1: Vector<'a, ^n1>) (v2: Vector<'a, ^n2>) : Vector<'a, ^``n1 + ^n2``> =
    let len = Singleton< ^n1 > +^ Singleton< ^n2 >
    unsafeCreate len [| yield! toArray v1; yield! toArray v2 |]

  let toRowVector (v: Vector<'a, 'n>) : Matrix<'a, S<Z>, 'n> = { Items = array2D [ v.Items ] }
  let toColumnVector (v: Vector<'a, 'n>) : Matrix<'a, 'n, S<Z>> = { Items = array2D [ for x in v.Items -> [x] ] }

[<AutoOpen>]
module MatrixOperators =
  let inline matrix (definition: '``('a * .. * 'a) * .. * ('a * .. * 'a)``) : Matrix<'a, 'm, 'n> =
    Matrix.create definition

  let inline vector (definition: '``a * 'a * .. * 'a``) : Vector<'a, 'n> =
    Vector.create definition

  let inline (|Vector|) (vect: Vector<'a, 'n>) =
    let n = Singleton<'n>
    let t = ArrayToTuple.Invoke(Vector.toArray vect, n) |> MatrixHelpers.retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'a>, n)
    t

  let inline (|Matrix|) (mtx: Matrix<'a, 'm, 'n>) =
    let m = Singleton<'m>
    let n = Singleton<'n>
    let items = Matrix.toArray2D mtx
    let xs : 'ta[] = [|
      for i = 0 to Array2D.length1 items - 1 do
        let col = [| for j = 0 to Array2D.length2 items - 1 do yield items.[i, j] |]
        let t = ArrayToTuple.Invoke(col, n) |> MatrixHelpers.retype
        AssertTupleType.Invoke(t, Unchecked.defaultof<'t>, n)
        yield t
    |]
    let t = ArrayToTuple.Invoke(xs, m) |> MatrixHelpers.retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'ta>, m)
    t

(*
let (Vector(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) = vector(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5);;
*)
  let m2 =
    matrix (
      (1,0,0,0),
      (0,1,0,0),
      (0,0,1,0)
    )

  let v2 = vector (1,2,3,4,5)
  
