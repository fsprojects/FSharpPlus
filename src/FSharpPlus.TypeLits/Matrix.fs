namespace FSharpPlus.TypeLits

open TypeLevelOperators
open System.Runtime.CompilerServices

module MatrixHelpers =
  open System

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

  let inline concat (v1: Vector<'a, ^n1>) (v2: Vector<'a, ^n2>) : Vector<'a, ^``n1 + ^n2``> =
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

  let m2 =
    matrix (
      (1,0,0,0),
      (0,1,0,0),
      (0,0,1,0)
    )

  let v2 = vector (1,2,3,4,5)
