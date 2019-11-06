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
      TypeBool.Assert(TypeBool.Not (TypeNat.IsZero n))
      Tuple<_,_,_,_,_,_,_,_>(
        xs.[i],xs.[i+0],xs.[i+2],xs.[i+3],xs.[i+4],xs.[i+5],xs.[i+6],
        ArrayToTuple.Invoke(xs,n,i+7)
      )

    static member ArrayToTuple (xs:_[], S Z, i) = Tuple<_>(xs.[i])
    static member ArrayToTuple (xs:_[], S (S Z), i) = (xs.[i], xs.[i+1])
    static member ArrayToTuple (xs:_[], S (S (S Z)), i) = (xs.[i], xs.[i+1], xs.[i+2])
    static member ArrayToTuple (xs:_[], S (S (S (S Z))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3])
    static member ArrayToTuple (xs:_[], S (S (S (S (S Z)))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4])
    static member ArrayToTuple (xs:_[], S (S (S (S (S (S Z))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5])
    static member ArrayToTuple (xs:_[], S (S (S (S (S (S (S Z)))))), i) = (xs.[i], xs.[i+1], xs.[i+2], xs.[i+3], xs.[i+4], xs.[i+5], xs.[i+6])

  let inline foldiRange initValue startIndex endIndex f =
    let mutable result = initValue
    for i = startIndex to endIndex do
      result <- f i result
    result
  
  let inline solveImpl (lu:'a[,]) (b:'a[]) =
    let n = Array2D.length1 lu
    let x = Array.copy b
    for i = 1 to n - 1 do
      x.[i] <- foldiRange x.[i] 0 (i-1) (fun j sum -> sum - lu.[i, j] * x.[j])
    x.[n-1] <- x.[n-1] / lu.[n-1, n-1]
    for i in n-2 .. -1 .. 0 do
      let sum = foldiRange x.[i] (i+1) (n-1) (fun j sum -> sum - lu.[i, j] * x.[j])
      x.[i] <- sum / lu.[i, i]
    x

open MatrixHelpers

// Items : 'Item[ 'Column, 'Row ]
[<Struct; StructuredFormatDisplay("{Items}")>]
type Matrix< 'Item, 'Row, 'Column > = private { Items: 'Item[,] } with
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  member this.UnsafeGet (i, j) = this.Items.[i, j]
  interface System.Collections.Generic.IReadOnlyCollection<'Item> with
    member this.Count = this.Items.Length
    member this.GetEnumerator() = this.Items.GetEnumerator()
    member this.GetEnumerator() =
      let items = this.Items
      (seq {
        for i = 0 to (items |> Array2D.length1) - 1 do
          for j = 0 to (items |> Array2D.length2) - 1 do
            yield items.[i,j]
      }).GetEnumerator()

[<Struct; StructuredFormatDisplayAttribute("{Items}")>]
type Vector<'Item, 'Length> = private { Items: 'Item[] } with
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  member this.UnsafeGet i = this.Items.[i]
  interface System.Collections.Generic.IReadOnlyList<'Item> with
    member this.Count = this.Items.Length
    member this.Item with get i = this.Items.[i]
    member this.GetEnumerator() = this.Items.GetEnumerator()
    member this.GetEnumerator() = (this.Items :> seq<_>).GetEnumerator()
 
module Vector =
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map (f: 'a -> 'b) (vec: Vector<'a, 'n>) : Vector<'b, 'n> = { Items = Array.map f vec.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi (f: int -> 'a -> 'b) (vec: Vector<'a, 'n>) : Vector<'b, 'n> = { Items = Array.mapi f vec.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map2 (f: 'a -> 'b -> 'c) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'c, 'n> = { Items = Array.map2 f vec1.Items vec2.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi2 (f: int -> 'a -> 'b -> 'c) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'c, 'n> = { Items = Array.mapi2 f vec1.Items vec2.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map3 (f: 'a -> 'b -> 'c -> 'd) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) (vec3: Vector<'c, 'n>) : Vector<'d, 'n> = { Items = Array.map3 f vec1.Items vec2.Items vec3.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter (f: 'a -> unit) (vec: Vector<'a, 'n>) : unit = vec.Items |> Array.iter f
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter2 (f: 'a -> 'b -> unit) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : unit = Array.iter2 f vec1.Items vec2.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri2 (f: int -> 'a -> 'b -> unit) (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : unit = Array.iteri2 f vec1.Items vec2.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let zip (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) : Vector<'a * 'b, 'n> = { Items = Array.zip vec1.Items vec2.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let zip3 (vec1: Vector<'a, 'n>) (vec2: Vector<'b, 'n>) (vec3: Vector<'c, 'n>) : Vector<'a*'b*'c, 'n> = { Items = Array.zip3 vec1.Items vec2.Items vec3.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unzip (vec: Vector<'a*'b, 'n>) : Vector<'a,'n> * Vector<'b,'n> =
    let xs, ys = Array.unzip vec.Items in { Items = xs }, { Items = ys }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unzip3 (vec: Vector<'a*'b*'c, 'n>) : Vector<'a,'n> * Vector<'b,'n> * Vector<'c, 'n> =
    let xs, ys, zs = Array.unzip3 vec.Items in { Items = xs }, { Items = ys }, { Items = zs }

  let inline length (_vec: Vector<'a, 'n>) : 'n = Singleton<'n>
  
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length' (xs: Vector<'a, 'n>) = xs.Items.Length

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toArray (vec: Vector<'a, 'n>) : 'a[] = vec.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toSeq (vec: Vector<'a, 'n>) : 'a seq = vec.Items :> _
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toList (vec: Vector<'a, 'n>) : 'a list = vec.Items |> List.ofArray

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

  // constants
  let inline zero<'a, ^m, ^n
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and ^a: (static member Zero: ^a)> : Vector<'a, ^m> =
    replicate Singleton LanguagePrimitives.GenericZero

  let inline append (v1: Vector<'a, ^n1>) (v2: Vector<'a, ^n2>) : Vector<'a, ^``n1 + ^n2``> =
    let len = Singleton< ^n1 > +^ Singleton< ^n2 >
    unsafeCreate len [| yield! toArray v1; yield! toArray v2 |]

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let fold f (state: 'State) (v: Vector<'a, 'n>) = Array.fold f state v.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let fold2 f (state: 'State) (v1: Vector<'a, 'n>) (v2: Vector<'b, 'n>) = Array.fold2 f state v1.Items v2.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let foldBack f (v: Vector<'a, 'n>) (state: 'State) = Array.foldBack f v.Items state
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let foldBack2 f (v1: Vector<'a, 'n>) (v2: Vector<'b, 'n>) (state: 'State) = Array.foldBack2 f v1.Items v2.Items state
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let forall p (v: Vector<'a, 'n>) = Array.forall p v.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let exists p (v: Vector<'a, 'n>) = Array.exists p v.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let forall2 p (v1: Vector<'a, 'n>) (v2: Vector<'b, 'n>) = Array.forall2 p v1.Items v2.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let exists2 p (v1: Vector<'a, 'n>) (v2: Vector<'b, 'n>) = Array.exists2 p v1.Items v2.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let permute f (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.permute f v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let sort (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.sort v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let sortBy f (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.sortBy f v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let sortDescending (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.sortDescending v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let sortByDescending f (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.sortByDescending f v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let sortWith f (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.sortWith f v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let rev (v: Vector<'a, 'n>) : Vector<'a, 'n> = { Items = Array.rev v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let head (v: Vector<'a, S<_>>) = v.Items.[0]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let tail (v: Vector<'a, S<'n>>) : Vector<'a, 'n> = { Items = Array.tail v.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let last (v: Vector<'a, S<_>>) = Array.last v.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let indexed (v: Vector<'a, 'n>) : Vector<int * 'a, 'n> = { Items = Array.indexed v.Items } 

  let inline concat (vv: Vector<Vector<'a, 'm>, 'n>) : Vector<'a, '``m * 'n``> =
    let len = Singleton<'m> *^ Singleton<'n>
    vv |> toArray |> Array.map toArray |> Array.concat |> unsafeCreate len

  let inline take (n: 'n) (v: Vector<'a, '``m when 'm >= 'n``>) : Vector<'a, 'n> =
    let m = length v
    TypeBool.Assert (m >=^ n)
    v |> toArray |> Array.take (RuntimeValue n) |> unsafeCreate n

  let inline skip (n: '``n when 'n <= 'm``) (v: Vector<'a, 'm>) : Vector<'a, '``m - 'n``> =
    let m = length v
    let len = m -^ n
    TypeBool.Assert (len >^ Z)
    v |> toArray |> Array.skip (RuntimeValue n) |> unsafeCreate len

  let inline slice (startIndex: ^``i when ^i < ^n``) (endIndex: ^``j when ^i <= ^j < ^n``) (v: Vector<'a, ^n>) : Vector<'a, S< ^``j - ^i`` >> =
    TypeBool.Assert (startIndex <=^ endIndex)
    TypeBool.Assert (endIndex <^ Singleton< ^n >)
    let len = S (endIndex -^ startIndex)
    (toArray v).[RuntimeValue startIndex .. RuntimeValue endIndex] |> unsafeCreate len
    
  let inline allPairs (v1: Vector<'a, 'm>) (v2: Vector<'b, 'n>) : Vector<'a*'b, '``m * 'n``> =
    let len = Singleton<'m> *^ Singleton<'n>
    Array.allPairs (toArray v1) (toArray v2) |> unsafeCreate len

  let inline pairwise (v: Vector<'a, 'n>) : Vector<'a * 'a, '``n - 1``> =
    let n' = Singleton<'n> -^ S Z
    unsafeCreate n' (toArray v |> Array.pairwise)

  let inline windowed (m: 'm) (v: Vector<'a, 'n>) : Vector<Vector<'a, 'm>, S<'``n - 'm``>> =
    let n = Singleton<'n>
    let nm1 = S (n -^ m)
    unsafeCreate nm1 (toArray v |> Array.windowed (RuntimeValue m) |> Array.map (unsafeCreate m))

  let inline chunkBySize (n: 'n) (v: Vector<'a, '``k * 'n``>) : Vector<Vector<'a, 'n>, 'k> =
    let kn = Singleton<'``k * 'n``>
    let k = kn /^ n
    TypeBool.Assert ((k *^ n) =^ kn)
    unsafeCreate k (toArray v |> Array.chunkBySize (RuntimeValue n) |> Array.map (unsafeCreate n))

  let inline splitInto (n: 'n) (v: Vector<'a, '``n * 'k``>) : Vector<Vector<'a, 'k>, 'n> =
    let nk = Singleton<'``n * 'k``>
    let k = nk /^ n
    TypeBool.Assert ((k *^ n) =^ nk)
    unsafeCreate n (toArray v |> Array.splitInto (RuntimeValue n) |> Array.map (unsafeCreate k))

  let inline mapFold (f: 'State -> 'a -> 'b * 'State) state (v: Vector<'a, 'n>) : Vector<'b, 'n> * 'State =
    let xs, s = toArray v |> Array.mapFold f state
    unsafeCreate Singleton xs, s
  let inline mapFoldBack (f: 'a -> 'State -> 'b * 'State) (v: Vector<'a, 'n>) state : Vector<'b, 'n> * 'State =
    let xs, s = Array.mapFoldBack f (toArray v) state
    unsafeCreate Singleton xs, s

  let inline apply (f: Vector<'a -> 'b, 'n>) (v: Vector<'a, 'n>) : Vector<'b, 'n> = map2 id f v

  let inline norm (v: Vector< ^a, ^n >) : ^a =
    v |> toArray |> Array.sumBy (fun x -> x * x) |> sqrt
  let inline maximumNorm (v: Vector< ^a, ^n >) : ^a =
    v |> toArray |> Array.maxBy abs |> abs
  let inline pNorm (p: ^a) (v: Vector< ^a, ^n >) : ^a =
    (v |> toArray |> Array.maxBy (fun x -> x ** p)) ** (LanguagePrimitives.GenericOne< ^a > / p)

  let inline normalize (v: Vector< ^a, ^n >) : Vector< ^a, ^n > =
    let n = norm v
    v |> map (fun x -> x / n)

  let inline innerProduct (v1: Vector<_, S<'n>>) (v2: Vector<_, S<'n>>) =
    let v = map2 (fun x y -> x * y) v1 v2
    let h, t = head v, tail v
    t |> fold (+) h

  let inline vectorProduct3 (v1: Vector<_, S<S<S<Z>>>>) (v2: Vector<_, S<S<S<Z>>>>) : Vector<_, S<S<S<Z>>>> =
    let x, y = toArray v1, toArray v2
    create (
      x.[1] * y.[2] - x.[2] * y.[1], 
      x.[2] * y.[0] - x.[0] * y.[2], 
      x.[0] * y.[1] - x.[1] * y.[0]
    )

  let toRow (v: Vector<'a, 'n>) : Matrix<'a, S<Z>, 'n> = { Items = array2D [ v.Items ] }
  let toCol (v: Vector<'a, 'n>) : Matrix<'a, 'n, S<Z>> = { Items = array2D [ for x in v.Items -> [x] ] }

module Matrix =
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map (f: 'a -> 'b) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> =
    { Items = Array2D.map f m.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let map2 (f: 'a -> 'b -> 'c) (m1: Matrix<'a, 'm, 'n>) (m2: Matrix<'b, 'm, 'n>) : Matrix<'c, 'm, 'n> =
    { Items =
        Array2D.init (Array2D.length1 m1.Items) (Array2D.length2 m1.Items)
          (fun i j -> f m1.Items.[i, j] m2.Items.[i, j] ) }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi (f: int -> int -> 'a -> 'b) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> =
    { Items = Array2D.mapi (fun i j -> f i j) m.Items }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let mapi2 (f: int -> int -> 'a -> 'b -> 'c) (m1: Matrix<'a, 'm, 'n>) (m2: Matrix<'b, 'm, 'n>) : Matrix<'c, 'm, 'n> =
    { Items =
        Array2D.init (Array2D.length1 m1.Items) (Array2D.length2 m1.Items)
          (fun i j -> f i j m1.Items.[i, j] m2.Items.[i, j] ) }
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter (f: 'a -> unit) (m: Matrix<'a, 'm, 'n>) : unit =
    Array2D.iter f m.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iter2 (f: 'a -> 'b -> unit) (m1: Matrix<'a, 'm, 'n>) (m2: Matrix<'b, 'm, 'n>) : unit =
    for i = 0 to Array2D.length1 m1.Items - 1 do
      for j = 0 to Array2D.length2 m1.Items - 1 do
        f m1.Items.[i, j] m2.Items.[i, j]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri (f: int -> int -> 'a -> unit) (m: Matrix<'a, 'm, 'n>) : unit =
    Array2D.iteri (fun i j -> f i j) m.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let iteri2 (f: int -> int -> 'a -> 'b -> unit) (m1: Matrix<'a, 'm, 'n>) (m2: Matrix<'b, 'm, 'n>) : unit =
    for i = 0 to Array2D.length1 m1.Items - 1 do
      for j = 0 to Array2D.length2 m1.Items - 1 do
        f i j m1.Items.[i, j] m2.Items.[i, j]

  let inline length1 (_: Matrix<'a, 'm, 'n>) : 'm = Singleton<'m>
  let inline length2 (_: Matrix<'a, 'm, 'n>) : 'n = Singleton<'n>
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length1' (mtx: Matrix<'a, 'm, 'n>) = mtx.Items |> Array2D.length1
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let length2' (mtx: Matrix<'a, 'm, 'n>) = mtx.Items |> Array2D.length2

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toArray2D (m: Matrix<'a, 'm, 'n>) = m.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toJaggedArray (m: Matrix<'a, 'm, 'n>) : 'a[][] =
    Array.init (Array2D.length1 m.Items) (fun i ->
      Array.init (Array2D.length2 m.Items) (fun j ->
        m.Items.[i, j]
      )
    )

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let indexed (m: Matrix<'a, 'm, 'n>) : Matrix<int * int * 'a, 'm, 'n> =
    { Items =
        Array2D.init (Array2D.length1 m.Items) (Array2D.length2 m.Items)
          (fun i j -> i,j,m.Items.[i,j])
    }

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

  let inline zeroCreate (m: 'm) (n: 'n) : Matrix<'a, 'm, 'n> =
    Array2D.zeroCreate (RuntimeValue m) (RuntimeValue n) |> unsafeCreate m n
  let inline replicate (m: 'm) (n: 'n) (value: 'a) : Matrix<'a, 'm, 'n> =
    Array2D.create (RuntimeValue m) (RuntimeValue n) value |> unsafeCreate m n
  let inline init (m: 'm) (n: 'n) (f: int -> int -> 'a) : Matrix<'a, 'm, 'n> =
    Array2D.init (RuntimeValue m) (RuntimeValue n) f |> unsafeCreate m n

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeGet i j (m: Matrix<'a, 'm, 'n>) = m.Items.[i,j]

  let inline get (row: ^``i when ^i < ^m``) (column: ^``j when ^j < ^n``) (mat: Matrix<'a, ^m, ^n>) : 'a =
    let m = Singleton<'m>
    let n = Singleton<'n>
    TypeBool.Assert (row <^ m)
    TypeBool.Assert (column <^ n)
    unsafeGet (RuntimeValue row) (RuntimeValue column) mat

  let inline slice
    (rowStart: ^``a when ^a < ^m``) (rowEnd: ^``b when ^a <= ^b < ^m``)
    (colStart: ^``c when ^c < ^n``) (colEnd: ^``d when ^c <= ^d < ^n``)
    (mat: Matrix<'a, ^m, ^n>) : Matrix<'a, S< ^``b - ^a`` >, S< ^``d - ^c`` >> =
    let m, n = Singleton< ^m >, Singleton< ^n >
    TypeBool.Assert (rowStart <=^ rowEnd); TypeBool.Assert (rowEnd <^ m)
    TypeBool.Assert (colStart <=^ colEnd); TypeBool.Assert (colEnd <^ n)
    (toArray2D mat).[RuntimeValue rowStart .. RuntimeValue rowEnd, RuntimeValue colStart .. RuntimeValue colEnd]
    |> unsafeCreate (S (rowEnd -^ rowStart)) (S (colEnd -^ colStart))

  // constants
  let inline zero<'a, ^m, ^n
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and ^n: (static member RuntimeValue: ^n -> int)
                    and ^n: (static member Singleton: ^n -> ^n)
                    and ^a: (static member Zero: ^a)> : Matrix<'a, ^m, ^n> =
    replicate Singleton Singleton LanguagePrimitives.GenericZero

  let inline identity< ^a, ^m
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and ^a: (static member Zero: ^a)
                    and ^a: (static member One: ^a)> : Matrix<'a, ^m, ^m> =
    init Singleton Singleton (fun i j ->
      if i = j then LanguagePrimitives.GenericOne
      else LanguagePrimitives.GenericZero
    )

  let diagonal (mtx: Matrix<'a, 'n, 'n>) : Vector<'a, 'n> =
    { Items = Array.init (length1' mtx) (fun i -> mtx.Items.[i,i]) }

  let inline trace (mtx: Matrix<'a, 'n, 'n>) = diagonal mtx |> Vector.toArray |> Array.sum

  let inline rowVec  (i: ^``i when ^i < ^m``) (mtx: Matrix<'a, 'm, 'n>) : Vector<'a, 'n> =
    TypeBool.Assert (i <^ Singleton<'m>)
    Vector.init Singleton<'n> (fun j -> mtx |> unsafeGet (RuntimeValue i) j)
  let inline row (i: ^``i when ^i < ^m``) (mtx: Matrix<'a, 'm, 'n>) : Matrix<'a, S<Z>, 'n> =
    TypeBool.Assert (i <^ Singleton<'m>)
    init (S Z) Singleton<'n> (fun _ j -> mtx |> unsafeGet (RuntimeValue i) j)
  let inline colVec  (j: ^``j when ^j < ^n``) (mtx: Matrix<'a, 'm, 'n>) : Vector<'a, 'm> =
    TypeBool.Assert (j <^ Singleton<'n>)
    Vector.init Singleton<'m> (fun i -> mtx |> unsafeGet i (RuntimeValue j))
  let inline col (j: ^``j when ^j < ^n``) (mtx: Matrix<'a, 'm, 'n>) : Matrix<'a, 'm, S<Z>> =
    TypeBool.Assert (j <^ Singleton<'n>)
    init Singleton<'m> (S Z) (fun i _ -> mtx |> unsafeGet i (RuntimeValue j))

  let inline ofRows (xs: Vector<Vector<'a, 'n>, 'm>) : Matrix<'a, 'm, 'n> =
    init Singleton<'m> Singleton<'n> (fun i j -> xs.UnsafeGet(i).UnsafeGet(j))
  let inline toRows (mtx: Matrix<'a, 'm, 'n>) : Vector<Vector<'a, 'n>, 'm> =
    let m,  n  = Singleton<'m>, Singleton<'n>
    Vector.init m (fun i ->
      Vector.init n (fun j ->
        mtx |> unsafeGet i j
      )
    )
  let inline ofCols (xs: Vector<Vector<'a, 'm>, 'n>) : Matrix<'a, 'm, 'n> =
    init Singleton<'m> Singleton<'n> (fun i j -> xs.UnsafeGet(j).UnsafeGet(i))
  let inline toCols (mtx: Matrix<'a, 'm, 'n>) : Vector<Vector<'a, 'm>, 'n> =
    let m,  n  = Singleton<'m>, Singleton<'n>
    Vector.init n (fun j ->
      Vector.init m (fun i ->
        mtx |> unsafeGet i j
      )
    )

  let transpose (mtx: Matrix<'t, 'm, 'n>) : Matrix<'t, 'n, 'm> =
    let m = mtx |> length1'
    let n = mtx |> length2'
    let ys = Array2D.init n m (fun j i -> mtx.Items.[i, j])
    { Items = ys }

  let inline matrixProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'n, 'p>) : Matrix<'t, 'm, 'p> =
    let n' = length2' m1
    Array2D.init (length1' m1) (length2' m2) (fun m p ->
      foldiRange LanguagePrimitives.GenericZero<'t> 0 (n' - 1) (fun n result ->
        result + unsafeGet m n m1 * unsafeGet n p m2
      )
    ) |> unsafeCreate Singleton Singleton

  /// returns `(L - E + U, P, swapCount)`. `P ** A = L ** U`.
  let inline decomposeLU (mtx: Matrix<'a, 'n, 'n>) : Matrix<'a, 'n, 'n> * _ * int =
    let n, n' = length1 mtx, length1' mtx
    let zero = LanguagePrimitives.GenericZero<'a>
    let inline swapRows k l (xs: _[,]) =
      let tmp = xs.[k, 0..]
      xs.[k, 0..] <- xs.[l, 0..]
      xs.[l, 0..] <- tmp

    let xs = mtx |> toArray2D |> Array2D.copy
    let pivot = Array.init n' id
    let mutable swapCount = 0

    for i = 0 to n' - 2 do
      let rec go k max row =
        if k >= n' then row
        else
          let x = abs xs.[k, i]
          if x > max then go (k+1) x k
          else go (k+1) max row
      let row = go i zero i
      if row <> i then
        swapCount <- swapCount + 1
        swapRows i row xs
        let tmp = pivot.[row]
        pivot.[row] <- pivot.[i]
        pivot.[i] <- tmp
      for j = i + 1 to n' - 1 do
        xs.[j,i] <- xs.[j,i] / xs.[i,i]
        for k = i + 1 to n' - 1 do
          xs.[j,k] <- xs.[j,k] - xs.[j,i] * xs.[i,k]

    unsafeCreate n n xs, pivot, swapCount

  let inline det (mtx: Matrix<'a, 'n, 'n>) =
    let n' = length1' mtx
    let one = LanguagePrimitives.GenericOne<'a>
    let lu, _, s = decomposeLU mtx
    let detLU = foldiRange one 0 (n'-1) (fun i result -> result * unsafeGet i i lu)
    pown (-one) s * detLU 

  let inline inverse (mtx: Matrix<'t, 'n, 'n>) =
    let lu, perm, _ = decomposeLU mtx
    let n, n' = length1 mtx, length1' mtx
    let b = Array.zeroCreate n'
    let res = Array2D.zeroCreate n' n'
    let lu' = toArray2D lu
    for i = 0 to n' - 1 do
      for j = 0 to n' - 1 do
        b.[j] <-
          if i = perm.[j] then
            LanguagePrimitives.GenericOne<'t>
          else
            LanguagePrimitives.GenericZero<'t>
      res.[0.., i] <- solveImpl lu' b
    unsafeCreate n n res

  let inline kroneckerProduct (mtx1: Matrix<'t, ^m1, ^n1>) (mtx2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 * ^m2``, ^``n1 * ^n2``> =
    let m1,  n1,  m2,  n2  = length1  mtx1, length2  mtx1, length1  mtx2, length2  mtx2
    let m1', n1', m2', n2' = length1' mtx1, length2' mtx1, length1' mtx2, length2' mtx2
    let m1m2, n1n2 = m1 *^ m2, n1 *^ n2
    Array2D.init (m1' * m2') (n1' * n2') (fun i1i2 j1j2 ->
      let i1, i2 = i1i2 / m2', i1i2 % m2'
      let j1, j2 = j1j2 / n2', j1j2 % n2'
      unsafeGet i1 j1 mtx1 * unsafeGet i2 j2 mtx2
    ) |> unsafeCreate m1m2 n1n2

  let inline kroneckerSum (mtx1: Matrix<'t, ^m, ^m>) (mtx2: Matrix<'t, ^n, ^n>) : Matrix<'t, ^``m * ^n``, ^``m * ^n``> =
    map2 (+) (kroneckerProduct mtx1 identity<'t, ^n>) (kroneckerProduct identity<'t, ^m> mtx2)

  let inline directSum (mtx1: Matrix<'t, ^m1, ^n1>) (mtx2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 + ^m2``, ^``n1 + ^n2``> =
    let m1,  n1,  m2,  n2  = length1  mtx1, length2  mtx1, length1  mtx2, length2  mtx2
    let m1', n1', m2', n2' = length1' mtx1, length2' mtx1, length1' mtx2, length2' mtx2
    let xs = Array2D.zeroCreate (m1' + m2') (n1' + n2')
    for i = 0 to m1' - 1 do
      for j = 0 to n1' - 1 do
        xs.[i,j] <- unsafeGet i j mtx1
    for i = 0 to m2' - 1 do
      for j = 0 to n2' - 1 do
        xs.[m1'+i, n1'+j] <- unsafeGet i j mtx2
    unsafeCreate (m1 +^ m2) (n1 +^ n2) xs

  let inline verticalSum (m1: Matrix<'t, ^m1, ^n>) (m2: Matrix<'t, ^m2, ^n>) : Matrix<'t, ^``m1 + ^m2``, ^n> =
    let m1m2 = Singleton< ^m1 > +^ Singleton< ^m2 >
    let n = Singleton< ^n >
    unsafeCreate m1m2 n <| failwith "TODO"

  let inline horizontalSum (m1: Matrix<'t, ^m, ^n1>) (m2: Matrix<'t, ^m, ^n2>) : Matrix<'t, ^m, ^``n1 + ^n2``> =
    let m = Singleton< ^m >
    let n1n2 = Singleton< ^n1 > +^ Singleton< ^n2 >
    unsafeCreate m n1n2 <| failwith "TODO"

  let inline hadamardProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'm, 'n>) : Matrix<'t, 'm, 'n> =
    map2 (*) m1 m2

type Matrix<'Item, 'Row, 'Column> with
  static member inline Item (mtx: Matrix<'a, 'm, 'n>, (m, n)) = Matrix.get m n mtx
  static member inline Map  (mtx: Matrix<'a, 'm, 'n>, f: 'a -> 'b) = Matrix.map f mtx
  static member inline Return (x: 'x) : Matrix<'x, 'm, 'n> = Matrix.replicate Singleton Singleton x
  static member inline ( <*> ) (f: Matrix<'x -> 'y, 'm, 'n>, x: Matrix<'x, 'm, 'n>) = Matrix.map2 id f x
  static member inline get_Zero () : Matrix<'a, 'm, 'n> = Matrix.zero
  static member inline ( + ) (m1, m2) = Matrix.map2 (+) m1 m2
  static member inline ( - ) (m1, m2) = Matrix.map2 (-) m1 m2
  static member inline ( * ) (m1, m2) = Matrix.map2 (*) m1 m2
  static member inline ( / ) (m1, m2) = Matrix.map2 (/) m1 m2
  static member inline ( *. ) (m: Matrix<'a,_,_>, s: 'a) = Matrix.map ((*) s) m
  static member inline ( .* ) (s: 'a, m: Matrix<'a,_,_>) = Matrix.map ((*) s) m
  static member inline ( /. ) (m: Matrix<'a,_,_>, s: 'a) = Matrix.map (fun x -> x / s) m
  static member inline ( ~- ) m = Matrix.map ((~-)) m

type Vector<'Item, 'Length> with
  static member inline Item (v: Vector<'a, 'n>, i) = Vector.get i v
  static member inline Map (v: Vector<'a, 'n>, f: 'a -> 'b) : Vector<'b, 'n> = Vector.map f v
  static member inline Return (x: 'x) : Vector<'x, 'n> = Vector.replicate Singleton x
  static member inline ( <*> ) (f: Vector<'x -> 'y, 'n>, x: Vector<'x, 'n>) : Vector<'y, 'n> = Vector.apply f x
  static member inline get_Zero () : Vector<'x, 'n> = Vector.zero
  static member inline ( + ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (+) v1 v2
  static member inline ( - ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (-) v1 v2
  static member inline ( * ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (*) v1 v2
  static member inline ( / ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (/) v1 v2
  static member inline ( *. ) (v: Vector<'a, 'n>, s: 'a) = Vector.map (fun x -> x * s) v
  static member inline ( .* ) (s: 'a, v: Vector<'a, 'n>) = Vector.map (fun x -> x * s) v
  static member inline ( /. ) (v: Vector<'a, 'n>, s: 'a) = Vector.map (fun x -> x / s) v
  static member inline ( ~- ) (v: Vector<_, 'n>) = v |> Vector.map ((~-))
  static member inline ToSeq (v: Vector<'x, 'n>) = v |> Vector.toSeq
  static member inline FoldBack (v: Vector<'x, 'n>, f, z) = Array.foldBack f (Vector.toArray v) z
  static member op_Explicit (v: Vector<'x, 'n>) : Matrix<'x, S<Z>, 'n> = Vector.toRow v
  static member op_Explicit (v: Vector<'x, 'n>) : Matrix<'x, 'n, S<Z>> = Vector.toCol v

[<AutoOpen>]
module MatrixOperators =
  let inline vector (definition: '``a * 'a * .. * 'a``) : Vector<'a, 'n> =
    Vector.create definition
 
  let inline matrix (definition: '``('a * .. * 'a) * .. * ('a * .. * 'a)``) : Matrix<'a, 'm, 'n> =
    Matrix.create definition
 
  let inline (|Vector|) (vect: Vector<'a, 'n>) : '``a * 'a * .. * 'a`` =
    let n = Singleton<'n>
    let t = ArrayToTuple.Invoke(Vector.toArray vect, n) |> MatrixHelpers.retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'a>, n)
    t

  let inline (|Matrix|) (mtx: Matrix<'a, 'm, 'n>) : '``('a * .. * 'a) * .. * ('a * .. * 'a)`` =
    let m = Singleton<'m>
    let n = Singleton<'n>
    let items = Matrix.toArray2D mtx
    let xs : 'at[] = [|
      for i = 0 to Array2D.length1 items - 1 do
        let col = [| for j = 0 to Array2D.length2 items - 1 do yield items.[i, j] |]
        let t = ArrayToTuple.Invoke(col, n) |> MatrixHelpers.retype
        AssertTupleType.Invoke(t, Unchecked.defaultof<'a>, n)
        yield t
    |]
    let t = ArrayToTuple.Invoke(xs, m) |> MatrixHelpers.retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'at>, m)
    t

#if DEBUG
module MatrixTests =
  let v1 = vector (1,2,3,4,5)
  let v2 = vector (1,2,3,4,5,6,7,8,9,0,1,2,3,4,5)
  let (Vector(_,_,_,_,_)) = v1
  let (Vector(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) = v2

  let m1 =
    matrix (
      (1,0,0,0),
      (0,1,0,0),
      (0,0,1,0)
    )
  let m2 =
    matrix (
      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
      (0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
      (0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
      (0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
      (0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
    )
  let (Matrix(_x1,_x2,_x3)) = m1
  let (Matrix(_y1: int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int,_y2,_y3,_y4,_y5,_y6,_y7,_y8)) = m2

#endif 
