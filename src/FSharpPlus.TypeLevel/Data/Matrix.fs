namespace FSharpPlus.Data

#if !FABLE_COMPILER

open System.Runtime.CompilerServices
open System.ComponentModel
open FSharpPlus.Control
open FSharpPlus.TypeLevel
open TypeLevelOperators
open FSharpPlus.Internals.Prelude

// Items : 'Item[ 'Column, 'Row ]
[<Struct; StructuredFormatDisplay("{Items}")>]
type Matrix< 'Item, 'Row, 'Column > = private { Items: 'Item[,] } with
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  member this.UnsafeGet (i, j) = this.Items.[i, j]
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member UnsafeCreate (_row: 'm, _column: 'n, items: _[,]) : Matrix<_, 'm, 'n> =
    { Items = items }
  
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
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  static member UnsafeCreate (_length: 'n, items: _[]) : Vector<_, 'n> =
    { Items = items }
  
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

  let inline length  (_: Vector<'a, 'n>) : 'n  = Singleton<'n>
  
  let inline length' (_: Vector<'a, ^n>) : int = RuntimeValue (Singleton< ^n >)

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toArray (vec: Vector<'a, 'n>) : 'a[] = vec.Items
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toSeq (vec: Vector<'a, 'n>) : 'a seq = vec.Items :> _
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let toList (vec: Vector<'a, 'n>) : 'a list = vec.Items |> List.ofArray

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeCreate (_length: 'n) (xs: _[]) : Vector<_, 'n> =
    { Items = xs }

  /// Tries to create a vector of length `n`.
  /// If the length of `xs` does not match, it will return `None`.
  /// Otherwise, it will return the vector with `Some`.
  ///
  /// You can also use `Vector.TryCreate<n>.OfArray xs`.
  let inline tryOfArray (length: 'n) (xs: _[]) : Vector<_, 'n> option =
    if RuntimeValue length = xs.Length then
      Some (unsafeCreate length xs)
    else
      None

  /// Tries to create a vector of length `n`.
  /// If the length of `xs` does not match, it will return `None`.
  /// Otherwise, it will return the vector with `Some`.
  ///
  /// You can also use `Vector.TryCreate<n>.OfList xs`.
  let inline tryOfList (length: 'n) (xs: _ list) : Vector<_, 'n> option = tryOfArray length (List.toArray xs)

  /// Tries to create a vector of length `n`.
  /// If the length of `xs` does not match, it will return `None`.
  /// Otherwise, it will return the vector with `Some`.
  ///
  /// You can also use `Vector.TryCreate<n>.OfSeq xs`.
  let inline tryOfSeq (length: 'n) (xs: _ seq) : Vector<_, 'n> option = tryOfArray length (Seq.toArray xs)

  let inline create (definition: '``a * 'a * .. * 'a``) : Vector<'a, 'n> =
    unsafeCreate (CountTuple.Invoke definition) (TupleToList.Invoke definition |> Array.ofList)

  let singleton (x: 'a) : Vector<'a, S<Z>> = { Items = [| x |] }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeGet index (vec: Vector<'a, 'n>) = vec.Items.[index]

  /// You can also use `Vector.Get<i>.Invoke vec`.
  let inline get (index: ^``i when ^i < ^n``) (vec: Vector<'a, ^n>) =
    let n = Singleton< ^n >
    TypeBool.Assert (index <^ n)
    vec |> unsafeGet (RuntimeValue index)

  /// You can also use `Vector.ZeroCreate<n>.Invoke()`.
  let inline zeroCreate (n: 'n) : Vector<'a, 'n> =
    Array.zeroCreate (RuntimeValue n) |> unsafeCreate n
  /// You can also use `Vector.Replicate<n>.Invoke value`.
  let inline replicate (n: 'n) (value: 'a) : Vector<'a, 'n> =
    Array.replicate (RuntimeValue n) value |> unsafeCreate n
  /// You can also use `Vector.Init<n>.Invoke f`.
  let inline init (n: 'n) (f: int -> 'a) : Vector<'a, 'n > =
    Array.init (RuntimeValue n) f |> unsafeCreate n

  // constants
  let inline zero<'a, ^m, ^n
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and (Zero or ^a): (static member Zero: ^a * Zero -> ^a)
                  > : Vector<'a, ^m> =
    replicate Singleton (Zero.Invoke ())

  let empty<'a> : Vector<'a, Z> = { Items = [||] }

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

  /// You can also use `Vector.Take<n>.Invoke v`.
  let inline take (n: 'n) (v: Vector<'a, '``m when 'm >= 'n``>) : Vector<'a, 'n> =
    let m = length v
    TypeBool.Assert (m >=^ n)
    v |> toArray |> Array.take (RuntimeValue n) |> unsafeCreate n

  /// You can also use `Vector.Skip<n>.Invoke v`.
  let inline skip (n: '``n when 'n <= 'm``) (v: Vector<'a, 'm>) : Vector<'a, '``m - 'n``> =
    let m = length v
    let len = m -^ n
    TypeBool.Assert (len >^ Z)
    v |> toArray |> Array.skip (RuntimeValue n) |> unsafeCreate len

  /// You can also use `Vector.Slice<i, j>.Invoke v`.
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

  /// You can also use `Vector.Windowed<m>.Invoke v`.
  let inline windowed (m: 'm) (v: Vector<'a, 'n>) : Vector<Vector<'a, 'm>, S<'``n - 'm``>> =
    let n = Singleton<'n>
    let nm1 = S (n -^ m)
    unsafeCreate nm1 (toArray v |> Array.windowed (RuntimeValue m) |> Array.map (unsafeCreate m))

  /// You can also use `Vector.ChunkBySize<n>.Invoke v`.
  let inline chunkBySize (n: 'n) (v: Vector<'a, '``k * 'n``>) : Vector<Vector<'a, 'n>, 'k> =
    let kn = Singleton<'``k * 'n``>
    let k = kn /^ n
    TypeBool.Assert ((k *^ n) =^ kn)
    unsafeCreate k (toArray v |> Array.chunkBySize (RuntimeValue n) |> Array.map (unsafeCreate n))

  /// You can also use `Vector.SplitInto<n>.Invoke v`.
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

  /// <description>
  ///   Converts the vector of vectors to a square matrix and returns its diagonal.
  /// </description>
  /// <seealso href="https://stackoverflow.com/questions/5802628/monad-instance-of-a-number-parameterised-vector" />
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let join (vv: Vector<Vector<'a, 'n>, 'n>): Vector<'a, 'n> =
    { Items = Array.init (Array.length vv.Items) (fun i -> vv.Items.[i].Items.[i]) }

  let inline bind (f: 'a -> Vector<'b, 'n>) (v: Vector<'a, 'n>) : Vector<'b, 'n> =
    v |> map f |> join

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

  let inline directProduct (v1: Vector<'a, ^m>) (v2: Vector<'a, ^n>) : Matrix<'a, ^m, ^n> =
    let m, n = Singleton< ^m>, Singleton< ^n>
    let items =
      Array2D.init (RuntimeValue m) (RuntimeValue n) (fun i j ->
        unsafeGet i v1 * unsafeGet j v2
      )
    Matrix<_, _, _>.UnsafeCreate(m, n, items)

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
  let map3 (f: 'a -> 'b -> 'c -> 'd) (m1: Matrix<'a, 'm, 'n>) (m2: Matrix<'b, 'm, 'n>) (m3: Matrix<'c, 'm, 'n>) : Matrix<'d, 'm, 'n> =
    { Items =
        Array2D.init (Array2D.length1 m1.Items) (Array2D.length2 m1.Items)
          (fun i j -> f m1.Items.[i, j] m2.Items.[i, j] m3.Items.[i, j] ) }

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

  let inline apply (f: Matrix<'a -> 'b, 'm, 'n>) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> = map2 id f m

  /// <description>
  ///   Converts the matrix of matrices to a 3D cube matrix and returns its diagonal.
  /// </description>
  /// <seealso href="https://stackoverflow.com/questions/5802628/monad-instance-of-a-number-parameterised-vector" />
  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let join (m: Matrix<Matrix<'a, 'm, 'n>, 'm, 'n>) : Matrix<'a, 'm, 'n> =
    { Items =
        Array2D.init (Array2D.length1 m.Items) (Array2D.length2 m.Items)
          (fun i j -> m.Items.[i, j].Items.[i, j] ) }

  let inline bind (f: 'a -> Matrix<'b, 'm, 'n>) (m: Matrix<'a, 'm, 'n>) : Matrix<'b, 'm, 'n> = m |> map f |> join

  let inline rowLength (_: Matrix<'a, 'm, 'n>) : 'm = Singleton<'m>
  let inline colLength (_: Matrix<'a, 'm, 'n>) : 'n = Singleton<'n>
  let inline rowLength' (_: Matrix<'a, ^m, 'n>) : int = RuntimeValue (Singleton< ^m >)
  let inline colLength' (_: Matrix<'a, 'm, ^n>) : int = RuntimeValue (Singleton< ^n >)

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
    { Items = Array2D.mapi (fun i j x -> i,j,x) m.Items }

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeCreate (_row: 'm) (_column: 'n) (items: _[,]) : Matrix<_, 'm, 'n> =
    { Items = items }

  /// Tries to create a matrix of given dimension.
  /// If the dimension does not match, returns `None`. Otherwise returns the matrix with `Some`.
  ///
  /// You can also use `Matrix.TryCreate.OfArray2D`.
  let inline tryOfArray2D (row: ^m) (column: ^n) (items: _[,]) : Matrix<_, ^m, ^n> option =
    if RuntimeValue row = Array2D.length1 items && RuntimeValue column = Array2D.length2 items then
      Some (unsafeCreate row column items)
    else
      None

  /// Tries to create a matrix of given dimension.
  /// If the dimension does not match, returns `None`. Otherwise returns the matrix with `Some`.
  ///
  /// You can also use `Matrix.TryCreate.OfJaggedSeq`.
  let inline tryOfJaggedSeq (row: ^m) (column: ^n) (items: #seq<_> seq) : Matrix<_, ^m, ^n> option =
    try
      tryOfArray2D row column (array2D items)
    with
      | :? System.ArgumentException -> None

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

  /// You can also use `Matrix.ZeroCreate<m, n>.Invoke()`.
  let inline zeroCreate (m: 'm) (n: 'n) : Matrix<'a, 'm, 'n> =
    Array2D.zeroCreate (RuntimeValue m) (RuntimeValue n) |> unsafeCreate m n
  /// You can also use `Matrix.Replicate<m, n>.Invoke value`.
  let inline replicate (m: 'm) (n: 'n) (value: 'a) : Matrix<'a, 'm, 'n> =
    Array2D.create (RuntimeValue m) (RuntimeValue n) value |> unsafeCreate m n
  /// You can also use `Matrix.Init<m, n>.Invoke f`.
  let inline init (m: 'm) (n: 'n) (f: int -> int -> 'a) : Matrix<'a, 'm, 'n> =
    Array2D.init (RuntimeValue m) (RuntimeValue n) f |> unsafeCreate m n

  [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
  let unsafeGet i j (m: Matrix<'a, 'm, 'n>) = m.Items.[i,j]

  /// You can also use `Matrix.Get<i, j>.Invoke mat`.
  let inline get (row: ^``i when ^i < ^m``) (column: ^``j when ^j < ^n``) (mat: Matrix<'a, ^m, ^n>) : 'a =
    let m = Singleton<'m>
    let n = Singleton<'n>
    TypeBool.Assert (row <^ m)
    TypeBool.Assert (column <^ n)
    unsafeGet (RuntimeValue row) (RuntimeValue column) mat

  /// You can also use `Matrix.Slice<a, b, c, d>.Invoke mat`.
  let inline slice
    (rowStart: ^``a when ^a < ^m``) (rowEnd: ^``b when ^a <= ^b < ^m``)
    (colStart: ^``c when ^c < ^n``) (colEnd: ^``d when ^c <= ^d < ^n``)
    (mat: Matrix<'t, ^m, ^n>) : Matrix<'t, S< ^``b - ^a`` >, S< ^``d - ^c`` >> =
    let m, n = Singleton< ^m >, Singleton< ^n >
    TypeBool.Assert (rowStart <=^ rowEnd); TypeBool.Assert (rowEnd <^ m)
    TypeBool.Assert (colStart <=^ colEnd); TypeBool.Assert (colEnd <^ n)
    (toArray2D mat).[RuntimeValue rowStart .. RuntimeValue rowEnd, RuntimeValue colStart .. RuntimeValue colEnd]
    |> unsafeCreate (S (rowEnd -^ rowStart)) (S (colEnd -^ colStart))

  /// You can also use `Matrix.SliceRow<a, b>.Invoke mat`.
  let inline sliceRow
    (rowStart: ^``a when ^a < ^m``) (rowEnd: ^``b when ^a <= ^b < ^m``)
    (mat: Matrix<'t, ^m, ^n>) : Matrix<'t, S< ^``b - ^a`` >, ^n> =
    let m, n = Singleton< ^m >, Singleton< ^n >
    TypeBool.Assert (rowStart <=^ rowEnd); TypeBool.Assert (rowEnd <^ m)
    (toArray2D mat).[RuntimeValue rowStart .. RuntimeValue rowEnd, 0 .. RuntimeValue n - 1]
    |> unsafeCreate (S (rowEnd -^ rowStart)) n

  /// You can also use `Matrix.SliceCol<a, b>.Invoke mat`.
  let inline sliceCol
    (colStart: ^``c when ^c < ^n``) (colEnd: ^``d when ^c <= ^d < ^n``)
    (mat: Matrix<'t, ^m, ^n>) : Matrix<'t, ^m, S< ^``d - ^c`` >> =
    let m, n = Singleton< ^m >, Singleton< ^n >
    TypeBool.Assert (colStart <=^ colEnd); TypeBool.Assert (colEnd <^ n)
    (toArray2D mat).[0 .. RuntimeValue m - 1, RuntimeValue colStart .. RuntimeValue colEnd]
    |> unsafeCreate m (S (colEnd -^ colStart))

  // constants
  let inline zero<'a, ^m, ^n
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and ^n: (static member RuntimeValue: ^n -> int)
                    and ^n: (static member Singleton: ^n -> ^n)
                    and (Zero or ^a): (static member Zero: ^a * Zero -> ^a)
                  > : Matrix<'a, ^m, ^n> =
    replicate Singleton Singleton (Zero.Invoke ())

  let inline identity< ^a, ^m
                   when ^m: (static member RuntimeValue: ^m -> int)
                    and ^m: (static member Singleton: ^m -> ^m)
                    and (Zero or ^a): (static member Zero: ^a * Zero -> ^a)
                    and (One or ^a):  (static member One: ^a * One -> ^a)
                  > : Matrix<'a, ^m, ^m> =
    init Singleton Singleton (fun i j ->
      if i = j then One.Invoke ()
      else Zero.Invoke ()
    )

  let diagonal (mtx: Matrix<'a, 'n, 'n>) : Vector<'a, 'n> =
    { Items = Array.init (Array2D.length1 mtx.Items) (fun i -> mtx.Items.[i,i]) }

  let inline trace (mtx: Matrix<'a, 'n, 'n>) = diagonal mtx |> Vector.toArray |> Array.sum

  /// You can also use `Matrix.Row<i>.AsVector mtx`.
  let inline rowVec  (i: ^``i when ^i < ^m``) (mtx: Matrix<'a, 'm, 'n>) : Vector<'a, 'n> =
    TypeBool.Assert (i <^ Singleton<'m>)
    Vector.init Singleton<'n> (fun j -> mtx |> unsafeGet (RuntimeValue i) j)
  /// You can also use `Matrix.Row<i>.AsMatrix mtx`.
  let inline row (i: ^``i when ^i < ^m``) (mtx: Matrix<'a, 'm, 'n>) : Matrix<'a, S<Z>, 'n> =
    TypeBool.Assert (i <^ Singleton<'m>)
    init (S Z) Singleton<'n> (fun _ j -> mtx |> unsafeGet (RuntimeValue i) j)
  /// You can also use `Matrix.Col<j>.AsVector mtx`.
  let inline colVec  (j: ^``j when ^j < ^n``) (mtx: Matrix<'a, 'm, 'n>) : Vector<'a, 'm> =
    TypeBool.Assert (j <^ Singleton<'n>)
    Vector.init Singleton<'m> (fun i -> mtx |> unsafeGet i (RuntimeValue j))
  /// You can also use `Matrix.Col<j>.AsMatrix mtx`.
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

  let inline transpose (mtx: Matrix<'t, 'm, 'n>) : Matrix<'t, 'n, 'm> =
    let m = mtx |> rowLength
    let n = mtx |> colLength
    init n m (fun j i -> unsafeGet i j mtx)

  let inline private foldiRange initValue startIndex endIndex f =
    let mutable result = initValue
    for i = startIndex to endIndex do
      result <- f i result
    result

  let inline matrixProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'n, 'p>) : Matrix<'t, 'm, 'p> =
    let n' = colLength' m1
    Array2D.init (rowLength' m1) (colLength' m2) (fun m p ->
      foldiRange LanguagePrimitives.GenericZero<'t> 0 (n' - 1) (fun n result ->
        result + unsafeGet m n m1 * unsafeGet n p m2
      )
    ) |> unsafeCreate Singleton Singleton

  let inline kroneckerProduct (mtx1: Matrix<'t, ^m1, ^n1>) (mtx2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 * ^m2``, ^``n1 * ^n2``> =
    let m1,  n1,  m2,  n2  = rowLength  mtx1, colLength  mtx1, rowLength  mtx2, colLength  mtx2
    let m1', n1', m2', n2' = rowLength' mtx1, colLength' mtx1, rowLength' mtx2, colLength' mtx2
    let m1m2, n1n2 = m1 *^ m2, n1 *^ n2
    Array2D.init (m1' * m2') (n1' * n2') (fun i1i2 j1j2 ->
      let i1, i2 = i1i2 / m2', i1i2 % m2'
      let j1, j2 = j1j2 / n2', j1j2 % n2'
      unsafeGet i1 j1 mtx1 * unsafeGet i2 j2 mtx2
    ) |> unsafeCreate m1m2 n1n2

  let inline kroneckerSum (mtx1: Matrix<'t, ^m, ^m>) (mtx2: Matrix<'t, ^n, ^n>) : Matrix<'t, ^``m * ^n``, ^``m * ^n``> =
    map2 (+) (kroneckerProduct mtx1 identity<'t, ^n>) (kroneckerProduct identity<'t, ^m> mtx2)

  let inline directSum (mtx1: Matrix<'t, ^m1, ^n1>) (mtx2: Matrix<'t, ^m2, ^n2>) : Matrix<'t, ^``m1 + ^m2``, ^``n1 + ^n2``> =
    let m1,  n1,  m2,  n2  = rowLength  mtx1, colLength  mtx1, rowLength  mtx2, colLength  mtx2
    let m1', n1', m2', n2' = rowLength' mtx1, colLength' mtx1, rowLength' mtx2, colLength' mtx2
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
    let m1' = rowLength' m1
    let n = Singleton< ^n >
    init m1m2 n (fun i j ->
      if i < m1' then unsafeGet i j m1
      else unsafeGet (i - m1') j m2
    )

  let inline horizontalSum (m1: Matrix<'t, ^m, ^n1>) (m2: Matrix<'t, ^m, ^n2>) : Matrix<'t, ^m, ^``n1 + ^n2``> =
    let m = Singleton< ^m >
    let n1n2 = Singleton< ^n1 > +^ Singleton< ^n2 >
    let n1' = rowLength' m1
    init m n1n2 (fun i j ->
      if j < n1' then unsafeGet i j m1
      else unsafeGet i (j - n1') m2
    )

  let inline hadamardProduct (m1: Matrix<'t, 'm, 'n>) (m2: Matrix<'t, 'm, 'n>) : Matrix<'t, 'm, 'n> =
    map2 (*) m1 m2

type Matrix<'Item, 'Row, 'Column> with
  static member inline Item (mtx: Matrix<'a, 'm, 'n>, (m, n)) = Matrix.get m n mtx
  static member inline Map  (mtx: Matrix<'a, 'm, 'n>, f: 'a -> 'b) = Matrix.map f mtx

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member inline Map2 (f, m1, m2) : Matrix<'x, 'm, 'n> = Matrix.map2 f m1 m2

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member inline Map3 (f, m1, m2, m3) : Matrix<'x, 'm, 'n> = Matrix.map3 f m1 m2 m3

  static member inline Return (x: 'x) : Matrix<'x, 'm, 'n> = Matrix.replicate Singleton Singleton x
  static member inline Pure   (x: 'x) : Matrix<'x, 'm, 'n> = Matrix.replicate Singleton Singleton x
  static member inline ( <*> ) (f: Matrix<'x -> 'y, 'm, 'n>, x: Matrix<'x, 'm, 'n>) = Matrix.apply f x
  static member inline ( <.> ) (f: Matrix<'x -> 'y, 'm, 'n>, x: Matrix<'x, 'm, 'n>) = Matrix.apply f x
  static member inline Join (x: Matrix<Matrix<'x, 'm, 'n>, 'm, 'n>) = Matrix.join x
  static member inline ( >>= ) (x: Matrix<'x, 'm, 'n>, f: 'x -> Matrix<'y, 'm, 'n>) = Matrix.bind f x
  static member inline get_Zero () : Matrix<'a, 'm, 'n> = Matrix.zero
  static member inline ( + ) (m1, m2) = Matrix.map2 (+) m1 m2
  static member inline ( - ) (m1, m2) = Matrix.map2 (-) m1 m2
  /// matrix product
  static member inline ( |*| ) (m1, m2) = Matrix.matrixProduct m1 m2
  /// kronecker (tensor) product
  static member inline ( @* ) (m1, m2) = Matrix.kroneckerProduct m1 m2
  /// hadamard (element-wise) product
  static member inline ( * ) (m1, m2) = Matrix.map2 (*) m1 m2
  static member inline ( / ) (m1, m2) = Matrix.map2 (/) m1 m2
  static member inline ( @| ) (m1: Matrix<_,'m,^n1>, m2: Matrix<_,'m,^n2>) : Matrix<_,'m,^``n1 + ^n2``> = Matrix.horizontalSum m1 m2
  static member inline ( @- ) (m1: Matrix<_,^m1,'n>, m2: Matrix<_,^m2,'n>) : Matrix<_,^``m1 + ^m2``,'n> = Matrix.verticalSum m1 m2
  static member inline ( ~- ) m = Matrix.map ((~-)) m

  // As generic number literals
  static member inline FromInt32  (i: int   ) = Matrix.replicate Singleton Singleton (FromInt32.Invoke  i)
  static member inline FromInt64  (i: int64 ) = Matrix.replicate Singleton Singleton (FromInt64.Invoke  i)
  static member inline FromBigInt (i: bigint) = Matrix.replicate Singleton Singleton (FromBigInt.Invoke i)


type Vector<'Item, 'Length> with
  static member inline Item (v: Vector<'a, 'n>, i) = Vector.get i v
  static member inline Map (v: Vector<'a, 'n>, f: 'a -> 'b) : Vector<'b, 'n> = Vector.map f v
  
  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member inline Map2 (f, vec1, vec2) : Vector<'x, 'n> = Vector.map2 f vec1 vec2

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member inline Map3 (f, vec1, vec2, vec3) : Vector<'x, 'n> = Vector.map3 f vec1 vec2 vec3

  static member inline Return (x: 'x) : Vector<'x, 'n> = Vector.replicate Singleton x
  static member inline Pure   (x: 'x) : Vector<'x, 'n> = Vector.replicate Singleton x
  static member inline ( <*> ) (f: Vector<'x -> 'y, 'n>, x: Vector<'x, 'n>) : Vector<'y, 'n> = Vector.apply f x
  static member inline ( <.> ) (f: Vector<'x -> 'y, 'n>, x: Vector<'x, 'n>) : Vector<'y, 'n> = Vector.apply f x
  static member inline Join (x: Vector<Vector<'x, 'n>, 'n>) : Vector<'x, 'n> = Vector.join x
  static member inline ( >>= ) (x: Vector<'x, 'n>, f: 'x -> Vector<'y, 'n>) = Vector.bind f x
  
  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member inline Zip (x, y) = Vector.zip x y

  static member inline get_Zero () : Vector<'x, 'n> = Vector.zero
  static member inline ( + ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (+) v1 v2
  static member inline ( - ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (-) v1 v2
  /// dot (inner) product
  static member inline ( |*| ) (v1, v2) = Vector.innerProduct v1 v2
  /// direct (tensor) product
  static member inline ( @* ) (v1, v2) = Vector.directProduct v1 v2
  /// cross product
  static member inline ( %* ) (v1, v2) = Vector.vectorProduct3 v1 v2
  /// hadamard (element-wise) product
  static member inline ( * ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (*) v1 v2
  static member inline ( / ) (v1: Vector<_, 'n>, v2: Vector<_, 'n>) = Vector.map2 (/) v1 v2
  static member inline ( ~- ) (v: Vector<_, 'n>) = v |> Vector.map ((~-))
  static member inline ( @@ ) (v1: Vector<_, ^m>, v2: Vector<_, ^n>) : Vector<_, ^``m + ^n``> = Vector.append v1 v2  
  static member inline ToSeq (v: Vector<'x, 'n>) = v |> Vector.toSeq
  static member inline FoldBack (v: Vector<'x, 'n>, f, z) = Array.foldBack f (Vector.toArray v) z
  static member inline Reduce   (v: Vector<'x, S<'n>>, f) = Array.reduce   f (Vector.toArray v)
  static member op_Explicit (v: Vector<'x, 'n>) : Matrix<'x, S<Z>, 'n> = Vector.toRow v
  static member op_Explicit (v: Vector<'x, 'n>) : Matrix<'x, 'n, S<Z>> = Vector.toCol v

  // As generic number literals
  static member inline FromInt32  (i: int   ) = Vector.replicate Singleton (FromInt32.Invoke  i)
  static member inline FromInt64  (i: int64 ) = Vector.replicate Singleton (FromInt64.Invoke  i)
  static member inline FromBigInt (i: bigint) = Vector.replicate Singleton (FromBigInt.Invoke i)

[<AutoOpen>]
module MatrixOperators =
  let inline vector (definition: '``a * 'a * .. * 'a``) : Vector<'a, 'n> =
    Vector.create definition
 
  let inline matrix (definition: '``('a * .. * 'a) * .. * ('a * .. * 'a)``) : Matrix<'a, 'm, 'n> =
    Matrix.create definition
 
  let inline (|Vector|) (vect: Vector<'a, 'n>) : '``a * 'a * .. * 'a`` =
    let n = Singleton<'n>
    let t = ArrayToTuple.Invoke(Vector.toArray vect, n) |> retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'a>, n)
    t

  let inline (|Matrix|) (mtx: Matrix<'a, 'm, 'n>) : '``('a * .. * 'a) * .. * ('a * .. * 'a)`` =
    let m = Singleton<'m>
    let n = Singleton<'n>
    let items = Matrix.toArray2D mtx
    let xs : 'at[] = [|
      for i = 0 to Array2D.length1 items - 1 do
        let col = [| for j = 0 to Array2D.length2 items - 1 do yield items.[i, j] |]
        let t = ArrayToTuple.Invoke(col, n) |> retype
        AssertTupleType.Invoke(t, Unchecked.defaultof<'a>, n)
        yield t
    |]
    let t = ArrayToTuple.Invoke(xs, m) |> retype
    AssertTupleType.Invoke(t, Unchecked.defaultof<'at>, m)
    t

#endif