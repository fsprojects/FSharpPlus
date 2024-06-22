namespace FSharpPlus.Data

open System.Reflection
open System.ComponentModel
open System.Runtime.CompilerServices
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

open FSharpPlus.TypeLevel
open TypeLevelOperators

[<EditorBrowsable(EditorBrowsableState.Never)>]
module VectorProviderHelpers =
  type Get<'i>() = class end
  type ZeroCreate<'n>() = class end
  type Replicate<'n>() = class end
  type Init<'n>() = class end
  type Take<'n>() = class end
  type Skip<'n>() = class end
  type Slice<'i, 'j>() = class end
  type Windowed<'m>() = class end
  type ChunkBySize<'n>() = class end
  type SplitInto<'n>() = class end
  type TryCreate<'n>() = class end

open VectorProviderHelpers

[<Extension; EditorBrowsable(EditorBrowsableState.Never)>]
type VectorProviderHelpersImpl =
  [<Extension>]
  static member inline Invoke (_: unit -> Get< ^i >, vec) =
    Vector.get Singleton< ^i > vec
  [<Extension>]
  static member inline Invoke (_: unit -> ZeroCreate< ^n >) =
    Vector.zeroCreate Singleton< ^n >
  [<Extension>]
  static member inline Invoke (_: unit -> Replicate< ^n >, x) =
    Vector.replicate Singleton< ^n > x
  [<Extension>]
  static member inline Invoke (_: unit -> Init< ^n >, f) =
    Vector.init Singleton< ^n > f
  [<Extension>]
  static member inline Invoke (_: unit -> Take< ^n >, vec) =
    Vector.take Singleton< ^n > vec
  [<Extension>]
  static member inline Invoke (_: unit -> Skip< ^n >, vec) =
    Vector.skip Singleton< ^n > vec
  [<Extension>]
  static member inline Invoke (_: unit -> Slice< ^i, ^j >, vec) =
    Vector.slice Singleton< ^i > Singleton< ^j > vec
  [<Extension>]
  static member inline Invoke (_: unit -> Windowed< ^m >, vec) =
    Vector.windowed Singleton< ^m > vec
  [<Extension>]
  static member inline Invoke (_: unit -> ChunkBySize< ^n >, vec) =
    Vector.chunkBySize Singleton< ^n > vec
  [<Extension>]
  static member inline Invoke (_: unit -> SplitInto< ^n >, vec) =
    Vector.splitInto Singleton< ^n > vec
  [<Extension>]
  static member inline OfArray (_: unit -> TryCreate< ^n >, xs) =
    Vector.tryOfArray Singleton< ^n > xs
  [<Extension>]
  static member inline OfList (_: unit -> TryCreate< ^n >, xs) =
    Vector.tryOfList Singleton< ^n > xs
  [<Extension>]
  static member inline OfSeq (_: unit -> TryCreate< ^n >, xs) =
    Vector.tryOfSeq Singleton< ^n > xs

[<EditorBrowsable(EditorBrowsableState.Never); TypeProvider>]
type VectorProvider (cfg) as this =
  inherit TypeProviderForNamespaces(cfg)
  let thisAsm = Assembly.GetExecutingAssembly()
  let root = "FSharpPlus.Data"
  let vty = ProvidedTypeDefinition(thisAsm, root, "Vector", None)
  let createMethod name args (ty: System.Type) =
    let m = new ProvidedMethod(name, [], typeof<int>, isStatic = true)
    m.DefineStaticParameters(
      [ for arg in args do yield ProvidedStaticParameter(arg, typeof<int>)],
      fun name -> function 
        | xs when xs |> Array.forall (fun x -> x.GetType() = typeof<int>) ->
          let args = xs |> Array.map (fun x -> ProviderUtils.createNatType (x :?> int))
          let ty = ty.GetGenericTypeDefinition().MakeGenericType(args)
          let m = new ProvidedMethod(name, [], ty, isStatic=true, invokeCode=fun _ -> Expr.NewObject(ty.GetConstructor[||], []))
          vty.AddMember m
          m
        | _ -> failwith "unexpected parameter values"
    )
    m
  do vty.AddMember (createMethod "Get" ["index"] typeof<Get<obj>>)
  do vty.AddMember (createMethod "ZeroCreate" ["count"] typeof<ZeroCreate<obj>>)
  do vty.AddMember (createMethod "Replicate" ["count"] typeof<Replicate<obj>>)
  do vty.AddMember (createMethod "Init" ["count"] typeof<Init<obj>>)
  do vty.AddMember (createMethod "Take" ["count"] typeof<Take<obj>>)
  do vty.AddMember (createMethod "Skip" ["count"] typeof<Skip<obj>>)
  do vty.AddMember (createMethod "Slice" ["startIndex"; "endIndex"] typeof<Slice<obj, obj>>)
  do vty.AddMember (createMethod "Windowed" ["windowSize"] typeof<Windowed<obj>>)
  do vty.AddMember (createMethod "ChunkBySize" ["chunkSize"] typeof<ChunkBySize<obj>>)
  do vty.AddMember (createMethod "SplitInto" ["count"] typeof<SplitInto<obj>>)
  do vty.AddMember (createMethod "TryCreate" ["count"] typeof<TryCreate<obj>>)
  do this.AddNamespace(root, [vty])

[<EditorBrowsable(EditorBrowsableState.Never)>]
module MatrixProviderHelpers =
  type Get<'i, 'j>() = class end
  type ZeroCreate<'m, 'n>() = class end
  type Replicate<'m, 'n>() = class end
  type Init<'m, 'n>() = class end
  type Slice<'a, 'b, 'c, 'd>() = class end
  type SliceRow<'a, 'b>() = class end
  type SliceCol<'c, 'd>() = class end
  type TryCreate<'m, 'n>() = class end
  type Row<'i>() = class end
  type Col<'j>() = class end

open MatrixProviderHelpers

[<Extension; EditorBrowsable(EditorBrowsableState.Never)>]
type MatrixProviderHelpersImpl =
  [<Extension>]
  static member inline Invoke (_: unit -> Get< ^i, ^j >, mtx) =
    Matrix.get Singleton< ^i > Singleton< ^j > mtx
  [<Extension>]
  static member inline Invoke (_: unit -> ZeroCreate< ^m, ^n >) =
    Matrix.zeroCreate Singleton< ^m > Singleton< ^n >
  [<Extension>]
  static member inline Invoke (_: unit -> Replicate< ^m, ^n>, value) =
    Matrix.replicate Singleton< ^m > Singleton< ^n > value
  [<Extension>]
  static member inline Invoke (_: unit -> Init< ^m, ^n >, f) =
    Matrix.init Singleton< ^m > Singleton< ^n > f
  [<Extension>]
  static member inline Invoke (_: unit -> Slice< ^a, ^b, ^c, ^d>, mtx: Matrix<_, ^m, ^n>) =
    Matrix.slice Singleton< ^a> Singleton< ^b> Singleton< ^c> Singleton< ^d> mtx
  [<Extension>]
  static member inline Invoke (_: unit -> SliceRow< ^a, ^b>, mtx: Matrix<_, ^m, ^n>) =
    Matrix.sliceRow Singleton< ^a > Singleton< ^b > mtx
  [<Extension>]
  static member inline Invoke (_: unit -> SliceCol< ^c, ^d>, mtx: Matrix<_, ^m, ^n>) =
    Matrix.sliceCol Singleton< ^c > Singleton< ^d > mtx
  [<Extension>]
  static member inline OfArray2D (_: unit -> TryCreate< ^m, ^n>, items: _[,]) =
    Matrix.tryOfArray2D Singleton< ^m> Singleton< ^n> items
  [<Extension>]
  static member inline OfJaggedSeq (_: unit -> TryCreate< ^m, ^n>, items: #seq<_> seq) =
    Matrix.tryOfJaggedSeq Singleton< ^m> Singleton< ^n> items
  [<Extension>]
  static member inline AsMatrix (_: unit -> Row< ^i>, mtx) =
    Matrix.row Singleton< ^i > mtx
  [<Extension>]
  static member inline AsVector (_: unit -> Row< ^i>, mtx) =
    Matrix.rowVec Singleton< ^i > mtx
  [<Extension>]
  static member inline AsMatrix (_: unit -> Col< ^j>, mtx) =
    Matrix.col Singleton< ^j > mtx
  [<Extension>]
  static member inline AsVector (_: unit -> Col< ^j>, mtx) =
    Matrix.colVec Singleton< ^j > mtx

[<EditorBrowsable(EditorBrowsableState.Never); TypeProvider>]
type MatrixProvider (cfg) as this =
  inherit TypeProviderForNamespaces(cfg)
  let thisAsm = Assembly.GetExecutingAssembly()
  let root = "FSharpPlus.Data"
  let vty = ProvidedTypeDefinition(thisAsm, root, "Matrix", None)
  let createMethod name args (ty: System.Type) =
    let m = new ProvidedMethod(name, [], typeof<int>, isStatic = true)
    m.DefineStaticParameters(
      [ for arg in args do yield ProvidedStaticParameter(arg, typeof<int>)],
      fun name -> function 
        | xs when xs |> Array.forall (fun x -> x.GetType() = typeof<int>) ->
          let args = xs |> Array.map (fun x -> ProviderUtils.createNatType (x :?> int))
          let ty = ty.GetGenericTypeDefinition().MakeGenericType(args)
          let m = new ProvidedMethod(name, [], ty, isStatic=true, invokeCode=fun _ -> Expr.NewObject(ty.GetConstructor[||], []))
          vty.AddMember m
          m
        | _ -> failwith "unexpected parameter values"
    )
    m
  do vty.AddMember (createMethod "Get" ["row"; "col"] typeof<Get<obj,obj>>)
  do vty.AddMember (createMethod "ZeroCreate" ["rowSize"; "colSize"] typeof<ZeroCreate<obj,obj>>)
  do vty.AddMember (createMethod "Replicate" ["rowSize"; "colSize"] typeof<Replicate<obj,obj>>)
  do vty.AddMember (createMethod "Init" ["rowSize"; "colSize"] typeof<Init<obj,obj>>)
  do vty.AddMember (createMethod "Slice" ["rowStart"; "rowEnd"; "colStart"; "colEnd"] typeof<Slice<obj,obj,obj,obj>>)
  do vty.AddMember (createMethod "SliceRow" ["rowStart"; "rowEnd"] typeof<SliceRow<obj,obj>>)
  do vty.AddMember (createMethod "SliceCol" ["colStart"; "colEnd"] typeof<SliceCol<obj,obj>>)
  do vty.AddMember (createMethod "TryCreate" ["rowSize"; "colSize"] typeof<TryCreate<obj,obj>>)
  do vty.AddMember (createMethod "Row" ["index"] typeof<Row<obj>>)
  do vty.AddMember (createMethod "Col" ["index"] typeof<Col<obj>>)
  do this.AddNamespace(root, [vty])

