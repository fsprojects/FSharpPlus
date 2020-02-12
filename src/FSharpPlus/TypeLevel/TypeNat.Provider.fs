namespace FSharpPlus.TypeLevel

open System.Reflection
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes

module internal ProviderUtils =
  let private sTy = typedefof<S<Z>>.GetGenericTypeDefinition()
  let private zTy = typeof<Z>
  let mutable private memot = Map.ofList [0, zTy]
  let rec createNatType n =
    match memot |> Map.tryFind n with
    | Some x -> x
    | None ->
      if n > 0 then
        let x = sTy.MakeGenericType(createNatType(n-1))
        memot <- memot |> Map.add n x
        x
      else
        zTy

  let mutable private memov = Map.ofList [0, <@@ Z @@>]
  let rec createNatValue n =
    match memov |> Map.tryFind n with
    | Some x -> x
    | None ->
      if n > 0 then
        let uci = createNatType n |> FSharpType.GetUnionCases
                                  |> Seq.head
        let x = Expr.NewUnionCase(uci, [createNatValue(n-1)])
        memov <- memov |> Map.add n x
        x
      else
        <@@ zTy @@>

[<TypeProvider>]
type NatProvider (cfg) as this =
  inherit TypeProviderForNamespaces(cfg)
  let thisAsm = Assembly.GetExecutingAssembly()
  let root = "FSharpPlus.TypeLevel"
  let baseTy = typeof<obj>
  let prm = [ProvidedStaticParameter("value", typeof<int>)]
  let ty = ProvidedTypeDefinition(thisAsm, root, "TypeNat", Some baseTy)
  do ty.DefineStaticParameters(
      parameters = prm,
      instantiationFunction = (fun tyName prmValues ->
        match prmValues with
          | [| :? int as value |] ->
            if value < 0 then
              failwith "value is negative"
            let n = ProviderUtils.createNatValue value in
            let ty = ProvidedTypeDefinition(thisAsm, root, tyName, baseType = Some n.Type)
            let valuet = ProvidedProperty("Value", n.Type, isStatic = true, getterCode = fun _ -> Expr.Coerce(n, n.Type))
            let sing = ProvidedMethod("Singleton", [ ProvidedParameter("defaultValue", ty)], ty, (fun _ -> Expr.Coerce(n, ty)), isStatic = true)
            valuet.AddXmlDoc <| sprintf "The value-level representation of type-level natural '%i'." value
            ty.AddMember valuet
            ty.AddMember sing
            ty.AddXmlDoc <| sprintf "Type-level natural '%i'." value
            ty
          | _ -> failwith "unexpected parameter values"
        )
      )
  do this.AddNamespace(root, [ty])
  
[<assembly:TypeProviderAssembly>]
do ()
