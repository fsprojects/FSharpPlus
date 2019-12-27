namespace FSharpPlus.Internals

namespace FSharpPlus.Control

open System.Runtime.InteropServices
open FSharpPlus.Internals
type BifoldBack =
    inherit Default1

    static member inline BifoldBack (x: 'BF, f: 'a->'b->'b, g: 'c->'b->'b, z: 'b, [<Optional>]_impl: Default1) = (^BF : (static member BifoldBack : ^BF ->_->_->_-> ^b) x, f, g, z)

    static member inline Invoke (folder1: 'T->'State->'State) (folder2: 'U->'State->'State) (state: 'State) (bifoldable: '``Bifoldable'<T,U>``) : 'State =
        let inline call_2 (a: ^a, b: ^b, f, g, z) = ((^a or ^b) : (static member BifoldBack : _*_*_*_*_ -> _) b, f, g, z, a)
        let inline call (a: 'a,  b: 'b, f, g, z) = call_2 (a,b,f,g,z)
        call (Unchecked.defaultof<BifoldBack>, bifoldable, folder1, folder2, state)
(*
type BifoldMap =
    inherit Default1

    static member inline BifoldMap (x: Result<_,_>, f, g, [<Optional>]_impl: BifoldMap) =
        match x with
        | Ok x -> Ok (FoldMap.FoldMap(x, f))
        | Error x -> Error (FoldMap.FoldMap(x, g))

    static member inline BifoldMap (x: Choice<_,_>, f, g, [<Optional>]_impl: BifoldMap) =
        match x with
        | Choice1Of2 x -> Ok (FoldMap.FoldMap(x, f))
        | Choice2Of2 x -> Error (FoldMap.FoldMap(x, g))

    static member inline BifoldMap (x,y : _*_ , f, g, [<Optional>]_impl: BifoldMap) =
        FoldMap.FoldMap(x, f), FoldMap.FoldMap(y, g)

    static member inline BifoldMap (x,y : struct (_*_) , f, g, [<Optional>]_impl: BifoldMap) =
        struct (FoldMap.FoldMap(x, f), FoldMap.FoldMap(y, g))
*)