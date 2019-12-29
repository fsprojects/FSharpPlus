namespace FSharpPlus.Internals

namespace FSharpPlus.Control

open System.Runtime.InteropServices
open FSharpPlus.Internals

type BifoldMap =
    inherit Default1

    static member        BifoldMap (x: Result<_,_>, f, g, _impl: BifoldMap) = match x with Ok x -> f x | Error x -> g x
    static member        BifoldMap (x: Choice<_,_>, f, g, _impl: BifoldMap) = match x with Choice1Of2 x -> f x | Choice2Of2 x -> g x
    static member inline BifoldMap ((x,y)         , f, g, _impl: BifoldMap) = Plus.Invoke (f x) (g y)
    
    static member inline Invoke (map1: 'T1->'U) (map2: 'T2->'U) (bifoldmap: '``BifoldMap<T1,T2>``) : 'U =
        let inline call (a: ^a, b: ^b, f, g) = ((^a or ^b) : (static member BifoldMap : _*_*_*_ -> _) b,f,g,a)
        call (Unchecked.defaultof<BifoldMap>, bifoldmap, map1, map2)

type BifoldMap with
    static member inline BifoldMap (x: '``Bifoldable<'T1,'T2>``, f: _ -> 'b, g: _ -> 'b, [<Optional>]_impl: Default1)  = (^``Bifoldable<'T1,'T2>`` : (static member BifoldMap : ^``Bifoldable<'T1,'T2>`` -> _ -> _ -> ^b) x, f, g)
    static member inline BifoldMap (_: '``Bifoldable<'T1,'T2>`` when '``Bifoldable<'T1,'T2>`` : null and '``Bifoldable<'T1,'T2>``: struct, _, _, _: Default1) = failwithf "impossible to be called"


type BifoldBack =
    inherit Default1

    static member inline BifoldBack (x: Result<_,_>, f, g, z, _impl: BifoldBack) = match x with Ok x -> f x z | Error x -> g x z
    static member inline BifoldBack (x: Choice<_,_>, f: 'a->'s->'s, g : 'b->'s->'s, z: 's, _impl: BifoldBack) = match x with Choice1Of2 x -> f x z | Choice2Of2 x -> g x z
    static member inline BifoldBack ((x,y)          , f: 'a->'s->'s, g : 'b->'s->'s, z: 's, _impl: BifoldBack) = (f x (g y z))

    static member inline Invoke (fold1: 'T1->'S->'S) (fold2: 'T2->'S->'S) (z: 'S) (bifoldback: '``BifoldBack<'T1,'T2>``) : 'S =
        let inline call (a: ^a, b: ^b, f, g, z) = ((^a or ^b) : (static member BifoldBack : _*_*_*_*_ -> _) b,f,g,z,a)
        call (Unchecked.defaultof<BifoldBack>, bifoldback, fold1, fold2, z)

(*
type BifoldBack with
    static member inline BifoldBack (x: '``Bifoldable<'T1,'T2>``, f, g, z, [<Optional>]_impl: Default1) = (^``Bifoldable<'T1,'T2>`` : (static member BifoldBack : ^``Bifoldable<'T1,'T2>``->_->_->_-> ^b) x, f, g, z)
    static member inline BifoldBack (x: '``Bifoldable<'T1,'T2>`` when '``Bifoldable<'T1,'T2>`` : null and '``Bifoldable<'T1,'T2>``: struct, _, _, _, _: Default1) = failwithf "impossible to be called"
*)

(*
type Bifold =
  //
*)