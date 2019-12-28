namespace FSharpPlus.Internals

namespace FSharpPlus.Control

open System.Runtime.InteropServices
open FSharpPlus.Internals

type BifoldMap =
    inherit Default1

    static member        BifoldMap (x: Result<_,_>, f, g, _impl: BifoldMap) = match x with Ok x -> f x | Error x -> g x
    static member        BifoldMap (x: Choice<_,_>, f, g, _impl: BifoldMap) = match x with Choice1Of2 x -> f x | Choice2Of2 x -> g x
    static member inline BifoldMap ((x,y) : _*_   , f, g, _impl: BifoldMap) = Plus.Invoke (f x) (g y)
    
    static member inline Invoke (map1: 'T1->'U) (map2: 'T2->'U) (bifoldmap: '``BifoldMap<T1,T2>``) : 'U =
        let inline call_2 (a: ^a, b: ^b, f, g) = ((^a or ^b) : (static member BifoldMap : _*_*_*_ -> _) b,f,g,a)
        let inline call (a: 'a, b: 'b, f, g) = call_2 (a,b,f,g)
        call (Unchecked.defaultof<BifoldMap>, bifoldmap, map1, map2)

type BifoldMap with
    static member inline BifoldMap (x: 'BF         , f: _ -> 'b, g: _ -> 'b, [<Optional>]_impl: Default1)  = (^BF : (static member BifoldMap : ^BF -> _ -> _ -> ^b) x, f, g)
    static member inline BifoldMap (_: 'BF when 'BF : null and 'BF: struct, f, g, _impl: Default1) = failwithf "impossible to be called"
