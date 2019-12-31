namespace FSharpPlus.Internals

namespace FSharpPlus.Control

open System.Runtime.InteropServices
open FSharpPlus.Internals

type BifoldMap =
    inherit Default1

    static member        BifoldMap (x: Result<_,_>, f, g, _impl: BifoldMap) = match x with Ok x -> f x | Error x -> g x
    static member        BifoldMap (x: Choice<_,_>, f, g, _impl: BifoldMap) = match x with Choice1Of2 x -> f x | Choice2Of2 x -> g x
    static member inline BifoldMap ((x,y)         , f, g, _impl: BifoldMap) = Plus.Invoke (f x) (g y)
    
    static member inline Invoke (f: 'T1->'U) (g: 'T2->'U) (source: '``Bifoldable<T1,T2>``) : 'U =
        let inline call (a: ^a, b: ^b, f, g) = ((^a or ^b) : (static member BifoldMap : _*_*_*_ -> _) b,f,g,a)
        call (Unchecked.defaultof<BifoldMap>, source, f, g)
        
    static member inline InvokeOnInstance (f: 'T1->'U) (g: 'T2->'U) (source: '``Bifoldable<T1,T2>``) : 'U =
        (^``Bifoldable<T1,T2>`` : (static member BifoldMap : _*_*_ -> _) source, f, g)

type BifoldMap with
    static member inline BifoldMap (x: '``Bifoldable<'T1,'T2>``, f: _ -> 'b, g: _ -> 'b, [<Optional>]_impl: Default1) = BifoldMap.InvokeOnInstance f g x : 'b
    static member inline BifoldMap (_: '``Bifoldable<'T1,'T2>`` when '``Bifoldable<'T1,'T2>`` : null and '``Bifoldable<'T1,'T2>``: struct, _, _, _: Default1) = ()


type BifoldBack =
    inherit Default1

    static member inline BifoldBack (x: Result<_,_>, f, g, z, _impl: BifoldBack) = match x with Ok x -> f x z | Error x -> g x z
    static member inline BifoldBack (x: Choice<_,_>, f: 'a->'s->'s, g : 'b->'s->'s, z: 's, _impl: BifoldBack) = match x with Choice1Of2 x -> f x z | Choice2Of2 x -> g x z
    static member inline BifoldBack ((x,y)          , f: 'a->'s->'s, g : 'b->'s->'s, z: 's, _impl: BifoldBack) = (f x (g y z))

    static member inline Invoke (f: 'T1->'S->'S) (g: 'T2->'S->'S) (z: 'S) (source: '``Bifoldable<'T1,'T2>``) : 'S =
        let inline call (a: ^a, b: ^b, f, g, z) = ((^a or ^b) : (static member BifoldBack : _*_*_*_*_ -> _) b,f,g,z,a)
        call (Unchecked.defaultof<BifoldBack>, source, f, g, z)

    static member inline InvokeOnInstance (f: 'T1->'S->'S) (g: 'T2->'S->'S) (z: 'S) (source: '``Bifoldable<'T1,'T2>``) : 'S =
      (^``Bifoldable<'T1,'T2>`` : (static member BifoldBack : _*_*_*_ -> _) source, f, g, z)
      
type BifoldBack with
    static member inline BifoldBack (x: '``Bifoldable<'T1,'T2>``, f, g, z, [<Optional>]_impl: Default1) = BifoldBack.InvokeOnInstance f g z x : '``Bifoldable<'T1,'T2>``
    static member inline BifoldBack (x: '``Bifoldable<'T1,'T2>`` when '``Bifoldable<'T1,'T2>`` : null and '``Bifoldable<'T1,'T2>``: struct, _, _, _, _: Default1) = ()


type Bifold =
    inherit Default1

    static member        Bifold (x: Result<_,_>, _impl: Bifold) = match x with Ok x -> x | Error x -> x 
    static member        Bifold (x: Choice<_,_>, _impl: Bifold) = match x with Choice1Of2 x -> x | Choice2Of2 x -> x
    static member inline Bifold ((x,y)         , _impl: Bifold) = Plus.Invoke x y

    static member inline Invoke (source: '``Bifoldable<'T1,'T2>``) : 'U =
        let inline call (a: ^a, b: ^b) = ((^a or ^b) : (static member Bifold : _*_ -> _) b,a)
        call (Unchecked.defaultof<Bifold>, source)
    static member inline InvokeOnInstance (source: '``Bifoldable<'T1,'T2>``) : 'U =
        (^``Bifoldable<'T1,'T2>`` : (static member BifoldMap : _*_*_ -> _) source, id, id)
        
type Bifold with
    static member inline Bifold (x: '``Bifoldable<'T1,'T2>``, [<Optional>]_impl: Default1) = BifoldMap.InvokeOnInstance id id x : '``Bifoldable<'T1,'T2>``
    static member inline Bifold (_: '``Bifoldable<'T1,'T2>`` when '``Bifoldable<'T1,'T2>`` : null and '``Bifoldable<'T1,'T2>``: struct, _: Default1) = ()
