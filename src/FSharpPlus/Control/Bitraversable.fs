namespace FSharpPlus.Control

#if !FABLE_COMPILER || (FABLE_COMPILER_3 || FABLE_COMPILER_4)

// open System.Runtime.InteropServices
open FSharpPlus.Internals


type Bitraverse =
    inherit Default1

    static member inline Bitraverse (x: Result<'T1,'Error1>, f: 'Error1->'``Functor<'Error2>``, g: 'T1->'``Functor<'T2>``, _impl: Bitraverse) : '``Functor<Result<'Error2,'T2>>`` = match x with Ok a         -> Map.Invoke Result<'Error2,'T2>.Ok         (g a) | Error e      -> Map.Invoke Result<'Error2,'T2>.Error      (f e)
    static member inline Bitraverse (x: Choice<'T1,'Error1>, f: 'Error1->'``Functor<'Error2>``, g: 'T1->'``Functor<'T2>``, _impl: Bitraverse) : '``Functor<Choice<'Error2,'T2>>`` = match x with Choice1Of2 a -> Map.Invoke Choice<'Error2,'T2>.Choice1Of2 (g a) | Choice2Of2 e -> Map.Invoke Choice<'Error2,'T2>.Choice2Of2 (f e)

    static member inline Bitraverse ((x: 'T1, y: 'U1), f: 'T1->'``Functor<'T2>``, g: 'U1->'``Functor<'U2>``, _impl: Bitraverse) : '``Functor<'T2 * 'U2>`` = Lift2.Invoke (fun a b -> (a, b)) (f x) (g y)
    static member inline Bitraverse (struct (x: 'T1, y: 'U1), f: 'T1->'``Functor<'T2>``, g: 'U1->'``Functor<'U2>``, _impl: Bitraverse) : '``Functor<struct ('T2 * 'U2)>`` = Lift2.Invoke (fun (a: 'T) (b: 'U) -> struct (a, b)) (f x) (g y)

    static member inline Invoke (f: 'T1->'``Functor<'T2>``) (g: 'U1->'``Functor<'U2>``) (source: '``Bitraversable<'T1,'U1>``) : '``Functor<'Bitraversable<'T2,'U2>>`` =
        let inline call (a: ^a, b: ^b, _: 'r) = ((^a or ^b or ^r) : (static member Bitraverse : _*_*_*_ -> _) b,f,g,a)
        call (Unchecked.defaultof<Bitraverse>, source, Unchecked.defaultof<'``Functor<'Bitraversable<'T2,'U2>>``>)
    
    static member inline InvokeOnInstance (f: 'T1->'``Functor<'T2>``) (g: 'U1->'``Functor<'U2>``) (source: '``Bitraversable<'T1,'U1>``) : '``Functor<'Bitraversable<'T2,'U2>>`` =
        (^``Bitraversable<'T1,'U1>`` : (static member Bitraverse : _*_*_ -> _) source, f, g)



type Bisequence =
    inherit Default1

    static member inline Bisequence (x: Result<'``Functor<'Error>``, '``Functor<'T>``>, _impl: Bisequence) : '``Functor<Result<'Error,'T>>`` = match x with Ok a         -> Map.Invoke Result<'Error,'T>.Ok         a | Error e      -> Map.Invoke Result<'Error,'T>.Error      e
    static member inline Bisequence (x: Choice<'``Functor<'Error>``, '``Functor<'T>``>, _impl: Bisequence) : '``Functor<Choice<'Error,'T>>`` = match x with Choice1Of2 a -> Map.Invoke Choice<'Error,'T>.Choice1Of2 a | Choice2Of2 e -> Map.Invoke Choice<'Error,'T>.Choice2Of2 e

    static member inline Bisequence ((x: '``Functor<'T>``, y: '``Functor<'U>``), _impl: Bisequence) : '``Functor<'T2 * 'U>`` = Lift2.Invoke (fun a b -> (a, b)) x y
    static member inline Bisequence (struct (x: '``Functor<'T>``, y: '``Functor<'U>``), _impl: Bisequence) : '``Functor<struct ('T2 * 'U)>`` = Lift2.Invoke (fun a b -> struct (a, b)) x y

    static member inline Invoke (source: '``Bitraversable<'Functor<'T>,'Functor<'U>>``) : '``Functor<'Bitraversable<'T,'U>>`` =
        let inline call (a: ^a, b: ^b, _: 'r) = ((^a or ^b or ^r) : (static member Bisequence : _*_ -> _) b, a)
        call (Unchecked.defaultof<Bisequence>, source, Unchecked.defaultof<'``Functor<'Bitraversable<'T,'U>>``>)
    
    static member inline InvokeOnInstance (source: '``Bitraversable<'Functor<'T>,'Functor<'U>>``) : '``Functor<'Bitraversable<'T,'U>>`` =
        (^``Bitraversable<'Functor<'T>,'Functor<'U>>`` : (static member Bisequence : _ -> _) source)

type Bisequence with
    static member inline Bisequence (x: '``Bitraversable<'Functor<'T>,'Functor<'U>>``, _impl: Default2) = Bitraverse.InvokeOnInstance id id x : '``Functor<'Bitraversable<'T,'U>>``
    static member inline Bisequence (x: '``Bitraversable<'Functor<'T>,'Functor<'U>>``, _impl: Default1) = Bisequence.InvokeOnInstance x       : '``Functor<'Bitraversable<'T,'U>>``
    static member inline Bisequence (_: 't when 't : null and 't : struct, _: Default1) = ()



type Bitraverse with
    static member inline Bitraverse (x: '``Bitraversable<'T1,'U1>``, f: 'T1->'``Functor<'T2>``, g: 'U1->'``Functor<'U2>``, _impl: Default2) = Bimap.InvokeOnInstance f g x |> Bisequence.InvokeOnInstance : '``Functor<'Bitraversable<'T2,'U2>>``
    static member inline Bitraverse (x: '``Bitraversable<'T1,'U1>``, f: 'T1->'``Functor<'T2>``, g: 'U1->'``Functor<'U2>``, _impl: Default1) = Bitraverse.InvokeOnInstance f g x : '``Functor<'Bitraversable<'T2,'U2>>``
    static member inline Bitraverse (_: 't when 't : null and 't : struct, _, _, _: Default1) = id

#endif