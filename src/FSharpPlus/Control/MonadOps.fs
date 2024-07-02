namespace FSharpPlus.Internals

#if !FABLE_COMPILER

module internal MonadOps =
    open FSharpPlus.Control

    let inline (>>=) x f = Bind.Invoke x f
    let inline result  x = Return.Invoke x
    let inline (<*>) f x = Apply.Invoke f x
    let inline pur x = Pure.Invoke x
    let inline (<.>) f x = ZipApply.Invoke f x
    let inline (<|>) x y = Append.Invoke x y
    let inline (>=>) (f: 'a->'``Monad<'b>``) (g: 'b->'``Monad<'c>``) (x: 'a) : '``Monad<'c>`` = f x >>= g

#endif