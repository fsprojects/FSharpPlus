namespace FSharpPlus.Internals

module internal MonadOps =
    open FSharpPlus.Control

    let inline (>>=) x f = Bind.Invoke x f
    #if !FABLE_COMPILER
    let inline result  x = Return.Invoke x
    #endif
    let inline (<*>) f x = Apply.Invoke f x
    let inline (<|>) x y = Append.Invoke x y
    let inline (>=>) (f: 'a->'``Monad<'b>``) (g: 'b->'``Monad<'c>``) (x: 'a) : '``Monad<'c>`` = f x >>= g