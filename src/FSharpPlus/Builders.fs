namespace FSharpPlus

[<AutoOpenAttribute>]
module Builders =    

    open FSharpPlus.Operators

    // Idiom brackets
    type Ii = Ii
    type Ji = Ji
    type J = J
    type Idiomatic = Idiomatic with
        static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
        static member        ($) (Idiomatic, Ii) = id
    let inline idiomatic a b = (Idiomatic $ b) a
    let inline iI x = (idiomatic << result) x
    type Idiomatic with static member inline ($) (Idiomatic, Ji) = fun xii -> join xii
    type Idiomatic with static member inline ($) (Idiomatic, J ) = fun fii x -> (Idiomatic $ x) (join fii)


    type Zero =
        inherit FsControl.MZero
        static member inline MZero (_output : '``FunctorZero<unit>``, _mthd : FsControl.Internals.Default2) = FsControl.Return.Invoke () : '``FunctorZero<unit>``
        static member inline Invoke' () : '``FunctorZero<'T>`` =
            let inline call (mthd : ^M, output : ^R) = ((^M or ^R) : (static member MZero: _*_ -> _) output, mthd)
            call (Unchecked.defaultof<Zero>, Unchecked.defaultof<'``FunctorZero<'T>``>)

    type Plus =
        inherit FsControl.MPlus
        static member inline MPlus (x :'``FunctorPlus<unit>``, y:'``FunctorPlus<'T>``, _mthd : FsControl.Internals.Default2) = FsControl.Bind.Invoke x (fun () -> y) :'``FunctorPlus<'T>``    
        static member inline Invoke' (x :'``FunctorPlus<'T or unit>``) (y:'``FunctorPlus<'T>``)  : '``FunctorPlus<'T>`` =
            let inline call (mthd : ^M, input1 : ^U, input2 : ^I) = ((^M or ^I) : (static member MPlus: _*_*_ -> _) input1, input2, mthd)
            call (Unchecked.defaultof<Plus>, x, y)


    // Do notation
    type MonadBuilder() =
        member inline b.Return(x)    = result x
        member inline b.Bind(p,rest) = p >>= rest
        member inline b.Delay(expr:unit -> 't) = FsControl.Delay.Invoke(expr) : 't
        member        b.Let (p,rest) = rest p        
        member    b.ReturnFrom(expr) = expr

    type MonadPlusBuilder() =
        member inline b.Return(x) = result x
        member inline b.Bind(p,rest) = p >>= rest
        member inline b.Delay(expr:unit -> 't) = FsControl.Delay.Invoke(expr) : 't
        member b.Let(p,rest) = rest p
        member b.ReturnFrom(expr) = expr
        member inline x.Zero() = Zero.Invoke' ()
        member inline x.Combine(a, b) = Plus.Invoke' a b
        member x.TryWith(body, handler) =
            try x.ReturnFrom(body)
            with e -> handler e
        member x.TryFinally(body, compensation) =
            try x.ReturnFrom(body)
            finally compensation()
        member x.Using(disposable:#System.IDisposable, body) =
            let body = fun () -> body disposable
            x.TryFinally(body, fun () -> 
                match disposable with 
                | null -> () 
                | disp -> disp.Dispose())

    
    let monad     = new MonadBuilder()
    let monadPlus = new MonadPlusBuilder()


    // Linq
    type LinqBuilder() =
        member inline __.Return(x)     = result x
        member inline __.Bind(p, rest) = p >>= rest
        member inline __.Delay(expr:unit -> 't) = FsControl.Delay.Invoke(expr) : 't
        member        __.Let (p, rest) = rest p
        member    __.ReturnFrom(expr)  = expr
        member inline __.For(p, rest)  = p >>= rest
        member inline __.Yield(x)      = result x
        member inline __.Zero()        = Zero.Invoke' ()
        member inline __.Combine(a, b) = Plus.Invoke' a b
        member __.TryWith(body, handler) =
            try __.ReturnFrom(body)
            with e -> handler e
        member __.TryFinally(body, compensation) =
            try __.ReturnFrom(body)
            finally compensation()
        member __.Using(disposable:#System.IDisposable, body) =
            let body = fun () -> body disposable
            __.TryFinally(body, fun () -> 
                match disposable with 
                | null -> () 
                | disp -> disp.Dispose())

        [<CustomOperation("select", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
        member inline __.Select(x, [<ProjectionParameter>] f) = map f x

        [<CustomOperation("where", MaintainsVariableSpace=true)>]
        member inline __.Where(x, [<ProjectionParameter>] p) = mfilter p x

        [<CustomOperation("first")>] 
        member inline __.First(source) = head source

        [<CustomOperation("nth")>]
        member inline __.Nth(source, n) = nth n source

        [<CustomOperation("groupBy", AllowIntoPattern=true)>]
        member inline __.GroupBy (x,[<ProjectionParameter>] f : 't -> 'key) = groupBy f x

        [<CustomOperation("chunkBy", AllowIntoPattern=true)>]
        member inline __.ChunkBy (x,[<ProjectionParameter>] f : 't -> 'key) = chunkBy f x

        [<CustomOperation("sortBy", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
        member inline __.SortBy (x,[<ProjectionParameter>] f : 't -> 'key) = sortBy f x

    let linq = LinqBuilder()
