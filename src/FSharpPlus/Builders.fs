namespace FSharpPlus

/// <namespacedoc>
/// <summary>
/// Extension modules, along with, Builders, Lens, Memoization, Operators, and Parsing.
/// </summary>
/// <remarks>
/// See Operators for generic functions.
/// </remarks>
/// </namespacedoc>

#nowarn "40"

#if !FABLE_COMPILER || FABLE_COMPILER_3

/// Constructs to express generic computations
[<AutoOpenAttribute>]
module GenericBuilders =

    open FSharpPlus.Operators
    open FSharpPlus.Data

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

    
    // Workflows

    open System
    open System.Collections.Generic
    open FSharpPlus.Control

    type Builder<'``monad<'t>``> () =
        member        __.ReturnFrom (expr) = expr                                        : '``monad<'t>``
        member inline __.Return (x: 'T) = result x                                       : '``Monad<'T>``
        member inline __.Yield  (x: 'T) = result x                                       : '``Monad<'T>``
        member inline __.Bind (p: '``Monad<'T>``, rest: 'T->'``Monad<'U>``) = p >>= rest : '``Monad<'U>``
        member inline __.MergeSources  (t1: '``Monad<'T>``, t2: '``Monad<'U>``)          : '``Monad<'T * 'U>`` = Lift2.Invoke tuple2 t1 t2
        member inline __.MergeSources3 (t1: '``Monad<'T>``, t2: '``Monad<'U>``, t3: '``Monad<'V>``) : '``Monad<'T * 'U * 'V>`` = Lift3.Invoke tuple3 t1 t2 t3

        [<CustomOperation("select", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline __.Select (x, [<ProjectionParameter>] f) = map f x

        [<CustomOperation("where", MaintainsVariableSpaceUsingBind=true)>]
        member inline __.Where (x, [<ProjectionParameter>] p) = mfilter p x

        [<CustomOperation("top", MaintainsVariableSpaceUsingBind=true)>]
        member inline __.Top (source, n) = limit n source

        [<CustomOperation("groupBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline __.GroupBy (x,[<ProjectionParameter>] f : 'T -> 'key) = groupBy f x

        [<CustomOperation("chunkBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline __.ChunkBy (x,[<ProjectionParameter>] f : 'T -> 'key) = chunkBy f x

        [<CustomOperation("orderBy", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline __.OrderBy (x,[<ProjectionParameter>] f : 'T -> 'key) = sortBy f x

    type StrictBuilder<'``monad<'t>``> () =
        inherit Builder<'``monad<'t>``> ()
        member        __.Delay expr = expr : unit -> '``Monad<'T>``
        member        __.Run f = f ()              : '``monad<'t>``
        member inline __.TryWith    (expr, handler)      = TryWith.InvokeForStrict    expr handler      : '``Monad<'T>``
        member inline __.TryFinally (expr, compensation) = TryFinally.InvokeForStrict expr compensation : '``Monad<'T>``
        
        member inline __.Using (disposable: #IDisposable, body) = Using.Invoke disposable body

    type DelayedBuilder<'``monad<'t>``> () =
        inherit Builder<'``monad<'t>``> ()
        member inline __.Delay (expr: _->'``Monad<'T>``) = Delay.Invoke expr : '``Monad<'T>``
        member        __.Run f = f                                           : '``monad<'t>``
        member inline __.TryWith    (expr, handler     ) = TryWith.Invoke    expr handler      : '``Monad<'T>``
        member inline __.TryFinally (expr, compensation) = TryFinally.Invoke expr compensation : '``Monad<'T>``
        member inline __.Using (disposable: #IDisposable, body) = Using.Invoke disposable body : '``Monad<'T>``

    type MonadPlusStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        member        __.YieldFrom expr = expr                           : '``monad<'t>``
        member inline __.Zero () = Empty.Invoke ()                       : '``MonadPlus<'T>``
        member inline __.Combine (a: '``MonadPlus<'T>``, b) = a <|> b () : '``MonadPlus<'T>``
        member inline __.While (guard, body: unit -> '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
            let rec loop guard body =
                if guard () then body () <|> loop guard body
                else Empty.Invoke ()
            loop guard body
        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) =
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``MonadPlus<'U>``)

    type MonadFxStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        
        member inline __.Zero () = result ()                                       : '``Monad<unit>``
        member inline __.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b ()) : '``Monad<'T>``
        
        member inline __.While (guard, body: unit -> '``Monad<unit>``)             : '``Monad<unit>`` =
            let rec loop guard body =
                if guard () then body () >>= fun () -> loop guard body
                else result ()
            loop guard body
        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<unit>``) =
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``Monad<unit>``)

    type MonadPlusBuilder<'``monad<'t>``> () =
        inherit DelayedBuilder<'``monad<'t>``>()
        member        __.YieldFrom expr = expr                        : '``monad<'t>``
        member        __.strict = new MonadPlusStrictBuilder<'``monad<'t>``> ()
        member inline __.Zero () = Empty.Invoke ()                    : '``MonadPlus<'T>``
        member inline __.Combine (a: '``MonadPlus<'T>``, b) = a <|> b : '``MonadPlus<'T>``

        member inline __.WhileImpl (guard, body: '``MonadPlus<'T>``)  : '``MonadPlus<'T>`` =
            let rec fix () = Delay.Invoke (fun () -> if guard () then body <|> fix () else Empty.Invoke ())
            fix ()
        
        member inline this.While (guard, body: '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
            // Check the type is lazy, otherwise display a warning.
            let __ ()  = TryWith.InvokeForWhile (Unchecked.defaultof<'``MonadPlus<'T>``>) (fun (_: exn) -> Unchecked.defaultof<'``MonadPlus<'T>``>) : '``MonadPlus<'T>``

            this.WhileImpl (guard, body)

        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) : '``MonadPlus<'U>`` =
            let mutable isReallyDelayed = true
            Delay.Invoke (fun () -> isReallyDelayed <- false; Empty.Invoke () : '``MonadPlus<'U>``) |> ignore
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                if isReallyDelayed then this.WhileImpl (enum.MoveNext, Delay.Invoke (fun () -> rest enum.Current))
                else this.strict.While (enum.MoveNext, fun () -> rest enum.Current))

    type MonadFxBuilder<'``monad<'t>``> () =
        inherit DelayedBuilder<'``monad<'t>``> ()
        member        __.strict  = new MonadFxStrictBuilder<'``monad<'t>``> ()

        /// Makes it a (lazy) monadplus computation expression.
        member        __.plus    = new MonadPlusBuilder<'``monad<'t>``> ()

        /// Makes it a strict monadplus computation expression.
        member        __.plus'   = new MonadPlusStrictBuilder<'``monad<'t>``> ()

        /// Makes it a (lazy) monadic computation expression with side-effects
        member        this.fx    = this

        /// Makes it a strict monadic computation expression with side-effects
        member        __.fx'     = new MonadFxStrictBuilder<'``monad<'t>``> ()

        member inline __.Zero () = result ()                                    : '``Monad<unit>``

        member inline __.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b) : '``Monad<'T>``
        
        member inline __.WhileImpl (guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
            let rec loop guard body =
                if guard () then body >>= (fun () -> loop guard body)
                else result ()
            loop guard body

        member inline this.While (guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
            // Check the type is lazy, otherwise display a warning.
            let __ ()  = TryWith.InvokeForWhile (Unchecked.defaultof<'``Monad<unit>``>) (fun (_: exn) -> Unchecked.defaultof<'``Monad<unit>``>) : '``Monad<unit>``
            this.WhileImpl (guard, body)

        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<unit>``) : '``Monad<unit>``=
            let mutable isReallyDelayed = true
            Delay.Invoke (fun () -> isReallyDelayed <- false; Return.Invoke () : '``Monad<unit>``) |> ignore
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                if isReallyDelayed then this.WhileImpl (enum.MoveNext, Delay.Invoke (fun () -> rest enum.Current))
                else this.strict.While (enum.MoveNext, fun () -> rest enum.Current))


    /// Generic Applicative CE builder.
    type ApplicativeBuilder<'``applicative<'t>``> () =
        member        _.ReturnFrom (expr) = expr   : '``applicative<'t>``
        member inline _.Return (x: 'T) = result x  : '``Applicative<'T>``
        member inline _.Yield  (x: 'T) = result x  : '``Applicative<'T>``
        member inline _.BindReturn(x, f) = map f x : '``Applicative<'U>``
        member inline _.MergeSources  (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``) : '``Applicative<'T * 'U>`` = Lift2.Invoke tuple2 t1 t2
        member inline _.MergeSources3 (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``, t3: '``Applicative<'V>``) : '``Applicative<'T * 'U * 'V>`` = Lift3.Invoke tuple3 t1 t2 t3
        member        _.Run f = f : '``applicative<'t>``
    
    /// Generic 2 layers Applicative CE builder.
    type ApplicativeBuilder2<'``applicative1<applicative2<'t>>``> () =
        member        _.ReturnFrom (expr) = expr  : '``applicative1<applicative2<'t>>``
        member inline _.Return (x: 'T) = result x : Compose<_>
        member inline _.Yield  (x: 'T) = result x : Compose<_>
        member inline _.BindReturn (x: Compose<_>, f: _ -> _) = Compose.Map (x, f) : Compose<_>
        member inline _.MergeSources  (t1: Compose<_>, t2: Compose<_>) : Compose<_> = Compose.Lift2 (tuple2, t1, t2)
        member inline _.MergeSources3 (t1: Compose<_>, t2: Compose<_>, t3: Compose<_>) : Compose<_> = Compose.Lift3 (tuple3, t1, t2, t3)
        member        _.Source x = Compose (x: '``Applicative1<Applicative2<'T>>``)
        member        _.Run x = Compose.run x : '``applicative1<applicative2<'t>>``
    


    /// Creates a (lazy) monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad<'``monad<'t>``> = new MonadFxBuilder<'``monad<'t>``> ()

    /// Creates a strict monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad'<'``monad<'t>``> = new MonadFxStrictBuilder<'``monad<'t>``> ()

    /// Creates an applicative computation expression.
    let applicative<'``Applicative<'T>``> = ApplicativeBuilder<'``Applicative<'T>``> ()

    /// Creates an applicative computation expression which compose effects of two Applicatives.
    let applicative2<'``Applicative1<Applicative2<'T>>``> = ApplicativeBuilder2<'``Applicative1<Applicative2<'T>>``> ()

#endif
