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
#nowarn "44"

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4
open System
open System.ComponentModel

[<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
module Builders =    
    open FSharpPlus.Operators

    // Idiom brackets
    type Ii = Ii
    type Ji = Ji
    type J = J
    type Idiomatic = Idiomatic with
        static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
        static member        ($) (Idiomatic, Ii) = id
    
    type Idiomatic with static member inline ($) (Idiomatic, Ji) = fun xii -> join xii
    type Idiomatic with static member inline ($) (Idiomatic, J ) = fun fii x -> (Idiomatic $ x) (join fii)

    
    // Workflows

    open System.Collections.Generic
    open FSharpPlus.Control

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type Builder () =
        member        __.ReturnFrom (expr) = expr                                        : '``Monad<'T>``

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type StrictBuilder () =
        inherit Builder ()
        member        __.Run f = f ()              : '``Monad<'T>``
        member        __.TryWith    (expr, handler)      = try expr () with e -> handler e
        member        __.TryFinally (expr, compensation) = try expr () finally compensation ()
        
        member        rs.Using (disposable: #IDisposable, body) =
            let body = fun () -> body disposable
            rs.TryFinally (body, fun () -> dispose disposable)

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type DelayedBuilder () =
        inherit Builder ()
        member        __.Run f = f                                           : '``Monad<'T>``
    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type MonadPlusStrictBuilder () =
        inherit StrictBuilder ()
        member        __.YieldFrom  (expr) = expr                        : '``Monad<'T>``

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type MonadFxStrictBuilder () =
        inherit StrictBuilder ()

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type MonadPlusBuilder () =
        inherit DelayedBuilder()
        member        __.YieldFrom  (expr) = expr                     : '``Monad<'T>``
        member        __.strict = new MonadPlusStrictBuilder ()

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    type MonadFxBuilder () =
        inherit DelayedBuilder ()
        member        __.strict  = new MonadFxStrictBuilder ()

        member        __.plus    = new MonadPlusBuilder ()

        member        __.plus'   = new MonadPlusStrictBuilder ()

        member        this.fx    = this

        member        __.fx'     = new MonadFxStrictBuilder ()


    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    let monad = new MonadFxBuilder ()

    [<Obsolete("Compatibility with v1"); EditorBrowsable(EditorBrowsableState.Never)>]
    let monad' = new MonadFxStrictBuilder ()

#endif
#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

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
        member        _.ReturnFrom (expr) = expr                                        : '``monad<'t>``
        member inline _.Return (x: 'T) = result x                                       : '``Monad<'T>``
        member inline _.Yield  (x: 'T) = result x                                       : '``Monad<'T>``
        #if !NET45
        member inline _.Bind (p: '``Monad<'T>``, [<InlineIfLambda>]rest: 'T->'``Monad<'U>``) = p >>= rest : '``Monad<'U>``
        #else
        member inline _.Bind (p: '``Monad<'T>``, rest: 'T->'``Monad<'U>``) = p >>= rest : '``Monad<'U>``
        #endif
        member inline _.MergeSources  (t1: '``Monad<'T>``, t2: '``Monad<'U>``)          : '``Monad<'T * 'U>`` = Lift2.Invoke tuple2 t1 t2
        member inline _.MergeSources3 (t1: '``Monad<'T>``, t2: '``Monad<'U>``, t3: '``Monad<'V>``) : '``Monad<'T * 'U * 'V>`` = Lift3.Invoke tuple3 t1 t2 t3

        [<CustomOperation("select", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline _.Select (x, [<ProjectionParameter>] f) = map f x

        [<CustomOperation("where", MaintainsVariableSpaceUsingBind=true)>]
        member inline _.Where (x, [<ProjectionParameter>] p) = mfilter p x

        [<CustomOperation("top", MaintainsVariableSpaceUsingBind=true)>]
        member inline _.Top (source, n) = limit n source

        [<CustomOperation("groupBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline _.GroupBy (x,[<ProjectionParameter>] f : 'T -> 'key) = groupBy f x

        [<CustomOperation("chunkBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline _.ChunkBy (x,[<ProjectionParameter>] f : 'T -> 'key) = chunkBy f x

        [<CustomOperation("orderBy", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline _.OrderBy (x,[<ProjectionParameter>] f : 'T -> 'key) = sortBy f x

    type StrictBuilder<'``monad<'t>``> () =
        inherit Builder<'``monad<'t>``> ()
        #if !NET45
        member inline _.Delay ([<InlineIfLambda>]expr) = expr : unit -> '``Monad<'T>``
        member inline _.Run ([<InlineIfLambda>]f) = f ()              : '``monad<'t>``
        member inline _.TryWith    ([<InlineIfLambda>]expr, [<InlineIfLambda>]handler)      = TryWith.InvokeForStrict    expr handler      : '``Monad<'T>``
        member inline _.TryFinally ([<InlineIfLambda>]expr, [<InlineIfLambda>]compensation) = TryFinally.InvokeForStrict expr compensation : '``Monad<'T>``
        
        member inline _.Using (disposable: #IDisposable, [<InlineIfLambda>]body) = Using.Invoke disposable body
        #else
        member inline _.Delay (expr) = expr : unit -> '``Monad<'T>``
        member inline _.Run (f) = f ()              : '``monad<'t>``
        member inline _.TryWith    (expr, handler)      = TryWith.InvokeForStrict    expr handler      : '``Monad<'T>``
        member inline _.TryFinally (expr, compensation) = TryFinally.InvokeForStrict expr compensation : '``Monad<'T>``
        
        member inline _.Using (disposable: #IDisposable, body) = Using.Invoke disposable body
        #endif

    type DelayedBuilder<'``monad<'t>``> () =
        inherit Builder<'``monad<'t>``> ()
        #if !NET45
        member inline _.Delay ([<InlineIfLambda>]expr: _->'``Monad<'T>``) = Delay.Invoke expr : '``Monad<'T>``
        member        _.Run f = f                                           : '``monad<'t>``
        member inline _.TryWith    (expr, [<InlineIfLambda>]handler     ) = TryWith.Invoke    expr handler      : '``Monad<'T>``
        member inline _.TryFinally (expr, [<InlineIfLambda>]compensation) = TryFinally.Invoke expr compensation : '``Monad<'T>``
        member inline _.Using (disposable: #IDisposable, [<InlineIfLambda>]body) = Using.Invoke disposable body : '``Monad<'T>``
        #else
        member inline _.Delay (expr: _->'``Monad<'T>``) = Delay.Invoke expr : '``Monad<'T>``
        member        _.Run f = f                                           : '``monad<'t>``
        member inline _.TryWith    (expr, handler     ) = TryWith.Invoke    expr handler      : '``Monad<'T>``
        member inline _.TryFinally (expr, compensation) = TryFinally.Invoke expr compensation : '``Monad<'T>``
        member inline _.Using (disposable: #IDisposable, body) = Using.Invoke disposable body : '``Monad<'T>``
        #endif

    type MonadPlusStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        member        _.YieldFrom expr = expr                           : '``monad<'t>``
        member inline _.Zero () = Empty.Invoke ()                       : '``MonadPlus<'T>``
        #if !NET45
        member inline _.Combine (a: '``MonadPlus<'T>``, [<InlineIfLambda>]b) = a <|> b () : '``MonadPlus<'T>``
        #else
        member inline _.Combine (a: '``MonadPlus<'T>``, b) = a <|> b () : '``MonadPlus<'T>``
        #endif
        #if !NET45
        member inline _.While ([<InlineIfLambda>]guard, [<InlineIfLambda>]body: unit -> '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
        #else
        member inline _.While (guard, body: unit -> '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
        #endif
            let rec loop guard body =
                if guard () then body () <|> loop guard body
                else Empty.Invoke ()
            loop guard body
        #if !NET45
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``MonadPlus<'U>``) =
        #else
        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) =
        #endif
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``MonadPlus<'U>``)

    type MonadFxStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        
        member inline _.Zero () = result ()                                       : '``Monad<unit>``
        #if !NET45
        member inline _.Combine (a: '``Monad<unit>``, [<InlineIfLambda>]b) = a >>= (fun () -> b ()) : '``Monad<'T>``
        #else
        member inline _.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b ()) : '``Monad<'T>``
        #endif
        
        #if !NET45
        member inline _.While ([<InlineIfLambda>]guard, [<InlineIfLambda>]body: unit -> '``Monad<unit>``)             : '``Monad<unit>`` =
        #else
        member inline _.While (guard, body: unit -> '``Monad<unit>``)             : '``Monad<unit>`` =
        #endif
            let rec loop guard body =
                if guard () then body () >>= fun () -> loop guard body
                else result ()
            loop guard body
        #if !NET45
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``Monad<unit>``) =
        #else
        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<unit>``) =
        #endif
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``Monad<unit>``)

    type MonadPlusBuilder<'``monad<'t>``> () =
        inherit DelayedBuilder<'``monad<'t>``>()
        member        _.YieldFrom expr = expr                        : '``monad<'t>``
        member        _.strict = new MonadPlusStrictBuilder<'``monad<'t>``> ()
        member inline _.Zero () = Empty.Invoke ()                    : '``MonadPlus<'T>``
        member inline _.Combine (a: '``MonadPlus<'T>``, b) = a <|> b : '``MonadPlus<'T>``

        #if !NET45
        member inline _.WhileImpl ([<InlineIfLambda>]guard, body: '``MonadPlus<'T>``)  : '``MonadPlus<'T>`` =
        #else
        member inline _.WhileImpl (guard, body: '``MonadPlus<'T>``)  : '``MonadPlus<'T>`` =
        #endif
            let rec fix () = Delay.Invoke (fun () -> if guard () then body <|> fix () else Empty.Invoke ())
            fix ()
        
        #if !NET45
        member inline this.While ([<InlineIfLambda>]guard, body: '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
        #else
        member inline this.While (guard, body: '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
        #endif
            // Check the type is lazy, otherwise display a warning.
            let __ ()  = TryWith.InvokeForWhile (Unchecked.defaultof<'``MonadPlus<'T>``>) (fun (_: exn) -> Unchecked.defaultof<'``MonadPlus<'T>``>) : '``MonadPlus<'T>``

            this.WhileImpl (guard, body)

        #if !NET45
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``MonadPlus<'U>``) : '``MonadPlus<'U>`` =
        #else
        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) : '``MonadPlus<'U>`` =
        #endif
            let mutable isReallyDelayed = true
            Delay.Invoke (fun () -> isReallyDelayed <- false; Empty.Invoke () : '``MonadPlus<'U>``) |> ignore
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                if isReallyDelayed then this.WhileImpl (enum.MoveNext, Delay.Invoke (fun () -> rest enum.Current))
                else this.strict.While (enum.MoveNext, fun () -> rest enum.Current))

    type MonadFxBuilder<'``monad<'t>``> () =
        inherit DelayedBuilder<'``monad<'t>``> ()
        member        _.strict  = new MonadFxStrictBuilder<'``monad<'t>``> ()

        /// Makes it a (lazy) monadplus computation expression.
        member        _.plus    = new MonadPlusBuilder<'``monad<'t>``> ()

        /// Makes it a strict monadplus computation expression.
        member        _.plus'   = new MonadPlusStrictBuilder<'``monad<'t>``> ()

        /// Makes it a (lazy) monadic computation expression with side-effects
        member        this.fx    = this

        /// Makes it a strict monadic computation expression with side-effects
        member        _.fx'     = new MonadFxStrictBuilder<'``monad<'t>``> ()

        member inline _.Zero () = result ()                                    : '``Monad<unit>``

        member inline _.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b) : '``Monad<'T>``
        
        #if !NET45
        member inline _.WhileImpl ([<InlineIfLambda>]guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
        #else
        member inline _.WhileImpl (guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
        #endif
            let rec loop guard body =
                if guard () then body >>= (fun () -> loop guard body)
                else result ()
            loop guard body

        #if !NET45
        member inline this.While ([<InlineIfLambda>]guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
        #else
        member inline this.While (guard, body: '``Monad<unit>``) : '``Monad<unit>`` =
        #endif
            // Check the type is lazy, otherwise display a warning.
            let __ ()  = TryWith.InvokeForWhile (Unchecked.defaultof<'``Monad<unit>``>) (fun (_: exn) -> Unchecked.defaultof<'``Monad<unit>``>) : '``Monad<unit>``
            this.WhileImpl (guard, body)

        #if !NET45
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``Monad<unit>``) : '``Monad<unit>``=
        #else
        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<unit>``) : '``Monad<unit>``=
        #endif
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
        #if !NET45
        member inline _.BindReturn(x, [<InlineIfLambda>]f) = map f x : '``Applicative<'U>``
        #else
        member inline _.BindReturn(x, f) = map f x : '``Applicative<'U>``
        #endif
        member inline _.MergeSources  (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``) : '``Applicative<'T * 'U>`` = Lift2.Invoke tuple2 t1 t2
        member inline _.MergeSources3 (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``, t3: '``Applicative<'V>``) : '``Applicative<'T * 'U * 'V>`` = Lift3.Invoke tuple3 t1 t2 t3
        member        _.Run f = f : '``Applicative<'T>``
    
    /// Generic 2 layers Applicative CE builder.
    type ApplicativeBuilder2<'``applicative1<applicative2<'t>>``> () =
        member        _.ReturnFrom expr : '``applicative1<applicative2<'t>>`` = expr
        member inline _.Return (x: 'T) : '``Applicative1<Applicative2<'T>>`` = (result >> result) x
        member inline _.Yield  (x: 'T) : '``Applicative1<Applicative2<'T>>`` = (result >> result) x
        #if !NET45
        member inline _.BindReturn (x: '``Applicative1<Applicative2<'T>>``, [<InlineIfLambda>]f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map) f x
        #else
        member inline _.BindReturn (x: '``Applicative1<Applicative2<'T>>``, f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map) f x
        #endif
        member inline _.MergeSources  (t1, t2)     : '``Applicative1<Applicative2<'T>>`` = (lift2 >> lift2) tuple2 t1 t2
        member inline _.MergeSources3 (t1, t2, t3) : '``Applicative1<Applicative2<'T>>`` = (lift3 >> lift3) tuple3 t1 t2 t3
        member        _.Run x : '``Applicative1<Applicative2<'T>>`` = x
    
    /// Generic 3 layers Applicative CE builder.
    type ApplicativeBuilder3<'``applicative1<applicative2<applicative3<'t>>>``> () =
        member        _.ReturnFrom expr : '``applicative1<applicative2<applicative3<'t>>>`` = expr
        member inline _.Return (x: 'T) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (result >> result >> result) x
        member inline _.Yield  (x: 'T) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (result >> result >> result) x
        #if !NET45
        member inline _.BindReturn (x: '``Applicative1<Applicative2<Applicative3<'T>>>``, [<InlineIfLambda>]f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map >> map) f x
        #else
        member inline _.BindReturn (x: '``Applicative1<Applicative2<Applicative3<'T>>>``, f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map >> map) f x
        #endif
        member inline _.MergeSources  (t1, t2)     : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (lift2 >> lift2 >> lift2) tuple2 t1 t2
        member inline _.MergeSources3 (t1, t2, t3) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (lift3 >> lift3 >> lift3) tuple3 t1 t2 t3
        member        _.Run x : '``Applicative1<Applicative2<Applicative3<'T>>>`` = x


    /// Generic ZipApplicative CE builder.
    type ZipApplicativeBuilder<'``applicative<'t>``> () =
        member        _.ReturnFrom (expr) = expr   : '``applicative<'t>``
        member inline _.Return (x: 'T) = pur x : '``Applicative<'T>``
        member inline _.Yield  (x: 'T) = pur x : '``Applicative<'T>``
        #if !NET45
        member inline _.BindReturn(x, [<InlineIfLambda>]f) = map f x : '``Applicative<'U>``
        #else
        member inline _.BindReturn(x, f) = map f x : '``Applicative<'U>``
        #endif
        member inline _.MergeSources  (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``) : '``Applicative<'T * 'U>`` = map2 tuple2 t1 t2
        member inline _.MergeSources3 (t1: '``Applicative<'T>``, t2: '``Applicative<'U>``, t3: '``Applicative<'V>``) : '``Applicative<'T * 'U * 'V>`` = map3 tuple3 t1 t2 t3
        member        _.Run f : '``Applicative<'T>`` = f

    /// Generic 2 layers ZipApplicative CE builder.
    type ZipApplicativeBuilder2<'``applicative1<applicative2<'t>>``> () =
        member        _.ReturnFrom expr : '``applicative1<applicative2<'t>>`` = expr
        member inline _.Return (x: 'T) : '``Applicative1<Applicative2<'T>>`` = (pur >> pur) x
        member inline _.Yield  (x: 'T) : '``Applicative1<Applicative2<'T>>`` = (pur >> pur) x
        #if !NET45
        member inline _.BindReturn (x: '``Applicative1<Applicative2<'T>>``, [<InlineIfLambda>]f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map) f x
        #else
        member inline _.BindReturn (x: '``Applicative1<Applicative2<'T>>``, f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map) f x
        #endif
        member inline _.MergeSources  (t1, t2)     : '``Applicative1<Applicative2<'T>>`` = (map2 >> map2) tuple2 t1 t2
        member inline _.MergeSources3 (t1, t2, t3) : '``Applicative1<Applicative2<'T>>`` = (map3 >> map3) tuple3 t1 t2 t3
        member        _.Run x : '``Applicative1<Applicative2<'T>>`` = x

    /// Generic 3 layers ZipApplicative CE builder.
    type ZipApplicativeBuilder3<'``applicative1<applicative2<applicative3<'t>>>``> () =
        member        _.ReturnFrom expr : '``applicative1<applicative2<applicative3<'t>>>`` = expr
        member inline _.Return (x: 'T) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (pur >> pur >> pur) x
        member inline _.Yield  (x: 'T) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (pur >> pur >> pur) x
        #if !NET45
        member inline _.BindReturn (x: '``Applicative1<Applicative2<Applicative3<'T>>>``, [<InlineIfLambda>]f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map >> map) f x
        #else
        member inline _.BindReturn (x: '``Applicative1<Applicative2<Applicative3<'T>>>``, f: _ -> _) : '``Applicative1<Applicative2<'U>>`` = (map >> map >> map) f x
        #endif
        member inline _.MergeSources  (t1, t2)     : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (map2 >> map2 >> map2) tuple2 t1 t2
        member inline _.MergeSources3 (t1, t2, t3) : '``Applicative1<Applicative2<Applicative3<'T>>>`` = (map3 >> map3 >> map3) tuple3 t1 t2 t3
        member        _.Run x : '``Applicative1<Applicative2<Applicative3<'T>>>`` = x

    /// Creates a (lazy) monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad<'``monad<'t>``> = new MonadFxBuilder<'``monad<'t>``> ()

    /// Creates a strict monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad'<'``monad<'t>``> = new MonadFxStrictBuilder<'``monad<'t>``> ()

    /// Creates a (sequential) applicative computation expression.
    let applicative<'``Applicative<'T>``> = ApplicativeBuilder<'``Applicative<'T>``> ()

    /// Creates a (sequential) applicative computation expression which compose effects of two Applicatives.
    let applicative2<'``Applicative1<Applicative2<'T>>``> = ApplicativeBuilder2<'``Applicative1<Applicative2<'T>>``> ()

    /// Creates a (sequential) applicative computation expression which compose effects of three Applicatives.
    let applicative3<'``Applicative1<Applicative2<Applicative3<'T>>>``> = ApplicativeBuilder3<'``Applicative1<Applicative2<Applicative3<'T>>>``> ()

    /// Creates a (non sequential) applicative computation expression.
    let applicative'<'``ZipApplicative<'T>``> = ZipApplicativeBuilder<'``ZipApplicative<'T>``> ()

    /// Creates a (non sequential) applicative computation expression which compose effects of two Applicatives.
    let applicative2'<'``ZipApplicative1<ZipApplicative2<'T>>``> = ZipApplicativeBuilder2<'``ZipApplicative1<ZipApplicative2<'T>>``> ()

    /// Creates a (non sequential) applicative computation expression which compose effects of three Applicatives.
    let applicative3'<'``Applicative1<Applicative2<Applicative3<'T>>>``> = ZipApplicativeBuilder3<'``Applicative1<Applicative2<Applicative3<'T>>>``> ()

#endif
