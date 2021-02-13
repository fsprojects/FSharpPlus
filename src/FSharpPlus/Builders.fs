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

    
    // Workflows

    open System
    open System.Collections.Generic
    open FSharpPlus.Control

    type Builder () =
        member        __.ReturnFrom (expr) = expr                                        : '``Monad<'T>``
        member inline __.Return (x: 'T) = result x                                       : '``Monad<'T>``
        member inline __.Yield  (x: 'T) = result x                                       : '``Monad<'T>``
        member inline __.Bind (p: '``Monad<'T>``, rest: 'T->'``Monad<'U>``) = p >>= rest : '``Monad<'U>``
        member inline __.MergeSources (t1: '``Monad<'T>``, t2: '``Monad<'U>``)           : '``Monad<'T * 'U>`` = Lift2.Invoke tuple2 t1 t2

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

    type StrictBuilder () =
        inherit Builder ()
        member inline __.Delay expr = expr : unit -> '``Monad<'T>``
        member        __.Run f = f ()              : '``Monad<'T>``
        member inline __.TryWith    (expr, handler)      = TryWith.InvokeForStrict    expr handler      : '``Monad<'T>``
        member inline __.TryFinally (expr, compensation) = TryFinally.InvokeForStrict expr compensation : '``Monad<'T>``
        
        member inline __.Using (disposable: #IDisposable, body) = Using.Invoke disposable body

    type DelayedBuilder () =
        inherit Builder ()
        member inline __.Delay (expr: _->'``Monad<'T>``) = Delay.Invoke expr : '``Monad<'T>``
        member        __.Run f = f                                           : '``Monad<'T>``
        member inline __.TryWith    (expr, handler     ) = TryWith.Invoke    expr handler      : '``Monad<'T>``
        member inline __.TryFinally (expr, compensation) = TryFinally.Invoke expr compensation : '``Monad<'T>``
        member inline __.Using (disposable: #IDisposable, body) = Using.Invoke disposable body : '``Monad<'T>``

    type MonadPlusStrictBuilder () =
        inherit StrictBuilder ()
        member        __.YieldFrom  (expr) = expr                        : '``Monad<'T>``
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

    type MonadFxStrictBuilder () =
        inherit StrictBuilder ()
        
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

    type MonadPlusBuilder () =
        inherit DelayedBuilder()
        member        __.YieldFrom  (expr) = expr                     : '``Monad<'T>``
        member        __.strict = new MonadPlusStrictBuilder ()
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

    type MonadFxBuilder () =
        inherit DelayedBuilder ()
        member        __.strict  = new MonadFxStrictBuilder ()

        /// Makes it a (lazy) monadplus computation expression.
        member        __.plus    = new MonadPlusBuilder ()

        /// Makes it a strict monadplus computation expression.
        member        __.plus'   = new MonadPlusStrictBuilder ()

        /// Makes it a (lazy) monadic computation expression with side-effects
        member        this.fx    = this

        /// Makes it a strict monadic computation expression with side-effects
        member        __.fx'     = new MonadFxStrictBuilder ()

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


    /// Creates a (lazy) monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad = new MonadFxBuilder ()

    /// Creates a strict monadic computation expression with side-effects (see http://fsprojects.github.io/FSharpPlus/computation-expressions.html for more information)
    let monad' = new MonadFxStrictBuilder ()

#endif