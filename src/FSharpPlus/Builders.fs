namespace FSharpPlus

#nowarn "40"

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
    open FSharpPlus.Control

    type Builder () =
        member        __.ReturnFrom (expr) = expr                                       : '``Monad<'T>``
        member inline __.Return (x: 'T) = result x                                      : '``Monad<'T>``
        member inline __.Yield  (x: 'T) = result x                                      : '``Monad<'T>``
        member inline __.Bind (p: '``Monad<'T>``, rest: 'T->'``Monad<'U>``) = p >>= rest: '``Monad<'U>``

        [<CustomOperation("select", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline __.Select (x, [<ProjectionParameter>] f) = map f x

        [<CustomOperation("where", MaintainsVariableSpaceUsingBind=true)>]
        member inline __.Where (x, [<ProjectionParameter>] p) = mfilter p x

        [<CustomOperation("first")>] 
        member inline __.First (source) = head source

        [<CustomOperation("nth")>]
        member inline __.Nth (source, n) = nth n source

        [<CustomOperation("groupBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline __.GroupBy (x,[<ProjectionParameter>] f : 'T -> 'key) = groupBy f x

        [<CustomOperation("chunkBy", AllowIntoPattern=true, MaintainsVariableSpaceUsingBind=true)>]
        member inline __.ChunkBy (x,[<ProjectionParameter>] f : 'T -> 'key) = chunkBy f x

        [<CustomOperation("sortBy", MaintainsVariableSpaceUsingBind=true, AllowIntoPattern=true)>]
        member inline __.SortBy (x,[<ProjectionParameter>] f : 'T -> 'key) = sortBy f x

    type StrictBuilder () =
        inherit Builder ()
        member inline __.Delay expr = expr : unit -> '``Monad<'T>``
        member        __.Run f = f ()              : '``Monad<'T>``
        member        __.TryWith    (expr, handler)      = try expr () with e -> handler e
        member        __.TryFinally (expr, compensation) = try expr () finally compensation ()
        member        rs.Using (disposable: #IDisposable, body) =
            let body = fun () -> (body ()) disposable
            rs.TryFinally (body, fun () -> dispose disposable)

    type DelayedBuilder () =
        inherit Builder ()
        member inline __.Delay (expr: _->'``Monad<'T>``) = Delay.Invoke expr : '``Monad<'T>``
        member        __.Run f = f                                           : '``Monad<'T>``
        member inline __.TryWith    (expr, handler     ) = TryWith.Invoke    expr handler      : '``Monad<'T>``
        member inline __.TryFinally (expr, compensation) = TryFinally.Invoke expr compensation : '``Monad<'T>``
        member inline __.Using (disposable:#IDisposable, body) = Using.Invoke disposable body  : '``Monad<'T>``

    type MonadPlusStrictBuilder () =
        inherit StrictBuilder ()
        member inline __.Zero () = Empty.Invoke ()                       : '``MonadPlus<'T>``
        member inline __.Combine (a: '``MonadPlus<'T>``, b) = a <|> b () : '``MonadPlus<'T>``
        member inline __.While (guard, body: unit -> '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
            let rec loop guard body =
                if guard () then body () <|> loop guard body
                else Empty.Invoke ()
            loop guard body
        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) =
            let fusing (resource: #IDisposable) body = try body resource finally dispose resource   
            fusing (p.GetEnumerator ()) (fun enum -> (this.While (enum.MoveNext, fun () -> rest enum.Current) : '``MonadPlus<'U>``))
    
    type MonadFxStrictBuilder () =
        inherit StrictBuilder ()
        member inline __.Zero () = result ()                                       : '``Monad<unit>``
        member inline __.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b ()) : '``Monad<'T>``
        member inline __.While (guard, body: unit -> '``Monad<'T>``) : '``Monad<'T>`` =
            let rec loop guard body =
                if guard () then body () >>= fun () -> loop guard body
                else result ()
            loop guard body
        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<'U>``) =
            let fusing (resource: #IDisposable) body = try body resource finally dispose resource
            fusing (p.GetEnumerator ()) (fun enum -> (this.While (enum.MoveNext, fun () -> rest enum.Current) : '``Monad<'U>``))
 
    type MonadPlusBuilder () =
        inherit DelayedBuilder()     
        member inline __.Zero () = Empty.Invoke ()                    : '``MonadPlus<'T>``
        member inline __.Combine (a: '``MonadPlus<'T>``, b) = a <|> b : '``MonadPlus<'T>``
        member inline __.While (guard, body: '``MonadPlus<'T>``)      : '``MonadPlus<'T>`` =
            let rec fix () = Delay.Invoke (fun () -> if guard () then body <|> fix () else Empty.Invoke ())
            fix ()
        member inline this.For (p: #seq<'T>, rest: 'T->'``MonadPlus<'U>``) =
            Using.Invoke (p.GetEnumerator ()) (fun enum -> (this.While (enum.MoveNext, Delay.Invoke (fun () -> rest enum.Current)) : '``MonadPlus<'U>``))

        member __.strict = new MonadPlusStrictBuilder ()

    type MonadFxBuilder () =
        inherit DelayedBuilder ()
        member inline __.Zero () = result ()                                    : '``Monad<unit>``
        member inline __.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b) : '``Monad<'T>``
        member inline __.While (guard, body: '``Monad<unit>``) : '``Monad<'T>`` =
            let rec loop guard body =
                if guard () then body >>= (fun () -> loop guard body)
                else result ()
            loop guard body
        member inline this.For (p: #seq<'T>, rest: 'T->'``Monad<unit>``) =
            Using.Invoke (p.GetEnumerator ()) (fun enum -> (this.While (enum.MoveNext, Delay.Invoke (fun () -> rest enum.Current)) : '``Monad<unit>``))

    
        member __.plus   = new MonadPlusBuilder ()
        member __.strict = new MonadFxStrictBuilder ()

        member this.fx   = this

    let monad = new MonadFxBuilder ()