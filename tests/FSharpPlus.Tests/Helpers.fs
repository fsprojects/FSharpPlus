module FSharpPlus.Tests.Helpers

open NUnit.Framework
open System.Collections

let areEqual (x:'t) (y:'t) = Assert.AreEqual (x, y)
let areStEqual x y = Assert.IsTrue( (x = y), sprintf "Expected %A to be structurally equal to %A" x y)
let areEquivalent (x:IEnumerable) (y:IEnumerable) = CollectionAssert.AreEquivalent (x, y)
let require (x: bool) (msg: string) = Assert.IsTrue (x, msg)

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset () = effects.Clear ()
    let add x = effects.Add (x)
    let get () = effects |> Seq.toList
    let are lst = areEquivalent lst (get ())

module Rebuilders =
    // following code is copied from Builders.fs with a modified Run method
    open System
    open System.Collections.Generic
    open FSharpPlus
    open FSharpPlus.Control

    type StrictBuilder<'``monad<'t>``> () =
        inherit FSharpPlus.GenericBuilders.Builder<'``monad<'t>``> ()
        member inline _.Delay ([<InlineIfLambda>]expr) = expr : unit -> '``Monad<'T>``
        member inline this.Run ([<InlineIfLambda>]expr: _->'``Monad<'T>``) = this.Bind (this.Return (), expr)
        member inline _.TryWith    ([<InlineIfLambda>]expr, [<InlineIfLambda>]handler)      = TryWith.InvokeForStrict    expr handler      : '``Monad<'T>``
        member inline _.TryFinally ([<InlineIfLambda>]expr, [<InlineIfLambda>]compensation) = TryFinally.InvokeForStrict expr compensation : '``Monad<'T>``
        
        member inline _.Using (disposable: #IDisposable, [<InlineIfLambda>]body) = Using.Invoke disposable body



    type MonadPlusStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        member        _.YieldFrom expr = expr                           : '``monad<'t>``
        member inline _.Zero () = Empty.Invoke ()                       : '``MonadPlus<'T>``
        member inline _.Combine (a: '``MonadPlus<'T>``, [<InlineIfLambda>]b) = a <|> b () : '``MonadPlus<'T>``
        member inline _.While ([<InlineIfLambda>]guard, [<InlineIfLambda>]body: unit -> '``MonadPlus<'T>``) : '``MonadPlus<'T>`` =
            let rec loop guard body =
                if guard () then body () <|> loop guard body
                else Empty.Invoke ()
            loop guard body
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``MonadPlus<'U>``) =
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``MonadPlus<'U>``)

    type MonadFxStrictBuilder<'``monad<'t>``> () =
        inherit StrictBuilder<'``monad<'t>``> ()
        
        member inline _.Zero () = result ()                                       : '``Monad<unit>``
        member inline _.Combine (a: '``Monad<unit>``, [<InlineIfLambda>]b) = a >>= fun () -> b () : '``Monad<'T>``
        
        member inline _.While ([<InlineIfLambda>]guard, [<InlineIfLambda>]body: unit -> '``Monad<unit>``)             : '``Monad<unit>`` =
            let rec loop guard body =
                if guard () then body () >>= fun () -> loop guard body
                else result ()
            loop guard body
        member inline this.For (p: #seq<'T>, [<InlineIfLambda>]rest: 'T->'``Monad<unit>``) =
            Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
                let enum = enum :?> IEnumerator<_>
                this.While (enum.MoveNext, fun () -> rest enum.Current) : '``Monad<unit>``)


/// Creates a strict monadic computation expression with a delayed Run method
let drMonad<'``monad<'t>``> = new Rebuilders.MonadFxStrictBuilder<'``monad<'t>``> ()
