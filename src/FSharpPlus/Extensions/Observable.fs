namespace FSharpPlus

#if !FABLE_COMPILER

open System
open System.Threading
open System.Threading.Tasks
open System.Runtime.ExceptionServices
open FSharpPlus.Data

/// Additional operations on Observable<'T>
[<RequireQualifiedAccess>]
module Observable =
    /// Union type that represents different messages that can be sent to the
    /// IObserver interface. The IObserver type is equivalent to a type that has
    /// just OnNext method that gets 'ObservableUpdate' as an argument.
    type (*internal*) ObservableUpdate<'T> =
        | Next of 'T
        | Error of ExceptionDispatchInfo
        | Completed


    /// Turns observable into an observable that only calls OnNext method of the
    /// observer, but gives it a discriminated union that represents different
    /// kinds of events (error, next, completed)
    let asUpdates (source: IObservable<'T>) = {
        new IObservable<_> with
            member _.Subscribe observer =
                source.Subscribe {
                    new IObserver<_> with
                    member _.OnNext v = observer.OnNext (Next v)
                    member _.OnCompleted () = observer.OnNext Completed
                    member _.OnError e = observer.OnNext (Error (ExceptionDispatchInfo.Capture e)) }}




    let toAsyncSeq (source: System.IObservable<'T>) : SeqT<Async<bool>, 'T> = monad.plus {
        let cts = new CancellationTokenSource ()
        try 
            // The body of this agent returns immediately.  It turns out this is a valid use of an F# agent, and it
            // leaves the agent available as a queue that supports an asynchronous receive.
            //
            // This makes the cancellation token is somewhat meaningless since the body has already returned.  However
            // if we don't pass it in then the default cancellation token will be used, so we pass one in for completeness.
            use agent = MailboxProcessor<_>.Start ((fun _inbox -> async.Return ()), cancellationToken = cts.Token)
            use _d = source |> asUpdates |> Observable.subscribe agent.Post
            let fin = ref false
            while not fin.Value do 
                let! msg = SeqT.lift (agent.Receive ())
                match msg with
                | ObservableUpdate.Error e -> e.Throw ()
                | Completed -> fin := true
                | Next v -> yield v 
        finally 
                // Cancel on early exit 
                cts.Cancel () }

    let toTaskSeq (source: System.IObservable<'T>) : SeqT<Task<bool>, 'T> = monad.plus {
        let cts = new CancellationTokenSource ()
        try 
            // The body of this agent returns immediately.  It turns out this is a valid use of an F# agent, and it
            // leaves the agent available as a queue that supports an asynchronous receive.
            //
            // This makes the cancellation token is somewhat meaningless since the body has already returned.  However
            // if we don't pass it in then the default cancellation token will be used, so we pass one in for completeness.
            use agent = MailboxProcessor<_>.Start ((fun _inbox -> async.Return ()), cancellationToken = cts.Token)
            use _d = source |> asUpdates |> Observable.subscribe agent.Post
            let fin = ref false
            while not fin.Value do 
                let! msg = SeqT.lift (Async.StartAsTask (agent.Receive (), cancellationToken = cts.Token))
                match msg with
                | ObservableUpdate.Error e -> e.Throw ()
                | Completed -> fin := true
                | Next v -> yield v 
        finally 
                // Cancel on early exit 
                cts.Cancel () }

#endif