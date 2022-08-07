namespace FSharpPlus

#if !FABLE_COMPILER

open System
open System.Threading
open System.Threading.Tasks
open FSharpPlus.Data

/// Additional operations on Observable<'T>
[<RequireQualifiedAccess>]
module AsyncEnumerable =

    let toAsyncSeq (source: Collections.Generic.IAsyncEnumerable<_>) : SeqT<Async<_>, _> = monad.plus {
        let! ct = SeqT.lift Async.CancellationToken
        let e = source.GetAsyncEnumerator ct
        use _ =
            { new IDisposable with
                member _.Dispose () =
                    e.DisposeAsync().AsTask () |> Async.AwaitTask |> Async.RunSynchronously }

        let mutable currentResult = true
        while currentResult do
            let! r = e.MoveNextAsync().AsTask () |> Async.AwaitTask |> SeqT.lift
            currentResult <- r
            if r then yield e.Current
    }

    let ofAsyncSeq (source: SeqT<Async<_>, 'a>) = {
        new Collections.Generic.IAsyncEnumerable<'a> with
            member _.GetAsyncEnumerator (cancellationToken: CancellationToken) =
                let mutable current = Unchecked.defaultof<_>
                let enumerator = source.GetEnumerator()
                { new Collections.Generic.IAsyncEnumerator<'a> with
                    member _.Current = current
                    member _.MoveNextAsync() =
                        let moveNextAsync = async {
                            let! enumerationResult = enumerator.MoveNext ()
                            match enumerationResult with
                            | Some v ->
                                current <- v
                                return true
                            | _ -> return false
                        }
                        Async.StartAsTask(moveNextAsync, cancellationToken = cancellationToken) |> ValueTask<bool>
                    member _.DisposeAsync () =
                        enumerator.Dispose ()
                        ValueTask ()
                }
    }

#endif