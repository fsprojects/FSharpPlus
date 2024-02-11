namespace FSharpPlus

#if !FABLE_COMPILER && (NETSTANDARD2_1_OR_GREATER || NETCOREAPP3_0_OR_GREATER || NET5_0_OR_GREATER)

open System
open System.Threading
open System.Threading.Tasks
open FSharpPlus.Data
open FSharpPlus.Extensions

/// Additional operations on Observable<'T>
[<RequireQualifiedAccess>]
module AsyncEnumerable =

    let toAsyncSeq (source: Collections.Generic.IAsyncEnumerable<_>) : SeqT<Async<_>, _> = monad.plus {
        let! ct = SeqT.lift Async.CancellationToken
        let e = source.GetAsyncEnumerator ct
        use _ = { new IDisposable with member _.Dispose () = e.DisposeAsync().AsTask().Wait () }

        let mutable currentResult = true
        while currentResult do
            let! r = e.MoveNextAsync().AsTask () |> Async.Await |> SeqT.lift
            currentResult <- r
            if r then yield e.Current
    }

    let ofAsyncSeq (source: SeqT<Async<_>, 'T>) = {
        new Collections.Generic.IAsyncEnumerable<'T> with
            member _.GetAsyncEnumerator (cancellationToken: CancellationToken) =
                let mutable current = Unchecked.defaultof<_>
                let enumerator = (source :> IEnumerableM<Async<_>, 'T>).GetEnumerator ()
                { new Collections.Generic.IAsyncEnumerator<'T> with
                    member _.Current = current
                    member _.MoveNextAsync() =
                        let moveNextAsync = async {
                            let! enumerationResult = enumerator.MoveNext ()
                            if enumerationResult then
                                current <- enumerator.Current
                                return true
                            else return false }
                        Async.StartAsTask(moveNextAsync, cancellationToken = cancellationToken) |> ValueTask<bool>
                    member _.DisposeAsync () =
                        enumerator.Dispose ()
                        ValueTask ()
                }
    }

#endif