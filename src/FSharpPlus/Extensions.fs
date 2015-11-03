namespace FSharpPlus

open System
open FSharpPlus.Operators

module Extensions =

    type Collections.Generic.IEnumerable<'T>  with
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   -> this |> Seq.skip a
            | None  , Some b -> this |> Seq.take b
            | Some a, Some b -> this |> Seq.skip a |> Seq.take (b-a+1)

    [<AutoOpen>]
    module Seq =
        let foldBack f (s:seq<_>) z = Array.foldBack f (Seq.toArray s) z
        let inline sequence ms =
            let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
            foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

        let inline mapM f as' = sequence (Seq.map f as')
              
        let groupAdjBy keyMapper (source:_ seq) = seq {
            use e = source.GetEnumerator()
            if (e.MoveNext()) then
                let groupKey = ref (keyMapper e.Current)
                let values   = ref (new ResizeArray<_>())
                (!values).Add(e.Current)
                while (e.MoveNext()) do
                    let key = keyMapper e.Current
                    if !groupKey = key then (!values).Add(e.Current)
                    else
                        yield (!groupKey, !values :> seq<_>)
                        groupKey := key
                        values   := new ResizeArray<_>()
                        (!values).Add(e.Current)
                yield (!groupKey, !values :> seq<_>)}

        // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
        let intersperse sep list =
            seq {
                let notFirst = ref false
                for element in list do 
                    if !notFirst then yield sep
                    yield element
                    notFirst := true}

    type List<'T> with
        static member singleton x = [x]
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   when a < 0 -> this |> skip (this.Length + a)
            | Some a, None              -> this |> skip                a 
            | None  , Some b when b < 0 -> this |> take (this.Length + b)
            | None  , Some b            -> this |> take                b
            | Some a, Some b when a >= 0 && b >= 0 -> this |> skip a |> take b
            | Some a, Some b -> 
                let l = this.Length
                let f i = if i < 0 then l + i else i
                let a = f a
                this |> skip a |> take (f b - a + 1)

    module List =
        let inline sequence (ms:list<'Monad'a>) =
            let k m m' = m >>= fun (x:'t) -> m' >>= fun xs -> (result :list<'t> -> 'Monad'List'a) (x::xs)
            List.foldBack k ms ((result :list<'t> -> 'Monad'List'a) [])

        let inline mapM (f:'a->'Monad'b) (xs:list<'a>) :'Monad'List'b = sequence (List.map f xs)
    
        let inline foldM (f:'a->'b->'Monad'a) (a:'a) (bx:list<'b>) : 'Monad'a =
            let rec loopM a = function
                | x::xs -> (f a x) >>= fun fax -> loopM fax xs 
                | [] -> result a
            loopM a bx

        let inline filterM (f: 'a -> 'Monad'Bool) (xs: list<'a>) : 'Monad'List'a =
            let rec loopM = function
                | []   -> result []
                | h::t -> 
                    f h >>= (fun flg ->
                        loopM t >>= (fun ys ->
                            result (if flg then (h::ys) else ys)))
            loopM xs


    /// A convenient alias for Choice<_,_>
    type Result<'TSuccess,'TFailure> = Choice<'TSuccess,'TFailure>
    let (|Success|Failure|) = function
        | Choice1Of2 x -> Success x
        | Choice2Of2 x -> Failure x

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x



    [<RequireQualifiedAccess>]
    module Error =
        let inline map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
        let inline result x = Choice1Of2 x
        let inline throw  x = Choice2Of2 x
        let inline bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v -> f v | Choice2Of2 e -> Choice2Of2 e
        let inline catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v -> Choice1Of2 v | Choice2Of2 e -> f e

    /// Choice<'TSuccess,'TFailure> specialized in 'TFailure = Exception 
    [<Runtime.CompilerServices.Extension>]
    module ResultOrException =
        [<Runtime.CompilerServices.Extension>]
        let IsResult  :Choice<_,exn>   -> _ = function Choice1Of2 _ -> true | _ -> false

        [<Runtime.CompilerServices.Extension>]
        let IsException :Choice<_,exn> -> _ = function Choice2Of2 _ -> true | _ -> false

        [<Runtime.CompilerServices.Extension>]
        let Result :Choice<_,exn>      -> _ = function Choice1Of2 v -> v | Choice2Of2 e -> raise e

        [<Runtime.CompilerServices.Extension>]
        let Exception :Choice<_,exn>   -> _ = function Choice2Of2 e -> e | _ -> new Exception()

    // http://msdn.microsoft.com/en-us/library/system.threading.tasks.task.whenall.aspx 

    open System.Threading
    open System.Threading.Tasks

    let private (|Canceled|Faulted|Completed|) (t: Task<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted(t.Exception)
        else Completed(t.Result)

    type Task<'t> with
        static member WhenAll(tasks : Task<'a>[], ?cancellationToken : CancellationToken) =
            let tcs = TaskCompletionSource<'a[]>()
            let cancellationToken = defaultArg cancellationToken CancellationToken.None
            cancellationToken.Register((fun () -> tcs.TrySetCanceled() |> ignore)) |> ignore
            let results = Array.zeroCreate<'a>(tasks.Length)
            let pending = ref results.Length
            tasks 
            |> Seq.iteri (fun i t ->
                let continuation = function
                | Canceled -> tcs.TrySetCanceled() |> ignore
                | Faulted(e) -> tcs.TrySetException(e) |> ignore
                | Completed(r) -> 
                    results.[i] <- r
                    if Interlocked.Decrement(pending) = 0 then 
                        tcs.SetResult(results)
                t.ContinueWith(continuation, cancellationToken,
                               TaskContinuationOptions.ExecuteSynchronously,
                               TaskScheduler.Default) |> ignore)
            tcs.Task
