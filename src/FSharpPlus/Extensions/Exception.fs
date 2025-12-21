namespace FSharpPlus

/// Additional operations on Exception
[<RequireQualifiedAccess>]
module Exception =
    open System
    open System.Runtime.ExceptionServices
    open FSharpPlus.Internals.Errors
    
    #if !FABLE_COMPILER

    /// Throws the given exception with its original stacktrace.
    let inline rethrow<'T> (exn: exn) =
        #if !NET45
        let exn = nullArgCheck (nameof exn) exn
        #endif
        (ExceptionDispatchInfo.Capture exn).Throw ()
        Unchecked.defaultof<'T>    
    
    /// Combines exceptions from 2 exceptions into a single AggregateException.
    /// Exceptions already present in the first argument won't be added.
    let add (exn1: exn) (exn2: exn) =
        #if !NET45
        let exn1 = nullArgCheck (nameof exn1) exn1
        let exn2 = nullArgCheck (nameof exn2) exn2
        #endif
        let f (e: exn) =
            match e with
            :? AggregateException as a -> a.InnerExceptions :> seq<_>
            | _ -> Seq.singleton e
        let left = f exn1
        new AggregateException (seq { yield! left; yield! Seq.except left (f exn2) })

    #endif
