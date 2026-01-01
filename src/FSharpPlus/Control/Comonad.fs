namespace FSharpPlus.Control

open System
open System.Runtime.InteropServices
open System.Threading.Tasks

open FSharpPlus
open FSharpPlus.Extensions
open FSharpPlus.Internals
#if !FABLE_COMPILER_4

// Comonad class ----------------------------------------------------------

type Extract =
    static member        Extract (x: Async<'T>) =
    #if FABLE_COMPILER
        Async.RunSynchronously x
    #else
        Async.AsTask(x).Result
    #endif
    static member        Extract (x: Lazy<'T>     ) = x.Value
    static member        Extract ((_: 'W, a: 'T)  ) = a
    static member        Extract (struct (_: 'W, a: 'T)) = a
    static member        Extract (f: 'T Id        ) = f
    #if !FABLE_COMPILER
    static member inline Extract (f: 'Monoid -> 'T) = f (Zero.Invoke ())
    #else
    static member inline Extract (f: 'Monoid -> 'T) = f (LanguagePrimitives.GenericZero)
    #endif
    #if !FABLE_COMPILER
    static member        Extract (f: Task<'T>     ) = f.Result
    #endif
    #if !FABLE_COMPILER
    static member        Extract (f: ValueTask<'T>     ) = f.Result
    #endif
    static member inline Invoke (x: '``Comonad<'T>``) : 'T =
        let inline call_2 (_mthd: ^M, x: ^I) = ((^M or ^I) : (static member Extract : _ -> _) x)
        call_2 (Unchecked.defaultof<Extract>, x)

#nowarn "0025" // (see nowarn comment below)

type Extend =
    static member        (=>>) (g: Async<'T>    , f: Async<'T> -> 'U) = async.Return (f g)              : Async<'U>
    static member        (=>>) (g: Lazy<'T>     , f: Lazy<'T> -> 'U ) = Lazy<_>.Create  (fun () -> f g) : Lazy<'U>
    static member        (=>>) ((w: 'W, a: 'T)  , f: _ -> 'U        ) = (w, f (w, a))
    static member        (=>>) (struct (w: 'W, a: 'T), f: _ -> 'U   ) = struct (w, f (struct (w, a)))
    static member        (=>>) (g: Id<'T>       , f: Id<'T> -> 'U   ) = f g
    #if !FABLE_COMPILER
    static member inline (=>>) (g: 'Monoid -> 'T, f: _ -> 'U        ) = fun a -> f (fun b -> g (Plus.Invoke a b))
    #else
    static member inline (=>>) (g: 'Monoid -> 'T, f: _ -> 'U        ) = fun a -> f (fun b -> g (a + b))
    #endif
    #if !FABLE_COMPILER
    static member        (=>>) (g: Task<'T>     , f: Task<'T> -> 'U ) =
            if g.Status = TaskStatus.RanToCompletion then g.ContinueWith f
            else
                let tcs = TaskCompletionSource<'U> ()
                if g.Status   = TaskStatus.Canceled then tcs.SetCanceled ()
                elif g.Status = TaskStatus.Faulted  then tcs.SetException g.Exception.InnerExceptions
                else
                    g.ContinueWith (fun (k: Task<'T>) ->
                        if k.Status = TaskStatus.RanToCompletion then
                            try tcs.SetResult (f k)
                            with e -> tcs.SetException e
                        elif k.Status = TaskStatus.Canceled then tcs.SetCanceled ()
                        elif k.Status = TaskStatus.Faulted  then tcs.SetException k.Exception.InnerExceptions) |> ignore
                tcs.Task
    #endif

    #if !FABLE_COMPILER
    static member        (=>>) (g: ValueTask<'T>     , f: ValueTask<'T> -> 'U ) : ValueTask<'U> =
        if g.IsCompletedSuccessfully then
            try
                let r = f g
                ValueTask<'U> r
            with e -> ValueTask<'U> (Task.FromException<'U> e)
        else
            let tcs = TaskCompletionSource<'U> ()
            if g.IsCompleted then
                match g with
                | ValueTask.Faulted e -> tcs.SetException e
                | ValueTask.Canceled  -> tcs.SetCanceled ()
                // nowarn here, this case has been handled already if g.IsCompleted
            else
                g |> ValueTask.continueTask tcs (fun _ ->
                    try tcs.SetResult (f g)
                    with e -> tcs.SetException e)
            ValueTask<'U> tcs.Task
            
    #endif

    // Restricted Comonads
    static member        (=>>) (s: list<'T>     , g) = List.map  g (List.tails s) : list<'U>
    static member        (=>>) (s: 'T []        , g) = Array.map g (s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray) : 'U []
    static member        (=>>) (s: seq<'T>      , g) = Seq.map   g (s |> Seq.toList   |> List.tails |> List.toSeq   |> Seq.map   List.toSeq)   : 'U seq

#if !FABLE_COMPILER
    static member inline Invoke (g: '``Comonad<'T>``->'U) (s: '``Comonad<'T>``) : '``Comonad<'U>`` =
        let inline call (_mthd: 'M, source: 'I, _output: 'R) = ((^M or ^I or ^R) : (static member (=>>) : _*_ -> _) source, g)
        call (Unchecked.defaultof<Extend>, s, Unchecked.defaultof<'``Comonad<'U>``>)
#endif

#if !FABLE_COMPILER

type Duplicate =
    inherit Default1
    static member inline Duplicate (x: '``Comonad<'T>``, [<Optional>]_mthd: Default1 ) = Extend.Invoke id x          : '``Comonad<'Comonad<'T>>``
    static member        Duplicate (s: Async<'T>       , [<Optional>]_mthd: Duplicate) = async.Return s              : Async<Async<'T>>
    static member        Duplicate (s: Lazy<'T>        , [<Optional>]_mthd: Duplicate) = Lazy<_>.CreateFromValue s   : Lazy<Lazy<'T>>
    static member        Duplicate (s: Id<'T>          , [<Optional>]_mthd: Duplicate) = Id s                        : Id<Id<'T>>
    static member        Duplicate ((w: 'W, a: 'T)     , [<Optional>]_mthd: Duplicate) = w, (w, a)
    static member        Duplicate (struct (w: 'W, a: 'T), [<Optional>]_mthd: Duplicate) = struct (w, struct (w, a))
    static member inline Duplicate (f: 'Monoid -> 'T   , [<Optional>]_mthd: Duplicate) = fun a b -> f (Plus.Invoke a b)

    // Restricted Comonads
    static member Duplicate (s:  list<'T>       , [<Optional>]_mthd: Duplicate) = List.tails s
    static member Duplicate (s: array<'T>       , [<Optional>]_mthd: Duplicate) = s |> Array.toList |> List.tails |> List.toArray |> Array.map List.toArray  

    static member inline Invoke (x: '``Comonad<'T>``) : '``Comonad<'Comonad<'T>>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Duplicate : _*_ -> _) source, mthd)
        call (Unchecked.defaultof<Duplicate>, x, Unchecked.defaultof<'``Comonad<'Comonad<'T>>``>)

#endif
#endif