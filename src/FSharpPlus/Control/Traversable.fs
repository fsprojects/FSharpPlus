namespace FSharpPlus.Control

#if !FABLE_COMPILER

open System.Runtime.InteropServices
open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals
open FSharpPlus.Internals.MonadOps
open FSharpPlus.Extensions

type Sequence =
    inherit Default1
    static member inline InvokeOnInstance (t: '``Traversable<'Functor<'T>>``) = (^``Traversable<'Functor<'T>>`` : (static member Sequence : _ -> _) t) : '``Functor<'Traversable<'T>>``
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline ForInfiniteSequences (t: seq<_>, [<InlineIfLambda>]isFailure, [<InlineIfLambda>]conversion, [<InlineIfLambda>]result) =
        let add x y = y :: x
        let mutable go = true
        let mutable r = Unchecked.defaultof<_>
        let mutable isEmpty = true
        use e = t.GetEnumerator ()
        while go && e.MoveNext () do
            if isFailure e.Current then go <- false
            if isEmpty then r <- Map.Invoke List.singleton e.Current
            else r <- Map.Invoke add r <*> e.Current
            isEmpty <- false
        if isEmpty then result (conversion [])
        else Map.Invoke (List.rev >> conversion) r

type Traverse =
    inherit Default1
    static member inline InvokeOnInstance f (t: ^a) = (^a : (static member Traverse : _ * _ -> 'R) t, f)

    static member inline Traverse (t: '``Traversable<'T>``, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<'Traversable<'U>>``, [<Optional>]_impl: Default5) =
        #if TEST_TRACE
        Traces.add "Traverse 'Traversable, 'T -> Functor<'U>"
        #endif
        let mapped = Map.Invoke f t : '``Traversable<'Functor<'U>>``
        (^``Traversable<'T>`` : (static member Sequence : _ -> _) mapped) : '``Functor<'Traversable<'U>>``

    static member inline Traverse (t: Id<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default4) =
        #if TEST_TRACE
        Traces.add "Traverse Id"
        #endif
        Map.Invoke Id.create (f (Id.run t))

    static member inline Traverse (t: _ seq, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default4) =
        #if TEST_TRACE
        Traces.add "Traverse seq"
        #endif
        let cons_f x ys = Map.Invoke (Seq.cons: 'a -> seq<_> -> seq<_>) (f x) <*> ys
        Seq.foldBack cons_f t (result Seq.empty)

    static member inline Traverse (t: seq<'T>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<seq<'U>>``, [<Optional>]_impl: Default3) =
        #if TEST_TRACE
        Traces.add "Traverse seq, 'T -> Functor<'U>"
        #endif
        let mapped = Seq.map f t
        Sequence.ForInfiniteSequences (mapped, IsLeftZero.Invoke, List.toSeq, Return.Invoke) : '``Functor<seq<'U>>``

    #if !FABLE_COMPILER
    static member Traverse (t: 't seq, f: 't -> Async<'u>, [<Optional>]_output: Async<seq<'u>>, [<Optional>]_impl: Default2) : Async<seq<_>> =
        #if TEST_TRACE
        Traces.add "Traverse 't seq, 't -> Async<'u>"
        #endif
        async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.AsTask(f enum.Current, cancellationToken = ct).Result }}
    #endif

    static member inline Traverse (t: ^a, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse ^a"
        #endif
        Traverse.InvokeOnInstance f t
    static member inline Traverse (_: ^a when ^a : null and ^a :struct, _, _: 'R, _impl: Default1) = id
    
    static member Traverse (t: Id<'t>, f: 't -> option<'u>, [<Optional>]_output: option<Id<'u>>, [<Optional>]_impl: Traverse) =
        #if TEST_TRACE
        Traces.add "Traverse Id, 't -> option<'u>"
        #endif
        Option.map Id.create (f (Id.run t))
    
    static member inline Traverse (t: option<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse option"
        #endif
        match t with Some x -> Map.Invoke Some (f x) | _ -> result None
    
    static member inline Traverse (t: voption<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse voption"
        #endif
        match t with ValueSome x -> Map.Invoke ValueSome (f x) | _ -> result ValueNone

    static member inline Traverse (t:Map<_,_>  , f, [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse Map"
        #endif
        let insert_f m k v = Map.Invoke (Map.add k) v <*> m        
        Map.fold insert_f (result Map.empty) (Map.mapValues f t)

    static member inline Traverse (t: Result<'T,'Error>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<Result<'U, 'Error>>``, [<Optional>]_impl: Traverse) : '``Functor<Result<'U, 'Error>>`` =
        #if TEST_TRACE
        Traces.add "Traverse Result, 'T -> Functor<'U>"
        #endif
        match t with
        | Ok a    -> Map.Invoke Result<'U, 'Error>.Ok (f a)
        | Error e -> Return.Invoke (Result<'U, 'Error>.Error e)

    static member inline Traverse (t: Choice<'T,'Error>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<Choice<'U, 'Error>>``, [<Optional>]_impl: Traverse) : '``Functor<Choice<'U, 'Error>>`` =
        #if TEST_TRACE
        Traces.add "Traverse Choice, 'T -> Functor<'U>"
        #endif
        match t with
        | Choice1Of2 a -> Map.Invoke Choice<'U,'Error>.Choice1Of2 (f a)
        | Choice2Of2 e -> Return.Invoke (Choice<'U,'Error>.Choice2Of2 e)

    static member inline Traverse (t:list<_>,f , [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse list"
        #endif
        let rec loop acc = function
            | [] -> acc
            | x::xs ->
                let v = f x
                loop (v::acc) xs
        let cons_f x xs = Map.Invoke List.cons xs <*> x
        List.fold cons_f (result []) (loop [] t)

    static member inline Traverse (t:_ [],f , [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse []"
        #endif
        let rec loop acc = function
            | [||] -> acc
            | xxs ->
                let x, xs = Array.head xxs, Array.tail xxs
                let v = f x
                loop (Array.cons v acc) xs
        let cons_f x xs = Map.Invoke Array.cons xs <*> x
        Array.fold cons_f (result [||]) (loop [||] t)

    static member inline Invoke (f: 'T -> '``Functor<'U>``) (t: '``Traversable<'T>``) : '``Functor<'Traversable<'U>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f) = ((^a or ^b or ^c) : (static member Traverse : _*_*_*_ -> _) b, f, c, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, Unchecked.defaultof<'R>, f) : 'R
        call (Unchecked.defaultof<Traverse>, t, f)


type Sequence with

    static member inline Sequence (t: _ seq, [<Optional>]_output: 'R, [<Optional>]_impl: Default5) : 'R =
        let cons_f x ys = Map.Invoke (Seq.cons: 'a -> seq<_> -> seq<_>) x <*> ys
        Seq.foldBack cons_f t (result Seq.empty)

    static member inline Sequence (t: seq<'``Applicative<'T>``>, [<Optional>]_output: '``Applicative<seq<'T>>``   , [<Optional>]_impl: Default4) : '``Applicative<seq<'T>>`` =
        Sequence.ForInfiniteSequences (t, IsLeftZero.Invoke, List.toSeq, Return.Invoke)

    static member        Sequence (t: seq<option<'t>>   , [<Optional>]_output: option<seq<'t>>    , [<Optional>]_impl: Default3) : option<seq<'t>> = Option.Sequential t
    #if !FABLE_COMPILER
    static member        Sequence (t: seq<voption<'t>>  , [<Optional>]_output: voption<seq<'t>>   , [<Optional>]_impl: Default3) : voption<seq<'t>> = ValueOption.Sequential t
    #endif
    static member        Sequence (t: seq<Result<'t,'e>>, [<Optional>]_output: Result<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Result<seq<'t>, 'e> = Result.Sequential t
    static member        Sequence (t: seq<Choice<'t,'e>>, [<Optional>]_output: Choice<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Choice<seq<'t>, 'e> = Choice.Sequential t
    static member        Sequence (t: seq<list<'t>>     , [<Optional>]_output: list<seq<'t>>      , [<Optional>]_impl: Default3) : list<seq<'t>> = Sequence.ForInfiniteSequences (t, List.isEmpty, List.toSeq, List.singleton)
    static member        Sequence (t: seq<'t []>        , [<Optional>]_output: seq<'t> []         , [<Optional>]_impl: Default3) : seq<'t> [] = Sequence.ForInfiniteSequences (t, Array.isEmpty, List.toSeq, Array.singleton)

    #if !FABLE_COMPILER
    static member        Sequence (t: seq<Async<'t>>    , [<Optional>]_output: Async<seq<'t>>     , [<Optional>]_impl: Default3) : Async<seq<'t>> = Async.SequentialLazy t
    #endif

    static member inline Sequence (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default2) : 'R = Traverse.InvokeOnInstance id t
    static member inline Sequence (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R = Sequence.InvokeOnInstance t

    static member inline Sequence (t: option<_> , [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = match t with Some x -> Map.Invoke Some x | _ -> result None
    #if !FABLE_COMPILER
    static member inline Sequence (t: voption<_>, [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = match t with ValueSome x -> Map.Invoke ValueSome x | _ -> result ValueNone
    #endif
    static member inline Sequence (t: list<_>   , [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = Sequence.ForInfiniteSequences(t, IsLeftZero.Invoke, id, Return.Invoke)

    static member inline Sequence (t: Map<_,_>  , [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R =
       let insert_f k x ys = Map.Invoke (Map.add k) x <*> ys
       Map.foldBack insert_f t (result Map.empty)
    
    static member inline Sequence (t: Result<'``Functor<'T>``,'Error>, [<Optional>]_output: '``Functor<Result<'T,'Error>>``, [<Optional>]_impl: Sequence) : '``Functor<Result<'T,'Error>>`` =
        match t with
        | Ok a    -> Map.Invoke Result<'T,'Error>.Ok a
        | Error e -> Return.Invoke (Result<'T,'Error>.Error e)

    static member inline Sequence (t: Choice<'``Functor<'T>``,'Error>, [<Optional>]_output: '``Functor<Choice<'T,'Error>>``, [<Optional>]_impl: Sequence) : '``Functor<Choice<'T,'Error>>`` =
        match t with
        | Choice1Of2 a -> Map.Invoke Choice<'T,'Error>.Choice1Of2 a
        | Choice2Of2 e -> Return.Invoke (Choice<'T,'Error>.Choice2Of2 e)

    static member inline Sequence (t: _ []                         , [<Optional>]_output: 'R                            , [<Optional>]_impl: Sequence) : 'R = Sequence.ForInfiniteSequences(t, IsLeftZero.Invoke, Array.ofList, Return.Invoke)
 
    static member inline Sequence (t: Id<'``Functor<'T>``>         , [<Optional>]_output: '``Functor<Id<'T>>``          , [<Optional>]_impl: Sequence) : '``Functor<Id<'T>>`` = Traverse.Invoke id t
 
    static member inline Sequence (t: ResizeArray<'``Functor<'T>``>, [<Optional>]_output: '``Functor<ResizeArray<'T>>`` , [<Optional>]_impl: Sequence) : '``Functor<ResizeArray<'T>>``= Traverse.Invoke id t

    static member inline Invoke (t: '``Traversable<'Applicative<'T>>``) : '``Applicative<'Traversable<'T>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member Sequence : _*_*_ -> _) b, c, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'R>) : 'R
        call (Unchecked.defaultof<Sequence>, t)



// Pointwise/Parallel traversables


type Transpose =
    inherit Default1
    static member inline InvokeOnInstance (t: '``Traversable<'Functor<'T>>``) = (^``Traversable<'Functor<'T>>`` : (static member Transpose : _ -> _) t) : '``Functor<'Traversable<'T>>``
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline ForInfiniteSequences (t: seq<_>, [<InlineIfLambda>]isFailure, [<InlineIfLambda>]conversion, [<InlineIfLambda>]result) =
        let add x y = y :: x
        let mutable go = true
        let mutable r = Unchecked.defaultof<_>
        let mutable isEmpty = true
        use e = t.GetEnumerator ()
        while go && e.MoveNext () do
            if isFailure e.Current then go <- false
            if isEmpty then r <- Map.Invoke List.singleton e.Current
            else r <- Map.Invoke add r <.> e.Current
            isEmpty <- false
        if isEmpty then result (conversion [])
        else Map.Invoke (List.rev >> conversion) r

type Gather =
    inherit Default1
    static member inline InvokeOnInstance f (t: ^a) = (^a : (static member Gather : _ * _ -> 'R) t, f)

    static member inline Gather (t: '``Traversable<'T>``, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<'Traversable<'U>>``, [<Optional>]_impl: Default4) =
        #if TEST_TRACE
        Traces.add "Gather 'Traversable, 'T -> Functor<'U>"
        #endif
        let mapped = Map.Invoke f t : '``Traversable<'Functor<'U>>``
        (^``Traversable<'T>`` : (static member Transpose : _ -> _) mapped) : '``Functor<'Traversable<'U>>``

    static member inline Gather (t: Id<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) =
        #if TEST_TRACE
        Traces.add "Gather Id"
        #endif
        Map.Invoke Id.create (f (Id.run t))

    static member inline Gather (t: _ seq, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) =
        #if TEST_TRACE
        Traces.add "Gather seq"
        #endif
        let cons x y = seq {yield x; yield! y}
        let cons_f x ys = Map.Invoke (cons: 'a -> seq<_> -> seq<_>) (f x) <.> ys
        Seq.foldBack cons_f t (Pure.Invoke Seq.empty)



    #if !FABLE_COMPILER
    static member Gather (t: 't seq, f: 't -> Async<'u>, [<Optional>]_output: Async<seq<'u>>, [<Optional>]_impl: Default2) : Async<seq<_>> =
        #if TEST_TRACE
        Traces.add "Gather 't seq, 't -> Async<'u>"
        #endif
        async {
            let! ct = Async.CancellationToken
            return seq {
                use enum = t.GetEnumerator ()
                while enum.MoveNext() do
                    yield Async.AsTask(f enum.Current, cancellationToken = ct).Result } }
    #endif
    
    static member inline Gather (t: ^a, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R =
        #if TEST_TRACE
        Traces.add "Gather ^a"
        #endif
        Gather.InvokeOnInstance f t
    static member inline Gather (_: ^a when ^a : null and ^a :struct, _, _: 'R, _impl: Default1) = id

    
    static member Gather (t: Id<'t>, f: 't -> option<'u>, [<Optional>]_output: option<Id<'u>>, [<Optional>]_impl: Gather) =
        #if TEST_TRACE
        Traces.add "Gather Id, 't -> option<'u>"
        #endif
        Option.map Id.create (f (Id.run t))
    
    static member inline Gather (t: option<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Gather) : 'R =
        #if TEST_TRACE
        Traces.add "Gather option"
        #endif
        match t with Some x -> Map.Invoke Some (f x) | _ -> Pure.Invoke None
    
    static member inline Gather (t: voption<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Gather) : 'R =
        #if TEST_TRACE
        Traces.add "Gather voption"
        #endif
        match t with ValueSome x -> Map.Invoke ValueSome (f x) | _ -> Pure.Invoke ValueNone

    static member inline Gather (t:Map<_,_>  , f, [<Optional>]_output: 'R, [<Optional>]_impl: Gather) : 'R =
        #if TEST_TRACE
        Traces.add "Gather Map"
        #endif
        let insert_f m k v = Map.Invoke (Map.add k) v <.> m        
        Map.fold insert_f (Pure.Invoke Map.empty) (Map.mapValues f t)

    static member inline Gather (t: Result<'T,'Error>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<Result<'U, 'Error>>``, [<Optional>]_impl: Gather) : '``Functor<Result<'U, 'Error>>`` =
        #if TEST_TRACE
        Traces.add "Gather Result, 'T -> Functor<'U>"
        #endif
        match t with
        | Ok a    -> Map.Invoke Result<'U, 'Error>.Ok (f a)
        | Error e -> Pure.Invoke (Result<'U, 'Error>.Error e)

    static member inline Gather (t: Choice<'T,'Error>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<Choice<'U, 'Error>>``, [<Optional>]_impl: Gather) : '``Functor<Choice<'U, 'Error>>`` =
        #if TEST_TRACE
        Traces.add "Gather Choice, 'T -> Functor<'U>"
        #endif
        match t with
        | Choice1Of2 a -> Map.Invoke Choice<'U,'Error>.Choice1Of2 (f a)
        | Choice2Of2 e -> Pure.Invoke (Choice<'U,'Error>.Choice2Of2 e)

    static member inline Gather (t:list<_>,f , [<Optional>]_output: 'R, [<Optional>]_impl: Gather) : 'R =
        #if TEST_TRACE
        Traces.add "Gather list"
        #endif
        let rec loop acc = function
            | [] -> acc
            | x::xs ->
                let v = f x
                loop (v::acc) xs
        let cons_f x xs = Map.Invoke List.cons xs <.> x
        List.fold cons_f (Pure.Invoke []) (loop [] t)

    static member inline Gather (t:_ [],f , [<Optional>]_output: 'R, [<Optional>]_impl: Gather) : 'R =
        #if TEST_TRACE
        Traces.add "Gather []"
        #endif
        let rec loop acc = function
            | [||] -> acc
            | xxs ->
                let x, xs = Array.head xxs, Array.tail xxs
                let v = f x
                loop (Array.cons v acc) xs
        let cons_f x xs = Map.Invoke Array.cons xs <.> x
        Array.fold cons_f (Pure.Invoke [||]) (loop [||] t)

    static member inline Invoke (f: 'T -> '``Functor<'U>``) (t: '``Traversable<'T>``) : '``Functor<'Traversable<'U>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f) = ((^a or ^b or ^c) : (static member Gather : _*_*_*_ -> _) b, f, c, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, Unchecked.defaultof<'R>, f) : 'R
        call (Unchecked.defaultof<Gather>, t, f)


type Transpose with

    static member inline Transpose (t: _ seq, [<Optional>]_output: 'R, [<Optional>]_impl: Default5) : 'R =
        let cons_f x ys = Map.Invoke (Seq.cons: 'a -> seq<_> -> seq<_>) x <.> ys
        Seq.foldBack cons_f t (Pure.Invoke Seq.empty)

    static member inline Transpose (t: seq<'``Applicative<'T>``>, [<Optional>]_output: '``Applicative<seq<'T>>``   , [<Optional>]_impl: Default4) : '``Applicative<seq<'T>>`` =
        Transpose.ForInfiniteSequences (t, IsZipLeftZero.Invoke, List.toSeq, Pure.Invoke)

    static member        Transpose (t: seq<option<'t>>   , [<Optional>]_output: option<seq<'t>>    , [<Optional>]_impl: Default3) : option<seq<'t>> = Option.Sequential t
    #if !FABLE_COMPILER
    static member        Transpose (t: seq<voption<'t>>  , [<Optional>]_output: voption<seq<'t>>   , [<Optional>]_impl: Default3) : voption<seq<'t>> = ValueOption.Sequential t
    #endif
    static member inline Transpose (t: seq<Result<'t,'e>>, [<Optional>]_output: Result<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Result<seq<'t>, 'e> = Result.Parallel ((++), t)
    static member inline Transpose (t: seq<Choice<'t,'e>>, [<Optional>]_output: Choice<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Choice<seq<'t>, 'e> = Choice.Parallel ((++), t)
    static member        Transpose (t: seq<list<'t>>     , [<Optional>]_output: list<seq<'t>>      , [<Optional>]_impl: Default3) : list<seq<'t>> = Transpose.ForInfiniteSequences (t, List.isEmpty, List.toSeq, List.singleton >> List.cycle)

    #if !FABLE_COMPILER
    static member        Transpose (t: seq<Async<'t>>    , [<Optional>]_output: Async<seq<'t>>     , [<Optional>]_impl: Default3) : Async<seq<'t>> = Async.Parallel t |> Async.map Array.toSeq
    #endif

    static member inline Transpose (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default2) : 'R = Gather.InvokeOnInstance id t
    static member inline Transpose (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R = Transpose.InvokeOnInstance t

    static member inline Transpose (t: option<_> , [<Optional>]_output: 'R, [<Optional>]_impl: Transpose) : 'R = match t with Some x -> Map.Invoke Some x | _ -> Pure.Invoke None
    #if !FABLE_COMPILER
    static member inline Transpose (t: voption<_>, [<Optional>]_output: 'R, [<Optional>]_impl: Transpose) : 'R = match t with ValueSome x -> Map.Invoke ValueSome x | _ -> Pure.Invoke ValueNone
    #endif
    static member inline Transpose (t: list<_>   , [<Optional>]_output: 'R, [<Optional>]_impl: Transpose) : 'R = Transpose.ForInfiniteSequences (t, IsZipLeftZero.Invoke, id, Pure.Invoke)

    static member inline Transpose (t: Map<_,_>  , [<Optional>]_output: 'R, [<Optional>]_impl: Transpose) : 'R =
       let insert_f k x ys = Map.Invoke (Map.add k) x <.> ys
       Map.foldBack insert_f t (Pure.Invoke Map.empty)
    
    static member inline Transpose (t: Result<'``Functor<'T>``,'Error>, [<Optional>]_output: '``Functor<Result<'T,'Error>>``, [<Optional>]_impl: Transpose) : '``Functor<Result<'T,'Error>>`` =
        match t with
        | Ok a    -> Map.Invoke Result<'T,'Error>.Ok a
        | Error e -> Pure.Invoke (Result<'T,'Error>.Error e)

    static member inline Transpose (t: Choice<'``Functor<'T>``,'Error>, [<Optional>]_output: '``Functor<Choice<'T,'Error>>``, [<Optional>]_impl: Transpose) : '``Functor<Choice<'T,'Error>>`` =
        match t with
        | Choice1Of2 a -> Map.Invoke Choice<'T,'Error>.Choice1Of2 a
        | Choice2Of2 e -> Pure.Invoke (Choice<'T,'Error>.Choice2Of2 e)

    static member inline Transpose (t: _ []                         , [<Optional>]_output: 'R                           , [<Optional>]_impl: Transpose) : 'R = Transpose.ForInfiniteSequences (t, IsZipLeftZero.Invoke, Array.ofList, Pure.Invoke)
 
    static member inline Transpose (t: Id<'``Functor<'T>``>         , [<Optional>]_output: '``Functor<Id<'T>>``         , [<Optional>]_impl: Transpose) : '``Functor<Id<'T>>`` = Gather.Invoke id t
 
    static member inline Transpose (t: ResizeArray<'``Functor<'T>``>, [<Optional>]_output: '``Functor<ResizeArray<'T>>``, [<Optional>]_impl: Transpose) : '``Functor<ResizeArray<'T>>``= Gather.Invoke id t

    static member inline Invoke (t: '``Traversable<'Applicative<'T>>``) : '``Applicative<'Traversable<'T>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member Transpose : _*_*_ -> _) b, c, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'R>) : 'R
        call (Unchecked.defaultof<Transpose>, t)

#endif
