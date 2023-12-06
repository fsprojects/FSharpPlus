namespace FSharpPlus.Control

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

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
    static member inline ForInfiniteSequences (t: seq<_>, isFailure, conversion) =
        let add x y = y :: x
        let mutable go = true
        let mutable r = result []
        use e = t.GetEnumerator ()
        while go && e.MoveNext () do
            if isFailure e.Current then go <- false
            r <- Map.Invoke add r <*> e.Current
        Map.Invoke (List.rev >> conversion) r

type Traverse =
    inherit Default1
    static member inline InvokeOnInstance f (t: ^a) = (^a : (static member Traverse : _ * _ -> 'R) t, f)

    static member inline Traverse (t: '``Traversable<'T>``, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<'Traversable<'U>>``, [<Optional>]_impl: Default4) =
        #if TEST_TRACE
        Traces.add "Traverse 'Traversable, 'T -> Functor<'U>"
        #endif
        let mapped = Map.Invoke f t : '``Traversable<'Functor<'U>>``
        (^``Traversable<'T>`` : (static member Sequence : _ -> _) mapped) : '``Functor<'Traversable<'U>>``

    static member inline Traverse (t: Id<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) =
        #if TEST_TRACE
        Traces.add "Traverse Id"
        #endif
        Map.Invoke Id.create (f (Id.run t))

    static member inline Traverse (t: _ seq, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) =
        #if TEST_TRACE
        Traces.add "Traverse seq"
        #endif
        let cons x y = seq {yield x; yield! y}
        let cons_f x ys = Map.Invoke (cons: 'a -> seq<_> -> seq<_>) (f x) <*> ys
        Seq.foldBack cons_f t (result Seq.empty)

    static member inline Traverse (t: seq<'T>, f: 'T -> '``Functor<'U>``, [<Optional>]_output: '``Functor<seq<'U>>``, [<Optional>]_impl: Default2) =
        #if TEST_TRACE
        Traces.add "Traverse seq, 'T -> Functor<'U>"
        #endif
        let mapped = Seq.map f t
        Sequence.ForInfiniteSequences (mapped, IsLeftZero.Invoke, List.toSeq) : '``Functor<seq<'U>>``

    static member inline Traverse (t: ^a, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R =
        #if TEST_TRACE
        Traces.add "Traverse ^a"
        #endif
        Traverse.InvokeOnInstance f t
    static member inline Traverse (_: ^a when ^a : null and ^a :struct, _, _: 'R, _impl: Default1) = id

    #if !FABLE_COMPILER
    static member Traverse (t: 't seq, f: 't -> Async<'u>, [<Optional>]_output: Async<seq<'u>>, [<Optional>]_impl: Traverse) : Async<seq<_>> = async {
        #if TEST_TRACE
        Traces.add "Traverse 't seq, 't -> Async<'u>"
        #endif
        
        let! ct = Async.CancellationToken
        return seq {
            use enum = t.GetEnumerator ()
            while enum.MoveNext() do
                yield Async.RunSynchronously (f enum.Current, cancellationToken = ct) }}
    #endif
    
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
        let cons x y = Array.append [|x|] y
        let rec loop acc = function
            | [||] -> acc
            | xxs ->
                let x, xs = Array.head xxs, Array.tail xxs
                let v = f x
                loop (cons v acc) xs
        let cons_f x xs = Map.Invoke cons xs <*> x
        Array.fold cons_f (result [||]) (loop [||] t)

    static member inline Invoke (f: 'T -> '``Functor<'U>``) (t: '``Traversable<'T>``) : '``Functor<'Traversable<'U>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f) = ((^a or ^b or ^c) : (static member Traverse : _*_*_*_ -> _) b, f, c, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, Unchecked.defaultof<'R>, f) : 'R
        call (Unchecked.defaultof<Traverse>, t, f)


type Sequence with

    static member inline Sequence (t: _ seq, [<Optional>]_output: 'R, [<Optional>]_impl: Default5) : 'R =
        let cons x y = seq { yield x; yield! y }
        let cons_f x ys = Map.Invoke (cons: 'a -> seq<_> -> seq<_>) x <*> ys
        Seq.foldBack cons_f t (result Seq.empty)

    static member inline Sequence (t: seq<'``Applicative<'T>``>, [<Optional>]_output: '``Applicative<seq<'T>>``   , [<Optional>]_impl: Default4) : '``Applicative<seq<'T>>`` =
        Sequence.ForInfiniteSequences (t, IsLeftZero.Invoke, List.toSeq)

    static member        Sequence (t: seq<option<'t>>   , [<Optional>]_output: option<seq<'t>>    , [<Optional>]_impl: Default3) : option<seq<'t>> = Option.Sequence t
    #if !FABLE_COMPILER
    static member        Sequence (t: seq<voption<'t>>  , [<Optional>]_output: voption<seq<'t>>   , [<Optional>]_impl: Default3) : voption<seq<'t>> = ValueOption.Sequence t
    #endif
    static member        Sequence (t: seq<Result<'t,'e>>, [<Optional>]_output: Result<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Result<seq<'t>, 'e> = Result.Sequence t
    static member        Sequence (t: seq<Choice<'t,'e>>, [<Optional>]_output: Choice<seq<'t>, 'e>, [<Optional>]_impl: Default3) : Choice<seq<'t>, 'e> = Choice.Sequence t
    static member        Sequence (t: seq<list<'t>>     , [<Optional>]_output: list<seq<'t>>      , [<Optional>]_impl: Default3) : list<seq<'t>> = Sequence.ForInfiniteSequences (t, List.isEmpty, List.toSeq)
    static member        Sequence (t: seq<'t []>        , [<Optional>]_output: seq<'t> []         , [<Optional>]_impl: Default3) : seq<'t> [] = Sequence.ForInfiniteSequences (t, Array.isEmpty, List.toSeq)

    #if !FABLE_COMPILER
    static member        Sequence (t: seq<Async<'t>>    , [<Optional>]_output: Async<seq<'t>>     , [<Optional>]_impl: Default3) : Async<seq<'t>> = Async.Sequence t
    #endif

    static member inline Sequence (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default2) : 'R = Traverse.InvokeOnInstance id t
    static member inline Sequence (t: ^a        , [<Optional>]_output: 'R, [<Optional>]_impl: Default1) : 'R = Sequence.InvokeOnInstance t

    static member inline Sequence (t: option<_> , [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = match t with Some x -> Map.Invoke Some x | _ -> result None
    #if !FABLE_COMPILER
    static member inline Sequence (t: voption<_>, [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = match t with ValueSome x -> Map.Invoke ValueSome x | _ -> result ValueNone
    #endif
    static member inline Sequence (t: list<_>   , [<Optional>]_output: 'R, [<Optional>]_impl: Sequence) : 'R = Sequence.ForInfiniteSequences(t, IsLeftZero.Invoke, id)

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

    static member inline Sequence (t: _ []                         , [<Optional>]_output: 'R                            , [<Optional>]_impl: Sequence) : 'R = Sequence.ForInfiniteSequences(t, IsLeftZero.Invoke, Array.ofList)
 
    static member inline Sequence (t: Id<'``Functor<'T>``>         , [<Optional>]_output: '``Functor<Id<'T>>``          , [<Optional>]_impl: Sequence) : '``Functor<Id<'T>>`` = Traverse.Invoke id t
 
    static member inline Sequence (t: ResizeArray<'``Functor<'T>``>, [<Optional>]_output: '``Functor<ResizeArray<'T>>`` , [<Optional>]_impl: Sequence) : '``Functor<ResizeArray<'T>>``= Traverse.Invoke id t

    static member inline Invoke (t: '``Traversable<'Applicative<'T>>``) : '``Applicative<'Traversable<'T>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member Sequence : _*_*_ -> _) b, c, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'R>) : 'R
        call (Unchecked.defaultof<Sequence>, t)

#endif
