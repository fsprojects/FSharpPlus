namespace FSharpPlus.Control

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.ComponentModel
open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus.Internals.MonadOps
open FSharpPlus
open FSharpPlus.Extensions


type Sequence =
    inherit Default1
    static member inline InvokeOnInstance (t: ^a) = (^a : (static member Sequence : _ -> 'R) t)

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline ForInfiniteSequences (t: seq<_>, isFailure) =
        let mutable failure = None
        let buf = Seq.toArray (seq {
            use e = t.GetEnumerator ()
            while e.MoveNext () && failure.IsNone do
                if isFailure e.Current then failure <- Some e.Current
                else yield e.Current })
        let buf = match failure with None -> buf | Some e -> [|e|]
        let cons x y = Array.append [|x|] y
        let cons_f x ys = Map.Invoke cons x <*> ys
        let r = Array.foldBack cons_f buf (result [||])
        Map.Invoke Array.toSeq r
    

type Traverse =
    inherit Default1
    static member inline InvokeOnInstance f (t: ^a) = (^a : (static member Traverse : _*_ -> 'R) t, f)

    static member inline Traverse (t: ^a   , f, [<Optional>]_output: 'R, [<Optional>]_impl: Default4) = Map.Invoke f (Sequence.InvokeOnInstance t) : 'R
    static member inline Traverse (t: Id<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) = Map.Invoke Id.create (f (Id.run t))
    static member inline Traverse (t: _ seq, f, [<Optional>]_output: 'R, [<Optional>]_impl: Default3) =
       let cons x y = seq {yield x; yield! y}
       let cons_f x ys = Map.Invoke (cons: 'a->seq<_>->seq<_>) (f x) <*> ys
       Seq.foldBack cons_f t (result Seq.empty)

    static member Traverse (t: 't seq, f: 't->'u option, [<Optional>]_output: option<seq<'u>>, [<Optional>]_impl: Default2) =
       let ok = ref true
       let res = Seq.toArray (seq {
           use e = t.GetEnumerator ()
           while e.MoveNext() && ok.Value do
               match f e.Current with
               | Some v -> yield v
               | None   -> ok.Value <- false})
       if ok.Value then Some (Array.toSeq res) else None

    static member Traverse (t: 't seq, f: 't->Async<'u>, [<Optional>]_output: Async<seq<'u>>, [<Optional>]_impl: Default2) : Async<seq<_>> = async {
        let! ct = Async.CancellationToken
        return seq {
            use enum = t.GetEnumerator ()
            while enum.MoveNext() do
                yield Async.RunSynchronously (f enum.Current, cancellationToken = ct) }}

    static member inline Traverse (t: ^a   , f, [<Optional>]_output: 'R, [<Optional>]_impl: Default1) = Traverse.InvokeOnInstance f t : 'R
    static member inline Traverse (_: ^a when ^a : null and ^a :struct, _, _: 'R   , _impl: Default1) = id

    static member        Traverse (t: Id<'t>   , f: 't->option<'u>, [<Optional>]_output: option<Id<'u>>, [<Optional>]_impl: Traverse) = Option.map Id.create (f (Id.run t))
    static member inline Traverse (t: option<_>, f, [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R = match t with Some x -> Map.Invoke Some (f x) | _ -> result None

    static member inline Traverse (t:list<_>   ,f , [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
       let cons_f x ys = Map.Invoke List.cons (f x) <*> ys
       List.foldBack cons_f t (result [])

    static member inline Traverse (t:_ []      ,f , [<Optional>]_output: 'R, [<Optional>]_impl: Traverse) : 'R =
       let cons x y = Array.append [|x|] y
       let cons_f x ys = Map.Invoke cons (f x) <*> ys
       Array.foldBack cons_f t (result [||])

    static member inline Invoke f t =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f) = ((^a or ^b or ^c) : (static member Traverse : _*_*_*_ -> _) b, f, c, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, Unchecked.defaultof<'R>, f) : 'R
        call (Unchecked.defaultof<Traverse>, t, f)
    

type Sequence with

    static member inline Sequence (t:_ seq         , [<Optional>]_output: 'R, [<Optional>]_impl:Default4 ) : 'R =
                        let cons x y = seq {yield x; yield! y}
                        let cons_f x ys = Map.Invoke (cons: 'a->seq<_>->seq<_>) x <*> ys
                        Seq.foldBack cons_f t (result Seq.empty)

    static member        Sequence (t: seq<option<'t>>   , [<Optional>]_output: option<seq<'t>>    , [<Optional>]_impl: Default3) = Sequence.ForInfiniteSequences(t, Option.isNone)                                : option<seq<'t>>
    static member        Sequence (t: seq<Result<'t,'e>>, [<Optional>]_output: Result<seq<'t>, 'e>, [<Optional>]_impl: Default3) = Sequence.ForInfiniteSequences(t, function (Error _     ) -> true | _ -> false) : Result<seq<'t>, 'e>
    static member        Sequence (t: seq<Choice<'t,'e>>, [<Optional>]_output: Choice<seq<'t>, 'e>, [<Optional>]_impl: Default3) = Sequence.ForInfiniteSequences(t, function (Choice2Of2 _) -> true | _ -> false) : Choice<seq<'t>, 'e>
    static member        Sequence (t: seq<list<'t>>     , [<Optional>]_output: list<seq<'t>>      , [<Optional>]_impl: Default3) = Sequence.ForInfiniteSequences(t, List.isEmpty)                                 : list<seq<'t>>
    static member        Sequence (t: seq<'t []>        , [<Optional>]_output: seq<'t> []         , [<Optional>]_impl: Default3) = Sequence.ForInfiniteSequences(t, Array.isEmpty)                                : seq<'t> []
    static member        Sequence (t: seq<Async<'t>>    , [<Optional>]_output: Async<seq<'t>>     , [<Optional>]_impl: Default3) = Async.Sequence t                                                               : Async<seq<'t>>

    static member inline Sequence (t: ^a                , [<Optional>]_output: 'R                 , [<Optional>]_impl: Default2) = Traverse.InvokeOnInstance id t                                                        : 'R
    static member inline Sequence (t: ^a                , [<Optional>]_output: 'R                 , [<Optional>]_impl: Default1) = Sequence.InvokeOnInstance t                                                           : 'R
    static member inline Sequence (t: option<_>         , [<Optional>]_output: 'R                 , [<Optional>]_impl: Sequence) = match t with Some x -> Map.Invoke Some x | _ -> result None                           : 'R
    static member inline Sequence (t: list<_>           , [<Optional>]_output: 'R                 , [<Optional>]_impl: Sequence) = let cons_f x ys = Map.Invoke List.cons x <*> ys in List.foldBack cons_f t (result []) : 'R
    static member inline Sequence (t: _ []              , [<Optional>]_output: 'R                 , [<Optional>]_impl: Sequence) = let cons x y = Array.append [|x|] y in let cons_f x ys = Map.Invoke cons x <*> ys in Array.foldBack cons_f t (result [||]) : 'R
    static member inline Sequence (t: Id<_>             , [<Optional>]_output: 'R                 , [<Optional>]_impl: Sequence) = Traverse.Invoke id t                                                                  : 'R
    static member inline Sequence (t: _ ResizeArray     , [<Optional>]_output: 'R                 , [<Optional>]_impl: Sequence) = Traverse.Invoke id t                                                                  : 'R

    static member inline Invoke (t: '``Traversable<'Applicative<'T>>``) : '``Applicative<'Traversable<'T>>`` =
        let inline call_3 (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member Sequence : _*_*_ -> _) b, c, a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'R>) : 'R
        call (Unchecked.defaultof<Sequence>, t)
