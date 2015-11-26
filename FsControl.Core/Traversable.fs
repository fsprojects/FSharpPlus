namespace FsControl

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Internals
open FsControl.Internals.Prelude
open FsControl.Internals.MonadOps


[<Extension;Sealed>]
type Traverse =
    inherit Default1
    [<Extension>]static member inline Traverse (t:^a   , f, [<Optional>]output:'R, [<Optional>]impl:Default3 ) =  Map.Invoke f ((^a) : (static member SequenceA: _ -> 'R) t)
    [<Extension>]static member inline Traverse (t:^a   , f, [<Optional>]output:'R, [<Optional>]impl:Default2 ) =  ((^a) : (static member Traverse: _*_ -> 'R) t, f)
    [<Extension>]static member inline Traverse (t:Id<_>, f, [<Optional>]output:'R, [<Optional>]impl:Default1) = Map.Invoke Id.create (f (Id.run t))
    [<Extension>]static member inline Traverse (t:_ seq, f, [<Optional>]output:'R, [<Optional>]impl:Default1) = 
                    let cons x y = seq {yield x; yield! y}
                    let cons_f x ys = Map.Invoke (cons:'a->seq<_>->seq<_>) (f x) <*> ys
                    Seq.foldBack cons_f t (result (Seq.empty))

    [<Extension>]static member Traverse (t:'t seq ,f:'t->'u option , [<Optional>]output:option<seq<'t>>, [<Optional>]impl:Traverse) =
                    let ok = ref true
                    let res = Seq.toArray (seq {
                        use e = t.GetEnumerator()
                        while (e.MoveNext() && ok.Value) do
                            match f e.Current with
                            | Some v -> yield v
                            | None   -> ok.Value <- false})
                    if ok.Value then Some (Array.toSeq res) else None

    [<Extension>]static member        Traverse (t:'t seq   ,f:'t->Async<'u> , [<Optional>]output:Async<seq<'u>>, [<Optional>]impl:Traverse) :Async<seq<_>> = result <| Seq.map (Async.RunSynchronously) (Seq.map f t)
    [<Extension>]static member        Traverse (t:Id<'t>   ,f:'t->option<'u>    , [<Optional>]output:option<Id<'u>>, [<Optional>]impl:Traverse) = Option.map Id.create (f (Id.run t))
    [<Extension>]static member inline Traverse (t:option<_>,f , [<Optional>]output:'R           , [<Optional>]impl:Traverse) :'R = match t with Some x -> Map.Invoke Some (f x) | _ -> result None        

    [<Extension>]static member inline Traverse (t:list<_>  ,f , [<Optional>]output:'R           , [<Optional>]impl:Traverse) :'R =         
                    let cons_f x ys = Map.Invoke List.cons (f x) <*> ys
                    List.foldBack cons_f t (result [])

    [<Extension>]static member inline Traverse (t:_ []  ,f , [<Optional>]output :'R             , [<Optional>]impl:Traverse) :'R =
                    let cons x y = Array.append [|x|] y            
                    let cons_f x ys = Map.Invoke cons (f x) <*> ys
                    Array.foldBack cons_f t (result [||])

    static member inline Invoke f t =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Traverse: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'R>, f) :'R
        call (Unchecked.defaultof<Traverse>, t, f)
    

[<Extension;Sealed>]
type SequenceA =
    inherit Default1
    [<Extension>]static member inline SequenceA (t:^a            , [<Optional>]output:'R, [<Optional>]impl:Default2 ) =  ((^a) : (static member Traverse: _*_ -> 'R) t, id)                                     :'R
    [<Extension>]static member inline SequenceA (t:^a            , [<Optional>]output:'R, [<Optional>]impl:Default1 ) =  ((^a) : (static member SequenceA: _ -> 'R) t)                                          :'R
    [<Extension>]static member inline SequenceA (t:option<_>     , [<Optional>]output:'R, [<Optional>]impl:SequenceA) = match t with Some x -> Map.Invoke Some x | _ -> result None                             :'R
    [<Extension>]static member inline SequenceA (t:list<_>       , [<Optional>]output:'R, [<Optional>]impl:SequenceA) = let cons_f x ys = Map.Invoke List.cons x <*> ys in List.foldBack cons_f t (result [])   :'R
    [<Extension>]static member inline SequenceA (t:_ []          , [<Optional>]output:'R, [<Optional>]impl:SequenceA) = let cons x y = Array.append [|x|] y in let cons_f x ys = Map.Invoke cons x <*> ys in Array.foldBack cons_f t (result [||]) :'R
    [<Extension>]static member inline SequenceA (t:Id<_>         , [<Optional>]output:'R, [<Optional>]impl:SequenceA) = Traverse.Invoke id t                                                                    :'R
    [<Extension>]static member inline SequenceA (t: _ ResizeArray, [<Optional>]output:'R, [<Optional>]impl:SequenceA) = Traverse.Invoke id t                                                                    :'R
    [<Extension>]static member inline SequenceA (t:_ seq         , [<Optional>]output:'R, [<Optional>]impl:SequenceA) :'R =                                                                                         
                        let cons x y = seq {yield x; yield! y}
                        let cons_f x ys = Map.Invoke (cons:'a->seq<_>->seq<_>) x <*> ys
                        Seq.foldBack cons_f t (result Seq.empty)

    static member inline Invoke (t:'Traversable'Applicative'T) :'Applicative'Traversable'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SequenceA: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'R>) :'R
        call (Unchecked.defaultof<SequenceA>, t)