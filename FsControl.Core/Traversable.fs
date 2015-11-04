namespace FsControl

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps


[<Extension;Sealed>]
type Traverse =
    inherit Default1

    [<Extension>]static member inline Traverse (t:Id<_>, f, _, [<Optional>]impl:Default1) = Map.Invoke Id.create (f (Id.run t))
    [<Extension>]static member inline Traverse (t:_ seq, f, _, [<Optional>]impl:Default1) = 
                    let cons x y = seq {yield x; yield! y}
                    let cons_f x ys = Map.Invoke cons (f x) <*> ys
                    FoldBack.Invoke cons_f (result (Seq.empty)) t

    [<Extension>]static member Traverse (t:_ seq ,f , _:option<seq<_>>, [<Optional>]impl:Traverse) =
                    let ok = ref true
                    let res = Seq.toArray (seq {
                        use e = t.GetEnumerator()
                        while (e.MoveNext() && ok.Value) do
                            match f e.Current with
                            | Some v -> yield v
                            | None   -> ok.Value <- false})
                    if ok.Value then Some (Array.toSeq res) else None

    [<Extension>]static member        Traverse (t:_ seq    ,f , [<Optional>]output:Async<seq<_>>, [<Optional>]impl:Traverse) :Async<seq<_>> = result <| Seq.map (Async.RunSynchronously) (Seq.map f t)
    [<Extension>]static member        Traverse (t:Id<_>    ,f , [<Optional>]output:option<Id<_>>, [<Optional>]impl:Traverse) = Option.map Id.create (f (Id.run t))
    [<Extension>]static member inline Traverse (t:option<_>,f , [<Optional>]output              , [<Optional>]impl:Traverse) = match t with Some x -> Map.Invoke Some (f x) | _ -> result None        

    [<Extension>]static member inline Traverse (t:list<_>  ,f , [<Optional>]output              , [<Optional>]impl:Traverse) =         
                    let cons_f x ys = Map.Invoke List.cons (f x) <*> ys
                    List.foldBack cons_f t (result [])

    [<Extension>]static member inline Traverse (t:_ []  ,f , [<Optional>]output                 , [<Optional>]impl:Traverse) =
                    let cons x y = Array.append [|x|] y            
                    let cons_f x ys = Map.Invoke cons (f x) <*> ys
                    Array.foldBack cons_f t (result [||])

    static member inline Invoke f t =
        let inline call_3 (a:^a, b:^b, c:^c, f) = ((^a or ^b or ^c) : (static member Traverse: _*_*_*_ -> _) b, f, c, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, Unchecked.defaultof<'r>, f) :'r
        call (Unchecked.defaultof<Traverse>, t, f)
    

[<Extension;Sealed>]
type SequenceA =
    inherit Default1
    [<Extension>]static member inline SequenceA (t               , [<Optional>]output, [<Optional>]impl:Default1 ) = Traverse.Invoke id t
    [<Extension>]static member inline SequenceA (t:option<_>     , [<Optional>]output, [<Optional>]impl:SequenceA) = match t with Some x -> Map.Invoke Some x | _ -> result None       
    [<Extension>]static member inline SequenceA (t:list<_>       , [<Optional>]output, [<Optional>]impl:SequenceA) = let cons_f x ys = Map.Invoke List.cons x <*> ys in List.foldBack cons_f t (result [])
    [<Extension>]static member inline SequenceA (t:Id<_>         , [<Optional>]output, [<Optional>]impl:SequenceA) = Traverse.Invoke id t
    [<Extension>]static member inline SequenceA (t: _ ResizeArray, [<Optional>]output, [<Optional>]impl:SequenceA) = Traverse.Invoke id t

    static member inline Invoke (t:'Traversable'Applicative'T) :'Applicative'Traversable'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SequenceA: _*_*_ -> _) b, c, a)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (Unchecked.defaultof<SequenceA>, t)
