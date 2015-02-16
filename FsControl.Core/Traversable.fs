namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Foldable

module Traversable =

    type TraverseDefault() =

        static member inline Traverse (_:TraverseDefault, t:Id<_>    , _) = fun f -> fmap Id.create (f (Id.run t))

        static member inline Traverse (_:TraverseDefault, t:_ seq , _) = fun f ->
            let cons x y = Seq.append (Seq.singleton x) y            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (result (Seq.empty)) t

    type Traverse() =
        inherit TraverseDefault()
        static member val Instance = Traverse()

        static member Traverse (_:Traverse, t:_ seq , _:option<seq<_>>) = fun f ->
            let ok = ref true
            let res = Seq.toArray (seq {
                    use e = t.GetEnumerator()
                    while (e.MoveNext() && ok.Value) do
                        match f e.Current with
                        | Some v -> yield v
                        | None   -> ok.Value <- false})
            if ok.Value then Some (Array.toSeq res) else None

        static member Traverse (_:Traverse, t:Id<_>    , _:option<Id<_>>) = fun f -> Option.map Id.create (f (Id.run t))
  
        static member inline Traverse (_:Traverse, t:option<_>, _) = fun f -> match t with Some x -> fmap Some (f x) | _ -> result None        

        static member inline Traverse (_:Traverse, t:list<_>  , _) = fun f ->            
            let cons_f x ys = fmap List.cons (f x) <*> ys
            foldr cons_f (result []) t

        static member inline Traverse (_:Traverse, t:_ []  , _) = fun f ->
            let cons x y = Array.append [|x|] y            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (result [||]) t

    let inline internal traverse f t =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Traverse: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (Traverse.Instance, t) f
    

    type SequenceADefault() =
        static member inline SequenceA (_:SequenceADefault, t:#obj, _) = fun () -> traverse id t

    type SequenceA() =
        inherit SequenceADefault()
        static member val Instance = SequenceA()

        static member inline SequenceA (_:SequenceA, t:option<_>, _) = fun () -> match t with Some x -> fmap Some x | _ -> result None
        
        static member inline SequenceA (_:SequenceA, t:list<_>  , _) = fun () ->            
            let cons_f x ys = fmap List.cons x <*> ys
            foldr cons_f (result []) t

        static member inline SequenceA (_:SequenceA, t:seq<_>  , _) = fun () -> traverse id t
        static member inline SequenceA (_:SequenceA, t:Id<_>   , _) = fun () -> traverse id t

    let inline sequenceA (t:'Traversable'Applicative'T) :'Applicative'Traversable'T =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SequenceA: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (SequenceA.Instance, t) ()