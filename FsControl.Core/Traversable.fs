namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Monad


type Traverse() =
    inherit Default1()
    static member val Instance = Traverse()

    static member inline Traverse (_:Default1, t:Id<_>, _) = fun f -> Map.Invoke Id.create (f (Id.run t))
    static member inline Traverse (_:Default1, t:_ seq, _) = fun f ->
        let cons x y = Seq.append (Seq.singleton x) y            
        let cons_f x ys = Map.Invoke cons (f x) <*> ys
        Foldr.Invoke cons_f (result (Seq.empty)) t

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
  
    static member inline Traverse (_:Traverse, t:option<_>, _) = fun f -> match t with Some x -> Map.Invoke Some (f x) | _ -> result None        

    static member inline Traverse (_:Traverse, t:list<_>  , _) = fun f ->            
        let cons_f x ys = Map.Invoke List.cons (f x) <*> ys
        Foldr.Invoke cons_f (result []) t

    static member inline Traverse (_:Traverse, t:_ []  , _) = fun f ->
        let cons x y = Array.append [|x|] y            
        let cons_f x ys = Map.Invoke cons (f x) <*> ys
        Foldr.Invoke cons_f (result [||]) t

    static member inline internal Invoke f t =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Traverse: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Traverse.Instance, t) f
    

type SequenceA() =
    inherit Default1()
    static member val Instance = SequenceA()
    static member inline SequenceA (_:Default1 , t          , _) = Traverse.Invoke id t
    static member inline SequenceA (_:SequenceA, t:option<_>, _) = match t with Some x -> Map.Invoke Some x | _ -> result None       
    static member inline SequenceA (_:SequenceA, t:list<_>  , _) = let cons_f x ys = Map.Invoke List.cons x <*> ys in Foldr.Invoke cons_f (result []) t
    static member inline SequenceA (_:SequenceA, t:seq<_>   , _) = Traverse.Invoke id t
    static member inline SequenceA (_:SequenceA, t:Id<_>    , _) = Traverse.Invoke id t

    static member inline Invoke (t:'Traversable'Applicative'T) :'Applicative'Traversable'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SequenceA: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = call_3 (a, b, Unchecked.defaultof<'r>) :'r
        call (SequenceA.Instance, t)