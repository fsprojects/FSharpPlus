namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Foldable

module Traversable =

    type Traverse = Traverse with
    
        static member inline instance (Traverse, t:Id<_>    , _) = fun f -> fmap Id.create (f (Id.run t))
        static member inline instance (Traverse, t:option<_>, _) = fun f -> match t with Some x -> fmap Some (f x) | _ -> pure' None        

        static member inline instance (Traverse, t:List<_>  , _) = fun f ->            
            let cons_f x ys = fmap List.cons (f x) <*> ys
            foldr cons_f (pure' []) t

        static member inline instance (Traverse, t:_ []  , _) = fun f ->
            let cons x y = Array.append [|x|] y            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (pure' [||]) t

        static member inline instance (Traverse, t:_ seq , _) = fun f ->
            let cons x y = Seq.append (Seq.singleton x) y            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (pure' (Seq.empty)) t
        

    let inline internal traverse f t = Inline.instance (Traverse, t) f
    

    type SequenceADefault() =
        static member inline instance (_:SequenceADefault, t:#obj, _) = fun () -> traverse id t

    type SequenceA() =
        inherit SequenceADefault()

        static member inline instance (_:SequenceA, t:option<_>, _) = fun () -> match t with Some x -> fmap Some x | _ -> pure' None
        
        static member inline instance (_:SequenceA, t:List<_>  , _) = fun () ->            
            let cons_f x ys = fmap List.cons x <*> ys
            foldr cons_f (pure' []) t

    let SequenceA = SequenceA()