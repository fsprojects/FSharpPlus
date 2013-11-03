namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Foldable

module Traversable = 
    type Traverse = Traverse with
        static member inline instance (Traverse, t:option<_>, _) = fun f -> match t with Some x -> fmap Some (f x) | _ -> pure' None    
        static member inline instance (Traverse, t:List<_>  , _) = fun f ->            
            let cons_f x ys = fmap cons (f x) <*> ys
            foldr cons_f (pure' []) t