namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open FsControl.Core.Types
open Monoid
open Dual
open Endo

module Foldable =
    type Foldr = Foldr with
        static member instance (Foldr, x:option<_>, _) = fun (f,z) -> match x with Some t -> f t z | _ -> z
        static member instance (Foldr, x:List<_>  , _) = fun (f,z) -> List.foldBack          f x z
        static member instance (Foldr, x:Set<_>   , _) = fun (f,z) -> Set.foldBack           f x z

    type DefaultImpl =
        static member inline FoldMapFromFoldr f x = Inline.instance (Foldr, x) (mappend << f, mempty())
   
    type FoldMap = FoldMap with
        static member inline instance (FoldMap, x:option<_>, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (FoldMap, x:List<_>  , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (FoldMap, x:Set<_>   , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (FoldMap, x:array<_> , _) = fun f -> Array.foldBack (mappend << f) x (mempty())

    type DefaultImpl with
        static member inline FoldrFromFoldMap f z x = 
            let inline foldMap f x = Inline.instance (FoldMap, x) f
            appEndo (foldMap (Endo << f ) x) z

        static member inline FoldlFromFoldMap f z t = 
            let inline foldMap f x = Inline.instance (FoldMap, x) f
            appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z

    type Foldr with
        static member inline instance (Foldr, x:array<_>, _) = fun (f,z) -> DefaultImpl.FoldrFromFoldMap f z x

    type Foldl = Foldl with
        static member instance (Foldl, x:option<_>, _) = fun (f,z) -> match x with Some t ->       f z t | _ -> z
        static member instance (Foldl, x:List<_>  , _) = fun (f,z) -> List.fold                    f z x
        static member instance (Foldl, x:Set<_>   , _) = fun (f,z) -> Set.fold                     f z x
        static member instance (Foldl, x:array<_> , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x

    let inline internal foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldr, x) (f,z)