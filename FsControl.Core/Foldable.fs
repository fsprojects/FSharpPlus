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
   
    
    type FoldMapDefault() =
        static member inline instance (_:FoldMapDefault, x:#obj, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x

    type FoldMap() =
        inherit FoldMapDefault()
        static member inline instance (_:FoldMap, x:option<_>, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (_:FoldMap, x:List<_>  , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (_:FoldMap, x:Set<_>   , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (_:FoldMap, x:array<_> , _) = fun f -> Array.foldBack (mappend << f) x (mempty())

    let FoldMap = FoldMap()

    type DefaultImpl with
        static member inline FoldrFromFoldMap f z x = 
            let inline foldMap f x = Inline.instance (FoldMap, x) f
            appEndo (foldMap (Endo << f ) x) z

        static member inline FoldlFromFoldMap f z t = 
            let inline foldMap f x = Inline.instance (FoldMap, x) f
            appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z


    type Foldr with
        static member inline instance (Foldr, x:array<_>, _) = fun (f,z) -> DefaultImpl.FoldrFromFoldMap f z x

    let inline internal foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldr, x) (f,z)


    type FoldlDefault() =
        static member inline instance (_:FoldlDefault, x:#obj , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x

    type Foldl() =
        inherit FoldlDefault()
        static member instance (_:Foldl, x:option<_>, _) = fun (f,z) -> match x with Some t ->       f z t | _ -> z
        static member instance (_:Foldl, x:List<_>  , _) = fun (f,z) -> List.fold                    f z x
        static member instance (_:Foldl, x:Set<_>   , _) = fun (f,z) -> Set.fold                     f z x
        static member instance (_:Foldl, x:array<_> , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x

    let Foldl = Foldl()
