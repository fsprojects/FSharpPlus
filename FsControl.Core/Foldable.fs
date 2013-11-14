namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open Monoid
open Dual
open Endo
open Applicative
open System
open System.Text

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


    type ToList() =
        static member instance (_:ToList, x:string        , _) = fun () -> x.ToCharArray() |> Array.toList
        static member instance (_:ToList, x:StringBuilder , _) = fun () -> x.ToString().ToCharArray() |> Array.toList
        static member instance (_:ToList, x:'a []         , _) = fun () -> Array.toList x
        static member instance (_:ToList, x:'a ResizeArray, _) = fun () -> Seq.toList x
        static member instance (_:ToList, x:List<'a>      , _) = fun () -> x
        static member instance (_:ToList, x:Set<'a>       , _) = fun () -> Set.toList x

    let ToList = ToList()


    type FilterDefault() =
        static member inline instance (_:FilterDefault, x:'t when 't :> obj, _:'t) = fun p ->
            let m:'t = mempty()
            Inline.instance (Foldr, x) (mappend << (fun a -> if p a then pure' a else m), m) :'t
   
    type Filter() =
        inherit FilterDefault()

        static member instance (_:Filter, x:'t list, _:'t list) = fun p -> List.filter  p x
        static member instance (_:Filter, x:'t []  , _:'t []  ) = fun p -> Array.filter p x
        static member instance (_:Filter, x:'t IObservable, _:'t IObservable) = fun p -> Observable.filter p x
        static member instance (_:Filter, x:'t ResizeArray, _:'t ResizeArray) = fun p -> ResizeArray(Seq.filter p x)

    let Filter = Filter()