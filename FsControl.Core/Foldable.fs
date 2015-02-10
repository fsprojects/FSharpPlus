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

    type Foldr() =
        inherit Typ1()
        static member inline instance (_:Typ1, x:'F, _:'b) =
            fun (f:'a->'b->'b,z:'b) -> ((^F) : (static member FoldBack: _ -> ^F -> _-> ^b) (f, x, z))

        static member instance (_:Foldr, x:option<_>    , _) = fun (f,z) -> match x with Some t -> f t z | _ -> z
        static member instance (_:Foldr, x:list<_>      , _) = fun (f,z) -> List.foldBack          f x z
        static member instance (_:Foldr, x:Set<_>       , _) = fun (f,z) -> Set.foldBack           f x z
        static member instance (_:Foldr, x:string       , _) = fun (f,z) -> Array.foldBack f (x.ToCharArray()) z
        static member instance (_:Foldr, x:StringBuilder, _) = fun (f,z) -> Array.foldBack f (x.ToString().ToCharArray()) z
        static member instance (_:Foldr, x:seq<_>       , _) = fun (f,z) -> List.foldBack  f (Seq.toList x) z
        static member instance (_:Foldr, x:Id<'a>       , _) = fun (f,z) -> f (Id.run x) z
    
    let Foldr = Foldr()

    type DefaultImpl =
        static member inline FoldMapFromFoldr f x = Inline.instance (Foldr, x) (mappend << f, mempty())
   
    
    type FoldMapDefault() =
        static member inline instance (_:FoldMapDefault, x:#obj, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x

    type FoldMap() =
        inherit FoldMapDefault()
        static member inline instance (_:FoldMap, x:option<_>, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline instance (_:FoldMap, x:list<_>  , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
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
        static member inline instance (_:Foldr, x:array<_>, _) = fun (f,z) -> DefaultImpl.FoldrFromFoldMap f z x

    let inline internal foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldr, x) (f,z)


    type FoldlDefault() =
        static member inline instance (_:FoldlDefault, x:#obj , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x

    type Foldl() =
        inherit FoldlDefault()
        static member instance (_:Foldl, x:option<_>, _) = fun (f,z) -> match x with Some t ->       f z t | _ -> z
        static member instance (_:Foldl, x:list<_>  , _) = fun (f,z) -> List.fold                    f z x
        static member instance (_:Foldl, x:Set<_>   , _) = fun (f,z) -> Set.fold                     f z x
        static member instance (_:Foldl, x:array<_> , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x

    let Foldl = Foldl()


    type ToList() =
        inherit Typ1()
        static member inline instance (_:Typ1, x , _) = fun () -> foldr List.cons [] x
        static member instance (_:ToList, x:seq<'a>       , _) = fun () -> Seq.toList x
        static member instance (_:ToList, x:Set<'a>       , _) = fun () -> Set.toList x
        static member instance (_:ToList, x:string        , _) = fun () -> x.ToCharArray() |> Array.toList
        static member instance (_:ToList, x:StringBuilder , _) = fun () -> x.ToString().ToCharArray() |> Array.toList
        static member instance (_:ToList, x:'a []         , _) = fun () -> Array.toList x
        static member instance (_:ToList, x:'a ResizeArray, _) = fun () -> Seq.toList x
        static member instance (_:ToList, x:list<'a>      , _) = fun () -> x

    let ToList = ToList()
    let inline toList  value :'t list = Inline.instance (ToList , value) ()

    type ToArray() =
        inherit Typ1()
        static member inline instance (_:Typ1, x , _) = fun () -> foldr (fun x y -> Array.concat [[|x|];y]) [||] x
        static member instance (_:ToArray, x:seq<'a>       , _) = fun () -> Seq.toArray x
        static member instance (_:ToArray, x:Set<'a>       , _) = fun () -> Set.toArray x
        static member instance (_:ToArray, x:string        , _) = fun () -> x.ToCharArray()
        static member instance (_:ToArray, x:StringBuilder , _) = fun () -> x.ToString().ToCharArray()
        static member instance (_:ToArray, x:'a []         , _) = fun () -> x
        static member instance (_:ToArray, x:'a ResizeArray, _) = fun () -> Seq.toArray x
        static member instance (_:ToArray, x:list<'a>      , _) = fun () -> List.toArray x

    let ToArray = ToArray()
    
    type Exists() =
        inherit Typ1()
        static member inline instance (_:Typ1, x     , _:bool) = fun f -> foldr (fun x b -> f x || b) false x :bool
        static member instance (_:Exists, x:Id<'T>   , _:bool) = fun f -> f x.getValue :bool
        static member instance (_:Exists, x:seq<'a>       , _) = fun f -> Seq.exists    f x
        static member instance (_:Exists, x:list<'a>      , _) = fun f -> List.exists   f x
        static member instance (_:Exists, x:'a []         , _) = fun f -> Array.exists  f x
        static member instance (_:Exists, x:Set<'a>       , _) = fun f -> Set.exists    f x
        static member instance (_:Exists, x:string        , _) = fun f -> String.exists f x
        static member instance (_:Exists, x:'a ResizeArray, _) = fun f -> Seq.exists    f x
        static member instance (_:Exists, x:StringBuilder , _) = fun f -> x.ToString() |> String.exists f
        
    let Exists = Exists()


    type Find() =
        inherit Typ1()
        static member inline instance (_:Typ1, x   , _:'T) = fun f ->
            let (<|>) a b = Functor.plus a b 
            let r = foldr (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
            (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
        static member instance (_:Find, x:Id<'T>   , _:'T) = fun f -> List.find  f [x.getValue]
        static member instance (_:Find, x:seq<'T>  , _:'T) = fun f -> Seq.find   f x
        static member instance (_:Find, x:list<'T> , _:'T) = fun f -> List.find  f x
        static member instance (_:Find, x:'T []    , _:'T) = fun f -> Array.find f x

    let Find = Find()

    type TryFind() =
        inherit Typ1()
        static member inline instance (_:Typ1, x   , _:'T option) = fun f ->
            let (<|>) a b = Functor.plus a b 
            foldr (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
        static member instance (_:TryFind, x:Id<'T>  , _:'T option) = fun f -> List.tryFind  f [x.getValue]
        static member instance (_:TryFind, x:seq<'T> , _:'T option) = fun f -> Seq.tryFind   f x
        static member instance (_:TryFind, x:list<'T>, _:'T option) = fun f -> List.tryFind  f x
        static member instance (_:TryFind, x:'T []   , _:'T option) = fun f -> Array.tryFind f x

    let TryFind = TryFind()


    type Pick() =
        inherit Typ1()
        static member inline instance (_:Typ1, x  , _:'U) = fun (f:_->'U option) ->
            let (<|>) a b = Functor.plus a b 
            let r = foldr (fun x b -> f x <|> b) None x
            (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
        static member instance (_:Pick, x:Id<'T>  , _:'U) = fun (f:_->'U option) -> List.pick  f [x.getValue]
        static member instance (_:Pick, x:seq<'T> , _:'U) = fun (f:_->'U option) -> Seq.pick   f x
        static member instance (_:Pick, x:list<'T>, _:'U) = fun (f:_->'U option) -> List.pick  f x
        static member instance (_:Pick, x:'T []   , _:'U) = fun (f:_->'U option) -> Array.pick f x

    let Pick = Pick()


    type TryPick() =
        inherit Typ1()
        static member inline instance (_:Typ1, x     , _:'U option) = fun (f:_->'U option) ->
            let (<|>) a b = Functor.plus a b 
            foldr (fun x b -> f x <|> b) None x  :'U option
        static member instance (_:TryPick, x:Id<'T>  , _:'U option) = fun (f:_->'U option) -> invalidOp "TryPick on ID" :'U option
        static member instance (_:TryPick, x:seq<'T> , _:'U option) = fun (f:_->'U option) -> Seq.tryPick   f x
        static member instance (_:TryPick, x:list<'T>, _:'U option) = fun (f:_->'U option) -> List.tryPick  f x
        static member instance (_:TryPick, x:'T []   , _:'U option) = fun (f:_->'U option) -> Array.tryPick f x

    let TryPick = TryPick()


    type FilterDefault() =
        static member inline instance (_:FilterDefault, x:'t when 't :> obj, _:'t) = fun p ->
            let m:'t = mempty()
            Inline.instance (Foldr, x) (mappend << (fun a -> if p a then pure' a else m), m) :'t
   
    type Filter() =
        inherit FilterDefault()

        static member instance (_:Filter, x:'t seq, _:'t seq) = fun p -> Seq.filter p x
        static member instance (_:Filter, x:'t Set, _:'t Set) = fun p -> Set.filter p x
        static member instance (_:Filter, x:'t option, _:'t option) = fun p -> match x with None -> None | Some a -> if p a then x else None
        static member instance (_:Filter, x:'t list, _:'t list) = fun p -> List.filter  p x
        static member instance (_:Filter, x:'t []  , _:'t []  ) = fun p -> Array.filter p x
        static member instance (_:Filter, x:'t IObservable, _:'t IObservable) = fun p -> Observable.filter p x
        static member instance (_:Filter, x:'t ResizeArray, _:'t ResizeArray) = fun p -> ResizeArray(Seq.filter p x)

    let Filter = Filter()