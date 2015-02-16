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
        static member val Instance = Foldr()
        static member inline Foldr (_:Typ1, x:'F, _:'b) =
            fun (f:'a->'b->'b,z:'b) -> ((^F) : (static member FoldBack: _ -> ^F -> _-> ^b) (f, x, z))

        static member Foldr (_:Foldr, x:option<_>    , _) = fun (f,z) -> match x with Some t -> f t z | _ -> z
        static member Foldr (_:Foldr, x:list<_>      , _) = fun (f,z) -> List.foldBack          f x z
        static member Foldr (_:Foldr, x:Set<_>       , _) = fun (f,z) -> Set.foldBack           f x z
        static member Foldr (_:Foldr, x:string       , _) = fun (f,z) -> Array.foldBack f (x.ToCharArray()) z
        static member Foldr (_:Foldr, x:StringBuilder, _) = fun (f,z) -> Array.foldBack f (x.ToString().ToCharArray()) z
        static member Foldr (_:Foldr, x:seq<_>       , _) = fun (f,z) -> List.foldBack  f (Seq.toList x) z
        static member Foldr (_:Foldr, x:Id<'a>       , _) = fun (f,z) -> f (Id.run x) z

    type DefaultImpl =
        static member inline FoldMapFromFoldr f x = 
            let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Foldr: _*_*_ -> _) a, b, c)
            let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
            instance (Foldr.Instance, x) (mappend << f, mempty())   
    
    type FoldMapDefault() =
        static member inline FoldMap (_:FoldMapDefault, x:#obj, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x

    type FoldMap() =
        inherit FoldMapDefault()
        static member val Instance = FoldMap()
        static member inline FoldMap (_:FoldMap, x:option<_>, _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline FoldMap (_:FoldMap, x:Id<_>    , _) = fun f -> mappend (f x.getValue) (mempty())
        static member inline FoldMap (_:FoldMap, x:seq<_>   , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline FoldMap (_:FoldMap, x:list<_>  , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline FoldMap (_:FoldMap, x:Set<_>   , _) = fun f -> DefaultImpl.FoldMapFromFoldr f x
        static member inline FoldMap (_:FoldMap, x:array<_> , _) = fun f -> Array.foldBack (mappend << f) x (mempty())

    type DefaultImpl with
        static member inline FoldrFromFoldMap f z x = 
            let inline foldMap f x = 
                let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member FoldMap: _*_*_ -> _) a, b, c)
                let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
                instance (FoldMap.Instance, x) f
            appEndo (foldMap (Endo << f ) x) z

        static member inline FoldlFromFoldMap f z t = 
            let inline foldMap f x = 
                let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member FoldMap: _*_*_ -> _) a, b, c)
                let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
                instance (FoldMap.Instance, x) f
            appEndo (getDual (foldMap (Dual << Endo << flip f) t)) z


    type Foldr with
        static member inline Foldr (_:Foldr, x:array<_>, _) = fun (f,z) -> DefaultImpl.FoldrFromFoldMap f z x

    let inline internal foldr (f: 'T -> 'State -> 'State) (z:'State) x :'State =
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Foldr: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (Foldr.Instance, x) (f,z)


    type Foldl() =
        inherit Typ1()
        static member val Instance = Foldl()
        static member inline Foldl (_:Typ1, x    , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x
        static member Foldl (_:Foldl, x:option<_>, _) = fun (f,z) -> match x with Some t ->       f z t | _ -> z
        static member Foldl (_:Foldl, x:Id<_>    , _) = fun (f,z) -> f z x.getValue
        static member Foldl (_:Foldl, x:seq<_>   , _) = fun (f,z) -> Seq.fold                     f z x
        static member Foldl (_:Foldl, x:list<_>  , _) = fun (f,z) -> List.fold                    f z x
        static member Foldl (_:Foldl, x:Set<_>   , _) = fun (f,z) -> Set.fold                     f z x
        static member Foldl (_:Foldl, x:array<_> , _) = fun (f,z) -> DefaultImpl.FoldlFromFoldMap f z x


    type ToList() =
        inherit Typ1()
        static member val Instance = ToList()
        static member inline ToList (_:Typ1, x , _) = fun () -> foldr List.cons [] x
        static member ToList (_:ToList, x:seq<'a>       , _) = fun () -> Seq.toList x
        static member ToList (_:ToList, x:Set<'a>       , _) = fun () -> Set.toList x
        static member ToList (_:ToList, x:string        , _) = fun () -> x.ToCharArray() |> Array.toList
        static member ToList (_:ToList, x:StringBuilder , _) = fun () -> x.ToString().ToCharArray() |> Array.toList
        static member ToList (_:ToList, x:'a []         , _) = fun () -> Array.toList x
        static member ToList (_:ToList, x:'a ResizeArray, _) = fun () -> Seq.toList x
        static member ToList (_:ToList, x:list<'a>      , _) = fun () -> x

    let inline toList  value :'t list = 
        let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToList: _*_*_ -> _) a, b, c)
        let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
        instance (ToList.Instance , value) ()

 
    type ToArray() =
        inherit Typ1()
        static member val Instance = ToArray()
        static member inline ToArray (_:Typ1, x , _) = fun () -> foldr (fun x y -> Array.concat [[|x|];y]) [||] x
        static member ToArray (_:ToArray, x:seq<'a>       , _) = fun () -> Seq.toArray x
        static member ToArray (_:ToArray, x:Set<'a>       , _) = fun () -> Set.toArray x
        static member ToArray (_:ToArray, x:string        , _) = fun () -> x.ToCharArray()
        static member ToArray (_:ToArray, x:StringBuilder , _) = fun () -> x.ToString().ToCharArray()
        static member ToArray (_:ToArray, x:'a []         , _) = fun () -> x
        static member ToArray (_:ToArray, x:'a ResizeArray, _) = fun () -> Seq.toArray x
        static member ToArray (_:ToArray, x:list<'a>      , _) = fun () -> List.toArray x
    
 
    type Exists() =
        inherit Typ1()
        static member val Instance = Exists()
        static member inline Exists (_:Typ1, x     , _:bool) = fun f -> foldr (fun x b -> f x || b) false x :bool
        static member Exists (_:Exists, x:Id<'T>   , _:bool) = fun f -> f x.getValue :bool
        static member Exists (_:Exists, x:seq<'a>       , _) = fun f -> Seq.exists    f x
        static member Exists (_:Exists, x:list<'a>      , _) = fun f -> List.exists   f x
        static member Exists (_:Exists, x:'a []         , _) = fun f -> Array.exists  f x
        static member Exists (_:Exists, x:Set<'a>       , _) = fun f -> Set.exists    f x
        static member Exists (_:Exists, x:string        , _) = fun f -> String.exists f x
        static member Exists (_:Exists, x:'a ResizeArray, _) = fun f -> Seq.exists    f x
        static member Exists (_:Exists, x:StringBuilder , _) = fun f -> x.ToString() |> String.exists f

 
    open Functor

    type Find() =
        inherit Typ1()
        static member val Instance = Find()
        static member inline Find (_:Typ1, x   , _:'T) = fun f ->
            let r = foldr (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
            (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
        static member Find (_:Find, x:Id<'T>   , _:'T) = fun f -> List.find  f [x.getValue]
        static member Find (_:Find, x:seq<'T>  , _:'T) = fun f -> Seq.find   f x
        static member Find (_:Find, x:list<'T> , _:'T) = fun f -> List.find  f x
        static member Find (_:Find, x:'T []    , _:'T) = fun f -> Array.find f x


    type TryFind() =
        inherit Typ1()
        static member val Instance = TryFind()
        static member inline TryFind (_:Typ1, x   , _:'T option) = fun f ->
            foldr (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
        static member TryFind (_:TryFind, x:Id<'T>  , _:'T option) = fun f -> List.tryFind  f [x.getValue]
        static member TryFind (_:TryFind, x:seq<'T> , _:'T option) = fun f -> Seq.tryFind   f x
        static member TryFind (_:TryFind, x:list<'T>, _:'T option) = fun f -> List.tryFind  f x
        static member TryFind (_:TryFind, x:'T []   , _:'T option) = fun f -> Array.tryFind f x


    type Pick() =
        inherit Typ1()
        static member val Instance = Pick()
        static member inline Pick (_:Typ1, x  , _:'U) = fun (f:_->'U option) ->
            let r = foldr (fun x b -> f x <|> b) None x
            (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
        static member Pick (_:Pick, x:Id<'T>  , _:'U) = fun (f:_->'U option) -> List.pick  f [x.getValue]
        static member Pick (_:Pick, x:seq<'T> , _:'U) = fun (f:_->'U option) -> Seq.pick   f x
        static member Pick (_:Pick, x:list<'T>, _:'U) = fun (f:_->'U option) -> List.pick  f x
        static member Pick (_:Pick, x:'T []   , _:'U) = fun (f:_->'U option) -> Array.pick f x


    type TryPick() =
        inherit Typ1()
        static member val Instance = TryPick()
        static member inline TryPick (_:Typ1, x     , _:'U option) = fun (f:_->'U option) -> foldr (fun x b -> f x <|> b) None x  :'U option
        static member TryPick (_:TryPick, x:Id<'T>  , _:'U option) = fun (f:_->'U option) -> invalidOp "TryPick on ID" :'U option
        static member TryPick (_:TryPick, x:seq<'T> , _:'U option) = fun (f:_->'U option) -> Seq.tryPick   f x
        static member TryPick (_:TryPick, x:list<'T>, _:'U option) = fun (f:_->'U option) -> List.tryPick  f x
        static member TryPick (_:TryPick, x:'T []   , _:'U option) = fun (f:_->'U option) -> Array.tryPick f x


    type FilterDefault() =
        static member inline Filter (_:FilterDefault, x:'t when 't :> obj, _:'t) = fun p ->
            let m:'t = mempty()
            let inline instance_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Foldr: _*_*_ -> _) a, b, c)
            let inline instance (a:'a, b:'b) = fun (x:'x) -> instance_3 (a, b, Unchecked.defaultof<'r>) x :'r
            instance (Foldr.Instance, x) (mappend << (fun a -> if p a then result a else m), m) :'t
   
    type Filter() =
        inherit FilterDefault()
        static member val Instance = Filter()
        static member Filter (_:Filter, x:'t seq, _:'t seq) = fun p -> Seq.filter p x
        static member Filter (_:Filter, x:'t Set, _:'t Set) = fun p -> Set.filter p x
        static member Filter (_:Filter, x:'t option, _:'t option) = fun p -> match x with None -> None | Some a -> if p a then x else None
        static member Filter (_:Filter, x:'t list, _:'t list) = fun p -> List.filter  p x
        static member Filter (_:Filter, x:'t []  , _:'t []  ) = fun p -> Array.filter p x
        static member Filter (_:Filter, x:'t IObservable, _:'t IObservable) = fun p -> Observable.filter p x
        static member Filter (_:Filter, x:'t ResizeArray, _:'t ResizeArray) = fun p -> ResizeArray(Seq.filter p x)