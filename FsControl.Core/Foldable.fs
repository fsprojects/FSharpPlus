namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open Dual
open Endo
open System
open System.Text


type Foldr() =
    inherit Default1()
    static member val Instance = Foldr()
    static member inline Foldr (x:'F           , _:Default1) = fun (f:'a->'b->'b,z:'b) -> ((^F) : (static member FoldBack: _ -> ^F -> _-> ^b) (f, x, z))
    static member        Foldr (x:option<_>    , _:Foldr   ) = fun (f,z) -> match x with Some t -> f t z | _ -> z
    static member        Foldr (x:list<_>      , _:Foldr   ) = fun (f,z) -> List.foldBack          f x z
    static member        Foldr (x:Set<_>       , _:Foldr   ) = fun (f,z) -> Set.foldBack           f x z
    static member        Foldr (x:string       , _:Foldr   ) = fun (f,z) -> Array.foldBack f (x.ToCharArray()) z
    static member        Foldr (x:StringBuilder, _:Foldr   ) = fun (f,z) -> Array.foldBack f (x.ToString().ToCharArray()) z
    static member        Foldr (x:seq<_>       , _:Foldr   ) = fun (f,z) -> List.foldBack  f (Seq.toList x) z
    static member        Foldr (x:Id<'a>       , _:Foldr   ) = fun (f,z) -> f (Id.run x) z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Foldr: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Foldr.Instance, foldable) (folder, state)

type FoldMap() =
    inherit Default1()
    static member val Instance = FoldMap()
    static member inline FromFoldr f x = Foldr.Invoke (Mappend.Invoke << f) (Mempty.Invoke()) x  
    
    static member inline FoldMap (x          , _:Default1) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (x:option<_>, _:FoldMap ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (x:Id<_>    , _:FoldMap ) = fun f -> Mappend.Invoke (f x.getValue) (Mempty.Invoke())
    static member inline FoldMap (x:seq<_>   , _:FoldMap ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (x:list<_>  , _:FoldMap ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (x:Set<_>   , _:FoldMap ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (x:array<_> , _:FoldMap ) = fun f -> Array.foldBack (Mappend.Invoke << f) x (Mempty.Invoke())

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FoldMap: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (FoldMap.Instance, x) f


type Foldr with
    static member inline FromFoldMap f z x = appEndo (FoldMap.Invoke (Endo << f ) x) z
    static member inline Foldr (x:array<_>, _:Foldr) = fun (f,z) -> Foldr.FromFoldMap f z x


type Foldl() =
    inherit Default1()
    static member val Instance = Foldl()

    static member inline FromFoldMap f z t = appEndo (getDual (FoldMap.Invoke (Dual << Endo << flip f) t)) z

    static member inline Foldl (x          , _:Default1) = fun (f,z) -> Foldl.FromFoldMap      f z x
    static member        Foldl (x:option<_>, _:Foldl   ) = fun (f,z) -> match x with Some t -> f z t | _ -> z
    static member        Foldl (x:Id<_>    , _:Foldl   ) = fun (f,z) -> f z x.getValue
    static member        Foldl (x:seq<_>   , _:Foldl   ) = fun (f,z) -> Seq.fold               f z x
    static member        Foldl (x:list<_>  , _:Foldl   ) = fun (f,z) -> List.fold              f z x
    static member        Foldl (x:Set<_>   , _:Foldl   ) = fun (f,z) -> Set.fold               f z x
    static member        Foldl (x:array<_> , _:Foldl   ) = fun (f,z) -> Foldl.FromFoldMap      f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Foldl: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Foldl.Instance, foldable) (folder, state)


type ToList() =
    inherit Default1()
    static member val Instance = ToList()
    static member inline ToList (x               , _:Default1) = Foldr.Invoke List.cons [] x
    static member        ToList (x:seq<'a>       , _:ToList  ) = Seq.toList x
    static member        ToList (x:Set<'a>       , _:ToList  ) = Set.toList x
    static member        ToList (x:string        , _:ToList  ) = x.ToCharArray() |> Array.toList
    static member        ToList (x:StringBuilder , _:ToList  ) = x.ToString().ToCharArray() |> Array.toList
    static member        ToList (x:'a []         , _:ToList  ) = Array.toList x
    static member        ToList (x:'a ResizeArray, _:ToList  ) = Seq.toList x
    static member        ToList (x:list<'a>      , _:ToList  ) = x

    static member inline Invoke  value :'t list = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToList: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToList.Instance , value)

 
type ToArray() =
    inherit Default1()
    static member val Instance = ToArray()
    static member inline ToArray (x               , _:Default1) = Foldr.Invoke (fun x y -> Array.concat [[|x|];y]) [||] x
    static member        ToArray (x:seq<'a>       , _:ToArray ) = Seq.toArray x
    static member        ToArray (x:Set<'a>       , _:ToArray ) = Set.toArray x
    static member        ToArray (x:string        , _:ToArray ) = x.ToCharArray()
    static member        ToArray (x:StringBuilder , _:ToArray ) = x.ToString().ToCharArray()
    static member        ToArray (x:'a []         , _:ToArray ) = x
    static member        ToArray (x:'a ResizeArray, _:ToArray ) = Seq.toArray x
    static member        ToArray (x:list<'a>      , _:ToArray ) = List.toArray x
 
    static member inline Invoke value :'t []   =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToArray: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToArray.Instance, value)
    
 
type Exists() =
    inherit Default1()
    static member val Instance = Exists()
    static member inline Exists (x               , _:Default1) = fun f -> Foldr.Invoke (fun x b -> f x || b) false x :bool
    static member        Exists (x:Id<'T>        , _:Exists  ) = fun f -> f x.getValue :bool
    static member        Exists (x:seq<'a>       , _:Exists  ) = fun f -> Seq.exists    f x
    static member        Exists (x:list<'a>      , _:Exists  ) = fun f -> List.exists   f x
    static member        Exists (x:'a []         , _:Exists  ) = fun f -> Array.exists  f x
    static member        Exists (x:Set<'a>       , _:Exists  ) = fun f -> Set.exists    f x
    static member        Exists (x:string        , _:Exists  ) = fun f -> String.exists f x
    static member        Exists (x:'a ResizeArray, _:Exists  ) = fun f -> Seq.exists    f x
    static member        Exists (x:StringBuilder , _:Exists  ) = fun f -> x.ToString() |> String.exists f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Exists: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Exists.Instance,  source) predicate          :bool
 
open Monad

type Find() =
    inherit Default1()
    static member val Instance = Find()
    static member inline Find (x, _:Default1) = fun f ->
        let r = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
    static member Find (x:Id<'T>  , _:Find) = fun f -> List.find  f [x.getValue]
    static member Find (x:seq<'T> , _:Find) = fun f -> Seq.find   f x
    static member Find (x:list<'T>, _:Find) = fun f -> List.find  f x
    static member Find (x:'T []   , _:Find) = fun f -> Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Find: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Find.Instance,    source) predicate    :'T


type TryFind() =
    inherit Default1()
    static member val Instance = TryFind()
    static member inline TryFind (x         , _:Default1) = fun f -> Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
    static member        TryFind (x:Id<'T>  , _:TryFind ) = fun f -> List.tryFind  f [x.getValue]
    static member        TryFind (x:seq<'T> , _:TryFind ) = fun f -> Seq.tryFind   f x
    static member        TryFind (x:list<'T>, _:TryFind ) = fun f -> List.tryFind  f x
    static member        TryFind (x:'T []   , _:TryFind ) = fun f -> Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryFind: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (TryFind.Instance, source) predicate          :'T option


type Pick() =
    inherit Default1()
    static member val Instance = Pick()
    static member inline Pick (x, _:Default1) = fun (f:_->'U option) ->
        let r = Foldr.Invoke (fun x b -> f x <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
    static member Pick (x:Id<'T>  , _:Pick) = fun (f:_->'U option) -> List.pick  f [x.getValue]
    static member Pick (x:seq<'T> , _:Pick) = fun (f:_->'U option) -> Seq.pick   f x
    static member Pick (x:list<'T>, _:Pick) = fun (f:_->'U option) -> List.pick  f x
    static member Pick (x:'T []   , _:Pick) = fun (f:_->'U option) -> Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Pick: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Pick.Instance,   source) chooser            :'U


type TryPick() =
    inherit Default1()
    static member val Instance = TryPick()
    static member inline TryPick (x         , _:Default1) = fun (f:_->'U option) -> Foldr.Invoke (fun x b -> f x <|> b) None x  :'U option
    static member        TryPick (x:Id<'T>  , _:TryPick ) = fun (f:_->'U option) -> invalidOp "TryPick on ID" :'U option
    static member        TryPick (x:seq<'T> , _:TryPick ) = fun (f:_->'U option) -> Seq.tryPick   f x
    static member        TryPick (x:list<'T>, _:TryPick ) = fun (f:_->'U option) -> List.tryPick  f x
    static member        TryPick (x:'T []   , _:TryPick ) = fun (f:_->'U option) -> Array.tryPick f x

    static member inline Invoke  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryPick: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (TryPick.Instance, source) chooser            :'U option

  
type Filter() =
    inherit Default1()
    static member val Instance = Filter()
    static member inline Filter (x:'t            , _:Default1) = fun p -> let m:'t = Mempty.Invoke() in Foldr.Invoke (Mappend.Invoke << (fun a -> if p a then result a else m)) m x :'t
    static member        Filter (x:'t seq        , _:Filter  ) = fun p -> Seq.filter p x
    static member        Filter (x:'t Set        , _:Filter  ) = fun p -> Set.filter p x
    static member        Filter (x:'t option     , _:Filter  ) = fun p -> match x with None -> None | Some a -> if p a then x else None
    static member        Filter (x:'t list       , _:Filter  ) = fun p -> List.filter  p x
    static member        Filter (x:'t []         , _:Filter  ) = fun p -> Array.filter p x
    static member        Filter (x:'t IObservable, _:Filter  ) = fun p -> Observable.filter p x
    static member        Filter (x:'t ResizeArray, _:Filter  ) = fun p -> ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Filter: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Filter.Instance, x) predicate