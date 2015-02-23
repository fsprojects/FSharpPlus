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
    static member inline Foldr (x:'F           ,f:'a->'b->'b,z:'b , _:Default1) = ((^F) : (static member FoldBack: ^F -> _ -> _-> ^b) (x, f, z))
    static member        Foldr (x:option<_>    ,f           ,z    , _:Foldr   ) = match x with Some t -> f t z | _ -> z
    static member        Foldr (x:list<_>      ,f           ,z    , _:Foldr   ) = List.foldBack          f x z
    static member        Foldr (x:Set<_>       ,f           ,z    , _:Foldr   ) = Set.foldBack           f x z
    static member        Foldr (x:string       ,f           ,z    , _:Foldr   ) = Array.foldBack f (x.ToCharArray()) z
    static member        Foldr (x:StringBuilder,f           ,z    , _:Foldr   ) = Array.foldBack f (x.ToString().ToCharArray()) z
    static member        Foldr (x:seq<_>       ,f           ,z    , _:Foldr   ) = List.foldBack  f (Seq.toList x) z
    static member        Foldr (x:Id<'a>       ,f           ,z    , _:Foldr   ) = f (Id.run x) z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Foldr: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Foldr.Instance, foldable, folder, state)

type FoldMap() =
    inherit Default1()
    static member val Instance = FoldMap()
    static member inline FromFoldr f x = Foldr.Invoke (Mappend.Invoke << f) (Mempty.Invoke()) x  
    
    static member inline FoldMap (x          , f, _:Default1) = FoldMap.FromFoldr f x
    static member inline FoldMap (x:option<_>, f, _:FoldMap ) = FoldMap.FromFoldr f x
    static member inline FoldMap (x:Id<_>    , f, _:FoldMap ) = Mappend.Invoke (f x.getValue) (Mempty.Invoke())
    static member inline FoldMap (x:seq<_>   , f, _:FoldMap ) = FoldMap.FromFoldr f x
    static member inline FoldMap (x:list<_>  , f, _:FoldMap ) = FoldMap.FromFoldr f x
    static member inline FoldMap (x:Set<_>   , f, _:FoldMap ) = FoldMap.FromFoldr f x
    static member inline FoldMap (x:array<_> , f, _:FoldMap ) = Array.foldBack (Mappend.Invoke << f) x (Mempty.Invoke())

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member FoldMap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (FoldMap.Instance, x, f)


type Foldr with
    static member inline FromFoldMap f z x = appEndo (FoldMap.Invoke (Endo << f ) x) z
    static member inline Foldr (x:array<_>, f, z, _:Foldr) = Foldr.FromFoldMap f z x


type Foldl() =
    inherit Default1()
    static member val Instance = Foldl()

    static member inline FromFoldMap f z t = appEndo (getDual (FoldMap.Invoke (Dual << Endo << flip f) t)) z

    static member inline Foldl (x          , f, z, _:Default1) = Foldl.FromFoldMap      f z x
    static member        Foldl (x:option<_>, f, z, _:Foldl   ) = match x with Some t -> f z t | _ -> z
    static member        Foldl (x:Id<_>    , f, z, _:Foldl   ) = f z x.getValue
    static member        Foldl (x:seq<_>   , f, z, _:Foldl   ) = Seq.fold               f z x
    static member        Foldl (x:list<_>  , f, z, _:Foldl   ) = List.fold              f z x
    static member        Foldl (x:Set<_>   , f, z, _:Foldl   ) = Set.fold               f z x
    static member        Foldl (x:array<_> , f, z, _:Foldl   ) = Foldl.FromFoldMap      f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Foldl: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Foldl.Instance, foldable, folder, state)


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
    static member inline Exists (x               , f, _:Default1) = Foldr.Invoke (fun x b -> f x || b) false x :bool
    static member        Exists (x:Id<'T>        , f, _:Exists  ) = f x.getValue :bool
    static member        Exists (x:seq<'a>       , f, _:Exists  ) = Seq.exists    f x
    static member        Exists (x:list<'a>      , f, _:Exists  ) = List.exists   f x
    static member        Exists (x:'a []         , f, _:Exists  ) = Array.exists  f x
    static member        Exists (x:Set<'a>       , f, _:Exists  ) = Set.exists    f x
    static member        Exists (x:string        , f, _:Exists  ) = String.exists f x
    static member        Exists (x:'a ResizeArray, f, _:Exists  ) = Seq.exists    f x
    static member        Exists (x:StringBuilder , f, _:Exists  ) = x.ToString() |> String.exists f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, f) = ((^a or ^b) : (static member Exists: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, f)
        call (Exists.Instance,  source, predicate)        :bool
 
open Monad

type Find() =
    inherit Default1()
    static member val Instance = Find()
    static member inline Find (x, f, _:Default1) =
        let r = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
    static member Find (x:Id<'T>  , f, _:Find) = List.find  f [x.getValue]
    static member Find (x:seq<'T> , f, _:Find) = Seq.find   f x
    static member Find (x:list<'T>, f, _:Find) = List.find  f x
    static member Find (x:'T []   , f, _:Find) = Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Find: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Find.Instance,    source, predicate)  :'T


type TryFind() =
    inherit Default1()
    static member val Instance = TryFind()
    static member inline TryFind (x         , f, _:Default1) = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
    static member        TryFind (x:Id<'T>  , f, _:TryFind ) = List.tryFind  f [x.getValue]
    static member        TryFind (x:seq<'T> , f, _:TryFind ) = Seq.tryFind   f x
    static member        TryFind (x:list<'T>, f, _:TryFind ) = List.tryFind  f x
    static member        TryFind (x:'T []   , f, _:TryFind ) = Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member TryFind: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (TryFind.Instance, source, predicate)   :'T option


type Pick() =
    inherit Default1()
    static member val Instance = Pick()
    static member inline Pick (x, f:_->'U option, _:Default1) =
        let r = Foldr.Invoke (fun x b -> f x <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
    static member Pick (x:Id<'T>  , f:_->'U option, _:Pick) = List.pick  f [x.getValue]
    static member Pick (x:seq<'T> , f:_->'U option, _:Pick) = Seq.pick   f x
    static member Pick (x:list<'T>, f:_->'U option, _:Pick) = List.pick  f x
    static member Pick (x:'T []   , f:_->'U option, _:Pick) = Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b ) : (static member Pick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Pick.Instance,   source, chooser)    :'U


type TryPick() =
    inherit Default1()
    static member val Instance = TryPick()
    static member inline TryPick (x         , f:_->'U option, _:Default1) = Foldr.Invoke (fun x b -> f x <|> b) None x  :'U option
    static member        TryPick (x:Id<'T>  , f:_->'U option, _:TryPick ) = invalidOp "TryPick on ID" :'U option
    static member        TryPick (x:seq<'T> , f:_->'U option, _:TryPick ) = Seq.tryPick   f x
    static member        TryPick (x:list<'T>, f:_->'U option, _:TryPick ) = List.tryPick  f x
    static member        TryPick (x:'T []   , f:_->'U option, _:TryPick ) = Array.tryPick f x

    static member inline Invoke  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b) : (static member TryPick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (TryPick.Instance, source, chooser)      :'U option

 
type Filter() =
    inherit Default1()
    static member val Instance = Filter()
    static member inline Filter (x:'t            , p, _:Default1) = let m:'t = Mempty.Invoke() in Foldr.Invoke (Mappend.Invoke << (fun a -> if p a then result a else m)) m x :'t
    static member        Filter (x:'t seq        , p, _:Filter  ) = Seq.filter p x
    static member        Filter (x:'t Set        , p, _:Filter  ) = Set.filter p x
    static member        Filter (x:'t option     , p, _:Filter  ) = match x with None -> None | Some a -> if p a then x else None
    static member        Filter (x:'t list       , p, _:Filter  ) = List.filter  p x
    static member        Filter (x:'t []         , p, _:Filter  ) = Array.filter p x
    static member        Filter (x:'t IObservable, p, _:Filter  ) = Observable.filter p x
    static member        Filter (x:'t ResizeArray, p, _:Filter  ) = ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Filter: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Filter.Instance, x, predicate)