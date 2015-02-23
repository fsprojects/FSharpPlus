namespace FsControl.Core.TypeMethods

open FsControl.Core
open FsControl.Core.Prelude
open FsControl.Core.Types
open Dual
open Endo
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text


[<Extension;Sealed>]
type Foldr() =
    inherit Default1()
    static member val Instance = Foldr()
    [<Extension>]static member inline Foldr (x:'F           , f:'a->'b->'b, z:'b , [<Optional>]impl:Default1) = ((^F) : (static member FoldBack: ^F -> _ -> _-> ^b) x, f, z)
    [<Extension>]static member        Foldr (x:option<_>    , f           , z    , [<Optional>]impl:Foldr   ) = match x with Some t -> f t z | _ -> z
    [<Extension>]static member        Foldr (x:list<_>      , f           , z    , [<Optional>]impl:Foldr   ) = List.foldBack          f x z
    [<Extension>]static member        Foldr (x:Set<_>       , f           , z    , [<Optional>]impl:Foldr   ) = Set.foldBack           f x z
    [<Extension>]static member        Foldr (x:string       , f           , z    , [<Optional>]impl:Foldr   ) = Array.foldBack f (x.ToCharArray()) z
    [<Extension>]static member        Foldr (x:StringBuilder, f           , z    , [<Optional>]impl:Foldr   ) = Array.foldBack f (x.ToString().ToCharArray()) z
    [<Extension>]static member        Foldr (x:seq<_>       , f           , z    , [<Optional>]impl:Foldr   ) = List.foldBack  f (Seq.toList x) z
    [<Extension>]static member        Foldr (x:Id<'a>       , f           , z    , [<Optional>]impl:Foldr   ) = f (Id.run x) z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Foldr: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Foldr.Instance, foldable, folder, state)


[<Extension;Sealed>]
type FoldMap() =
    inherit Default1()
    static member val Instance = FoldMap()
    static member inline FromFoldr f x = Foldr.Invoke (Mappend.Invoke << f) (Mempty.Invoke()) x  
    
    [<Extension>]static member inline FoldMap (x          , f, [<Optional>]impl:Default1) = FoldMap.FromFoldr f x
    [<Extension>]static member inline FoldMap (x:option<_>, f, [<Optional>]impl:FoldMap ) = FoldMap.FromFoldr f x
    [<Extension>]static member inline FoldMap (x:Id<_>    , f, [<Optional>]impl:FoldMap ) = Mappend.Invoke (f x.getValue) (Mempty.Invoke())
    [<Extension>]static member inline FoldMap (x:seq<_>   , f, [<Optional>]impl:FoldMap ) = FoldMap.FromFoldr f x
    [<Extension>]static member inline FoldMap (x:list<_>  , f, [<Optional>]impl:FoldMap ) = FoldMap.FromFoldr f x
    [<Extension>]static member inline FoldMap (x:Set<_>   , f, [<Optional>]impl:FoldMap ) = FoldMap.FromFoldr f x
    [<Extension>]static member inline FoldMap (x:array<_> , f, [<Optional>]impl:FoldMap ) = Array.foldBack (Mappend.Invoke << f) x (Mempty.Invoke())

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member FoldMap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (FoldMap.Instance, x, f)


[<Extension;Sealed>]
type Foldr with
    static member inline FromFoldMap f z x = appEndo (FoldMap.Invoke (Endo << f ) x) z
    [<Extension>]static member inline Foldr (x:array<_>, f, z, [<Optional>]impl:Foldr) = Foldr.FromFoldMap f z x


[<Extension;Sealed>]
type Foldl() =
    inherit Default1()
    static member val Instance = Foldl()

    static member inline FromFoldMap f z t = appEndo (getDual (FoldMap.Invoke (Dual << Endo << flip f) t)) z

    [<Extension>]static member inline Foldl (x          , f, z, [<Optional>]impl:Default1) = Foldl.FromFoldMap      f z x
    [<Extension>]static member        Foldl (x:option<_>, f, z, [<Optional>]impl:Foldl   ) = match x with Some t -> f z t | _ -> z
    [<Extension>]static member        Foldl (x:Id<_>    , f, z, [<Optional>]impl:Foldl   ) = f z x.getValue
    [<Extension>]static member        Foldl (x:seq<_>   , f, z, [<Optional>]impl:Foldl   ) = Seq.fold               f z x
    [<Extension>]static member        Foldl (x:list<_>  , f, z, [<Optional>]impl:Foldl   ) = List.fold              f z x
    [<Extension>]static member        Foldl (x:Set<_>   , f, z, [<Optional>]impl:Foldl   ) = Set.fold               f z x
    [<Extension>]static member        Foldl (x:array<_> , f, z, [<Optional>]impl:Foldl   ) = Foldl.FromFoldMap      f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Foldl: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Foldl.Instance, foldable, folder, state)


[<Extension;Sealed>]
type ToList() =
    inherit Default1()
    static member val Instance = ToList()
    [<Extension>]static member inline ToList (x               , [<Optional>]impl:Default1) = Foldr.Invoke List.cons [] x
    [<Extension>]static member        ToList (x:seq<'a>       , [<Optional>]impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:Set<'a>       , [<Optional>]impl:ToList  ) = Set.toList x
    [<Extension>]static member        ToList (x:string        , [<Optional>]impl:ToList  ) = x.ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:StringBuilder , [<Optional>]impl:ToList  ) = x.ToString().ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:'a []         , [<Optional>]impl:ToList  ) = Array.toList x
    [<Extension>]static member        ToList (x:'a ResizeArray, [<Optional>]impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:list<'a>      , [<Optional>]impl:ToList  ) = x

    static member inline Invoke  value :'t list = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToList: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToList.Instance , value)

 
[<Extension;Sealed>]
type ToArray() =
    inherit Default1()
    static member val Instance = ToArray()
    [<Extension>]static member inline ToArray (x               , [<Optional>]impl:Default1) = Foldr.Invoke (fun x y -> Array.concat [[|x|];y]) [||] x
    [<Extension>]static member        ToArray (x:seq<'a>       , [<Optional>]impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:Set<'a>       , [<Optional>]impl:ToArray ) = Set.toArray x
    [<Extension>]static member        ToArray (x:string        , [<Optional>]impl:ToArray ) = x.ToCharArray()
    [<Extension>]static member        ToArray (x:StringBuilder , [<Optional>]impl:ToArray ) = x.ToString().ToCharArray()
    [<Extension>]static member        ToArray (x:'a []         , [<Optional>]impl:ToArray ) = x
    [<Extension>]static member        ToArray (x:'a ResizeArray, [<Optional>]impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:list<'a>      , [<Optional>]impl:ToArray ) = List.toArray x
 
    static member inline Invoke value :'t []   =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToArray: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToArray.Instance, value)
    
 
[<Extension;Sealed>]
type Exists() =
    inherit Default1()
    static member val Instance = Exists()
    [<Extension>]static member inline Exists (x               , f, [<Optional>]impl:Default1) = Foldr.Invoke (fun x b -> f x || b) false x :bool
    [<Extension>]static member        Exists (x:Id<'T>        , f, [<Optional>]impl:Exists  ) = f x.getValue :bool
    [<Extension>]static member        Exists (x:seq<'a>       , f, [<Optional>]impl:Exists  ) = Seq.exists    f x
    [<Extension>]static member        Exists (x:list<'a>      , f, [<Optional>]impl:Exists  ) = List.exists   f x
    [<Extension>]static member        Exists (x:'a []         , f, [<Optional>]impl:Exists  ) = Array.exists  f x
    [<Extension>]static member        Exists (x:Set<'a>       , f, [<Optional>]impl:Exists  ) = Set.exists    f x
    [<Extension>]static member        Exists (x:string        , f, [<Optional>]impl:Exists  ) = String.exists f x
    [<Extension>]static member        Exists (x:'a ResizeArray, f, [<Optional>]impl:Exists  ) = Seq.exists    f x
    [<Extension>]static member        Exists (x:StringBuilder , f, [<Optional>]impl:Exists  ) = x.ToString() |> String.exists f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, f) = ((^a or ^b) : (static member Exists: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, f)
        call (Exists.Instance,  source, predicate)        :bool
 
open Monad

[<Extension;Sealed>]
type Find() =
    inherit Default1()
    static member val Instance = Find()
    [<Extension>]static member inline Find (x, f, [<Optional>]impl:Default1) =
                    let r = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
                    (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
    [<Extension>]static member Find (x:Id<'T>  , f, [<Optional>]impl:Find) = List.find  f [x.getValue]
    [<Extension>]static member Find (x:seq<'T> , f, [<Optional>]impl:Find) = Seq.find   f x
    [<Extension>]static member Find (x:list<'T>, f, [<Optional>]impl:Find) = List.find  f x
    [<Extension>]static member Find (x:'T []   , f, [<Optional>]impl:Find) = Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Find: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Find.Instance,    source, predicate)  :'T


[<Extension;Sealed>]
type TryFind() =
    inherit Default1()
    static member val Instance = TryFind()
    [<Extension>]static member inline TryFind (x         , f, [<Optional>]impl:Default1) = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
    [<Extension>]static member        TryFind (x:Id<'T>  , f, [<Optional>]impl:TryFind ) = List.tryFind  f [x.getValue]
    [<Extension>]static member        TryFind (x:seq<'T> , f, [<Optional>]impl:TryFind ) = Seq.tryFind   f x
    [<Extension>]static member        TryFind (x:list<'T>, f, [<Optional>]impl:TryFind ) = List.tryFind  f x
    [<Extension>]static member        TryFind (x:'T []   , f, [<Optional>]impl:TryFind ) = Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member TryFind: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (TryFind.Instance, source, predicate)   :'T option


[<Extension;Sealed>]
type Pick() =
    inherit Default1()
    static member val Instance = Pick()
    [<Extension>]static member inline Pick (x, f:_->'U option, [<Optional>]impl:Default1) =
                    let r = Foldr.Invoke (fun x b -> f x <|> b) None x
                    (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
    [<Extension>]static member Pick (x:Id<'T>  , f:_->'U option, [<Optional>]impl:Pick) = List.pick  f [x.getValue]
    [<Extension>]static member Pick (x:seq<'T> , f:_->'U option, [<Optional>]impl:Pick) = Seq.pick   f x
    [<Extension>]static member Pick (x:list<'T>, f:_->'U option, [<Optional>]impl:Pick) = List.pick  f x
    [<Extension>]static member Pick (x:'T []   , f:_->'U option, [<Optional>]impl:Pick) = Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b ) : (static member Pick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Pick.Instance,   source, chooser)    :'U


[<Extension;Sealed>]
type TryPick() =
    inherit Default1()
    static member val Instance = TryPick()
    [<Extension>]static member inline TryPick (x         , f:_->'U option, [<Optional>]impl:Default1) = Foldr.Invoke (fun x b -> f x <|> b) None x  :'U option
    [<Extension>]static member        TryPick (x:Id<'T>  , f:_->'U option, [<Optional>]impl:TryPick ) = invalidOp "TryPick on ID" :'U option
    [<Extension>]static member        TryPick (x:seq<'T> , f:_->'U option, [<Optional>]impl:TryPick ) = Seq.tryPick   f x
    [<Extension>]static member        TryPick (x:list<'T>, f:_->'U option, [<Optional>]impl:TryPick ) = List.tryPick  f x
    [<Extension>]static member        TryPick (x:'T []   , f:_->'U option, [<Optional>]impl:TryPick ) = Array.tryPick f x

    static member inline Invoke  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b) : (static member TryPick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (TryPick.Instance, source, chooser)      :'U option

 
[<Extension;Sealed>]
type Filter() =
    inherit Default1()
    static member val Instance = Filter()
    [<Extension>]static member inline Filter (x:'t            , p, [<Optional>]impl:Default1) = let m:'t = Mempty.Invoke() in Foldr.Invoke (Mappend.Invoke << (fun a -> if p a then result a else m)) m x :'t
    [<Extension>]static member        Filter (x:'t seq        , p, [<Optional>]impl:Filter  ) = Seq.filter p x
    [<Extension>]static member        Filter (x:'t Set        , p, [<Optional>]impl:Filter  ) = Set.filter p x
    [<Extension>]static member        Filter (x:'t option     , p, [<Optional>]impl:Filter  ) = match x with None -> None | Some a -> if p a then x else None
    [<Extension>]static member        Filter (x:'t list       , p, [<Optional>]impl:Filter  ) = List.filter  p x
    [<Extension>]static member        Filter (x:'t []         , p, [<Optional>]impl:Filter  ) = Array.filter p x
    [<Extension>]static member        Filter (x:'t IObservable, p, [<Optional>]impl:Filter  ) = Observable.filter p x
    [<Extension>]static member        Filter (x:'t ResizeArray, p, [<Optional>]impl:Filter  ) = ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Filter: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Filter.Instance, x, predicate)