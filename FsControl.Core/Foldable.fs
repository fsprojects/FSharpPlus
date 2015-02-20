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
    static member inline Foldr (_:Default1, x:'F ) = fun (f:'a->'b->'b,z:'b) -> ((^F) : (static member FoldBack: _ -> ^F -> _-> ^b) (f, x, z))
    static member Foldr (_:Foldr, x:option<_>    ) = fun (f,z) -> match x with Some t -> f t z | _ -> z
    static member Foldr (_:Foldr, x:list<_>      ) = fun (f,z) -> List.foldBack          f x z
    static member Foldr (_:Foldr, x:Set<_>       ) = fun (f,z) -> Set.foldBack           f x z
    static member Foldr (_:Foldr, x:string       ) = fun (f,z) -> Array.foldBack f (x.ToCharArray()) z
    static member Foldr (_:Foldr, x:StringBuilder) = fun (f,z) -> Array.foldBack f (x.ToString().ToCharArray()) z
    static member Foldr (_:Foldr, x:seq<_>       ) = fun (f,z) -> List.foldBack  f (Seq.toList x) z
    static member Foldr (_:Foldr, x:Id<'a>       ) = fun (f,z) -> f (Id.run x) z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Foldr: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Foldr.Instance, foldable) (folder, state)

type FoldMap() =
    inherit Default1()
    static member val Instance = FoldMap()
    static member inline FromFoldr f x = Foldr.Invoke (Mappend.Invoke << f) (Mempty.Invoke()) x  
    
    static member inline FoldMap (_:Default1, x          ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (_:FoldMap , x:option<_>) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (_:FoldMap , x:Id<_>    ) = fun f -> Mappend.Invoke (f x.getValue) (Mempty.Invoke())
    static member inline FoldMap (_:FoldMap , x:seq<_>   ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (_:FoldMap , x:list<_>  ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (_:FoldMap , x:Set<_>   ) = fun f -> FoldMap.FromFoldr f x
    static member inline FoldMap (_:FoldMap , x:array<_> ) = fun f -> Array.foldBack (Mappend.Invoke << f) x (Mempty.Invoke())

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FoldMap: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (FoldMap.Instance, x) f


type Foldr with
    static member inline FromFoldMap f z x = appEndo (FoldMap.Invoke (Endo << f ) x) z
    static member inline Foldr (_:Foldr, x:array<_>) = fun (f,z) -> Foldr.FromFoldMap f z x


type Foldl() =
    inherit Default1()
    static member val Instance = Foldl()

    static member inline FromFoldMap f z t = appEndo (getDual (FoldMap.Invoke (Dual << Endo << flip f) t)) z

    static member inline Foldl (_:Default1, x) = fun (f,z) -> Foldl.FromFoldMap      f z x
    static member Foldl (_:Foldl, x:option<_>) = fun (f,z) -> match x with Some t -> f z t | _ -> z
    static member Foldl (_:Foldl, x:Id<_>    ) = fun (f,z) -> f z x.getValue
    static member Foldl (_:Foldl, x:seq<_>   ) = fun (f,z) -> Seq.fold               f z x
    static member Foldl (_:Foldl, x:list<_>  ) = fun (f,z) -> List.fold              f z x
    static member Foldl (_:Foldl, x:Set<_>   ) = fun (f,z) -> Set.fold               f z x
    static member Foldl (_:Foldl, x:array<_> ) = fun (f,z) -> Foldl.FromFoldMap      f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Foldl: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Foldl.Instance, foldable) (folder, state)


type ToList() =
    inherit Default1()
    static member val Instance = ToList()
    static member inline ToList (_:Default1, x      ) = Foldr.Invoke List.cons [] x
    static member ToList (_:ToList, x:seq<'a>       ) = Seq.toList x
    static member ToList (_:ToList, x:Set<'a>       ) = Set.toList x
    static member ToList (_:ToList, x:string        ) = x.ToCharArray() |> Array.toList
    static member ToList (_:ToList, x:StringBuilder ) = x.ToString().ToCharArray() |> Array.toList
    static member ToList (_:ToList, x:'a []         ) = Array.toList x
    static member ToList (_:ToList, x:'a ResizeArray) = Seq.toList x
    static member ToList (_:ToList, x:list<'a>      ) = x

    static member inline Invoke  value :'t list = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToList: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToList.Instance , value)

 
type ToArray() =
    inherit Default1()
    static member val Instance = ToArray()
    static member inline ToArray (_:Default1, x       ) = Foldr.Invoke (fun x y -> Array.concat [[|x|];y]) [||] x
    static member ToArray (_:ToArray, x:seq<'a>       ) = Seq.toArray x
    static member ToArray (_:ToArray, x:Set<'a>       ) = Set.toArray x
    static member ToArray (_:ToArray, x:string        ) = x.ToCharArray()
    static member ToArray (_:ToArray, x:StringBuilder ) = x.ToString().ToCharArray()
    static member ToArray (_:ToArray, x:'a []         ) = x
    static member ToArray (_:ToArray, x:'a ResizeArray) = Seq.toArray x
    static member ToArray (_:ToArray, x:list<'a>      ) = List.toArray x
 
    static member inline Invoke value :'t []   =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToArray: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToArray.Instance, value)
    
 
type Exists() =
    inherit Default1()
    static member val Instance = Exists()
    static member inline Exists (_:Default1, x      ) = fun f -> Foldr.Invoke (fun x b -> f x || b) false x :bool
    static member Exists (_:Exists, x:Id<'T>        ) = fun f -> f x.getValue :bool
    static member Exists (_:Exists, x:seq<'a>       ) = fun f -> Seq.exists    f x
    static member Exists (_:Exists, x:list<'a>      ) = fun f -> List.exists   f x
    static member Exists (_:Exists, x:'a []         ) = fun f -> Array.exists  f x
    static member Exists (_:Exists, x:Set<'a>       ) = fun f -> Set.exists    f x
    static member Exists (_:Exists, x:string        ) = fun f -> String.exists f x
    static member Exists (_:Exists, x:'a ResizeArray) = fun f -> Seq.exists    f x
    static member Exists (_:Exists, x:StringBuilder ) = fun f -> x.ToString() |> String.exists f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Exists: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Exists.Instance,  source) predicate          :bool
 
open Monad

type Find() =
    inherit Default1()
    static member val Instance = Find()
    static member inline Find (_:Default1, x) = fun f ->
        let r = Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'T
    static member Find (_:Find, x:Id<'T>  ) = fun f -> List.find  f [x.getValue]
    static member Find (_:Find, x:seq<'T> ) = fun f -> Seq.find   f x
    static member Find (_:Find, x:list<'T>) = fun f -> List.find  f x
    static member Find (_:Find, x:'T []   ) = fun f -> Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Find: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Find.Instance,    source) predicate    :'T


type TryFind() =
    inherit Default1()
    static member val Instance = TryFind()
    static member inline TryFind (_:Default1, x ) = fun f -> Foldr.Invoke (fun x b -> (let r = f x in if r then Some x else None) <|> b) None x  :'T option
    static member TryFind (_:TryFind, x:Id<'T>  ) = fun f -> List.tryFind  f [x.getValue]
    static member TryFind (_:TryFind, x:seq<'T> ) = fun f -> Seq.tryFind   f x
    static member TryFind (_:TryFind, x:list<'T>) = fun f -> List.tryFind  f x
    static member TryFind (_:TryFind, x:'T []   ) = fun f -> Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryFind: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (TryFind.Instance, source) predicate          :'T option


type Pick() =
    inherit Default1()
    static member val Instance = Pick()
    static member inline Pick (_:Default1, x) = fun (f:_->'U option) ->
        let r = Foldr.Invoke (fun x b -> f x <|> b) None x
        (match r with Some r -> r | _ -> raise (Collections.Generic.KeyNotFoundException())) :'U
    static member Pick (_:Pick, x:Id<'T>  ) = fun (f:_->'U option) -> List.pick  f [x.getValue]
    static member Pick (_:Pick, x:seq<'T> ) = fun (f:_->'U option) -> Seq.pick   f x
    static member Pick (_:Pick, x:list<'T>) = fun (f:_->'U option) -> List.pick  f x
    static member Pick (_:Pick, x:'T []   ) = fun (f:_->'U option) -> Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b ) : (static member Pick: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Pick.Instance,   source) chooser            :'U


type TryPick() =
    inherit Default1()
    static member val Instance = TryPick()
    static member inline TryPick (_:Default1, x ) = fun (f:_->'U option) -> Foldr.Invoke (fun x b -> f x <|> b) None x  :'U option
    static member TryPick (_:TryPick, x:Id<'T>  ) = fun (f:_->'U option) -> invalidOp "TryPick on ID" :'U option
    static member TryPick (_:TryPick, x:seq<'T> ) = fun (f:_->'U option) -> Seq.tryPick   f x
    static member TryPick (_:TryPick, x:list<'T>) = fun (f:_->'U option) -> List.tryPick  f x
    static member TryPick (_:TryPick, x:'T []   ) = fun (f:_->'U option) -> Array.tryPick f x

    static member inline Invoke  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryPick: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (TryPick.Instance, source) chooser            :'U option

  
type Filter() =
    inherit Default1()
    static member val Instance = Filter()
    static member inline Filter (_:Default1, x:'t            ) = fun p -> let m:'t = Mempty.Invoke() in Foldr.Invoke (Mappend.Invoke << (fun a -> if p a then result a else m)) m x :'t
    static member        Filter (_:Filter  , x:'t seq        ) = fun p -> Seq.filter p x
    static member        Filter (_:Filter  , x:'t Set        ) = fun p -> Set.filter p x
    static member        Filter (_:Filter  , x:'t option     ) = fun p -> match x with None -> None | Some a -> if p a then x else None
    static member        Filter (_:Filter  , x:'t list       ) = fun p -> List.filter  p x
    static member        Filter (_:Filter  , x:'t []         ) = fun p -> Array.filter p x
    static member        Filter (_:Filter  , x:'t IObservable) = fun p -> Observable.filter p x
    static member        Filter (_:Filter  , x:'t ResizeArray) = fun p -> ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Filter: _*_ -> _) a, b)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_2 (a, b) x
        call (Filter.Instance, x) predicate