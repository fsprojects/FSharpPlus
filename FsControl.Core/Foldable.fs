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
open System.Collections
open System.Collections.Generic


[<Extension;Sealed>]
type ToSeq() =
    inherit Default1()
    static member val Instance = ToSeq()
    [<Extension>]static member inline ToSeq (x:'S when 'S :> Collections.IEnumerable, [<Optional>]impl:Default2) = let f i x :'T = ( ^S : (member get_Item : int -> 'T) x, i) in Seq.cast<'T> x : seq<'T>
    [<Extension>]static member inline ToSeq (x:'Foldable, [<Optional>]impl:Default1) = ((^Foldable) : (static member ToSeq: ^Foldable -> seq<'t>) x)
                 static member inline ToSeq (x:'T when 'T : null and 'T :struct     ,             _   :ToSeq   ) = ()
    [<Extension>]static member        ToSeq (x:seq<'T>   , [<Optional>]impl:ToSeq) = x
    [<Extension>]static member        ToSeq (x:Text.StringBuilder,  _:ToSeq) = x.ToString() :> seq<char>
    [<Extension>]static member        ToSeq (x:string            ,  _:ToSeq) = x            :> seq<char>
    [<Extension>]static member        ToSeq (x:option<'T>, [<Optional>]impl:ToSeq) = match x with Some x -> Seq.singleton x | None -> Seq.empty

    static member inline Invoke (source:'Collection'T)  : seq<'T>  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToSeq: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToSeq.Instance , source)


[<Extension;Sealed>]
type ToList() =
    inherit Default1()
    static member val Instance = ToList()
    [<Extension>]static member inline ToList (x               , [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.toList
    [<Extension>]static member        ToList (x:seq<'a>       , [<Optional>]impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:Set<'a>       , [<Optional>]impl:ToList  ) = Set.toList x
    [<Extension>]static member        ToList (x:string        , [<Optional>]impl:ToList  ) = x.ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:StringBuilder , [<Optional>]impl:ToList  ) = x.ToString().ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:'a []         , [<Optional>]impl:ToList  ) = Array.toList x
    [<Extension>]static member        ToList (x:'a ResizeArray, [<Optional>]impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:'a Id         , [<Optional>]impl:ToList  ) = [x.getValue]
    [<Extension>]static member        ToList (x:list<'a>      , [<Optional>]impl:ToList  ) = x

    static member inline Invoke  value :'t list = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToList: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToList.Instance , value)


[<Extension;Sealed>]
type ToArray() =
    inherit Default1()
    static member val Instance = ToArray()
    [<Extension>]static member inline ToArray (x               , [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.toArray
    [<Extension>]static member        ToArray (x:seq<'a>       , [<Optional>]impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:Set<'a>       , [<Optional>]impl:ToArray ) = Set.toArray x
    [<Extension>]static member        ToArray (x:string        , [<Optional>]impl:ToArray ) = x.ToCharArray()
    [<Extension>]static member        ToArray (x:StringBuilder , [<Optional>]impl:ToArray ) = x.ToString().ToCharArray()
    [<Extension>]static member        ToArray (x:'a []         , [<Optional>]impl:ToArray ) = x
    [<Extension>]static member        ToArray (x:'a ResizeArray, [<Optional>]impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:'a Id         , [<Optional>]impl:ToArray ) = [|x.getValue|]
    [<Extension>]static member        ToArray (x:list<'a>      , [<Optional>]impl:ToArray ) = List.toArray x

    static member inline Invoke  value : 't [] = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToArray: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (ToArray.Instance , value)


type FromSeq() =
    inherit Default1()
    static member val Instance = FromSeq()

    static member inline FromSeq (x:seq<'a>                 , _:'Foldable'T                     , _:Default5) = x |> Seq.map Return.Invoke |> Mconcat.Invoke :'Foldable'T
    static member        FromSeq (x:seq<'a>                 , _:seq<'a>                         , _:Default4) = x
    static member inline FromSeq (x:seq<'t>                 , _:'F                              , _:Default3) = let c = new 'F() in (Seq.iter (fun t -> ( ^F : (member Add : 't -> ^R) c, t) |> ignore) x); c
    static member        FromSeq (x:seq<KeyValuePair<'k,'v>>, _:ICollection<KeyValuePair<'k,'v>>, _:Default3) = let d = Dictionary() :> ICollection<KeyValuePair<'k,'v>> in Seq.iter d.Add x; d
    static member        FromSeq (x:seq<'K*'V>              , _:IDictionary<'k,'v>              , _:Default3) = dict x
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:IDictionary<'K,'V>              , _:Default3) = x |> Seq.map (function (KeyValue x) -> x) |> dict
    static member        FromSeq (x:seq<'K*'V>              , _:Collections.IDictionary         , _:Default3) = let d = Hashtable() in x |> Seq.iter d.Add; d :> IDictionary
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:Collections.IDictionary         , _:Default3) = let d = Hashtable() in x |> Seq.iter (function (KeyValue x) -> d.Add x); d :> IDictionary
    static member        FromSeq (x:seq<'K*'V>              , _:'T when 'T :> Collections.IDictionary, _:Default2) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:'T when 'T :> Collections.IDictionary, _:Default2) = let d = new 'T() in x |> Seq.iter (function (KeyValue x) -> d.Add x); d
    static member        FromSeq (x:seq<'K*'V>              , _:'T when 'T :> IDictionary<'K,'V>, _:Default1) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:'T when 'T :> IDictionary<'K,'V>, _:Default1) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member inline FromSeq (x:seq<'a>                 , _:'UserType                       , _:FromSeq ) = ((^UserType) : (static member FromSeq: seq<'a> -> ^F) x)     
    static member        FromSeq (x:seq<'K*'V>              , _:Dictionary<'K,'V>               , _:FromSeq ) = Dictionary (dict x)
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:Dictionary<'K,'V>               , _:FromSeq ) = Dictionary (dict (Seq.map (function (KeyValue x) -> x) x))
    static member        FromSeq (x:seq<'K*'V>              , _:SortedList<'K,'V>               , _:FromSeq ) = Generic.SortedList (dict x)
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:SortedList<'K,'V>               , _:FromSeq ) = Generic.SortedList (dict (Seq.map (function (KeyValue x) -> x) x))
    static member        FromSeq (x:seq<'K*'V>              , _:Map<'K,'V>                      , _:FromSeq ) = Collections.Map x
    static member        FromSeq (x:seq<KeyValuePair<'K,'V>>, _:Map<'K,'V>                      , _:FromSeq ) = Collections.Map (Seq.map (function (KeyValue x) -> x) x)
    static member        FromSeq (x                         , _:'a []                           , _:FromSeq ) = Array.ofSeq<'a> x
    static member        FromSeq (x                         , _:list<'a>                        , _:FromSeq ) = List.ofSeq<'a> x
    static member        FromSeq (x                         , _:Set<'a>                         , _:FromSeq ) = Set.ofSeq<'a> x
    static member        FromSeq (x:seq<char>               , _:string                          , _:FromSeq ) = String.Join ("", Array.ofSeq x)
    static member        FromSeq (x:seq<char>               , _:Text.StringBuilder              , _:FromSeq ) = (StringBuilder(), x) ||> Seq.fold (fun x -> x.Append)
    static member        FromSeq (x:seq<'a>                 , _:Generic.Stack<'a>               , _:FromSeq ) = Generic.Stack x                                                                                               

    static member inline Invoke  (value :seq<'t>) = 
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member FromSeq: _*_*_ -> _) s, b, a)
        let inline call (a:'a, s) = call_2 (a, Unchecked.defaultof<'r>, s) :'r
        call (FromSeq.Instance, value)


[<Extension;Sealed>]
type FoldBack() =
    inherit Default1()
    static member val Instance = FoldBack()
    [<Extension>]static member inline FoldBack (x:'F           , f:'a->'b->'b, z:'b , [<Optional>]impl:Default2) = List.foldBack  f (ToList.Invoke x) z
    [<Extension>]static member inline FoldBack (x:'F           , f:'a->'b->'b, z:'b , [<Optional>]impl:Default1) = ((^F) : (static member FoldBack: ^F -> _ -> _-> ^b) x, f, z)
    [<Extension>]static member        FoldBack (x:seq<_>       , f           , z    , [<Optional>]impl:FoldBack) = List.foldBack  f (Seq.toList x) z
    [<Extension>]static member        FoldBack (x:option<_>    , f           , z    , [<Optional>]impl:FoldBack) = match x with Some x -> f x z | _ -> z
    [<Extension>]static member        FoldBack (x:list<_>      , f           , z    , [<Optional>]impl:FoldBack) = List.foldBack          f x z
    [<Extension>]static member        FoldBack (x:_ []         , f           , z    , [<Optional>]impl:FoldBack) = Array.foldBack         f x z
    [<Extension>]static member        FoldBack (x:Set<_>       , f           , z    , [<Optional>]impl:FoldBack) = Set.foldBack           f x z
    [<Extension>]static member        FoldBack (x:_ ResizeArray, f           , z    , [<Optional>]impl:FoldBack) = Array.foldBack         f (x.ToArray()) z
    [<Extension>]static member        FoldBack (x:string       , f           , z    , [<Optional>]impl:FoldBack) = Array.foldBack f (x.ToCharArray()) z
    [<Extension>]static member        FoldBack (x:StringBuilder, f           , z    , [<Optional>]impl:FoldBack) = Array.foldBack f (x.ToString().ToCharArray()) z   
    [<Extension>]static member        FoldBack (x:Id<'a>       , f           , z    , [<Optional>]impl:FoldBack) = f x.getValue z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member FoldBack: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (FoldBack.Instance, foldable, folder, state)


[<Extension;Sealed>]
type FoldMap() =
    inherit Default1()
    static member val Instance = FoldMap()
    static member inline FromFoldFoldBack f x = FoldBack.Invoke (Mappend.Invoke << f) (Mempty.Invoke()) x  
    
    [<Extension>]static member inline FoldMap (x          , f, [<Optional>]impl:Default1) = Seq.fold   (fun x y -> Mappend.Invoke x (f y)) (Mempty.Invoke()) x
    [<Extension>]static member inline FoldMap (x:option<_>, f, [<Optional>]impl:FoldMap ) = match x with Some x -> f x | _ -> Mempty.Invoke()
    [<Extension>]static member        FoldMap (x:Id<_>    , f, [<Optional>]impl:FoldMap ) = f x.getValue
    [<Extension>]static member inline FoldMap (x:seq<_>   , f, [<Optional>]impl:FoldMap ) = Seq.fold   (fun x y -> Mappend.Invoke x (f y)) (Mempty.Invoke()) x
    [<Extension>]static member inline FoldMap (x:list<_>  , f, [<Optional>]impl:FoldMap ) = List.fold  (fun x y -> Mappend.Invoke x (f y)) (Mempty.Invoke()) x
    [<Extension>]static member inline FoldMap (x:Set<_>   , f, [<Optional>]impl:FoldMap ) = Seq.fold   (fun x y -> Mappend.Invoke x (f y)) (Mempty.Invoke()) x
    [<Extension>]static member inline FoldMap (x:_ []     , f, [<Optional>]impl:FoldMap ) = Array.fold (fun x y -> Mappend.Invoke x (f y)) (Mempty.Invoke()) x

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member FoldMap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (FoldMap.Instance, x, f)


type FoldBack with
    static member inline FromFoldMap f z x = appEndo (FoldMap.Invoke (Endo << f ) x) z


[<Extension;Sealed>]
type Fold() =
    inherit Default1()
    static member val Instance = Fold()

    static member inline FromFoldMap f z t = appEndo (getDual (FoldMap.Invoke (Dual << Endo << flip f) t)) z

    [<Extension>]static member inline Fold (x          , f, z, [<Optional>]impl:Default2) = Seq.fold f z (ToSeq.Invoke x)
    [<Extension>]static member inline Fold (x:'F       , f:'b->'a->'b, z:'b , [<Optional>]impl:Default1) = ((^F) : (static member Fold: ^F -> _ -> _-> ^b) x, f, z)
    [<Extension>]static member        Fold (x:option<_>, f, z, [<Optional>]impl:Fold    ) = match x with Some x -> f z x | _ -> z
    [<Extension>]static member        Fold (x:Id<_>    , f, z, [<Optional>]impl:Fold    ) = f z x.getValue
    [<Extension>]static member        Fold (x:seq<_>   , f, z, [<Optional>]impl:Fold    ) = Seq.fold               f z x
    [<Extension>]static member        Fold (x:list<_>  , f, z, [<Optional>]impl:Fold    ) = List.fold              f z x
    [<Extension>]static member        Fold (x:Set<_>   , f, z, [<Optional>]impl:Fold    ) = Set.fold               f z x
    [<Extension>]static member        Fold (x: _ []    , f, z, [<Optional>]impl:Fold    ) = Array.fold             f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Fold: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Fold.Instance, foldable, folder, state)
    
 
[<Extension;Sealed>]
type Exists() =
    inherit Default1()
    static member val Instance = Exists()
    [<Extension>]static member inline Exists (x               , f, [<Optional>]impl:Default1) = Seq.exists    f (ToSeq.Invoke x) :bool
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
 

[<Extension;Sealed>]
type Find() =
    inherit Default1()
    static member val Instance = Find()
    [<Extension>]static member inline Find (x         , f, [<Optional>]impl:Default1) = Seq.find   f (ToSeq.Invoke x) :'T
    [<Extension>]static member        Find (x:Id<'T>  , f, [<Optional>]impl:Find    ) = List.find  f [x.getValue]
    [<Extension>]static member        Find (x:seq<'T> , f, [<Optional>]impl:Find    ) = Seq.find   f x
    [<Extension>]static member        Find (x:list<'T>, f, [<Optional>]impl:Find    ) = List.find  f x
    [<Extension>]static member        Find (x:'T []   , f, [<Optional>]impl:Find    ) = Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Find: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Find.Instance,    source, predicate)  :'T


[<Extension;Sealed>]
type TryFind() =
    inherit Default1()
    static member val Instance = TryFind()
    [<Extension>]static member inline TryFind (x         , f, [<Optional>]impl:Default1) = Seq.tryFind   f (ToSeq.Invoke x)  :'T option
    [<Extension>]static member        TryFind (x:Id<'T>  , f, [<Optional>]impl:TryFind ) = List.tryFind  f [x.getValue]
    [<Extension>]static member        TryFind (x:seq<'T> , f, [<Optional>]impl:TryFind ) = Seq.tryFind   f x
    [<Extension>]static member        TryFind (x:list<'T>, f, [<Optional>]impl:TryFind ) = List.tryFind  f x
    [<Extension>]static member        TryFind (x:'T []   , f, [<Optional>]impl:TryFind ) = Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member TryFind: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (TryFind.Instance, source, predicate)   :'T option


[<Extension;Sealed>]
type Head() =
    inherit Default1()
    static member val Instance = Head()
    [<Extension>]static member inline Head (x              , [<Optional>]impl:Default1) = Seq.head (ToSeq.Invoke x) :'T      
    [<Extension>]static member        Head (x:'t list      , [<Optional>]impl:Head    ) = List.head x
    [<Extension>]static member        Head (x:'t []        , [<Optional>]impl:Head    ) = x.[0]
    [<Extension>]static member        Head (x:Id<'T>       , [<Optional>]impl:Head ) = x.getValue
    [<Extension>]static member        Head (x:string       , [<Optional>]impl:Head    ) = x.[0]
    [<Extension>]static member        Head (x:StringBuilder, [<Optional>]impl:Head    ) = x.ToString().[0]
    [<Extension>]static member        Head (x:'t seq       , [<Optional>]impl:Head    ) = Seq.head x

    static member inline Invoke (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Head: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Head.Instance,    source)  :'T


[<Extension;Sealed>]
type TryHead() =
    inherit Default1()
    static member val Instance = TryHead()
    [<Extension>]static member inline TryHead (x              , [<Optional>]impl:Default1) = let x = ToSeq.Invoke x in if Seq.isEmpty x then None else Some (Seq.head x) :'T option  
    [<Extension>]static member        TryHead (x:'t list      , [<Optional>]impl:TryHead ) = match x with [] -> None | _ -> Some (List.head x)
    [<Extension>]static member        TryHead (x:'t []        , [<Optional>]impl:TryHead ) = if Array.length x = 0 then None else Some x.[0]
    [<Extension>]static member        TryHead (x:Id<'T>       , [<Optional>]impl:TryHead ) = Some x.getValue
    [<Extension>]static member        TryHead (x:string       , [<Optional>]impl:TryHead ) = if String.length x = 0 then None else Some x.[0]   
    [<Extension>]static member        TryHead (x:StringBuilder, [<Optional>]impl:TryHead ) = if x.Length = 0 then None else Some (x.ToString().[0])
    [<Extension>]static member        TryHead (x:'t seq       , [<Optional>]impl:TryHead ) = if Seq.isEmpty x then None else Some (Seq.head x)

    static member inline Invoke (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryHead: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (TryHead.Instance,    source)  :'T option


[<Extension;Sealed>]
type Pick() =
    inherit Default1()
    static member val Instance = Pick()
    [<Extension>]static member inline Pick (x         , f:_->'U option, [<Optional>]impl:Default1) = Seq.pick   f (ToSeq.Invoke x) :'U
    [<Extension>]static member        Pick (x:Id<'T>  , f:_->'U option, [<Optional>]impl:Pick    ) = List.pick  f [x.getValue]
    [<Extension>]static member        Pick (x:seq<'T> , f:_->'U option, [<Optional>]impl:Pick    ) = Seq.pick   f x
    [<Extension>]static member        Pick (x:list<'T>, f:_->'U option, [<Optional>]impl:Pick    ) = List.pick  f x
    [<Extension>]static member        Pick (x:'T []   , f:_->'U option, [<Optional>]impl:Pick    ) = Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b ) : (static member Pick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Pick.Instance,   source, chooser)    :'U


[<Extension;Sealed>]
type TryPick() =
    inherit Default1()
    static member val Instance = TryPick()
    [<Extension>]static member inline TryPick (x         , f:_->'U option, [<Optional>]impl:Default1) = Seq.tryPick   f (ToSeq.Invoke x)  :'U option
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
    [<Extension>]static member        Filter (x:'t seq        , p, [<Optional>]impl:Default2) = Seq.filter p x
    [<Extension>]static member inline Filter (x:'Foldable'T   , p, [<Optional>]impl:Default1) = x |> ToSeq.Invoke |> Seq.filter p |> FromSeq.Invoke :'Foldable'T
    [<Extension>]static member        Filter (x:'t Set        , p, [<Optional>]impl:Filter  ) = Set.filter p x
    [<Extension>]static member        Filter (x:'t option     , p, [<Optional>]impl:Filter  ) = match x with None -> None | Some a -> if p a then x else None
    [<Extension>]static member        Filter (x:'t list       , p, [<Optional>]impl:Filter  ) = List.filter  p x
    [<Extension>]static member        Filter (x:'t []         , p, [<Optional>]impl:Filter  ) = Array.filter p x
    [<Extension>]static member        Filter (x:'t IObservable, p, [<Optional>]impl:Filter  ) = Observable.filter p x
    [<Extension>]static member        Filter (x:'t ResizeArray, p, [<Optional>]impl:Filter  ) = ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (i:^i, b:^b, f) = ((^i or ^b) : (static member Filter: _*_*_ -> ^b) b, f, i)
        let inline call (i:'i, b:'b, f:'f) = call_2 (i, b, f)
        call (Filter.Instance, x, predicate)