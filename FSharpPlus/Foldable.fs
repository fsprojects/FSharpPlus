#nowarn "77"
// Warn FS0077 -> Member constraints with the name 'get_Item' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// Those .NET types are string and array. String is explicitely handled here and array through the seq overload.

namespace FsControl.Internals

open FsControl


type _Dual<'T> =
    struct
        val Value : 'T
        new (value: 'T) = {Value = value}
    end
    static member inline get_Empty() = _Dual(Empty.Invoke())                                         : _Dual<'m>
    static member inline Append (x: _Dual<'m>, y: _Dual<'m>) = _Dual (Append.Invoke y.Value x.Value) : _Dual<'m>

type _Endo<'T> =
    struct
        val Value : 'T -> 'T
        new (value: 'T -> 'T) = {Value = value}
    end
    static member get_Empty() = _Endo id                                               : _Endo<'m>
    static member Append (f : _Endo<'m>, g : _Endo<'m>) = _Endo (f.Value << g.Value)   : _Endo<'m>

namespace FsControl

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections
open System.Collections.Generic
open FsControl.Internals
open FsControl.Internals.Prelude
open FSharpPlus



[<Extension;Sealed>]
type ToSeq =
    inherit Default1
    [<Extension>]static member inline ToSeq (x:'S when 'S :> Collections.IEnumerable, [<Optional>]_impl:Default2) = let _f i x :'T = (^S : (member get_Item : int -> 'T) x, i) in Seq.cast<'T> x : seq<'T>
    [<Extension>]static member inline ToSeq (x:'Foldable, [<Optional>]_impl:Default1) = ((^Foldable) : (static member ToSeq: ^Foldable -> seq<'t>) x)
                 static member inline ToSeq (_:'T when 'T : null and 'T :struct     ,             _   :ToSeq   ) = ()
    [<Extension>]static member        ToSeq (x:seq<'T>   , [<Optional>]_impl:ToSeq) = x
    [<Extension>]static member        ToSeq (x:Text.StringBuilder,  _:ToSeq) = x.ToString() :> seq<char>
    [<Extension>]static member        ToSeq (x:string            ,  _:ToSeq) = x            :> seq<char>
    [<Extension>]static member        ToSeq (x:option<'T>, [<Optional>]_impl:ToSeq) = match x with Some x -> Seq.singleton x | None -> Seq.empty

    static member inline Invoke (source:'Collection'T)  : seq<'T>  =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToSeq: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToSeq> , source)


[<Extension;Sealed>]
type ToList =
    inherit Default1
    [<Extension>]static member inline ToList (x               , [<Optional>]_impl:Default1) = x |> ToSeq.Invoke |> Seq.toList
    [<Extension>]static member        ToList (x:seq<'a>       , [<Optional>]_impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:Set<'a>       , [<Optional>]_impl:ToList  ) = Set.toList x
    [<Extension>]static member        ToList (x:string        , [<Optional>]_impl:ToList  ) = x.ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:StringBuilder , [<Optional>]_impl:ToList  ) = x.ToString().ToCharArray() |> Array.toList
    [<Extension>]static member        ToList (x:'a []         , [<Optional>]_impl:ToList  ) = Array.toList x
    [<Extension>]static member        ToList (x:'a ResizeArray, [<Optional>]_impl:ToList  ) = Seq.toList x
    [<Extension>]static member        ToList (x:'a Id         , [<Optional>]_impl:ToList  ) = [x.getValue]
    [<Extension>]static member        ToList (x:list<'a>      , [<Optional>]_impl:ToList  ) = x

    static member inline Invoke  value :'t list = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToList: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToList> , value)


[<Extension;Sealed>]
type ToArray =
    inherit Default1
    [<Extension>]static member inline ToArray (x               , [<Optional>]_impl:Default1) = x |> ToSeq.Invoke |> Seq.toArray
    [<Extension>]static member        ToArray (x:seq<'a>       , [<Optional>]_impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:Set<'a>       , [<Optional>]_impl:ToArray ) = Set.toArray x
    [<Extension>]static member        ToArray (x:string        , [<Optional>]_impl:ToArray ) = x.ToCharArray()
    [<Extension>]static member        ToArray (x:StringBuilder , [<Optional>]_impl:ToArray ) = x.ToString().ToCharArray()
    [<Extension>]static member        ToArray (x:'a []         , [<Optional>]_impl:ToArray ) = x
    [<Extension>]static member        ToArray (x:'a ResizeArray, [<Optional>]_impl:ToArray ) = Seq.toArray x
    [<Extension>]static member        ToArray (x:'a Id         , [<Optional>]_impl:ToArray ) = [|x.getValue|]
    [<Extension>]static member        ToArray (x:list<'a>      , [<Optional>]_impl:ToArray ) = List.toArray x

    static member inline Invoke  value : 't [] = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ToArray: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToArray> , value)


type OfSeq =
    inherit Default1
    static member inline OfSeq (x:seq<'t>                 , _:'Foldable'T                     , _:Default5) = x |> Seq.map Return.Invoke |> Concat.Invoke :'Foldable'T
    static member        OfSeq (x:seq<'t>                 , _:seq<'t>                         , _:Default4) = x
    static member        OfSeq (x:seq<'t>                 , _:ICollection<'t>                 , _:Default4) = let d = ResizeArray() in Seq.iter d.Add x; d:> ICollection<'t>
    static member        OfSeq (x:seq<'t>                 , _:IList<'t>                       , _:Default4) = let d = ResizeArray() in Seq.iter d.Add x; d:> IList<'t>
    static member        OfSeq (x:seq<'t>                 , _:IList                           , _:Default4) = let d = ResizeArray() in Seq.iter d.Add x; d:> IList
    static member        OfSeq (x:seq<'k*'v>              , _:IDictionary<'k,'v>              , _:Default4) = dict x
    static member        OfSeq (x:seq<KeyValuePair<'k,'v>>, _:IDictionary<'k,'v>              , _:Default4) = x |> Seq.map (function (KeyValue x) -> x) |> dict
    static member        OfSeq (x:seq<'k*'v>              , _:IDictionary                     , _:Default4) = let d = Hashtable() in x |> Seq.iter d.Add; d :> IDictionary
    static member        OfSeq (x:seq<KeyValuePair<'k,'v>>, _:IDictionary                     , _:Default4) = let d = Hashtable() in x |> Seq.iter (function (KeyValue x) -> d.Add x); d :> IDictionary
    static member inline OfSeq (x:seq<'t>                 , _:'R                              , _:Default3) = (^R : (new : seq<'t> -> ^R) x) : 'R
    static member inline OfSeq (x:seq<KeyValuePair<'k,'v>>, _:'R                              , _:Default3) = (^R : (new : seq<'k*'v> -> ^R) (Seq.map (function (KeyValue x) -> x) x)) : 'R
    static member inline OfSeq (x:seq<'t>                 , _:'F                              , _:Default2) = let c = new 'F() in (Seq.iter (fun t -> ( ^F : (member Add : 't -> ^R) c, t) |> ignore) x); c
    static member        OfSeq (x:seq<'t>                 , _:'T when 'T :> ICollection<'t>   , _:Default1) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member        OfSeq (x:seq<'k*'v>              , _:'T when 'T :> IDictionary       , _:Default1) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member        OfSeq (x:seq<KeyValuePair<'k,'v>>, _:'T when 'T :> IDictionary       , _:Default1) = let d = new 'T() in x |> Seq.iter (function (KeyValue x) -> d.Add x); d
    static member        OfSeq (x:seq<'k*'v>              , _:'T when 'T :> IDictionary<'k,'v>, _:OfSeq   ) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member        OfSeq (x:seq<KeyValuePair<'k,'v>>, _:'T when 'T :> IDictionary<'k,'v>, _:OfSeq   ) = let d = new 'T() in x |> Seq.iter d.Add; d
    static member inline OfSeq (x:seq<'t>                 , _:'UserType                       , _:OfSeq   ) = ((^UserType) : (static member OfSeq: seq<'t> -> ^UserType) x)
    static member        OfSeq (x                         , _:'t []                           , _:OfSeq   ) = Array.ofSeq<'t> x
    static member        OfSeq (x                         , _:'t list                         , _:OfSeq   ) = List.ofSeq<'t> x
    static member        OfSeq (x:seq<char>               , _:string                          , _:OfSeq   ) = String.Join ("", Array.ofSeq x)
    static member        OfSeq (x:seq<char>               , _:Text.StringBuilder              , _:OfSeq   ) = (StringBuilder(), x) ||> Seq.fold (fun x -> x.Append)
    static member        OfSeq (x:seq<'t>                 , _:Stack<'t>                       , _:OfSeq   ) = Generic.Stack x

    static member inline Invoke  (value :seq<'t>) = 
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member OfSeq: _*_*_ -> _) s, b, a)
        let inline call (a:'a, s) = call_2 (a, Unchecked.defaultof<'r>, s) :'r
        call (Unchecked.defaultof<OfSeq>, value)


type OfList =

#if NOTNET35
    static member OfList (_:string        , _:OfList) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList)
    static member OfList (_:StringBuilder , _:OfList) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList))
#else
    static member OfList (_:string        , _:OfList) = fun (x:list<char>) -> String.Join("",  x |> Array.ofList |> Array.map string)
    static member OfList (_:StringBuilder , _:OfList) = fun (x:list<char>) -> new StringBuilder(String.Join("", x |> Array.ofList |> Array.map string))
#endif
    static member OfList (_:'a []         , _:OfList) = Array.ofList<'a>
    static member OfList (_:'a ResizeArray, _:OfList) = fun (x:list<'a>)   -> ResizeArray x
    static member OfList (_:list<'a>      , _:OfList) = id<list<'a>>
    static member OfList (_:Set<'a>       , _:OfList) = Set.ofList<'a>
    static member OfList (_:seq<'a>       , _:OfList) = Seq.ofList<'a>

    static member inline Invoke (value :list<'t>) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member OfList: _*_ -> _) b, a)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<OfList> value


type FoldBack =
    inherit Default1
    static member inline FoldBack (x:'F           , f:'a->'b->'b, z:'b , [<Optional>]_impl:Default2) = List.foldBack  f (ToList.Invoke x) z
    static member inline FoldBack (x:'F           , f:'a->'b->'b, z:'b , [<Optional>]_impl:Default1) = ((^F) : (static member FoldBack: ^F -> _ -> _-> ^b) x, f, z)
    static member        FoldBack (x:seq<_>       , f           , z    , [<Optional>]_impl:FoldBack) = List.foldBack  f (Seq.toList x) z
    static member        FoldBack (x:option<_>    , f           , z    , [<Optional>]_impl:FoldBack) = match x with Some x -> f x z | _ -> z
    static member        FoldBack (x:list<_>      , f           , z    , [<Optional>]_impl:FoldBack) = List.foldBack          f x z
    static member        FoldBack (x:_ []         , f           , z    , [<Optional>]_impl:FoldBack) = Array.foldBack         f x z
    static member        FoldBack (x:Set<_>       , f           , z    , [<Optional>]_impl:FoldBack) = Set.foldBack           f x z
    static member        FoldBack (x:_ ResizeArray, f           , z    , [<Optional>]_impl:FoldBack) = Array.foldBack         f (x.ToArray()) z
    static member        FoldBack (x:string       , f           , z    , [<Optional>]_impl:FoldBack) = Array.foldBack f (x.ToCharArray()) z
    static member        FoldBack (x:StringBuilder, f           , z    , [<Optional>]_impl:FoldBack) = Array.foldBack f (x.ToString().ToCharArray()) z   
    static member        FoldBack (x:Id<'a>       , f           , z    , [<Optional>]_impl:FoldBack) = f x.getValue z

    static member inline Invoke (folder:'T->'State->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member FoldBack: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<FoldBack>, foldable, folder, state)


type FoldMap =
    inherit Default1
    static member inline FromFoldFoldBack f x = FoldBack.Invoke (Append.Invoke << f) (Empty.Invoke()) x  
    
    static member inline FoldMap (x          , f, [<Optional>]_impl:Default1) = Seq.fold   (fun x y -> Append.Invoke x (f y)) (Empty.Invoke()) x
    static member inline FoldMap (x:option<_>, f, [<Optional>]_impl:FoldMap ) = match x with Some x -> f x | _ -> Empty.Invoke()
    static member        FoldMap (x:Id<_>    , f, [<Optional>]_impl:FoldMap ) = f x.getValue
    static member inline FoldMap (x:seq<_>   , f, [<Optional>]_impl:FoldMap ) = Seq.fold   (fun x y -> Append.Invoke x (f y)) (Empty.Invoke()) x
    static member inline FoldMap (x:list<_>  , f, [<Optional>]_impl:FoldMap ) = List.fold  (fun x y -> Append.Invoke x (f y)) (Empty.Invoke()) x
    static member inline FoldMap (x:Set<_>   , f, [<Optional>]_impl:FoldMap ) = Seq.fold   (fun x y -> Append.Invoke x (f y)) (Empty.Invoke()) x
    static member inline FoldMap (x:_ []     , f, [<Optional>]_impl:FoldMap ) = Array.fold (fun x y -> Append.Invoke x (f y)) (Empty.Invoke()) x

    static member inline Invoke (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member FoldMap: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<FoldMap>, x, f)

type FoldBack with
    static member inline FromFoldMap f z x = let (f : _Endo<'t>) = FoldMap.Invoke (_Endo << f) x in f.Value z


type Fold =
    inherit Default1

    static member inline FromFoldMap f z t = let (f : _Dual<_Endo<'t>>) = FoldMap.Invoke (_Dual << _Endo << flip f) t in f.Value.Value z

    static member inline Fold (x          , f, z, [<Optional>]_impl:Default2) = Seq.fold f z (ToSeq.Invoke x)
    static member inline Fold (x:'F       , f:'b->'a->'b, z:'b , [<Optional>]_impl:Default1) = ((^F) : (static member Fold: ^F -> _ -> _-> ^b) x, f, z)
    static member        Fold (x:option<_>, f, z, [<Optional>]_impl:Fold    ) = match x with Some x -> f z x | _ -> z
    static member        Fold (x:Id<_>    , f, z, [<Optional>]_impl:Fold    ) = f z x.getValue
    static member        Fold (x:seq<_>   , f, z, [<Optional>]_impl:Fold    ) = Seq.fold               f z x
    static member        Fold (x:list<_>  , f, z, [<Optional>]_impl:Fold    ) = List.fold              f z x
    static member        Fold (x:Set<_>   , f, z, [<Optional>]_impl:Fold    ) = Set.fold               f z x
    static member        Fold (x: _ []    , f, z, [<Optional>]_impl:Fold    ) = Array.fold             f z x

    static member inline Invoke (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_2 (a:^a, b:^b, f, z) = ((^a or ^b) : (static member Fold: _*_*_*_ -> _) b, f, z, a)
        let inline call (a:'a, b:'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<Fold>, foldable, folder, state)
    
 
type Exists =
    inherit Default1
    static member inline Exists (x               , f, [<Optional>]_impl:Default1) = Seq.exists    f (ToSeq.Invoke x) :bool
    static member        Exists (x:Id<'T>        , f, [<Optional>]_impl:Exists  ) = f x.getValue :bool
    static member        Exists (x:seq<'a>       , f, [<Optional>]_impl:Exists  ) = Seq.exists    f x
    static member        Exists (x:list<'a>      , f, [<Optional>]_impl:Exists  ) = List.exists   f x
    static member        Exists (x:'a []         , f, [<Optional>]_impl:Exists  ) = Array.exists  f x
    static member        Exists (x:Set<'a>       , f, [<Optional>]_impl:Exists  ) = Set.exists    f x
    static member        Exists (x:string        , f, [<Optional>]_impl:Exists  ) = String.exists f x
    static member        Exists (x:'a ResizeArray, f, [<Optional>]_impl:Exists  ) = Seq.exists    f x
    static member        Exists (x:StringBuilder , f, [<Optional>]_impl:Exists  ) = x.ToString() |> String.exists f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, f) = ((^a or ^b) : (static member Exists: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, f)
        call (Unchecked.defaultof<Exists>,  source, predicate)        :bool
 

type ForAll =
    inherit Default1
    static member inline ForAll (x               , f, [<Optional>]_impl:Default1) = Seq.forall    f (ToSeq.Invoke x) :bool
    static member        ForAll (x:Id<'T>        , f, [<Optional>]_impl:ForAll  ) = f x.getValue :bool
    static member        ForAll (x:seq<'a>       , f, [<Optional>]_impl:ForAll  ) = Seq.forall    f x
    static member        ForAll (x:list<'a>      , f, [<Optional>]_impl:ForAll  ) = List.forall   f x
    static member        ForAll (x:'a []         , f, [<Optional>]_impl:ForAll  ) = Array.forall  f x
    static member        ForAll (x:Set<'a>       , f, [<Optional>]_impl:ForAll  ) = Set.forall    f x
    static member        ForAll (x:string        , f, [<Optional>]_impl:ForAll  ) = String.forall f x
    static member        ForAll (x:'a ResizeArray, f, [<Optional>]_impl:ForAll  ) = Seq.forall    f x
    static member        ForAll (x:StringBuilder , f, [<Optional>]_impl:ForAll  ) = x.ToString() |> String.forall f

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, f) = ((^a or ^b) : (static member ForAll: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, f) = call_3 (a, b, f)
        call (Unchecked.defaultof<ForAll>,  source, predicate)        :bool


type Find =
    inherit Default1
    static member inline Find (x         , f, [<Optional>]_impl:Default1) = Seq.find   f (ToSeq.Invoke x) :'T
    static member        Find (x:Id<'T>  , f, [<Optional>]_impl:Find    ) = List.find  f [x.getValue]
    static member        Find (x:seq<'T> , f, [<Optional>]_impl:Find    ) = Seq.find   f x
    static member        Find (x:list<'T>, f, [<Optional>]_impl:Find    ) = List.find  f x
    static member        Find (x:'T []   , f, [<Optional>]_impl:Find    ) = Array.find f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member Find: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<Find>,    source, predicate)  :'T


type TryFind =
    inherit Default1
    static member inline TryFind (x         , f, [<Optional>]_impl:Default1) = Seq.tryFind   f (ToSeq.Invoke x)  :'T option
    static member        TryFind (x:Id<'T>  , f, [<Optional>]_impl:TryFind ) = List.tryFind  f [x.getValue]
    static member        TryFind (x:seq<'T> , f, [<Optional>]_impl:TryFind ) = Seq.tryFind   f x
    static member        TryFind (x:list<'T>, f, [<Optional>]_impl:TryFind ) = List.tryFind  f x
    static member        TryFind (x:'T []   , f, [<Optional>]_impl:TryFind ) = Array.tryFind f x

    static member inline Invoke (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, f) = ((^a or ^b) : (static member TryFind: _*_*_ -> _) b, f, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<TryFind>, source, predicate)   :'T option


[<Extension;Sealed>]
type Head =
    inherit Default1
    [<Extension>]static member inline Head (x              , [<Optional>]_impl:Default1) = Seq.head (ToSeq.Invoke x) :'T      
    [<Extension>]static member        Head (x:'t list      , [<Optional>]_impl:Head    ) = List.head x
    [<Extension>]static member        Head (x:'t []        , [<Optional>]_impl:Head    ) = x.[0]
    [<Extension>]static member        Head (x:Id<'T>       , [<Optional>]_impl:Head ) = x.getValue
    [<Extension>]static member        Head (x:string       , [<Optional>]_impl:Head    ) = x.[0]
    [<Extension>]static member        Head (x:StringBuilder, [<Optional>]_impl:Head    ) = x.ToString().[0]
    [<Extension>]static member        Head (x:'t seq       , [<Optional>]_impl:Head    ) = Seq.head x

    static member inline Invoke (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Head: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Head>,    source)  :'T


[<Extension;Sealed>]
type TryHead =
    inherit Default1
    [<Extension>]static member inline TryHead (x              , [<Optional>]_impl:Default1) = let x = ToSeq.Invoke x in if Seq.isEmpty x then None else Some (Seq.head x) :'T option  
    [<Extension>]static member        TryHead (x:'t list      , [<Optional>]_impl:TryHead ) = match x with [] -> None | _ -> Some (List.head x)
    [<Extension>]static member        TryHead (x:'t []        , [<Optional>]_impl:TryHead ) = if Array.length x = 0 then None else Some x.[0]
    [<Extension>]static member        TryHead (x:Id<'T>       , [<Optional>]_impl:TryHead ) = Some x.getValue
    [<Extension>]static member        TryHead (x:string       , [<Optional>]_impl:TryHead ) = if String.length x = 0 then None else Some x.[0]   
    [<Extension>]static member        TryHead (x:StringBuilder, [<Optional>]_impl:TryHead ) = if x.Length = 0 then None else Some (x.ToString().[0])
    [<Extension>]static member        TryHead (x:'t seq       , [<Optional>]_impl:TryHead ) = if Seq.isEmpty x then None else Some (Seq.head x)

    static member inline Invoke (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryHead: _*_ -> _) b, a)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<TryHead>,    source)  :'T option


type Pick =
    inherit Default1
    static member inline Pick (x         , f:_->'U option, [<Optional>]_impl:Default1) = Seq.pick   f (ToSeq.Invoke x) :'U
    static member        Pick (x:Id<'T>  , f:_->'U option, [<Optional>]_impl:Pick    ) = List.pick  f [x.getValue]
    static member        Pick (x:seq<'T> , f:_->'U option, [<Optional>]_impl:Pick    ) = Seq.pick   f x
    static member        Pick (x:list<'T>, f:_->'U option, [<Optional>]_impl:Pick    ) = List.pick  f x
    static member        Pick (x:'T []   , f:_->'U option, [<Optional>]_impl:Pick    ) = Array.pick f x

    static member inline Invoke (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b ) : (static member Pick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<Pick>,   source, chooser)    :'U


type TryPick =
    inherit Default1
    static member inline TryPick (x         , f:_->'U option, [<Optional>]_impl:Default1) = Seq.tryPick   f (ToSeq.Invoke x)  :'U option
    static member        TryPick (_:Id<'T>  , _:_->'U option, [<Optional>]_impl:TryPick ) = invalidOp "TryPick on ID" :'U option
    static member        TryPick (x:seq<'T> , f:_->'U option, [<Optional>]_impl:TryPick ) = Seq.tryPick   f x
    static member        TryPick (x:list<'T>, f:_->'U option, [<Optional>]_impl:TryPick ) = List.tryPick  f x
    static member        TryPick (x:'T []   , f:_->'U option, [<Optional>]_impl:TryPick ) = Array.tryPick f x

    static member inline Invoke  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_2 (a:^a, b:^b, x) = ((^a or ^b) : (static member TryPick: _*_*_ -> _) b, x, a)
        let inline call (a:'a, b:'b, x:'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<TryPick>, source, chooser)      :'U option

 
type Filter =
    inherit Default1
    static member        Filter (x:'t seq        , p, [<Optional>]_impl:Default2) = Seq.filter p x
    static member inline Filter (x:'Foldable'T   , p, [<Optional>]_impl:Default1) = x |> ToSeq.Invoke |> Seq.filter p |> OfSeq.Invoke :'Foldable'T
    static member        Filter (x:'t Set        , p, [<Optional>]_impl:Filter  ) = Set.filter p x
    static member        Filter (x:'t option     , p, [<Optional>]_impl:Filter  ) = match x with None -> None | Some a -> if p a then x else None
    static member        Filter (x:'t list       , p, [<Optional>]_impl:Filter  ) = List.filter  p x
    static member        Filter (x:'t []         , p, [<Optional>]_impl:Filter  ) = Array.filter p x
    static member        Filter (x:'t IObservable, p, [<Optional>]_impl:Filter  ) = Observable.filter p x
    static member        Filter (x:'t ResizeArray, p, [<Optional>]_impl:Filter  ) = ResizeArray(Seq.filter p x)

    static member inline Invoke (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_2 (i:^i, b:^b, f) = ((^i or ^b) : (static member Filter: _*_*_ -> ^b) b, f, i)
        let inline call (i:'i, b:'b, f:'f) = call_2 (i, b, f)
        call (Unchecked.defaultof<Filter>, x, predicate)


[<Extension;Sealed>]
type Intercalate =
    inherit Default1
                 static member inline Intercalate (x:'``Foldable<'Monoid>``, e:'Monoid, [<Optional>]_impl:Default2) = let f t x = match (t, x) with (true, _) , x -> (false, x) | (_, acc ) , x -> (false, Append.Invoke (Append.Invoke acc e) x) in Fold.Invoke f (true, Empty.Invoke()) x |> snd
                 static member inline Intercalate (x:seq<'``Foldable<'T>``>, e:'``Foldable<'T>``, [<Optional>]_impl:Default1) = x |> Seq.map ToSeq.Invoke |> Seq.intercalate (ToSeq.Invoke e) |> OfSeq.Invoke :'``Foldable<'T>``
                 static member inline Intercalate (_:seq<'``Foldable<'T>``>, _ : ^t when ^t : null and ^t : struct, [<Optional>]_impl:Default1) = id
    [<Extension>]static member        Intercalate (x:seq<list<'T>>         , e:list<'T>         , [<Optional>]_impl:Intercalate) = x |> Seq.intercalate e |> Seq.toList
    [<Extension>]static member        Intercalate (x:seq<'T []>            , e:'T []            , [<Optional>]_impl:Intercalate) = x |> Seq.intercalate e |> Seq.toArray
    [<Extension>]static member        Intercalate (x:seq<string>           , e:string           , [<Optional>]_impl:Intercalate) = String.Join(e, x)
    [<Extension>]static member        Intercalate (x:seq<StringBuilder>    , e:StringBuilder    , [<Optional>]_impl:Intercalate) = StringBuilder(String.Join(e.ToString(), Seq.map (fun x -> x.ToString()) x))
 
    static member inline Invoke (sep:'Monoid) (source:'``Foldable<'Monoid>``) =
        let inline call_2 (a:^a, b:^b, s:^c) = ((^a or ^c) : (static member Intercalate: _*_*_ -> _) b, s, a)
        let inline call (a:'a, b:'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Intercalate>, source,sep) :'Monoid


[<Extension;Sealed>]
type Intersperse =
    inherit Default1
    [<Extension>]static member inline Intersperse (x:'``Foldable<'T>``, e:'T, [<Optional>]_impl:Default1   ) = x |> ToSeq.Invoke |> Seq.intersperse e |> OfSeq.Invoke :'``Foldable<'T>``
    [<Extension>]static member        Intersperse (x:list<'T>         , e:'T, [<Optional>]_impl:Intersperse) = x |> List.toSeq   |> Seq.intersperse e |> Seq.toList
    [<Extension>]static member        Intersperse (x:'T []            , e:'T, [<Optional>]_impl:Intersperse) = x |> Array.toSeq  |> Seq.intersperse e |> Seq.toArray
 
    static member inline Invoke (sep:'T) (source:'``Foldable<'T>``) =
        let inline call_2 (a:^a, b:^b, s) = ((^a or ^b) : (static member Intersperse: _*_*_ -> _) b, s, a)
        let inline call (a:'a, b:'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Intersperse>, source,sep) :'``Foldable<'T>``