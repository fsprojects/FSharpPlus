namespace FSharpPlus.Control

open System
open System.Text
open System.Runtime.CompilerServices
open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices
open FSharpPlus
open FSharpPlus.Internals


type OfSeq =
    inherit Default1
    static member inline OfSeq (x: seq<'t>                 , _: '``Foldable'<T>``               , _: Default5) = x |> Seq.map Return.Invoke |> Sum.Invoke : '``Foldable'<T>``
    static member        OfSeq (x: seq<'t>                 , _: seq<'t>                         , _: Default4) = x
    static member        OfSeq (x: seq<'t>                 , _: ICollection<'t>                 , _: Default4) = let d = ResizeArray () in Seq.iter d.Add x; d :> ICollection<'t>
    static member        OfSeq (x: seq<'t>                 , _: IList<'t>                       , _: Default4) = let d = ResizeArray () in Seq.iter d.Add x; d :> IList<'t>
    static member        OfSeq (x: seq<'t>                 , _: IList                           , _: Default4) = let d = ResizeArray () in Seq.iter d.Add x; d :> IList
    static member        OfSeq (x: seq<'k*'v>              , _: IReadOnlyDictionary<'k,'v>      , _: Default4) = Dict.toIReadOnlyDictionary (dict x)
    static member        OfSeq (x: seq<KeyValuePair<'k,'v>>, _: IReadOnlyDictionary<'k,'v>      , _: Default4) = x |> Seq.map (|KeyValue|) |> dict |> Dict.toIReadOnlyDictionary
    static member        OfSeq (x: seq<'k*'v>              , _: IDictionary<'k,'v>              , _: Default4) = dict x
    static member        OfSeq (x: seq<KeyValuePair<'k,'v>>, _: IDictionary<'k,'v>              , _: Default4) = x |> Seq.map (|KeyValue|) |> dict
    static member        OfSeq (x: seq<'k*'v>              , _: IDictionary                     , _: Default4) = let d = Hashtable () in x |> Seq.iter d.Add; d :> IDictionary
    static member        OfSeq (x: seq<KeyValuePair<'k,'v>>, _: IDictionary                     , _: Default4) = let d = Hashtable () in x |> Seq.iter (function (KeyValue x) -> d.Add x); d :> IDictionary
    static member inline OfSeq (x: seq<'t>                 , _: 'R                              , _: Default3) = (^R : (new : seq<'t> -> ^R) x) : 'R
    static member inline OfSeq (x: seq<KeyValuePair<'k,'v>>, _: 'R                              , _: Default3) = (^R : (new : seq<'k*'v> -> ^R) (Seq.map (|KeyValue|) x)) : 'R
    static member inline OfSeq (x: seq<'t>                 , _: 'F                              , _: Default2) = let c = new 'F () in (Seq.iter (fun t -> ( ^F : (member Add : 't -> ^R) c, t) |> ignore) x); c
    static member        OfSeq (x: seq<'t>                 , _: 'T when 'T :> ICollection<'t>   , _: Default1) = let d = new 'T () in x |> Seq.iter d.Add; d
    static member        OfSeq (x: seq<'k*'v>              , _: 'T when 'T :> IDictionary       , _: Default1) = let d = new 'T () in x |> Seq.iter d.Add; d
    static member        OfSeq (x: seq<KeyValuePair<'k,'v>>, _: 'T when 'T :> IDictionary       , _: Default1) = let d = new 'T () in x |> Seq.iter (function (KeyValue x) -> d.Add x); d
    static member        OfSeq (x: seq<'k*'v>              , _: 'T when 'T :> IDictionary<'k,'v>, _: OfSeq   ) = let d = new 'T () in x |> Seq.iter d.Add; d
    static member        OfSeq (x: seq<KeyValuePair<'k,'v>>, _: 'T when 'T :> IDictionary<'k,'v>, _: OfSeq   ) = let d = new 'T () in x |> Seq.iter d.Add; d
    static member inline OfSeq (x: seq<'t>                 , _: 'UserType                       , _: OfSeq   ) = (^UserType : (static member OfSeq : seq<'t> -> ^UserType) x)
    static member        OfSeq (x                          , _: 't []                           , _: OfSeq   ) = Array.ofSeq<'t> x
    static member        OfSeq (x                          , _: 't list                         , _: OfSeq   ) = List.ofSeq<'t> x
    static member        OfSeq (x: seq<char>               , _: string                          , _: OfSeq   ) = String.Join ("", Array.ofSeq x)
    static member        OfSeq (x: seq<char>               , _: Text.StringBuilder              , _: OfSeq   ) = (StringBuilder (), x) ||> Seq.fold (fun x -> x.Append)
    static member        OfSeq (x: seq<'t>                 , _: Stack<'t>                       , _: OfSeq   ) = Generic.Stack x

    static member inline Invoke (value: seq<'t>) = 
        let inline call_2 (a: ^a, b: ^b, s) = ((^a or ^b) : (static member OfSeq : _*_*_ -> _) s, b, a)
        let inline call (a: 'a, s) = call_2 (a, Unchecked.defaultof<'r>, s) : 'r
        call (Unchecked.defaultof<OfSeq>, value)


type OfList =
    inherit Default1
    static member inline OfList (x: list<'t>                 , _: '``Foldable'<T>``               , _: Default5) = x |> List.map Return.Invoke |> Sum.Invoke : '``Foldable'<T>``
    static member        OfList (x: list<'t>                 , _: seq<'t>                         , _: Default4) = List.toSeq x
    static member        OfList (x: list<'t>                 , _: ICollection<'t>                 , _: Default4) = let d = ResizeArray () in List.iter d.Add x; d :> ICollection<'t>
    static member        OfList (x: list<'t>                 , _: IList<'t>                       , _: Default4) = let d = ResizeArray () in List.iter d.Add x; d :> IList<'t>
    static member        OfList (x: list<'t>                 , _: IList                           , _: Default4) = let d = ResizeArray () in List.iter d.Add x; d :> IList
    static member        OfList (x: list<'k*'v>              , _: IReadOnlyDictionary<'k,'v>      , _: Default4) = Dict.toIReadOnlyDictionary (dict x)
    static member        OfList (x: list<KeyValuePair<'k,'v>>, _: IReadOnlyDictionary<'k,'v>      , _: Default4) = x |> List.map (|KeyValue|) |> dict |> Dict.toIReadOnlyDictionary
    static member        OfList (x: list<'k*'v>              , _: IDictionary<'k,'v>              , _: Default4) = dict x
    static member        OfList (x: list<KeyValuePair<'k,'v>>, _: IDictionary<'k,'v>              , _: Default4) = x |> List.map (|KeyValue|) |> dict
    static member        OfList (x: list<'k*'v>              , _: IDictionary                     , _: Default4) = let d = Hashtable () in x |> List.iter d.Add; d :> IDictionary
    static member        OfList (x: list<KeyValuePair<'k,'v>>, _: IDictionary                     , _: Default4) = let d = Hashtable () in x |> List.iter (function (KeyValue x) -> d.Add x); d :> IDictionary
    static member inline OfList (x: list<'t>                 , _: 'R                              , _: Default3) = (^R : (new : seq<'t> -> ^R) (List.toSeq x)) : 'R
    static member inline OfList (x: list<KeyValuePair<'k,'v>>, _: 'R                              , _: Default3) = (^R : (new : seq<'k*'v> -> ^R) (Seq.map (|KeyValue|) x)) : 'R
    static member inline OfList (x: list<'t>                 , _: 'F                              , _: Default2) = let c = new 'F () in (List.iter (fun t -> ( ^F : (member Add : 't -> ^R) c, t) |> ignore) x); c
    static member        OfList (x: list<'t>                 , _: 'T when 'T :> ICollection<'t>   , _: Default1) = let d = new 'T () in x |> List.iter d.Add; d
    static member        OfList (x: list<'k*'v>              , _: 'T when 'T :> IDictionary       , _: Default1) = let d = new 'T () in x |> List.iter d.Add; d
    static member        OfList (x: list<KeyValuePair<'k,'v>>, _: 'T when 'T :> IDictionary       , _: Default1) = let d = new 'T () in x |> List.iter (function (KeyValue x) -> d.Add x); d
    static member        OfList (x: list<'k*'v>              , _: 'T when 'T :> IDictionary<'k,'v>, _: OfList  ) = let d = new 'T () in x |> List.iter d.Add; d
    static member        OfList (x: list<KeyValuePair<'k,'v>>, _: 'T when 'T :> IDictionary<'k,'v>, _: OfList  ) = let d = new 'T () in x |> List.iter d.Add; d
    static member inline OfList (x: list<'t>                 , _: 'UserType                       , _: OfList  ) = (^UserType : (static member OfList : list<'t> -> ^UserType) x)
    static member        OfList (x                           , _: 't []                           , _: OfList  ) = Array.ofList<'t> x
    static member        OfList (x                           , _: 't list                         , _: OfList  ) = x
    static member        OfList (x: list<char>               , _: string                          , _: OfList  ) = String.Join ("", Array.ofList x)
    static member        OfList (x: list<char>               , _: Text.StringBuilder              , _: OfList  ) = (StringBuilder (), x) ||> List.fold (fun x -> x.Append)
    static member        OfList (x: list<'t>                 , _: Stack<'t>                       , _: OfList  ) = Generic.Stack x

    static member inline Invoke (value: list<'t>) = 
        let inline call_2 (a: ^a, b: ^b, s) = ((^a or ^b) : (static member OfList : _*_*_ -> _) s, b, a)
        let inline call (a: 'a, s) = call_2 (a, Unchecked.defaultof<'r>, s) : 'r
        call (Unchecked.defaultof<OfList>, value)


type Filter =
    inherit Default1
    static member        Filter (x: 't Set           , p, [<Optional>]_impl: Filter) = Set.filter p x
    static member        Filter (x: 't option        , p, [<Optional>]_impl: Filter) = match x with None -> None | Some a -> if p a then x else None
    static member        Filter (x: 't list          , p, [<Optional>]_impl: Filter) = List.filter  p x
    static member        Filter (x: 't []            , p, [<Optional>]_impl: Filter) = Array.filter p x
    static member        Filter (x: 't IObservable   , p, [<Optional>]_impl: Filter) = Observable.filter p x
    static member        Filter (x: 't ResizeArray   , p, [<Optional>]_impl: Filter) = ResizeArray (Seq.filter p x)

    static member inline Invoke (predicate: 'T->bool) (x: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (i: ^i, b: ^b, f) = ((^i or ^b) : (static member Filter : _*_*_ -> ^b) b, f, i)
        let inline call (i: 'i, b: 'b, f: 'f) = call_2 (i, b, f)
        call (Unchecked.defaultof<Filter>, x, predicate)

    static member inline InvokeOnInstance (predicate: 'T->bool) (source: '``Collection<'T>``) : '``Collection<'T>`` = (^``Collection<'T>``: (static member Filter : _*_ -> _) predicate, source)

type Filter with
    static member        Filter (x: 't seq             , p, [<Optional>]_impl: Default3) = Seq.filter p x
    static member inline Filter (x: '``Collection'<T>``, p, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.filter p |> OfSeq.Invoke : '``Collection'<T>``
    static member inline Filter (x: '``Collection'<T>``, p, [<Optional>]_impl: Default1) = Filter.InvokeOnInstance p x                       : '``Collection'<T>``
    static member inline Filter (_: ^t when ^t: null and ^t: struct  , _, _  : Default1) = ()


type Skip =
    inherit Default1
    static member inline Skip (x: '``Foldable<'T>``, n, [<Optional>]_impl: Default1) = x |> ToSeq.Invoke |> Seq.skip n |> OfSeq.Invoke : '``Foldable<'T>``
    static member        Skip (x: string           , n, [<Optional>]_impl: Skip    ) = x.[n..]
    static member        Skip (x: StringBuilder    , n, [<Optional>]_impl: Skip    ) = new StringBuilder(x.ToString().[n..])
    static member        Skip (x: 'a []            , n, [<Optional>]_impl: Skip    ) = x.[n..] : 'a []
    static member        Skip (x: 'a ResizeArray   , n, [<Optional>]_impl: Skip    ) = ResizeArray<'a> (Seq.skip n x)
    static member        Skip (x: list<'a>         , n, [<Optional>]_impl: Skip    ) = List.skip n x
    static member        Skip (x: 'a Id            , _, [<Optional>]_impl: Skip    ) = x

    static member inline Invoke (n: int) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Skip : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Skip>, source, n)


type Take =
    inherit Default1
    static member inline Take (x: '``Foldable<'T>``, n, [<Optional>]_impl: Default1) = x |> ToSeq.Invoke |> Seq.take n |> OfSeq.Invoke : '``Foldable<'T>``
    static member        Take (x: string           , n, [<Optional>]_impl: Take    ) = x.[..n-1]
    static member        Take (x: StringBuilder    , n, [<Optional>]_impl: Take    ) = new StringBuilder(x.ToString().[..n-1])
    static member        Take (x: 'a []            , n, [<Optional>]_impl: Take    ) = x.[..n-1] : 'a []
    static member        Take (x: 'a ResizeArray   , n, [<Optional>]_impl: Take    ) = ResizeArray<'a> (Seq.take n x)
    static member        Take (x: list<'a>         , n, [<Optional>]_impl: Take    ) = List.take n x
    static member        Take (x: 'a Id            , _, [<Optional>]_impl: Take    ) = x

    static member inline Invoke (n: int) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Take : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Take>, source, n)

type TakeWhile =
    inherit Default1
    static member inline TakeWhile (x: '``Foldable<'T>``, p, [<Optional>]_impl: Default1 ) = x |> ToSeq.Invoke |> Seq.takeWhile p |> OfSeq.Invoke : '``Foldable<'T>``
    static member        TakeWhile (x: 'a []            , p, [<Optional>]_impl: TakeWhile) = Array.takeWhile p x
    static member        TakeWhile (x: 'a ResizeArray   , p, [<Optional>]_impl: TakeWhile) = ResizeArray<'a> (Seq.takeWhile p x)
    static member        TakeWhile (x: list<'a>         , p, [<Optional>]_impl: TakeWhile) = List.takeWhile p x
    static member        TakeWhile (x: 'a Id            , _, [<Optional>]_impl: TakeWhile) = x

    static member inline Invoke (predicate: 'T->bool) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member TakeWhile : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<TakeWhile>, source, predicate)

type SkipWhile =
    inherit Default1
    static member inline SkipWhile (x: '``Foldable<'T>``, p, [<Optional>]_impl: Default1 ) = x |> ToSeq.Invoke |> Seq.skipWhile p |> OfSeq.Invoke : '``Foldable<'T>``
    static member        SkipWhile (x: 'a []            , p, [<Optional>]_impl: SkipWhile) = Array.skipWhile p x
    static member        SkipWhile (x: 'a ResizeArray   , p, [<Optional>]_impl: SkipWhile) = ResizeArray<'a> (Seq.skipWhile p x)
    static member        SkipWhile (x: list<'a>         , p, [<Optional>]_impl: SkipWhile) = List.skipWhile p x
    static member        SkipWhile (x: 'a Id            , _, [<Optional>]_impl: SkipWhile) = x

    static member inline Invoke (predicate: 'T->bool) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member SkipWhile : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<SkipWhile>, source, predicate)

type Drop =
    inherit Default1
    static member inline Drop (x: '``Foldable<'T>``, n, [<Optional>]_impl: Default1) = x |> ToSeq.Invoke |> Seq.drop n |> OfSeq.Invoke : '``Foldable<'T>``
    static member        Drop (x: string           , n, [<Optional>]_impl: Drop    ) = if n > 0 then (if x.Length > n then x.[n..] else "") else x
    static member        Drop (x: StringBuilder    , n, [<Optional>]_impl: Drop    ) = if n > 0 then (if x.Length > n then new StringBuilder(x.ToString().[n..]) else new StringBuilder ()) else new StringBuilder (string x)
    static member        Drop (x: 'a []            , n, [<Optional>]_impl: Drop    ) = if n > 0 then (if x.Length > n then x.[n..] else [||]) else x : 'a []
    static member        Drop (x: 'a ResizeArray   , n, [<Optional>]_impl: Drop    ) = ResizeArray<'a> (Seq.drop n x)
    static member        Drop (x: list<'a>         , n, [<Optional>]_impl: Drop    ) = List.drop n x
    static member        Drop (x: 'a Id            , _, [<Optional>]_impl: Drop    ) = x

    static member inline Invoke (n: int) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Drop : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Drop>, source, n)


type Limit =
    inherit Default1
    static member inline Limit (x: '``Foldable<'T>``, n, [<Optional>]_impl: Default1) = x |> ToSeq.Invoke |> Seq.truncate n |> OfSeq.Invoke : '``Foldable<'T>``
    static member        Limit (x: string           , n, [<Optional>]_impl: Limit   ) = if n < 1 then  ""  elif n < x.Length then x.[..n-1] else x
    static member        Limit (x: StringBuilder    , n, [<Optional>]_impl: Limit   ) = new StringBuilder(x.ToString().[..n-1])
    static member        Limit (x: 'a []            , n, [<Optional>]_impl: Limit   ) = if n < 1 then [||] elif n < x.Length then x.[..n-1] else x : 'a []
    static member        Limit (x: 'a ResizeArray   , n, [<Optional>]_impl: Limit   ) = ResizeArray<'a> (Seq.truncate n x)
    static member        Limit (x: list<'a>         , n, [<Optional>]_impl: Limit   ) = Seq.truncate n x |> Seq.toList
    static member        Limit (x: 'a Id            , _, [<Optional>]_impl: Limit   ) = x

    static member inline Invoke (n: int) (source: '``Collection<'T>``) : '``Collection<'T>`` =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Limit : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Limit>, source, n) 


type Choose =
    static member Choose (_: Id<'T>  , _: _->'U option, [<Optional>]_impl: Choose) = invalidOp "Choose on ID" :Id<'U>
    static member Choose (x: seq<'T> , f: _->'U option, [<Optional>]_impl: Choose) = Seq.choose   f x
    static member Choose (x: list<'T>, f: _->'U option, [<Optional>]_impl: Choose) = List.choose  f x
    static member Choose (x: 'T []   , f: _->'U option, [<Optional>]_impl: Choose) = Array.choose f x

    static member inline Invoke (chooser: 'T->'U option) (source: '``Collection<'T>``) =
        let inline call_3 (a: ^a, b: ^b, _: ^c, f) = ((^a or ^b or ^c) : (static member Choose : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, c) = call_3 (a, b, Unchecked.defaultof<'r>, c) : 'r
        call (Unchecked.defaultof<Choose>, source, chooser) : 'Collection'U


type Distinct =
    inherit Default1

    static member        Distinct (x: list<'a>, [<Optional>]_impl: Distinct) = List.distinct x
    static member        Distinct (x: 'a []   , [<Optional>]_impl: Distinct) = Array.distinct x

    static member inline Invoke (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Distinct : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Distinct>, source)
    static member inline InvokeOnInstance (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member Distinct : _->_) source) : ^``C<'T>``

    static member inline Distinct (x: '``Collection<'T>``  , [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.distinct |> OfSeq.Invoke         : '``Collection<'T>``
    static member inline Distinct (x: ^``Collection<'T>``  , [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member Distinct : _->_) x) : '``Collection<'T>``
    static member inline Distinct (_: ^t when ^t : null and ^t : struct, _mthd: Default1) = id


type DistinctBy =
    inherit Default1

    static member        DistinctBy (x: list<'a>   , f, [<Optional>]_impl: DistinctBy) = List.distinctBy  f x
    static member        DistinctBy (x: 'a []      , f, [<Optional>]_impl: DistinctBy) = Array.distinctBy f x

    static member inline Invoke (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member DistinctBy : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<DistinctBy>, source, projection)
    static member inline InvokeOnInstance (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member DistinctBy : _*_->_) projection, source) : ^``C<'T>``

    static member inline DistinctBy (x: '``Collection<'T>``, f              , [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.distinctBy f |> OfSeq.Invoke            : '``Collection<'T>``
    static member inline DistinctBy (x: ^``Collection<'T>``, f              , [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member DistinctBy : _*_->_) f, x) : '``Collection<'T>``
    static member inline DistinctBy (_:  ^t when ^t : null and ^t : struct, _ : 'T -> 'U, _mthd: Default1) = id


type GroupBy =
    static member GroupBy (x: Id<'T>  , f: 'T->'Key, _: Id<'Key*Id<'T>>    , [<Optional>]_impl: GroupBy) = let a = Id.run x in Id.create (f a, x)
    static member GroupBy (x: seq<'T> , f: 'T->'Key, _: seq<'Key*seq<'T>>  , [<Optional>]_impl: GroupBy) = Seq.groupBy f x
    static member GroupBy (x: list<'T>, f: 'T->'Key, _: list<'Key*list<'T>>, [<Optional>]_impl: GroupBy) = Seq.groupBy f x |> Seq.map (fun (x, y) -> x, Seq.toList  y) |> Seq.toList
    static member GroupBy (x: 'T []   , f: 'T->'Key, _: ('Key*('T [])) []  , [<Optional>]_impl: GroupBy) = Seq.groupBy f x |> Seq.map (fun (x, y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke    (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'Key * 'C<'T>>`` = 
        let inline call_3 (a: ^a, b: ^b, c: ^c, p) = ((^a or ^b or ^c) : (static member GroupBy : _*_*_*_ -> _) b, p, c, a)
        let inline call (a: 'a, b: 'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) : 'r
        call (Unchecked.defaultof<GroupBy>, source, projection)


type ChunkBy =
    static member ChunkBy (x: Id<'T>  , f: 'T->'Key, _: Id<'Key*Id<'T>>    , [<Optional>]_impl: ChunkBy) = let a = Id.run x in Id.create (f a, x)
    static member ChunkBy (x: seq<'T> , f: 'T->'Key, _: seq<'Key*seq<'T>>  , [<Optional>]_impl: ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, y :> _ seq)
    static member ChunkBy (x: list<'T>, f: 'T->'Key, _: list<'Key*list<'T>>, [<Optional>]_impl: ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, Seq.toList  y) |> Seq.toList
    static member ChunkBy (x: 'T []   , f: 'T->'Key, _: ('Key*('T [])) []  , [<Optional>]_impl: ChunkBy) = Seq.chunkBy f x |> Seq.map (fun (x,y) -> x, Seq.toArray y) |> Seq.toArray

    static member inline Invoke (projection: 'T->'Key) (source: '``Collection<'T>``) : '``Collection<'Key * 'Collection<'T>>`` = 
        let inline call_3 (a: ^a, b: ^b, c: ^c, p) = ((^a or ^b or ^c) : (static member ChunkBy : _*_*_*_ -> _) b, p, c, a)
        let inline call (a: 'a, b: 'b, p) = call_3 (a, b, Unchecked.defaultof<'r>, p) : 'r
        call (Unchecked.defaultof<ChunkBy>, source, projection)


type Replace =
    inherit Default1
    static member inline Replace (x: 'Collection  , o: 'Collection  , n: 'Collection  , [<Optional>]_impl: Default1) = x |> ToSeq.Invoke |> Seq.replace (ToSeq.Invoke o) (ToSeq.Invoke n) |> OfSeq.Invoke : 'Collection
    static member        Replace (x: Id<'T>       , o: Id<'T>       , n: Id<'T>       , [<Optional>]_impl: Default1) = if x = o then n else x
    static member        Replace (x: list<'T>     , o: list<'T>     , n: list<'T>     , [<Optional>]_impl: Replace ) = List.replace   o n x
    static member        Replace (x: 'T []        , o: 'T []        , n: 'T []        , [<Optional>]_impl: Replace ) = Array.replace  o n x
    static member        Replace (x: string       , o: string       , n: string       , [<Optional>]_impl: Replace ) = String.replace o n x
    static member        Replace (x: StringBuilder, o: StringBuilder, n: StringBuilder, [<Optional>]_impl: Replace ) = if o.Length = 0 then x else StringBuilder ((string x).Replace (string o, string n))
 
    static member inline Invoke (o: 'Collection) (n: 'Collection) (source: 'Collection) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Replace : _*_*_*_ -> _) b, o, n, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Replace>, source) : 'Collection


type Rev =
    inherit Default1

    static member        Rev (x: list<'a>   , [<Optional>]_impl: Rev  ) = List.rev x
    static member        Rev (x: 'a []      , [<Optional>]_impl: Rev  ) = Array.rev x

    static member inline Invoke (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Rev : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Rev>, source)
    static member inline InvokeOnInstance (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member Rev : _->_) source) : ^``C<'T>``

    static member inline Rev (x: '``Collection<'T>``, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.rev |> OfSeq.Invoke         : '``Collection<'T>``
    static member inline Rev (x: ^``Collection<'T>``, [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member Rev : _->_) x) : '``Collection<'T>``
    static member inline Rev (_: ^t when ^t: null and ^t: struct, _mthd: Default1) = id


type Scan =
    static member Scan (x: Id<'T>  , f ,z: 'S, [<Optional>]_output: Id<'S>  , [<Optional>]_impl: Scan) = Id.create (f z x.getValue)
    static member Scan (x: seq<'T> , f ,z: 'S, [<Optional>]_output: seq<'S> , [<Optional>]_impl: Scan) = Seq.scan   f z x
    static member Scan (x: list<'T>, f ,z: 'S, [<Optional>]_output: list<'S>, [<Optional>]_impl: Scan) = List.scan  f z x
    static member Scan (x: 'T []   , f ,z: 'S, [<Optional>]_output: 'S []   , [<Optional>]_impl: Scan) = Array.scan f z x

    static member inline Invoke (folder: 'State->'T->'State) (state: 'State) (source: '``Collection<'T>``) =
        let inline call_3 (a: ^a, b: ^b, c: ^c, f, z) = ((^a or ^b or ^c) : (static member Scan : _*_*_*_*_ -> _) b, f, z, c, a)
        let inline call (a: 'a, b: 'b, f, z) = call_3 (a, b, Unchecked.defaultof<'r>, f, z) : 'r
        call (Unchecked.defaultof<Scan>, source, folder, state) : '``Collection<'State>``


type Sort =
    inherit Default1

    static member        Sort (x: list<'a>, [<Optional>]_impl: Sort) = List.sort x
    static member        Sort (x: 'a []   , [<Optional>]_impl: Sort) = Array.sort x

    static member inline Invoke (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Sort : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Sort>, source)
    static member inline InvokeOnInstance (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member Sort : _->_) source) : ^``C<'T>``

    static member inline Sort (x: '``Collection<'T>``, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.sort |> OfSeq.Invoke         : '``Collection<'T>``
    static member inline Sort (x: ^``Collection<'T>``, [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member Sort : _->_) x) : '``Collection<'T>``
    static member inline Sort (_: ^t when ^t: null and ^t: struct, _mthd: Default1) = id


type SortBy =
    inherit Default1

    static member        SortBy (x: list<'a>, f, [<Optional>]_impl: SortBy) = List.sortBy  f x
    static member        SortBy (x: 'a []   , f, [<Optional>]_impl: SortBy) = Array.sortBy f x

    static member inline Invoke (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member SortBy : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<SortBy>, source, projection)
    static member inline InvokeOnInstance (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member SortBy : _*_->_) projection, source) : ^``C<'T>``

    static member inline SortBy (x: '``Collection<'T>``, f        , [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.sortBy f |> OfSeq.Invoke            : '``Collection<'T>``
    static member inline SortBy (x: ^``Collection<'T>``, f        , [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member SortBy : _*_->_) f, x) : '``Collection<'T>``
    static member inline SortBy (_: ^t when ^t: null and ^t: struct, _: 'T->'U, _mthd: Default1) = id

type SortByDescending =
    inherit Default1

    static member        SortByDescending (x: list<'a>, f, [<Optional>]_impl: SortBy) = List.sortByDescending  f x
    static member        SortByDescending (x: 'a []   , f, [<Optional>]_impl: SortBy) = Array.sortByDescending f x

    static member inline Invoke (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member SortByDescending : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<SortByDescending>, source, projection)
    static member inline InvokeOnInstance (projection: 'T->'Key) (source: '``C<'T>``) : '``C<'T>`` = (^``C<'T>`` : (static member SortByDescending : _*_->_) projection, source) : ^``C<'T>``

    static member inline SortByDescending (x: '``Collection<'T>``, f        , [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.sortByDescending f |> OfSeq.Invoke            : '``Collection<'T>``
    static member inline SortByDescending (x: ^``Collection<'T>``, f        , [<Optional>]_impl: Default1) = (^``Collection<'T>`` : (static member SortByDescending : _*_->_) f, x) : '``Collection<'T>``
    static member inline SortByDescending (_: ^t when ^t: null and ^t: struct, _: 'T->'U, _mthd: Default1) = id

type Split =
    inherit Default1
    
    static member Split ((e: seq<seq<'T>>      , x: seq<'T>      ), [<Optional>]_impl: Split) = x |> Seq.split e
    static member Split ((e: array<seq<'T>>    , x: seq<'T>      ), [<Optional>]_impl: Split) = x |> Seq.split e |> Seq.toArray
    static member Split ((e: list<seq<'T>>     , x: seq<'T>      ), [<Optional>]_impl: Split) = x |> Seq.split e |> Seq.toList
    static member Split ((e: seq<list<'T>>     , x: list<'T>     ), [<Optional>]_impl: Split) = x |> List.split e
    static member Split ((e: array<list<'T>>   , x: list<'T>     ), [<Optional>]_impl: Split) = x |> List.split e |> Seq.toArray
    static member Split ((e: list<list<'T>>    , x: list<'T>     ), [<Optional>]_impl: Split) = x |> List.split e |> Seq.toList
    static member Split ((e: seq<'T []>        , x: 'T []        ), [<Optional>]_impl: Split) = x |> Array.split e
    static member Split ((e: array<'T []>      , x: 'T []        ), [<Optional>]_impl: Split) = x |> Array.split e |> Seq.toArray
    static member Split ((e: list<'T []>       , x: 'T []        ), [<Optional>]_impl: Split) = x |> Array.split e |> Seq.toList
    static member Split ((e: seq<string>       , x: string       ), [<Optional>]_impl: Split) = x |> String.split e
    static member Split ((e: array<string>     , x: string       ), [<Optional>]_impl: Split) = x.Split (Seq.toArray e, StringSplitOptions.None)
    static member Split ((e: list<string>      , x: string       ), [<Optional>]_impl: Split) = x.Split (Seq.toArray e, StringSplitOptions.None) |> Array.toList
    static member Split ((e: seq<StringBuilder>, x: StringBuilder), [<Optional>]_impl: Split) = x.ToString().Split (e |> Seq.map string |> Seq.toArray, StringSplitOptions.None) |> Array.map StringBuilder :> seq<_>
    static member Split ((e: StringBuilder []  , x: StringBuilder), [<Optional>]_impl: Split) = x.ToString().Split (e |> Seq.map string |> Seq.toArray, StringSplitOptions.None) |> Array.map StringBuilder
    static member Split ((e: StringBuilder list, x: StringBuilder), [<Optional>]_impl: Split) = x.ToString().Split (e |> Seq.map string |> Seq.toArray, StringSplitOptions.None) |> Array.map StringBuilder |> Array.toList
 
    static member inline Invoke (sep: '``'Collection<'OrderedCollection>``) (source: 'OrderedCollection) =
        let inline call_2 (a: ^a, b: ^b, s) = ((^a or ^b) : (static member Split : (_*_)*_ -> _) (s, b), a)
        let inline call (a: 'a, b: 'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Split>, source, sep) : '``'Collection<'OrderedCollection>``

 type Split with
    static member inline Split ((e: '``'Collection<'OrderedCollection>``, x: '``'OrderedCollection``), [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.split (ToSeq.Invoke e) |> Seq.map OfSeq.Invoke |> OfSeq.Invoke : '``'Collection<'OrderedCollection>``
    static member inline Split ((e: '``'Collection<'OrderedCollection>``, x: '``'OrderedCollection``), [<Optional>]_impl: Default1) = (^``'OrderedCollection`` : (static member Split : _*_->_) e, x)                          : '``'Collection<'OrderedCollection>``
    static member inline Split ((_: ^t when ^t: null and ^t: struct, _                              ),             _mthd: Default1) = id


type Intersperse =
    inherit Default1
    static member inline Intersperse (x: '``Collection<'T>``, e: 'T, [<Optional>]_impl:Default1   ) = x |> ToSeq.Invoke |> Seq.intersperse e |> OfSeq.Invoke : '``Collection<'T>``
    static member        Intersperse (x: list<'T>           , e: 'T, [<Optional>]_impl:Intersperse) = List.intersperse  e x
    static member        Intersperse (x: 'T []              , e: 'T, [<Optional>]_impl:Intersperse) = Array.intersperse e x
 
    static member inline Invoke (sep: 'T) (source: '``Collection<'T>``) =
        let inline call_2 (a: ^a, b: ^b, s) = ((^a or ^b) : (static member Intersperse : _*_*_ -> _) b, s, a)
        let inline call (a: 'a, b: 'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Intersperse>, source, sep) : '``Collection<'T>``


// More Foldables

type Intercalate =
    inherit Default1
    static member inline Intercalate (x: '``Foldable<'Monoid>``, e: 'Monoid          , [<Optional>]_impl: Default2   ) = let f t x = match t, x with (true, _), x -> false, x | (_, acc ), x -> (false, Plus.Invoke (Plus.Invoke acc e) x) in Fold.Invoke f (true, Zero.Invoke ()) x |> snd
    static member inline Intercalate (x: seq<'``Foldable<'T>``>, e: '``Foldable<'T>``, [<Optional>]_impl: Default1   ) = x |> Seq.map ToSeq.Invoke |> Seq.intercalate (ToSeq.Invoke e) |> OfSeq.Invoke : '``Foldable<'T>``
    static member inline Intercalate (_: seq<'``Foldable<'T>``>, _: ^t when ^t: null and ^t: struct  , _: Default1   ) = id
    static member        Intercalate (x: seq<list<'T>>         , e: list<'T>         , [<Optional>]_impl: Intercalate) = List.intercalate  e x
    static member        Intercalate (x: seq<'T []>            , e: 'T []            , [<Optional>]_impl: Intercalate) = Array.intercalate e x
    static member        Intercalate (x: seq<string>           , e: string           , [<Optional>]_impl: Intercalate) = String.Join (e, x)
    static member        Intercalate (x: seq<StringBuilder>    , e: StringBuilder    , [<Optional>]_impl: Intercalate) = StringBuilder (String.Join (string e, Seq.map string x))
 
    static member inline Invoke (sep: 'Monoid) (source: '``Foldable<'Monoid>``) =
        let inline call_2 (a: ^a, b: ^b, s: ^c) = ((^a or ^c) : (static member Intercalate : _*_*_ -> _) b, s, a)
        let inline call (a: 'a, b: 'b, s) = call_2 (a, b, s)
        call (Unchecked.defaultof<Intercalate>, source, sep) : 'Monoid