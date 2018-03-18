namespace FSharpPlus.Data

open System.Text
open System.Runtime.InteropServices
open System.ComponentModel
open FSharpPlus.Control
open FSharpPlus

/// A type-safe list that contains at least one element.
type NonEmptyList<'t> = {Head: 't; Tail: 't list} with
    interface System.Collections.Generic.IEnumerable<'t> with member x.GetEnumerator() = (let {Head = x; Tail = xs} = x in seq (x::xs)).GetEnumerator()
    interface System.Collections.IEnumerable             with member x.GetEnumerator() = (let {Head = x; Tail = xs} = x in seq (x::xs)).GetEnumerator() :> System.Collections.IEnumerator
    member this.head = let {Head = a; Tail = _} = this in a
    member this.tail = let           {Tail = a} = this in a
    member this.Item = function 0 -> this.head | n -> this.tail.[n-1]
    member this.GetSlice = function
        | None  , None
        | Some 0, None
        | Some 0, Some 0 -> this
        | Some a, None   -> let {Head = _; Tail = xs} = this in {Head = xs.[a-1]; Tail = xs.[a..   ]}
        | None  , Some b 
        | Some 0, Some b -> let {Head = x; Tail = xs} = this in {Head = x       ; Tail = xs.[ ..b-1]}
        | Some a, Some b -> let {Head = _; Tail = xs} = this in {Head = xs.[a-1]; Tail = xs.[a..b-1]}

/// Basic operations on NonEmptyList
[<RequireQualifiedAccess>]
module NonEmptyList =
    let create x xs = {Head = x; Tail = xs}
    let singleton x = {Head = x; Tail = []}
    let toList {Head = x; Tail = xs} = x::xs
    let toSeq  {Head = x; Tail = xs} = seq { yield x; yield! xs; }
    let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
    let cons e {Head = x; Tail = xs} = {Head = e  ; Tail = x::xs}
    let rec tails s =
        let {Tail = xs} = s
        match xs with
        | []   -> {Head = s; Tail = []}
        | h::t -> cons s (tails {Head = h; Tail = t})
         
type NonEmptyList<'t> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x:NonEmptyList<'a>, f:'a->'b) = NonEmptyList.map f x
        
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (>>=) ({Head = x; Tail = xs}, f:_->NonEmptyList<'b>  ) =
        let {Head = y; Tail = ys} = f x
        let ys' = List.collect (NonEmptyList.toList << f) xs
        {Head = y; Tail = (ys @ ys')}

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Return (x:'a) = {Head = x; Tail = []}
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (<*>)  (f:NonEmptyList<'T->'U>, x:NonEmptyList<'T>) = 
        let r = NonEmptyList.toList f <*> NonEmptyList.toList x
        {Head = r.Head; Tail = r.Tail}

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Extract    {Head = h; Tail = _} = h : 't
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Duplicate (s:NonEmptyList<'a>, [<Optional>]_impl:Duplicate) = NonEmptyList.tails s
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (=>>)     (s, g) = NonEmptyList.map g (NonEmptyList.tails s) :NonEmptyList<'b>
    

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (+) ({Head = h; Tail = t},  x) = {Head = h; Tail = t @ NonEmptyList.toList x}

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member FoldBack ({Head = x; Tail = xs}, f, z) = List.foldBack f (x::xs) z
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList   (s:NonEmptyList<'a>, [<Optional>]_impl:ToList) = NonEmptyList.toList s
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq    (s:NonEmptyList<'a>, [<Optional>]_impl:ToSeq ) = NonEmptyList.toList s |> List.toSeq
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Traverse (s:NonEmptyList<'T>, f:'T->'``Functor<'U>``) =
        let lst = traverse f (toList s) : '``Functor<'List<'U>>``
        (NonEmptyList.create << List.head |> fun f x -> f x (List.tail x)) <!> lst : '``Functor<'NonEmptyList<'U>>``

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Replace (source:NonEmptyList<'T>, oldValue:NonEmptyList<'T>, newValue:NonEmptyList<'T>, _impl:Replace ) =
        let lst = source |> NonEmptyList.toSeq  |> Seq.replace oldValue newValue |> Seq.toList
        {Head = lst.Head; Tail = lst.Tail}