namespace FSharpPlus

open System.Text
open System.Runtime.InteropServices
open FsControl
open FSharpPlus
open FSharpPlus.Extensions

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
         
type NonEmptyList with
    static member Map (x:NonEmptyList<'a>, f:'a->'b) = NonEmptyList.map f x
        
    static member Bind ({Head = x; Tail = xs}, f:_->NonEmptyList<'b>  ) =
        let {Head = y; Tail = ys} = f x
        let ys' = List.collect (NonEmptyList.toList << f) xs
        {Head = y; Tail = (ys @ ys')}

    static member Return (x:'a) = {Head = x; Tail = []}
    static member (<*>)  (f:NonEmptyList<'T->'U>, x:NonEmptyList<'T>) = 
        let r = NonEmptyList.toList f <*> NonEmptyList.toList x
        {Head = r.Head; Tail = r.Tail}

    static member Extract    {Head = h; Tail = _} = h : 't
    static member Duplicate (s:NonEmptyList<'a>, [<Optional>]_impl:Duplicate) = NonEmptyList.tails s
    static member Extend    (s, g) = NonEmptyList.map g (NonEmptyList.tails s) :NonEmptyList<'b>
    

    static member Append ({Head = h; Tail = t},  x) = {Head = h; Tail = t @ NonEmptyList.toList x}

    static member FoldBack ({Head = x; Tail = xs}, f, z) = List.foldBack f (x::xs) z
    static member ToList   (s:NonEmptyList<'a>, [<Optional>]_impl:ToList) = NonEmptyList.toList s
    static member ToSeq    (s:NonEmptyList<'a>, [<Optional>]_impl:ToSeq ) = NonEmptyList.toList s |> List.toSeq

    static member inline ToString (s:NonEmptyList<'a>, [<Optional>]_impl:ToString) = fun (k:System.Globalization.CultureInfo) ->
            let b = StringBuilder()
            let inline append (s:string) = b.Append s |> ignore
            append "NonEmptyList ["
            let withSemiColons = NonEmptyList.toSeq s |> Seq.map (toStringWithCulture k) |> Seq.intersperse "; "
            Seq.iter append withSemiColons
            append "]"
            b.ToString()

    static member Replace (source:NonEmptyList<'T>, oldValue:NonEmptyList<'T>, newValue:NonEmptyList<'T>, _impl:Replace ) =
        let lst = source |> NonEmptyList.toSeq  |> Seq.replace oldValue newValue |> Seq.toList
        {Head = lst.Head; Tail = lst.Tail}