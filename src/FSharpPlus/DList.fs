namespace FSharpPlus.Data
open System.Collections.Generic
open FSharpPlus
open System.ComponentModel

// DList from FSharpx.Collections
//This implementation adds an additional parameter to allow O(1) retrieval of the list length.


/// DList is an ordered linear structure implementing the List signature (head, tail, cons), 
/// end-insertion (add), and O(1) append. Ordering is by insertion history.
/// DList is an implementation of [John Hughes' append list](http://dl.acm.org/citation.cfm?id=8475).
[<Sealed>]
type DList<'T>(length : int , data : DListData<'T> ) =
    let mutable hashCode = None
    member internal this.dc = data

    static member ofSeq (s : seq<'T>) =
         DList (Seq.fold (fun (i, state) x ->
            (i+1, 
                match state with
                    | Nil -> Unit x
                    | Unit _ -> Join(state, Unit x)
                    | Join(_,_) -> Join (state, Unit x))) (0, Nil) s)

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash + Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? DList<'T> as y -> 
            if this.Length <> y.Length then false 
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    ///O(1). Returns the count of elememts.
    member this.Length = length

    // O(n). FoldBack walks the DList using constant stack space. Implementation is from Norman Ramsey.
    // Called a "fold" in the article processes the linear representation from right to left
    // and so is more appropriately implemented under the foldBack signature
    // See http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5334068#5334068
    static member  foldBack (f : ('T -> 'State -> 'State)) (l:DList<'T>)  (state : 'State) =
        let rec walk lefts l xs =
            match l with
            | Nil       -> finish lefts xs
            | Unit x    -> finish lefts <| f x xs
            | Join(x,y) -> walk (x::lefts) y xs
        and finish lefts xs =
            match lefts with
            | []    -> xs
            | t::ts -> walk ts t xs
        walk [] l.dc state

    // making only a small adjustment to Ramsey's algorithm we get a left to right fold
    static member  fold (f : ('State -> 'T -> 'State)) (state : 'State) (l:DList<'T>)  =
        let rec walk rights l xs =
            match l with
            | Nil       -> finish rights xs
            | Unit x    -> finish rights <| f xs x
            | Join(x,y) -> walk (y::rights) x xs
        and finish rights xs =
            match rights with
            | []    -> xs
            | t::ts -> walk ts t xs
        walk [] l.dc state

    static member append (left, right) =
        match (left, right) with
        | Nil, _ -> right
        | _, Nil -> left
        | _, _   -> Join(left, right)

    static member appendLists ((left : DList<'T>), (right : DList<'T>)) = 
        DList( (left.Length + right.Length), (DList<'T>.append(left.dc, right.dc)))

    static member head data =
        match data with
        | Unit x' -> x'
        | Join(x', _) -> DList<'T>.head x'
        | _ -> failwith "DList.head: empty DList"

    static member tryHead data =
        match data with
        | Unit x' -> Some x'
        | Join(x', _) -> DList<'T>.tryHead x'
        | _ -> None
    ///O(1). Returns a new DList with the element added to the front.
    member this.Cons (hd : 'T) =
        match data with
        | Nil -> DList (1, (Unit hd))
        | _ ->  DList ((length + 1), Join(Unit hd, data) )

    ///O(log n). Returns the first element.
    member this.Head = DList<'T>.head data

    ///O(log n). Returns option first element
    member this.TryHead = DList<'T>.tryHead data

    ///O(1). Returns true if the DList has no elements.
    member this.IsEmpty = match data with Nil -> true | _ -> false

    ///O(1). Returns a new DList with the element added to the end.
    member this.Add (x:'T) = DList( (length + 1), DList<'T>.append(data, Unit x) )

    ///O(log n). Returns a new DList of the elements trailing the first element.
    member this.Tail =
        let rec step (xs:DListData<'T>) (acc:DListData<'T>) =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y) -> step x (DList<'T>.append(y, acc))
        if this.IsEmpty then failwith "DList.tail: empty DList"
        else DList( (length - 1), (step data Nil ))

    ///O(log n). Returns option DList of the elements trailing the first element.
    member this.TryTail =
        let rec step (xs:DListData<'T>) (acc:DListData<'T>) =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y) -> step x (DList<'T>.append(y, acc))
        if this.IsEmpty then None
        else Some (DList( (length - 1), (step data Nil )))

    ///O(log n). Returns the first element and tail.
    member this.Uncons = ((DList<'T>.head data), (this.Tail))

    ///O(log n). Returns option first element and tail.
    member this.TryUncons =
        match DList<'T>.tryHead data with
        | Some(x) -> Some (x, this.Tail)
        | None -> None

    member this.toSeq() =
        //adaptation of right-hand side of Norman Ramsey's "fold"
        let rec walk rights l = 
           seq {match l with
                | Nil       -> 
                    match rights with
                    | []    -> () 
                    | t::ts -> yield! walk ts t 
                | Unit x    -> 
                    yield x
                    match rights with
                    | []    -> () 
                    | t::ts -> yield! walk ts t 
                | Join(x,y) -> yield! walk (y::rights) x}
               
        (walk [] data).GetEnumerator()

    interface IEnumerable<'T> with
        member s.GetEnumerator() = s.toSeq()

    interface System.Collections.IEnumerable with
        override s.GetEnumerator() = (s.toSeq() :> System.Collections.IEnumerator)
            
and 
    DListData<'T> =
    | Nil
    | Unit of 'T
    | Join of DListData<'T> * DListData<'T>  

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DList =
    ///O(1). Returns a new DList of two lists.
    let append left right = DList<'T>.appendLists(left, right)

    ///O(1). Returns a new DList with the element added to the beginning.
    let cons hd (l:DList<'T>) = 
        match l.Length with
        | 0 -> DList(1, Unit hd)
        | _ -> DList(l.Length + 1, Join(Unit hd, l.dc) )

    ///O(1). Returns DList of no elements.
    [<GeneralizableValue>]
    let empty<'T> : DList<'T> = DList(0, Nil )

    ///O(n). Fold walks the DList using constant stack space. Implementation is from Norman Ramsey.
    /// See http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5334068#5334068
    let foldBack (f : ('T -> 'State -> 'State)) (l:DList<'T>) (state : 'State) =
        DList<'T>.foldBack f l state

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (l:DList<'T>) =
        DList<'T>.fold f state l

    ///O(log n). Returns the first element.
    let inline head (l:DList<'T>) = l.Head

    ///O(log n). Returns option first element.
    let inline tryHead (l:DList<'T>) = l.TryHead

    ///O(1). Returns true if the DList has no elements.
    let inline isEmpty (l:DList<'T>) = l.IsEmpty

    ///O(1). Returns the count of elememts.
    let inline length (l:DList<'T>) = l.Length

    ///O(1). Returns DList of one elements.
    let singleton x = DList(1, Unit x )

    ///O(1). Returns a new DList with the element added to the end.
    let inline add x (l:DList<'T>) = l.Add x

    ///O(log n). Returns a new DList of the elements trailing the first element.
    let inline tail (l:DList<'T>) = l.Tail

    ///O(log n). Returns option DList of the elements trailing the first element.
    let inline tryTail (l:DList<'T>) = l.TryTail

    ///O(log n). Returns the first element and tail.
    let inline uncons (l:DList<'T>) = l.Uncons

    ///O(log n). Returns option first element and tail.
    let inline tryUncons (l:DList<'T>) = l.TryUncons

    ///O(n). Returns a DList of the seq.
    let ofSeq s = DList<'T>.ofSeq s

    ///O(n). Returns a list of the DList elements.
    let inline toList l = foldBack (List.cons) l [] 

    ///O(n). Returns a seq of the DList elements.
    let inline toSeq (l:DList<'T>) = l :> seq<'T>

    // additions to fit f#+ :
    let inline map f (x:DList<_>)    = DList.foldBack (cons << f ) x empty
    let concat x = DList.fold append empty x 
    let inline ap f x = concat <| map (fun y -> map ((|>) y) f) x
    let inline bind m k              = DList.foldBack (append << k) empty m
type DList<'T> with
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member get_Zero = DList( 0, Nil)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (+) (x:DList<_>, y:DList<_>) = DList.append x y
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member get_Empty = DList( 0, Nil)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (<|>) (x:DList<_>, y:DList<_>) = DList.append x y
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq  x = DList.toSeq  x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList x = DList.toList x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member OfSeq  x = DList.ofSeq  x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Fold (x, f, z) = DList.fold f x z

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Return x = DList (1, x)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x, f) = DList.map f x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (<*>) (f, x) = DList.ap f x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member (>>=) (x, f) = DList.bind x f
