namespace FSharpPlus.Data
open System.Collections.Generic
open FSharpPlus
// DList from FSharpx.Collections
//This implementation adds an additional parameter to allow O(1) retrieval of the list length.

type DList<'T>(length : int , data : DListData<'T> ) =
    let mutable hashCode = None
    member internal this.dc = data

    static member ofSeq (s : seq<'T>) =
         DList(Seq.length s, (Seq.fold (fun state x ->
                    match state with 
                    | Nil -> Unit x
                    | Unit _ -> Join(state, Unit x)
                    | Join(_,_) as xs -> Join(state, Unit x)) Nil s))

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
        match left with
        | Nil -> right
        | _ -> match right with
               | Nil -> left
               | _ -> Join(left, right)

    static member appendLists ((left : DList<'T>), (right : DList<'T>)) = 
        DList( (left.Length + right.Length), (DList<'T>.append(left.dc, right.dc)))

    static member head data =
        match data with
        | Unit x' -> x'
        | Join(x',y) -> DList<'T>.head x'
        | _ -> failwith "DList.head: empty DList"

    static member tryHead data =
        match data with
        | Unit x' -> Some x'
        | Join(x',y) -> DList<'T>.tryHead x'
        | _ -> None

    member this.Cons (hd : 'T) =
        match data with
        | Nil -> DList (1, (Unit hd))
        | _ ->  DList ((length + 1), Join(Unit hd, data) )

    member this.Head = DList<'T>.head data

    member this.TryHead = DList<'T>.tryHead data

    member this.IsEmpty = match data with Nil -> true | _ -> false

    member this.Conj (x:'T) = DList( (length + 1), DList<'T>.append(data, Unit x) )

    member this.Tail =
        let rec step (xs:DListData<'T>) (acc:DListData<'T>) =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y) -> step x (DList<'T>.append(y, acc))
        if this.IsEmpty then failwith "DList.tail: empty DList"
        else DList( (length - 1), (step data Nil ))

    member this.TryTail =
        let rec step (xs:DListData<'T>) (acc:DListData<'T>) =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y) -> step x (DList<'T>.append(y, acc))
        if this.IsEmpty then None
        else Some (DList( (length - 1), (step data Nil )))

    member this.Uncons = ((DList<'T>.head data), (this.Tail))

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

    //pattern discriminators  (active pattern)
    let (|Cons|Nil|) (l : DList<'T>) = match l.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let append left right = DList<'T>.appendLists(left, right)

    let cons hd (l:DList<'T>) = 
        match l.Length with
        | 0 -> DList(1, Unit hd)
        | _ -> DList(l.Length + 1, Join(Unit hd, l.dc) )
    
    let empty<'T> : DList<'T> = DList(0, Nil )

    let foldBack (f : ('T -> 'State -> 'State)) (l:DList<'T>) (state : 'State) =
        DList<'T>.foldBack f l state

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (l:DList<'T>) =
        DList<'T>.fold f state l

    let inline head (l:DList<'T>) = l.Head

    let inline tryHead (l:DList<'T>) = l.TryHead

    let inline isEmpty (l:DList<'T>) = l.IsEmpty

    let inline length (l:DList<'T>) = l.Length
    
    let singleton x = DList(1, Unit x )

    let inline conj x (l:DList<'T>) = l.Conj x

    let inline tail (l:DList<'T>) = l.Tail

    let inline tryTail (l:DList<'T>) = l.TryTail

    let inline uncons (l:DList<'T>) = l.Uncons

    let inline tryUncons (l:DList<'T>) = l.TryUncons

    let ofSeq s = DList<'T>.ofSeq s

    let inline toList l = foldBack (List.cons) l [] 

    let inline toSeq (l:DList<'T>) = l :> seq<'T>

    // additions to fit f#+ :
    let inline map f (x:DList<_>)    = DList.foldBack (cons << f ) x empty
    let concat x = DList.fold append empty x 
    let inline join (f:DList<DList<_>>) = concat f
    let inline ap f x = join <| map (fun y -> map ((|>) y) f) x
    let inline bind m k              = DList.foldBack (append << k) empty m

type DList<'T> with
    
    static member get_Zero = DList( 0, Nil)
    static member (+) (x:DList<_>, y:DList<_>) = DList.append x y

    static member get_Empty = DList( 0, Nil)
    static member (<|>) (x:DList<_>, y:DList<_>) = DList.append x y
    
    static member ToSeq  x = DList.toSeq  x
    static member ToList x = DList.toList x
    static member OfSeq  x = DList.ofSeq  x
    static member Fold (x, f, z) = DList.fold f x z

    static member Return x = DList (1, x)
    static member Map (x, f) = DList.map f x
    static member (<*>) (f, x) = DList.ap f x
    static member Join x = DList.join x
    static member (>>=) (x, f) = DList.bind x f
