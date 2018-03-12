namespace FSharpPlus.Data
open System.Collections.Generic
open System.Runtime.InteropServices
open Microsoft.FSharp.Collections
/// List-like type supporting O(1) append
type DList<'T>(length : int , data : DListData<'T>) =
    let mutable hashCode = None
    member internal this.dc = data

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
    static member internal FoldBack' (f : ('T -> 'State -> 'State)) (l:DList<'T>)  (state : 'State) =
        let rec walk lefts l xs =
            match l with
            | Nil       -> finish lefts xs
            | Unit x    -> finish lefts <| f x xs
            | Join(x,y) -> walk (x::lefts) y xs
        and finish lefts xs =
            match lefts with
            | []    -> xs
            | t::ts -> walk ts t xs
        in walk [] l.dc state
    // making only a small adjustment to Ramsey's algorithm we get a left to right fold
    static member internal Fold' (f : ('State -> 'T -> 'State)) (state : 'State) (l:DList<'T>)  =
        let rec walk rights l xs =
            match l with
            | Nil       -> finish rights xs
            | Unit x    -> finish rights <| f xs x
            | Join(x,y) -> walk (y::rights) x xs
        and finish rights xs =
            match rights with
            | []    -> xs
            | t::ts -> walk ts t xs
        in walk [] l.dc state

    member internal this.Walk rights =
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
        walk rights this.dc

    member private this.ToSeq() =
        (this.Walk []).GetEnumerator()

    interface IEnumerable<'T> with
        member s.GetEnumerator() = s.ToSeq()

    interface System.Collections.IEnumerable with
        override s.GetEnumerator() = (s.ToSeq() :> System.Collections.IEnumerator)
and 
    DListData<'T> =
    | Nil
    | Unit of 'T
    | Join of DListData<'T> * DListData<'T> 
module internal DListData =
    let isEmpty data      = match data with Nil -> true | _ -> false
    let rec tryHead (x:DListData<_>)  =
        match x with
        | Unit x' -> Some x'
        | Join(x',_) -> tryHead x'
        | _ -> None

    let append (left: DListData<_>) (right: DListData<_>) = 
        match left with
        | Nil -> right
        | _ -> match right with
               | Nil -> left
               | _ -> Join(left, right)
module DList =
    let isEmpty (x:DList<_>)  = DListData.isEmpty x.dc
    let length (x:DList<_>)   = x.Length
    let empty<'a>             = DList<'a> (0, Nil)
    let toList (x:DList<_>)   = DList<_>.FoldBack' (fun head tail -> List.Cons(head, tail)) x []
    let toSeq  (x:DList<_>)   = x.Walk []
    let singleton x           = DList (1, Unit x)

    let private ofSeqT s len fold
                              = 
                                DList(len s, (fold (fun state x ->
                                    match state with 
                                    | Nil -> Unit x
                                    | Unit _ -> Join(state, Unit x)
                                    | Join(_,_) as xs -> Join(xs, Unit x)) Nil s))

    let ofSeq  source         = ofSeqT source Seq.length Seq.fold
    let ofList source         = ofSeqT source List.length List.fold

    let append (left: DList<_>) (right: DList<_>) = 
        let len =left.Length + right.Length
        let data= DListData.append left.dc right.dc
        DList(len, data)
    /// return a new DList with hd as its head
    let cons hd (f:DList<_>)  =
        match f.dc with
        | Nil -> DList (1, (Unit hd))
        | _ ->  DList ((f.Length + 1), Join(Unit hd, f.dc) )
    /// return a new DList with x appended in the end
    let snoc x (f:DList<_>)   = DList( (f.Length + 1), DListData.append (f.dc) (Unit x) )
    let fold f x              = DList<_>.Fold' f x
    let map f (x:DList<_>)    = DList<_>.FoldBack' (cons << f ) x empty
    let concat x              = DList<_>.Fold' append empty x
    let join (f:DList<DList<_>>) = concat f
    let ap f x                = join <| map (fun y -> map ((|>) y) f) x
    let bind m k              = DList<_>.FoldBack' (append << k) empty m
    let tryHead (x:DList<_>)  = DListData.tryHead x.dc
    let head x                = match tryHead x with | Some l -> l | None -> raise (System.ArgumentException "empty dlist")
    let tryTail (x:DList<_>)  =
        let rec step (xs:DListData<'T>) (acc:DListData<'T>) =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y) -> step x (DListData.append y acc)
        if isEmpty x then None
        else Some (DList( (x.Length - 1), (step x.dc Nil )))
    let tail x                = match tryTail x with | Some l -> l | None -> raise (System.ArgumentException "empty dlist")

type DList<'T> with
    
    static member get_Zero = DList( 0, Nil)
    static member (+) (x:DList<_>, y:DList<_>) = DList.append x y

    static member get_Empty = DList( 0, Nil)
    static member (<|>) (x:DList<_>, y:DList<_>) = DList.append x y
    
    static member ToSeq  x = DList.toSeq  x
    static member ToList x = DList.toList x
    static member OfSeq  x = DList.ofSeq  x
    static member OfList x = DList.ofList x
    static member Fold (x, f, z) = DList.fold f x z

    static member Return x = DList (1, x)
    static member Map (x, f) = DList.map f x
    static member (<*>) (f, x) = DList.ap f x
    static member Join x = DList.join x
    static member (>>=) (x, f) = DList.bind x f
