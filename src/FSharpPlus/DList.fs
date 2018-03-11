namespace FSharpPlus.Data
open System.Collections.Generic
open System.Runtime.InteropServices

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
    //static member Fold (x, f, z, [<Optional>]_impl) = fold f x z    
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
    let isEmpty data = match data with Nil -> true | _ -> false
    let append (left: DListData<_>) (right: DListData<_>) = 
        match left with
        | Nil -> right
        | _ -> match right with
               | Nil -> left
               | _ -> Join(left, right)
module DList =

    let empty<'a>             = DList<'a> (0, Nil)
    let toList (x:DList<_>)   = x.Walk [] |> Seq.toList
    let toSeq  (x:DList<_>)   = x.Walk []
    let ofSeq  source = 
        DList(Seq.length source, (Seq.fold (fun state x ->
            match state with 
            | Nil -> Unit x
            | Unit _ -> Join(state, Unit x)
            | Join(_,_) as xs -> Join(state, Unit x)) Nil source))
    let ofList source = 
        DList(List.length source, (List.fold (fun state x ->
            match state with 
            | Nil -> Unit x
            | Unit _ -> Join(state, Unit x)
            | Join(_,_) as xs -> Join(state, Unit x)) Nil source))

    let append (left: DList<_>) (right: DList<_>) = 
        let len =left.Length + right.Length
        let data= DListData.append left.dc right.dc
        DList(len, data)

    let singleton x = DList (1, Unit x)
    let cons hd (f:DList<_>) =
        match f.dc with
        | Nil -> DList (1, (Unit hd))
        | _ ->  DList ((f.Length + 1), Join(Unit hd, f.dc) )
    let snoc (f:DList<_>) x = DList( (f.Length + 1), DListData.append (f.dc) (Unit x) )
    let fold f x = DList<_>.Fold'(f x)
    let map f (x:DList<_>) = DList<_>.FoldBack' (cons << f ) x empty
    let concat x = DList<_>.Fold' append empty x
    let join (f:DList<DList<_>>) = concat f
    let ap f x = join <| map (fun y -> map ((|>) y) f) x
    let bind m k = DList<_>.FoldBack' (append << k) empty m

open DList

type DList<'T> with
    
    static member get_Zero = DList( 0, Nil)
    static member (+) (x:DList<_>, y:DList<_>) = DList.append x y

    static member get_Empty = DList( 0, Nil)
    static member Append (x:DList<_>, y:DList<_>) = DList.append x y
    
    static member ToSeq  x = toSeq  x
    static member ToList x = toList x
    static member OfSeq  x = ofSeq  x
    static member OfList x = ofList x
    static member Fold (x, f, z, [<Optional>]_impl) = fold f x z    

    static member Return x = DList (1, x)
    static member Map (x, f) = map f x
    static member (<*>) (f, x) = ap f x
    static member Join x = join x
    static member Bind (x, f) = bind x f