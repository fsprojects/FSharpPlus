namespace FSharpPlus

open System.Runtime.InteropServices

/// List-like type supporting O(1) append
type DList<'T> = DList of ('T list -> 'T list)

module DList =
    let empty = DList id
    let toList (DList x)   = x []
    let toSeq (DList x)    = x [] :> seq<_>
    let ofList source = source |> List.append |> DList
    let ofSeq  source = source |> Seq.toList |> List.append |> DList
    let append (DList x) (DList y) = DList (x << y)
    let singleton x = DList (fun r -> x::r)
    let cons x (DList f) = (DList (fun l -> x::(f l)))
    let snoc (DList f) x = (DList (fun l -> f(x::l)))
    let fold f x = List.fold f x << toList
    let map f (DList x) = x [] |> List.map f |> List.append |> DList
    let concat x = List.fold append empty x
    let join (DList f) = concat (f [])
    let ap f x = join <| map (fun y -> map ((|>) y) f) x
    let bind m k = concat << List.map k << toList <| m

open DList

type DList<'T> with
    
    static member get_Zero = DList id
    static member (+) (DList x, DList y) = DList (x << y)

    static member get_Empty = DList id
    static member Append (DList x, DList y) = DList (x << y)
    
    static member ToSeq  x = toSeq  x
    static member ToList x = toList x
    static member OfSeq  x = ofSeq  x
    static member OfList x = ofList x
    static member Fold (x, f, z, [<Optional>]_impl) = fold f x z    

    static member Return x = DList (fun r -> x::r)
    static member Map (x, f) = map f x
    static member (<*>) (f, x) = ap f x
    static member Join x = join x
    static member Bind (x, f) = bind x f