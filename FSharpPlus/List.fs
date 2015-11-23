namespace FSharpPlus

module List =

    let singleton x = [x]

    let apply f x = List.collect (fun f -> List.map ((<|) f) x) f

    let inline sequence (ms:list<'``Applicative<'T>``>) : '``Applicative<list<'T>>`` = sequenceA ms

    let inline traverse (f:'T->'``Applicative<'U>``) (xs:list<'T>) :'``Applicative<list<'U>>`` = traverse f xs
    
    let inline foldM (f:'T->'U->'``Monad<'T>``) (a:'T) (bx:list<'U>) : '``Monad<'T>`` =
        let rec loopM a = function
            | x::xs -> (f a x) >>= fun fax -> loopM fax xs 
            | [] -> result a
        loopM a bx

    let inline filterM (f: 'T -> '``Monad<Bool>``) (xs: list<'T>) : '``Monad<list<'T>>`` =
        let rec loopM = function
            | []   -> result []
            | h::t -> 
                f h >>= (fun flg ->
                    loopM t >>= (fun ys ->
                        result (if flg then (h::ys) else ys)))
        loopM xs

    let inline replicateM count (initial:'``Applicative<'T>``)  = sequence (List.replicate count initial)