namespace FSharpPlus

module List =

    let singleton x = [x]

    let apply f x = List.collect (fun f -> List.map ((<|) f) x) f

    let inline sequence (ms:list<'``Monad<'T>``>) =
        let k m m' = m >>= fun (x:'t) -> m' >>= fun xs -> (result :list<'t> -> '``Monad<list<'T>>``) (x::xs)
        List.foldBack k ms ((result :list<'t> -> '``Monad<list<'T>>``) [])

    let inline mapM (f:'a->'Monad'b) (xs:list<'a>) :'Monad'List'b = sequence (List.map f xs)
    
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

    let inline replicateM count (initial:'``Monad<'T>``) : '``Monad<list<'T>>`` = sequence (List.replicate count initial)