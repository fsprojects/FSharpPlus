    namespace FSharpPlus

    open FsControl.Core.Abstractions

    type NonEmptyList<'t> = {Head: 't; Tail: 't list}

    [<RequireQualifiedAccess>]
    module NonEmptyList =
        let toList {Head = x; Tail = xs} = x::xs
        let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
        let cons e {Head = x; Tail = xs} = {Head = e  ; Tail = x::xs}
        let rec tails s =
            let {Head = x; Tail = xs} = s
            match xs with
            | []   -> {Head = s; Tail = []}
            | h::t -> cons s (tails {Head = h; Tail = t})
         
    type NonEmptyList<'T> with
        static member instance (_:Functor.Map      , x:NonEmptyList<'a>, _:NonEmptyList<'b>) = fun (f:'a->'b) -> NonEmptyList.map f x
        
        static member instance (_:Monad.Bind, {Head = x; Tail = xs}, _:NonEmptyList<'b>   ) = fun (f:_->NonEmptyList<'b>  ) ->
            let {Head = y; Tail = ys} = f x
            let ys' = List.collect (NonEmptyList.toList << f) xs
            {Head = y; Tail = (ys @ ys')}

        static member instance (_:Applicative.Pure, _:NonEmptyList<'a>) = fun (x:'a)     -> {Head = x; Tail = []}
        static member instance (_:Applicative.Apply  , f:NonEmptyList<'a->'b>, x:NonEmptyList<'a> ,_:NonEmptyList<'b>) = fun () ->
             Applicative.DefaultImpl.ApplyFromMonad f x :NonEmptyList<'b>

        static member instance (_:Comonad.Extract  , {Head = h; Tail = _} ,_) = fun () -> h
        static member instance (_:Comonad.Duplicate, s:NonEmptyList<'a>, _:NonEmptyList<NonEmptyList<'a>>) = fun () -> NonEmptyList.tails s