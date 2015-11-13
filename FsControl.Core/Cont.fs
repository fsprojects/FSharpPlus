namespace FsControl

type Cont<'R,'A> = Cont of (('A->'R)->'R)

[<RequireQualifiedAccess>]
module Cont =
    let run (Cont x) = x
    let callCC (f:(_->Cont<'r,'b>)->_) = Cont <| fun k -> run (f (fun a -> Cont(fun _ -> k a))) k
    let map  f (Cont x) = Cont (fun c -> x (c << f))   
    let bind f (Cont x) = Cont (fun k -> x (fun a -> run(f a) k)) :Cont<'r,'b>
    let apply  (Cont f) (Cont x) = Cont (fun k -> f (fun f' -> x (k << f'))) :Cont<'r,'b>

type Cont<'R,'A> with
    static member Map   (x:Cont<'r,'a>, f) = Cont.map f x   :Cont<'r,'b>
    static member Return n = Cont (fun k -> k n)
    static member Bind  (x, f) = Cont.bind f x
    static member (<*>) (f, x:Cont<'r,'a>) = Cont.apply f x :Cont<'r,'b>
    static member CallCC (f:('a -> Cont<'r,'b>) -> _) = Cont.callCC f:Cont<'r,'a>