namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type Cont<'R,'A> = Cont of (('A->'R)->'R)

[<RequireQualifiedAccess>]
module Cont =
    let run (Cont x) = x
    let callCC (f:(_->Cont<'r,'b>)->_) = Cont <| fun k -> run (f (fun a -> Cont(fun _ -> k a))) k
    let map f (Cont x) = Cont(fun c -> x (c << f))
    
    let bind f (Cont x) = Cont(fun k -> x (fun a -> run(f a) k)) :Cont<'r,'b>
    let apply f x =
        let pure' n = Cont(fun k -> k n)
        let (>>=) x f = bind f x
        f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)

type Cont<'R,'A> with
    static member instance (_:Functor.Map, x:Cont<'r,'a>, _) = fun (f:_->'b) -> Cont.map f x
    static member instance (Applicative.Pure, _:Cont<'r,'a>     ) = fun n -> Cont(fun k -> k n)                          :Cont<'r,'a>
    static member instance (Monad.Bind ,   x, _:Cont<'r,'b>) = fun f -> Cont.bind f x :Cont<'r,'b>
    static member instance (_:Applicative.Apply, f:Cont<'r,_>, x:Cont<'r,'a>, _:Cont<'r,'b>) = fun () -> Cont.apply f x :Cont<'r,'b>