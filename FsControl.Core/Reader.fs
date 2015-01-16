namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type Reader<'R,'A> = Reader of ('R->'A)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x
    let map   f (Reader m) = Reader(f << m) :Reader<'r,_>
    let bind  f (Reader m) = Reader(fun r -> run (f (m r)) r) :Reader<'r,'b>
    let apply f x =
        let pure' a = Reader(fun _ -> a)
        let (>>=) x f = bind f x
        f >>= fun x1 -> x >>= fun x2 -> pure'(x1 x2)
    let local f (Reader m) = Reader(m << f) :Reader<'r,_>
    let ask() = Reader id

type Reader<'R,'A> with
    static member instance (_:Functor.Map, x:Reader<'r,'a>, _) = fun (f:_->'b) -> Reader.map f x
    static member instance (Applicative.Pure, _:Reader<'r,'a>       ) = fun a -> Reader(fun _ -> a)                     :Reader<'r,'a>
    static member instance (Monad.Bind ,   x, _:Reader<'r,'b>) = fun f -> Reader.bind f x :Reader<'r,'b>
    static member instance (_:Applicative.Apply, f:Reader<'r,_>, x:Reader<'r,'a>, _:Reader<'r,'b>) = fun () -> Reader.apply f x :Reader<'r,'b>