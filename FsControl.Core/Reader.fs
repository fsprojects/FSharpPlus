namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type Reader<'R,'A> = Reader of ('R->'A)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x
    let map   f (Reader m) = Reader(f << m) :Reader<'r,_>
    let bind  f (Reader m) = Reader(fun r -> run (f (m r)) r) :Reader<'r,'b>
    let local f (Reader m) = Reader(m << f) :Reader<'r,_>
    let ask() = Reader id

type Reader<'R,'A> with
    static member instance (_:Functor.Map, x:Reader<'r,'a>, _) = fun (f:_->'b) -> Reader.map f x
    static member instance (Applicative.Pure, _:Reader<'r,'a>       ) = fun a -> Reader(fun _ -> a)                     :Reader<'r,'a>
    static member instance (Monad.Bind ,   Reader m, _:Reader<'r,'b>) = fun k -> Reader(fun r -> Reader.run(k (m r)) r) :Reader<'r,'b>
    static member instance (_:Applicative.Apply, f:Reader<'r,_>, x:Reader<'r,'a>, _:Reader<'r,'b>) = fun () -> DefaultImpl.ApplyFromMonad f x :Reader<'r,'b>