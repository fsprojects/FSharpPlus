namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative

type Cont<'R,'A> = Cont of (('A->'R)->'R)

[<RequireQualifiedAccess>]
module Cont =
    let run (Cont x) = x
    let callCC (f:(_->Cont<'r,'b>)->_) = Cont <| fun k -> run (f (fun a -> Cont(fun _ -> k a))) k

type Cont<'R,'A> with
    static member instance (_:Functor.Map, Cont m:Cont<'r,'a>, _) = fun (f:_->'b) -> Cont(fun c -> m (c << f))
    static member instance (Applicative.Pure, _:Cont<'r,'a>     ) = fun n -> Cont(fun k -> k n)                          :Cont<'r,'a>
    static member instance (Monad.Bind ,   Cont m, _:Cont<'r,'b>) = fun f -> Cont(fun k -> m (fun a -> Cont.run(f a) k)) :Cont<'r,'b>
    static member instance (_:Applicative.Apply, f:Cont<'r,_>, x:Cont<'r,'a>, _:Cont<'r,'b>) = fun () -> DefaultImpl.ApplyFromMonad f x :Cont<'r,'b>