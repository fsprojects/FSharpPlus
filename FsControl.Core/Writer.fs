namespace FsControl.Core.Types

open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.Monoid

type Writer<'W,'A> = Writer of ('A * 'W)

[<RequireQualifiedAccess>]
module Writer =
    let run (Writer x) = x :_*'w
    let map f (Writer (a, w)) = Writer (f a, w)
    let inline bind  f (Writer (a, w)) = Writer (let (b, w') = run (f a) in (b, mappend w w')) :Writer<'w,'b>
    let inline apply   (Writer (f, a)) (Writer (x, b)) = Writer (f x, mappend a b)
    let exec (Writer m:Writer<'w,_> ) s = snd m    
    let tell              w       = Writer((),     w) :Writer<'w,_>
    let listen (Writer (a, w))     = Writer((a, w), w) :Writer<'w,_>
    let pass   (Writer((a, f), w)) = Writer( a,   f w) :Writer<'w,_>

type Writer<'W,'A> with
    static member        instance (_:Functor.Map   , x,            _) = fun f -> Writer.map f x :Writer<'w,_>
    static member inline instance (Applicative.Pure, _:Writer<'w,'a>) = fun a -> Writer(a, mempty())                                        :Writer<'w,'a>
    static member inline instance (Monad.Bind  , x, _:Writer<'w,'b> ) = fun f -> Writer.bind f x :Writer<'w,'b>
    static member inline instance (_:Applicative.Apply, f:Writer<'w,_>, x:Writer<'w,'a>, _:Writer<'w,'b>) = fun () -> 
        Writer.apply f x :Writer<'w,'b>