namespace FsControl.Core.Types

open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Monad
open FsControl.Core.Abstractions.Monoid
open FsControl.Core.Abstractions.Applicative

type Writer<'W,'A> = Writer of ('A * 'W)

module Writer =
    let runWriter (Writer x) = x :_*'w
    let mapWriter f (Writer m:Writer<'w1,_>)   = Writer(f m) :Writer<'w2,_>
    let execWriter  (Writer m:Writer<'w,_> ) s = snd m    
    let tell              w       = Writer((),     w)        :Writer<'w,_>
    let listen(Writer (a, w))     = Writer((a, w), w)        :Writer<'w,_>
    let pass  (Writer((a, f), w)) = Writer( a,   f w)        :Writer<'w,_>

open Writer    

type Writer<'W,'A> with
    static member        instance (Functor.Fmap,   Writer(a,w),                _) = fun f -> Writer(f a, w) :Writer<'w,_>
    static member inline instance (Monad.Return, _:Writer<'w,'a>                ) = fun a -> Writer(a, mempty())                                       :Writer<'w,'a>
    static member inline instance (Monad.Bind  ,   Writer(a, w), _:Writer<'w,'b>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w')) :Writer<'w,'b>
    static member inline instance (Applicative.Pure, _:Writer<'w,'a>) = fun (x:'a) -> DefaultImpl.PureFromMonad x :Writer<'w,_>
    static member inline instance (Applicative.Ap, f:Writer<'w,_>, x:Writer<'w,'a>, _:Writer<'w,'b>) = fun () -> DefaultImpl.ApFromMonad f x :Writer<'w,'b>