namespace FsControl.Core.Types

type Kleisli<'a, 'm> = Kleisli of ('a -> 'm)

[<RequireQualifiedAccess>]
module Kleisli = let run (Kleisli f) = f

namespace FsControl.Core.TypeMethods

open FsControl.Core.Prelude
open FsControl.Core.TypeMethods.Monad
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.Types
open FsControl.Core.TypeMethods.MonadPlus

module Category =

    type Id = Id with
        static member        instance (Id, _: 'r -> 'r     ) = fun () -> id              : 'r -> 'r
        static member inline instance (Id, _:Kleisli<'a,'b>) = fun () -> Kleisli return' :Kleisli<'a,'b>

    type Comp = Comp with
        static member        instance (Comp,         f, _) = fun (g: _ -> _) ->          g >>  f
        static member inline instance (Comp, Kleisli f, _) = fun (Kleisli g) -> Kleisli (g >=> f)

    let inline internal id'() = Inline.instance Id ()
    let inline internal (<<<) f g = Inline.instance (Comp, f) g

open Category

module Arrow =

    type Arr = Arr with
        static member        instance (Arr, _: _ -> _     ) = fun (f:_->_) -> f
        static member inline instance (Arr, _:Kleisli<_,_>) = fun  f       -> Kleisli (return' <<< f)

    type First = First with
        static member        instance (First, f        , _: 'a -> 'b   ) = fun () -> fun (x,y) -> (f x, y)
        static member inline instance (First, Kleisli f, _:Kleisli<_,_>) = fun () -> Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))

    let inline internal arr   f = Inline.instance  Arr    f
    let inline internal first f = Inline.instance (First, f) ()

    type SecondDefault() =
        static member inline instance (_:SecondDefault, f:#obj, _:#obj) = fun () ->
            let aswap = Inline.instance Arr (fun (x,y) -> (y,x))
            aswap <<< first f <<< aswap

    type Second() =
        inherit SecondDefault()
        static member        instance (_:Second, f        , _: 'a -> 'b   ) = fun () -> fun (x,y) -> (x, f y)
        static member inline instance (_:Second, Kleisli f, _:Kleisli<_,_>) = fun () -> Kleisli (fun (d,b) -> f b >>= fun c -> return' (d,c))
    
    let Second = Second()

open Arrow

module ArrowChoice =

    type AcEither = AcEither with
        static member inline instance (AcEither, _:Choice<_,_>->_) = fun (         f ,          g ) ->          choice f g
        static member inline instance (AcEither, _:Kleisli<_,_>  ) = fun ((Kleisli f), (Kleisli g)) -> Kleisli (choice f g)

    let inline internal (|||) f g = Inline.instance AcEither (f, g)

    type AcMerge = AcMerge with
        static member inline instance (AcMerge, _: _->    Choice<_,_>      ) = fun (f, g)  ->  (Choice2Of2 << f) ||| (Choice1Of2 << g)
        static member inline instance (AcMerge, _:Kleisli<Choice<'v,'t>,'z>) = fun ((Kleisli (f:'t->'u)), (Kleisli (g:'v->'w))) ->
            Kleisli (f >=> (return' <<< Choice2Of2)) ||| Kleisli (g >=> (return' <<< Choice1Of2)) :Kleisli<Choice<'v,'t>,'z>

    let inline internal (+++) f g = Inline.instance AcMerge (f, g)

    type AcLeft = AcLeft with
        static member inline instance (AcLeft, f:_->_   , _) = fun () ->          f  +++      id
        static member inline instance (AcLeft, Kleisli f, _) = fun () -> (Kleisli f) +++ arr (id'())

    type AcRight = AcRight with
        static member inline instance (AcRight, f:_->_   , _) = fun () -> id          +++ f
        static member inline instance (AcRight, Kleisli f, _) = fun () -> arr (id'()) +++ Kleisli f


module ArrowApply =

    type Apply = Apply with
        static member instance (Apply, _: ('a -> 'b) * 'a -> 'b          ) = fun () ->          fun (f,x)          -> f x
        static member instance (Apply, _: Kleisli<Kleisli<'a,'b> * 'a,'b>) = fun () -> Kleisli (fun (Kleisli f, x) -> f x)
        
type Dummy<'a, 'm> = Dummy of ('a -> 'm)

module ArrowZero =

    type ZeroArrow = ZeroArrow with
        static member inline instance (ZeroArrow, _:Dummy<_,_>  ) = fun () -> Dummy   (fun _ -> mzero ())
        static member inline instance (ZeroArrow, _:Kleisli<_,_>) = fun () -> Kleisli (fun _ -> mzero ())
 
module ArrowPlus =
    
    type Plus = Plus with
        static member inline instance (Plus, Dummy   f, _) = fun (Dummy   g) -> Dummy  (fun x -> mplus (f x) (g x))
        static member inline instance (Plus, Kleisli f, _) = fun (Kleisli g) -> Kleisli(fun x -> mplus (f x) (g x))