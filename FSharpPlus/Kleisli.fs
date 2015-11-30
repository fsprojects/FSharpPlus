namespace FSharpPlus

open FsControl
open FSharpPlus.Operators

type Kleisli<'t, '``monad<'u>``> = Kleisli of ('t -> '``monad<'u>``) with

    // Profunctor
    static member inline Dimap (Kleisli bmc :Kleisli<'B,'``Monad<'C>``>, ab:'A->'B, cd:'C->'D) = let cmd = map cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
    static member        LMap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, k:'A->'B ) = Kleisli (k >> f)      : Kleisli<'A,'``Monad<'C>``>
    static member inline RMap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, cd:'C->'D) = Kleisli (map cd << f) : Kleisli<'B,'``Monad<'D>``>
    
    // Category
    static member inline Id (_:Kleisli<'a,'b>, _:Id) = Kleisli result :Kleisli<'a,'b>
    static member inline Comp (Kleisli f, _, _:Comp) = fun (Kleisli g) -> Kleisli (g >=> f)

    // Arrow
    static member inline Arr (_:Kleisli<_,_>, _:Arr) = fun f -> Kleisli ((<<) result f)
    static member inline ArrFirst  (Kleisli f, _:Kleisli<_,_>, _:ArrFirst ) = Kleisli (fun (b,d) -> f b >>= fun c -> result (c,d))
    static member inline ArrSecond (Kleisli f, _:Kleisli<_,_>, _:ArrSecond) = Kleisli (fun (d,b) -> f b >>= fun c -> result (d,c))
    static member inline AcEither (_:Kleisli<_,_>, _:AcEither) = fun (Kleisli f, Kleisli g) -> Kleisli (choice f g)

    static member inline AcMerge (_:Kleisli<Choice<'v,'T>,'z>, _:AcMerge) = fun ((Kleisli (f:'T->'u)), (Kleisli (g:'v->'w))) ->
        AcEither.Invoke (Kleisli (f >=> ((<<) result Choice2Of2))) (Kleisli (g >=> ((<<) result Choice1Of2))) : Kleisli<Choice<'v,'T>,'z>

    static member inline AcLeft (Kleisli f, _, _:AcLeft) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Kleisli f) (Arr.Invoke (Id.Invoke()))
    static member inline AcRight (Kleisli f, _, _:AcRight) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke())) (Kleisli f)
    static member ArrApply (_: Kleisli<Kleisli<'a,'b> * 'a,'b>, _:ArrApply) = Kleisli (fun (Kleisli f, x) -> f x)
    
    // ArrowPlus
    static member inline MZero (output :Kleisli<'T,'``Monad<'U>``>, mthd :MZero) = Kleisli (fun _ -> MZero.Invoke ())
    static member inline MPlus (Kleisli f, Kleisli g, mthd:MPlus) = Kleisli (fun x -> MPlus.Invoke (f x) (g x))

[<RequireQualifiedAccess>]module Kleisli = let run (Kleisli f) = f