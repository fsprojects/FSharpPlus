namespace FSharpPlus.Data

#if (!FABLE_COMPILER || FABLE_COMPILER_3) && !FABLE_COMPILER_4

open FSharpPlus
open FSharpPlus.Control


/// Kleisli arrows of a monad. Represents a function 'T -> 'Monad<'U>
[<Struct; NoEquality; NoComparison>]
type Kleisli<'t, '``monad<'u>``> = Kleisli of ('t -> '``monad<'u>``) with

    // Profunctor
    static member inline Dimap (Kleisli bmc: Kleisli<'B,'``Monad<'C>``>, ab: 'A->'B, cd: 'C->'D) = let cmd = map cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
    static member        Contramap (Kleisli f : Kleisli<'B,'``Monad<'C>``>, k: 'A->'B) = Kleisli (k >> f)      : Kleisli<'A,'``Monad<'C>``>
    static member inline Map (Kleisli f : Kleisli<'B,'``Monad<'C>``>, cd: 'C->'D     ) = Kleisli (map cd << f) : Kleisli<'B,'``Monad<'D>``>
    
    
    // Category
    static member inline get_Id () = Kleisli result : Kleisli<'a,'b>
    static member inline (<<<) (Kleisli f, Kleisli g) = Kleisli (g >=> f)

    // Arrow
    static member inline Arr f = Kleisli ((<<) result f)
    static member inline First  (Kleisli f) = Kleisli (fun (b, d) -> f b >>= fun c -> result (c, d))
    static member inline Second (Kleisli f) = Kleisli (fun (d, b) -> f b >>= fun c -> result (d, c))
    static member inline (|||) (Kleisli f, Kleisli g) = Kleisli (Choice.either g f)

    static member inline (+++) (Kleisli (f: 'T->'u), Kleisli (g: 'v->'w)) =
        Fanin.InvokeOnInstance (Kleisli (f >=> ((<<) result Choice2Of2))) (Kleisli (g >=> ((<<) result Choice1Of2))) : Kleisli<Choice<'v,'T>,'z>

    static member inline Left  (Kleisli f) = AcMerge.Invoke (Kleisli f) (Arr.Invoke (Id.Invoke ()))
    static member inline Right (Kleisli f) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke ())) (Kleisli f)
    static member get_App () = Kleisli (fun (Kleisli f, x) -> f x)
    
    // ArrowPlus
    static member inline Empty (_output: Kleisli<'T,'``Monad<'U>``>, _mthd: Empty) = Kleisli (fun _ -> Empty.Invoke ())
    static member inline ``<|>`` (Kleisli f, Kleisli g, _mthd: Append) = Kleisli (fun x -> Append.Invoke (f x) (g x))

/// Basic operations on Kleisli
[<RequireQualifiedAccess>]module Kleisli = let run (Kleisli f) = f

#endif