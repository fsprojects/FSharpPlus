namespace FSharpPlus

open FSharpPlus.Operators

type Dual<'T> = Dual of 'T with
    static member inline get_Empty() = Dual (getEmpty())
    static member inline Append (Dual x, Dual y) = Dual (append y x)
[<RequireQualifiedAccess>]module Dual = let run (Dual x) = x

type Endo<'T> = Endo of ('T -> 'T) with
    static member get_Empty() = Endo id
    static member Append (Endo f, Endo g) = Endo (f << g)
[<RequireQualifiedAccess>]module Endo = let run (Endo f) = f


type All = All of bool with
    static member Empty = All true
    static member Append (All x, All y) = All (x && y)

type Any = Any of bool with
    static member Empty = Any false
    static member Append (Any x, Any y) = Any (x || y)


type Identity<'t> = Identity of 't with
    static member Return (_:Identity<'T>  , _:FsControl.Return) = fun x -> Identity x :Identity<'T>
    static member Apply (Identity (f:'T->'U)     , Identity (x: 'T)     , output:Identity<'U> , impl:FsControl.Apply) = Identity (f x)      : Identity<'U>
    static member Map (Identity x         , f:'T->'U) = Identity (f x)
[<RequireQualifiedAccess>]module Identity = let run (Identity x) = x

type Const<'t,'u> = Const of 't with
    static member inline get_Empty() = Const (getEmpty()) : Const<'T,'U>
    static member inline Append (Const x : Const<'T,'U>, Const y : Const<'T,'U>) = Const (append x y) : Const<'T,'U>
    static member inline Return (_:'U) = Const (getEmpty()) : Const<'T,'U>
    static member inline Apply (Const f : Const<'C,'T->'U>, Const x : Const<'C,'T>, output:Const<'C,'U>, impl:FsControl.Apply) = Const (append f x) : Const<'C,'U>
    static member        Map (Const x : Const<_,'T>, f:'T->'U) = Const x : Const<'C,'U>
    static member        Contramap (Const x : Const<'C,'T>, _:'U->'T) = Const x  :Const<'C,'U>
    static member        First  (Const x : Const<'T,'V>, f:'T->'U) = Const (f x) : Const<'U,'V>
    static member        Second (Const x : Const<'T,'V>, _:'V->'W) = Const x : Const<'T,'W>
[<RequireQualifiedAccess>]module Const = let run (Const t) = t


type First<'t> = First of Option<'t> with
    static member Empty  (_:First<'T>, _:FsControl.Empty   ) = First None :First<'T>
    static member Append (x:First<'T>, y:First<'T>) = match x, y with First None, r -> r | l, _ -> l
    static member run (First a) = a

type Last<'t> = Last of Option<'t> with
    static member Empty = Last None
    static member Append (x:Last<'T>, y:Last<'T>) = match x, y with l, Last None -> l | _, r -> r
[<RequireQualifiedAccess>]module Last = let run (Last a) = a