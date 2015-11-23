namespace FsControl
open FsControl.Core.Internals.Prelude

type Dual<'T> = Dual of 'T with
    static member inline Empty  (_:Dual<'m>, _:Empty) = Dual (Empty.Invoke()) :Dual<'m>
    static member inline Append (  Dual x  ,   Dual y) = Dual (Append.Invoke y x)
[<RequireQualifiedAccess>]module Dual = let run (Dual x) = x

type Endo<'T> = Endo of ('T -> 'T) with
    static member        Empty  (_:Endo<'m>, _:Empty) = Endo id  :Endo<'m>
    static member        Append (  Endo f  ,   Endo g) = Endo (f << g)
[<RequireQualifiedAccess>]module Endo = let run (Endo f) = f


type All = All of bool with
    static member Empty  (_:All, _:Empty) = All true
    static member Append (  All x, All y ) = All (x && y)

type Any = Any of bool with
    static member Empty  (_:Any, _:Empty) = Any false
    static member Append (  Any x, Any y ) = Any (x || y)


type Identity<'T> = Identity of 'T
[<RequireQualifiedAccess>]module Identity = let run (Identity x) = x

type Const<'T,'U> = Const of 'T with
    static member inline Empty  (_: Const<'t,'u>      , _:Empty             ) = Const (Empty.Invoke())    : Const<'t,'u>
    static member inline Append (Const x: Const<'t,'u>, Const y: Const<'t,'u>) = Const (Append.Invoke x y) : Const<'t,'u>
    static member inline Return  (_:'u) = Const (Empty.Invoke()) : Const<'t,'u>
[<RequireQualifiedAccess>]module Const = let run (Const t) = t


type First<'T> = First of Option<'T> with
    static member Empty  (_:First<'t>, _:Empty   ) = First None :First<'t>
    static member Append (x:First<'t>, y:First<'t>) = match x, y with First None, r -> r | l, _ -> l
    static member run (First a) = a

type Last<'T> = Last of Option<'T> with
    static member Empty  (_:Last<'t>, _:Empty   ) = Last None :Last<'t>
    static member Append (x:Last<'t>, y:Last<'t>) = match x, y with l, Last None -> l | _, r -> r
[<RequireQualifiedAccess>]module Last = let run (Last a) = a