namespace FsControl
open FsControl.Core.Internals.Prelude

type Dual<'T> = Dual of 'T with
    static member inline MEmpty  (_:Dual<'m>, _:MEmpty) = Dual (MEmpty.Invoke()) :Dual<'m>
    static member inline MAppend (  Dual x  ,   Dual y) = Dual (MAppend.Invoke y x)
[<RequireQualifiedAccess>]module Dual = let run (Dual x) = x

type Endo<'T> = Endo of ('T -> 'T) with
    static member        MEmpty  (_:Endo<'m>, _:MEmpty) = Endo id  :Endo<'m>
    static member        MAppend (  Endo f  ,   Endo g) = Endo (f << g)
[<RequireQualifiedAccess>]module Endo = let run (Endo f) = f


type All = All of bool with
    static member MEmpty  (_:All, _:MEmpty) = All true
    static member MAppend (  All x, All y ) = All (x && y)

type Any = Any of bool with
    static member MEmpty  (_:Any, _:MEmpty) = Any false
    static member MAppend (  Any x, Any y ) = Any (x || y)


type Identity<'T> = Identity of 'T
[<RequireQualifiedAccess>]module Identity = let run (Identity x) = x

type Const<'T,'U> = Const of 'T with
    static member inline MEmpty  (_: Const<'t,'u>      , _:MEmpty             ) = Const (MEmpty.Invoke())    : Const<'t,'u>
    static member inline MAppend (Const x: Const<'t,'u>, Const y: Const<'t,'u>) = Const (MAppend.Invoke x y) : Const<'t,'u>
    static member inline Return  (_:'u) = Const (MEmpty.Invoke()) : Const<'t,'u>
[<RequireQualifiedAccess>]module Const = let run (Const t) = t


type First<'T> = First of Option<'T> with
    static member MEmpty  (_:First<'t>, _:MEmpty   ) = First None :First<'t>
    static member MAppend (x:First<'t>, y:First<'t>) = match x, y with First None, r -> r | l, _ -> l
    static member run (First a) = a

type Last<'T> = Last of Option<'T> with
    static member MEmpty  (_:Last<'t>, _:MEmpty   ) = Last None :Last<'t>
    static member MAppend (x:Last<'t>, y:Last<'t>) = match x, y with l, Last None -> l | _, r -> r
[<RequireQualifiedAccess>]module Last = let run (Last a) = a