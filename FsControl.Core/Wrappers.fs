namespace FsControl
open FsControl.Core.Internals.Prelude

type Dual<'T> = Dual of 'T with
    static member inline Mempty  (_:Dual<'m>, _:Mempty) = Dual (Mempty.Invoke()) :Dual<'m>
    static member inline Mappend (  Dual x  ,   Dual y) = Dual (Mappend.Invoke y x)
[<RequireQualifiedAccess>]module Dual = let run (Dual x) = x

type Endo<'T> = Endo of ('T -> 'T) with
    static member        Mempty  (_:Endo<'m>, _:Mempty) = Endo id  :Endo<'m>
    static member        Mappend (  Endo f  ,   Endo g) = Endo (f << g)
[<RequireQualifiedAccess>]module Endo = let run (Endo f) = f


type All = All of bool with
    static member Mempty  (_:All, _:Mempty) = All true
    static member Mappend (  All x, All y ) = All (x && y)

type Any = Any of bool with
    static member Mempty  (_:Any, _:Mempty) = Any false
    static member Mappend (  Any x, Any y ) = Any (x || y)


type Identity<'T> = Identity of 'T
[<RequireQualifiedAccess>]module Identity = let run (Identity x) = x

type Const<'T,'U> = Const of 'T with
    static member inline Mempty  (_: Const<'t,'u>      , _:Mempty             ) = Const (Mempty.Invoke())    : Const<'t,'u>
    static member inline Mappend (Const x: Const<'t,'u>, Const y: Const<'t,'u>) = Const (Mappend.Invoke x y) : Const<'t,'u>
    static member inline Return  (_:'u) = Const (Mempty.Invoke()) : Const<'t,'u>
[<RequireQualifiedAccess>]module Const = let run (Const t) = t


type First<'T> = First of Option<'T> with
    static member Mempty  (_:First<'t>, _:Mempty   ) = First None :First<'t>
    static member Mappend (x:First<'t>, y:First<'t>) = match x, y with First None, r -> r | l, _ -> l
    static member run (First a) = a

type Last<'T> = Last of Option<'T> with
    static member Mempty  (_:Last<'t>, _:Mempty   ) = Last None :Last<'t>
    static member Mappend (x:Last<'t>, y:Last<'t>) = match x, y with l, Last None -> l | _, r -> r
[<RequireQualifiedAccess>]module Last = let run (Last a) = a