namespace FsControl.Core.Types
open FsControl.Core.Prelude
open FsControl.Core.TypeMethods

type Dual<'a> = Dual of 'a with
    static member inline Mempty  (_:Dual<'m>, _:Mempty) = Dual (Mempty.Invoke()) :Dual<'m>
    static member inline Mappend (  Dual x  ,   Dual y) = Dual (Mappend.Invoke y x)
module Dual = let inline  internal getDual (Dual x) = x

type Endo<'a> = Endo of ('a -> 'a) with
    static member        Mempty  (_:Endo<'m>, _:Mempty) = Endo id  :Endo<'m>
    static member        Mappend (  Endo f  ,   Endo g) = Endo (f << g)
module Endo = let inline  internal appEndo (Endo f) = f


type All = All of bool with
    static member Mempty  (_:All, _:Mempty) = All true
    static member Mappend (  All x, All y ) = All (x && y)

type Any = Any of bool with
    static member Mempty  (_:Any, _:Mempty) = Any false
    static member Mappend (  Any x, Any y ) = Any (x || y)