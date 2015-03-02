namespace FsControl.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsControl.Operators

module Combinators =
    let inline flip f x y = f y x
    let inline konst k _ = k
    let inline (</) x = (|>) x
    let inline (/>) x = flip x
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline option n f = function None -> n | Some x -> f x

open Combinators

[<TestClass>]
type Applicative() = 
    [<TestMethod>]
    member x.ApplicativeMath() = 
        let inline (+) (a:'T) (b:'T) :'T = a + b
        let inline ( |+  ) (x :'Functor't)     (y :'t)             = map ((+)/> y) x :'Functor't
        let inline (  +| ) (x :'t)             (y :'Functor't)     = map ((+)   x) y :'Functor't
        let inline ( |+| ) (x :'Applicative't) (y :'Applicative't) = (+) <!> x <*> y :'Applicative't

        let testVal = [1;2] |+| [10;20] |+| [100;200] |+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOfType (Some testVal, typeof<Option<list<int>>>)



type WrappedListA<'s> = WrappedListA of 's list with
    static member ToSeq    (WrappedListA lst) = List.toSeq lst
    static member FromSeq  lst = WrappedListA (Seq.toList lst)

[<TestClass>]
type Foldable() = 
    [<TestMethod>]
    member x.filter_Default_Custom() = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (testVal, WrappedListA [2])
        Assert.IsInstanceOfType(Some testVal, typeof<Option<WrappedListA<int>>>)

[<TestClass>]
type Traversable() = 
    [<TestMethod>]
    member x.sequenceA_Default_Primitive() = 
        let testVal = sequenceA [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOfType (testVal, typeof<Option<array<int>>>)

        
