namespace FSharpPlus.Tests

open System
open NUnit.Framework
open Helpers

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.TypeLevel

module VectorTests =
    [<Test>]
    let constructorAndDeconstructorWorks() =
        let v1 = vector (1,2,3,4,5)
        let v2 = vector (1,2,3,4,5,6,7,8,9,0,1,2,3,4,5)
        let (Vector(_,_,_,_,_)) = v1
        let (Vector(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) = v2
        ()

    [<Test>]
    let applicativeWorks() =
        let v = vector ((fun i -> i + 1), (fun i -> i * 2))
        let u = vector (2, 3)
        let vu = v <*> u
        NUnit.Framework.Assert.IsInstanceOf<Option<Vector<int,S<S<Z>>>>> (Some vu)
        CollectionAssert.AreEqual ([|3; 6|], Vector.toArray vu)

    [<Test>]
    let satisfiesApplicativeLaws() =
        let u = vector ((fun i -> i - 1), (fun i -> i * 2))
        let v = vector ((fun i -> i + 1), (fun i -> i * 3))
        let w = vector (1, 1)

        areEqual (result id <*> v) v
        areEqual (result (<<) <*> u <*> v <*> w) (u <*> (v <*> w))
        areEqual (result 2) ((result (fun i -> i + 1) : Vector<int -> int, S<S<Z>>>) <*> result 1)
        areEqual (u <*> result 1) (result ((|>) 1) <*> u)

    [<Test>]
    let satisfiesMonadLaws() =
        let k = fun (a: int) -> vector (a - 1, a * 2)
        let h = fun (a: int) -> vector (a + 1, a * 3)
        let m = vector (1, 2)

        areEqual (result 2 >>= k) (k 2)
        areEqual (m >>= result) m
        areEqual (m >>= (fun x -> k x >>= h)) ((m >>= k) >>= h)

module MatrixTests =
    [<Test>]
    let constructorAndDeconstructorWorks() =
        let m1 =
          matrix (
            (1,0,0,0),
            (0,1,0,0),
            (0,0,1,0)
          )
        let m2 =
          matrix (
            (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
          )
        let (Matrix(_x1,_x2,_x3)) = m1
        let (Matrix(_y1: int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int,_y2,_y3,_y4,_y5,_y6,_y7,_y8)) = m2
        ()

    [<Test>]
    let satisfiesApplicativeLaws() =
        let u = matrix (
          ((fun i -> i - 1), (fun i -> i * 2)),
          ((fun i -> i + 1), (fun i -> i * 3))
        )
        let v = matrix (
          ((fun i -> i - 2), (fun i -> i * 5)),
          ((fun i -> i + 2), (fun i -> i * 7))
        )
        let w = matrix ((1, 1), (1, 2))

        areEqual (result id <*> v) v
        areEqual (result (<<) <*> u <*> v <*> w) (u <*> (v <*> w))
        areEqual ((result (fun i -> i + 1) : Matrix<int -> int, S<S<Z>>, S<S<Z>>>) <*> result 1)  (result 2)
        areEqual (u <*> result 1) (result ((|>) 1) <*> u)

    [<Test>]
    let satisfiesMonadLaws() =
        let k = fun (a: int) -> matrix ((a - 1, a * 2), (a + 1, a * 3))
        let h = fun (a: int) -> matrix ((a - 2, a * 5), (a + 2, a * 7))
        let m = matrix ((1, 1), (1, 2))

        areEqual (result 2 >>= k) (k 2)
        areEqual (m >>= result) m
        areEqual (m >>= (fun x -> k x >>= h)) ((m >>= k) >>= h)
