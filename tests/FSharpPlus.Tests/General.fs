namespace FSharpPlus.Tests

open System
open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework

module SideEffects =
    let private effects = ResizeArray<string> []
    let reset() = effects.Clear()
    let add x = effects.Add(x)
    let get() = effects |> Seq.toList

type WrappedListA<'s> = WrappedListA of 's list with
    static member ToSeq    (WrappedListA lst) = List.toSeq lst
    static member OfSeq  lst = WrappedListA (Seq.toList lst)

type WrappedListB<'s> = WrappedListB of 's list with
    static member Return   (x) = WrappedListB [x]
    static member (+)  (WrappedListB l, WrappedListB x) = WrappedListB (l @ x)
    static member Zero   = WrappedListB List.empty
    static member ToSeq    (WrappedListB lst)     = List.toSeq lst
    static member FoldBack (WrappedListB x, f, z) = List.foldBack f x z

type WrappedListC<'s> = WrappedListC of 's list with
    static member (+)  (WrappedListC l, WrappedListC x) = WrappedListC (l @ x)
    static member Zero   = WrappedListC List.empty
    static member Sum  (lst: seq<WrappedListC<_>>)  = Seq.head lst

type WrappedListD<'s> = WrappedListD of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedListD [x]
    static member Bind ((WrappedListD x):WrappedListD<'T>, f) = WrappedListD (List.collect (f >> (fun (WrappedListD x) -> x)) x)
    static member inline FoldMap (WrappedListD x, f) =
        SideEffects.add "Using optimized foldMap"
        Seq.fold (fun x y -> x ++ (f y)) zero x

type WrappedListE<'s> = WrappedListE of 's list with
    static member Return  (x) = WrappedListE [x]
    static member Bind  (WrappedListE x: WrappedListE<'T>, f) = WrappedListE (List.collect (f >> (fun (WrappedListE x) -> x)) x)
    static member get_Empty() = WrappedListE List.empty
    static member Append (WrappedListE l, WrappedListE x) = WrappedListE (l @ x)
    
type WrappedListF<'s> = WrappedListF of 's list with
    static member Return  (x) = WrappedListF [x]
    static member Bind  (WrappedListF x: WrappedListF<'T>, f) = WrappedListF (List.collect (f >> (fun (WrappedListF x) -> x)) x)
    static member Join  (WrappedListF wlst) = SideEffects.add "Join";  WrappedListF wlst >>= id
    static member get_Empty() = WrappedListF List.empty
    static member Append (WrappedListF l, WrappedListF x) = WrappedListF (l @ x)

open System.Collections.Generic

module Monoid =
    [<Test>]
    let seqSumDefaultCustom() =
        let (WrappedListB x) = Seq.sum [WrappedListB [10] ;WrappedListB [15]]
        let (WrappedListC y) = Seq.sum [WrappedListC [10] ;WrappedListC [15]]
        Assert.AreEqual (x, [10;15])
        Assert.AreEqual (y, [10])

        let x = [ ("a", 1); ("b", 2); ("a", 3) ]
        let y = x |> map (Seq.singleton >> (ofSeq : seq<_*_> -> Dictionary<_,_>) >> map List.singleton) |> Seq.sum
        let z = x |> map (Seq.singleton >>             dict                      >> map List.singleton) |> Seq.sum
        Assert.IsInstanceOf<Option< Dictionary<string,int list>>> (Some y)
        Assert.IsInstanceOf<Option<IDictionary<string,int list>>> (Some z)


module Functor =
    [<Test>]
    let mapDefaultCustom() = 
        let testVal1 = map ((+) 1) {Head = 10; Tail = [20;30]}
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal1)

        let testVal2 = map ((+) 1) ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some testVal2)

        let testVal3 = map ((+) 1) (dict (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some testVal3)

    [<Test>]
    let unzip() = 
        let testVal = unzip {Head = (1, 'a'); Tail = [(2, 'b');(3, 'b')]}
        Assert.IsInstanceOf<Option<NonEmptyList<int> * NonEmptyList<char>>> (Some testVal)


module Foldable =

    [<Test>]
    let foldMapDefaultCustom() =
        SideEffects.reset()
        let x = foldMap ((+) 10) (WrappedListD [1..4]) //= 50 w side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get(), ["Using optimized foldMap"])

        SideEffects.reset()
        let y = foldMap ((+) 10) {1..4}  //= 50 w/o side effect
        Assert.AreEqual (x, 50)
        Assert.AreEqual (SideEffects.get(), [])

    [<Test>]
    let filterDefaultCustom() = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (testVal, WrappedListA [2])
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

    [<Test>]
    let foldAternatives() = 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        Assert.AreEqual (x, Some 3)
        Assert.AreEqual (y, [3;4])

    [<Test>]
    let fromToSeq() =
        let s = (seq [Collections.Generic.KeyValuePair(1, "One"); Collections.Generic.KeyValuePair(2, "Two")])
        let t = {'a'..'d'}

        let dc2:Collections.Generic.Dictionary<_,_> = ofSeq s
        let s' = toSeq dc2

        let arr:_ [] = ofSeq s
        let s'' = toSeq arr

        let str:string = ofSeq t
        let t' = toSeq str 

        Assert.AreEqual (toList s, toList s')
        Assert.AreEqual (toList s , toList s'')
        Assert.AreEqual (toList t , toList t')

        Assert.IsInstanceOf ((Some s').GetType(), Some s)
        Assert.IsInstanceOf ((Some s'').GetType(), Some s)
        Assert.IsInstanceOf ((Some t'  ).GetType(), Some t)


    [<Test>]
    let sortBy() =
        let l  = [10;4;6;89]
        let l' = sortBy id l
        let s  = WrappedListB [10;4;6;89]
        let s' = sortBy id s
        Assert.AreEqual (l', [4;6;10;89])
        Assert.AreEqual (s', WrappedListB [4;6;10;89])


module Monad = 
    [<Test>]
    let joinDefaultCustom() = 
        let x = join [[1];[2]]
        Assert.AreEqual (x, [1;2])
        let y : WrappedListE<_> = join (WrappedListE [WrappedListE [1];WrappedListE [2]])
        Assert.AreEqual (y, WrappedListE [1;2])
        SideEffects.reset()
        let z = join (WrappedListF [WrappedListF [1];WrappedListF [2]])
        Assert.AreEqual (z, WrappedListF [1;2])
        Assert.AreEqual (SideEffects.get(), ["Join"])

    [<Test>]
    let workFlow() =       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        Assert.IsInstanceOf<WrappedListD<int>>(testVal)

    [<Test>]
    let DelayForCont() = 
        // If Delay is not properly implemented this will stack-overflow
        // See http://stackoverflow.com/questions/11188779/stackoverflow-in-continuation-monad
#if MONO
        Assert.Ignore()
#else
        let map f xs =
            let rec loop xs =
                monad {
                    match xs with
                    | [] -> return []
                    | x :: xs ->
                        let! xs = loop xs
                        return f x :: xs }
            Cont.run (loop xs) id
        let q = [1..100000] |> map ((+) 1)
        Assert.Pass()
#endif


module Traversable = 
    [<Test>]
    let sequence_Default_Primitive() = 
        let testVal = sequence [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    [<Test>]
    let sequence_Specialization() =
        let inline seqSeq (x:_ seq ) = sequence x
        let inline seqArr (x:_ []  ) = sequence x
        let inline seqLst (x:_ list) = sequence x

        let a : list<_> = seqSeq (seq [[1];[3]])
        Assert.AreEqual ([seq [1; 3]], a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        Assert.AreEqual ([[|1; 3|]], b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        Assert.AreEqual ([[1; 3]], c)
        Assert.IsInstanceOf<list<list<int>>> c

    [<Test>]
    let traversableForNonPrimitive() =
        let nel = NonEmptyList.create (Some 1) [Some 2]
        let rs1  = traverse id nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs1
        let rs2  = sequence nel
        Assert.IsInstanceOf<option<NonEmptyList<int>>> rs2

    [<Test>]
    let traverseInfiniteOptions() =
        let toOptions x = if x <> 4 then Some x       else None
        let toChoices x = if x <> 4 then Choice1Of2 x else Choice2Of2 "This is a failure"
        let toLists   x = if x <> 4 then [x; x]       else []
        let a = traverse toOptions (Seq.initInfinite id)
        let b = sequence  (Seq.initInfinite toOptions)
        let c = sequence  (Seq.initInfinite toChoices)
        let d = sequence  (Seq.initInfinite toLists)
        Assert.AreEqual (None, a)
        Assert.AreEqual (None, b)
        Assert.True ((Choice2Of2 "This is a failure" = c))
        Assert.AreEqual ([], d)
        
type ZipList<'s> = ZipList of 's seq with
    static member Map    (ZipList x, f:'a->'b)               = ZipList (Seq.map f x)
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) :ZipList<'b>
    
type ZipList'<'s> = ZipList' of 's seq with
    static member Return (x:'a)                                = ZipList' (Seq.initInfinite (konst x))
    static member (<*>) (ZipList' (f:seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) :ZipList'<'b>

module Applicative = 
    [<Test>]
    let applicativeMath() = 
        let inline (+) (a:'T) (b:'T) :'T = a + b
        let inline ( .+  ) (x :'Functor't)     (y :'t)             = map ((+)/> y) x :'Functor't
        let inline (  +. ) (x :'t)             (y :'Functor't)     = map ((+)   x) y :'Functor't
        let inline ( .+. ) (x :'Applicative't) (y :'Applicative't) = (+) <!> x <*> y :'Applicative't

        let testVal = [1;2] .+. [10;20] .+. [100;200] .+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOf<Option<list<int>>> (Some testVal)


    [<Test>]
    let applicatives() = 

        let run (ZipList x) = x
        let run' (ZipList' x) = x

        // Test Applicative (functions)
        let res607 = map (+) ( (*) 100 ) 6 7
        let res606 = ( (+) <*>  (*) 100 ) 6
        let res508 = (map (+) ((+) 3 ) <*> (*) 100) 5

        // Test Applicative (ZipList)
        let res9n5   = map ((+) 1) (ZipList [8;4])
        let res20n30 = result (+) <*> result 10 <*> ZipList [10;20]
        let res18n14 = result (+) <*> ZipList [8;4] <*> result 10
        let res9n5'  = map ((+) 1) (ZipList' [8;4])

        Assert.AreEqual (607, res607)
        Assert.AreEqual (606, res606)
        Assert.AreEqual (508, res508)
        Assert.AreEqual (toList (run res9n5), toList (run' res9n5'))


// Idiom brackets from http://www.haskell.org/haskellwiki/Idiom_brackets
type Ii = Ii
type Ji = Ji
type J = J
type Idiomatic = Idiomatic with
    static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
    static member        ($) (Idiomatic, Ii) = id

module IdiomBrackets =
    [<Test>]
    let idiomBrackets() =    
        let inline idiomatic a b = (Idiomatic $ b) a
        let inline iI x = (idiomatic << result) x

        let res3n4''  = iI ((+) 2) [1;2] Ii
        let res3n4''' = iI (+) (result 2) [1;2] Ii   // fails to compile when constraints are not properly defined
        Assert.AreEqual ([3;4], res3n4'' )
        Assert.AreEqual ([3;4], res3n4''')


        let output = System.Text.StringBuilder()
        let append (x:string) = output.Append x |> ignore

        let v5: Lazy<_> = lazy (append "5"; 5)
        Assert.AreEqual (0, output.Length)
        let fPlus10 x   = lazy (append " + 10"; x + 10)
        Assert.AreEqual (0, output.Length)
        let v5plus10    = v5 >>= fPlus10
        Assert.AreEqual (0, output.Length)
        let v15 = v5plus10.Force()
        Assert.AreEqual ("5 + 10", output.ToString())
        Assert.AreEqual (15, v15)

        output.Clear() |> ignore

        let v4ll: Lazy<_> = lazy (append "outer"; lazy (append "inner"; 4))
        Assert.AreEqual (0, output.Length)
        let v4l = join v4ll
        Assert.AreEqual (0, output.Length)
        let v4  = v4l.Force()
        Assert.AreEqual ("outerinner", output.ToString())
        Assert.AreEqual (4, v4)
 

module MonadPlus = 
    [<Test>]
    let zeroAndPlus() = 
        let v = WrappedListE [1;2]
        let x = v <|> getEmpty()
        let y = getEmpty() <|> v
        Assert.AreEqual (v, x)
        Assert.AreEqual (v, y)


module NumericLiteralG =
    open FsControl
    let inline FromZero() = Zero.Invoke()
    let inline FromOne () = One.Invoke()
    let inline FromInt32  (i:int   ) = FromInt32.Invoke i
    let inline FromInt64  (i:int64 ) = FromInt64.Invoke i
    let inline FromString (i:string) = fromBigInt <| System.Numerics.BigInteger.Parse i

open MathNet.Numerics

module Numerics = 
    [<Test>]
    let genericMath() = 
        let argUint        :uint32       =              42G
        let argInt         :    int      =         -424242G
        let argBigInt      : bigint      = -42424242424242G
        let argFloat       : float       = -(42G + (42G/100G))  // -42.42
        let argFloat32     : float32     = -(42G + (42G/100G))  // -42.4199982f
        let argDecimal     : decimal     = -(42G + (42G/100G))
        let argComplex                   = Complex.mkRect(-42.42, 24.24)
        let argComplex32                 = Complex32.mkRect(-42.42f, 24.24f)
        let argBigRational : BigRational = -42424242424242G / 42424G

        let res01 = signum' argUint       
        let res02 = signum' argInt        
        let res03 = signum' argBigInt     
        let res04 = signum' argFloat      
        let res05 = signum' argFloat32    
        let res06 = signum' argDecimal    
        let res07 = signum' argComplex    
        let res08 = signum' argComplex32  
        let res09 = signum' argBigRational

        let res11 = abs' argUint       
        let res12 = abs' argInt        
        let res13 = abs' argBigInt     
        let res14 = abs' argFloat      
        let res15 = abs' argFloat32    
        let res16 = abs' argDecimal    
        let res17 = abs' argComplex    
        let res18 = abs' argComplex32  
        let res19 = abs' argBigRational

        Assert.AreEqual(res09 * res19, argBigRational)


type Sum<'a> = Sum of 'a with
    static member inline get_Zero() = Sum 0G
    static member inline (+) (Sum (x:'n), Sum(y:'n)) = Sum (x + y)


module Splits = 
    [<Test>]
    let splitArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|]  |> Seq.map System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((toList a1 = toList a2))
        Assert.IsTrue((toList b1 = toList b2))

    [<Test>]
    let replaceArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B  |> System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B |> System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b1 = b2))

    [<Test>]
    let intercalateArraysAndStrings() = 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B  |> System.Text.Encoding.ASCII.GetString

        let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        // Fails to compile but works in F#4.1
        // let c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        // 

        let d = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        Assert.IsTrue((d = Sum 13))


module Parsing = 
    [<Test>]
    let parse() = 
        let v1 : DateTime       = parse "2011-03-04T15:42:19+03:00"
        let v2 : DateTimeOffset = parse "2011-03-04T15:42:19+03:00"

        Assert.IsTrue((v1 = DateTime(2011,3,4,12,42,19)))
        Assert.IsTrue((v2 = DateTimeOffset(2011,3,4,15,42,19, TimeSpan.FromHours 3.)))

module ApplicativeInference =

    // test applicative from monad
    let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline ap     x y     = liftM2 id x y
    let res2n4n8 = result pown </ap/> result 2. <*> [1;2;3]

    let res9n5   = map ((+) 1) (ZipList(seq [8;4]))
    let res18n24 = result (+) <*> ZipList(seq [8;4]) <*> ZipList(seq [10;20])

    open FSharpPlus.Operators.GenericMath

    let res6n7n8 = result (+) <*> result 5G <*> ZipList [1;2;3]
    let res18n14 = result (+) <*> ZipList(seq [8;4]) <*> result 10

    open FSharpPlus.Builders

    let res3n4''  = iI ((+) 2) [1;2] Ii
    let res3n4''' = iI (+) (result 2) [1;2] Ii                               // *1
    let res18n24' = iI (+) (ZipList(seq [8;4])) (ZipList(seq [10;20])) Ii
    // let res6n7n8' = iI (+) (result 5G          ) (ZipList [1;2;3]     ) Ii   // *1, *2
    let res18n14' = iI (+) (ZipList(seq [8;4])) (result 10            ) Ii

    let safeDiv x y = if y = 0 then None else Some (x </div/> y)
    let resSome3    = join (iI safeDiv (Some 6) (Some 2) Ii)
    let resSome3'   =       iI safeDiv (Some 6) (Some 2) Ji

    let safeDivBy y = if y = 0 then None else Some (fun x -> x </div/> y)
    let resSome2  = join (result safeDivBy  <*> Some 4G) <*> Some 8G
    let resSome2' = join (   iI safeDivBy (Some 4G) Ii) <*> Some 8G

    let resSome2'' = iI safeDivBy (Some 4G) J (Some 8G) Ii
    let resNone = iI safeDivBy (Some 0G) J (Some 8G) Ii
    let res16n17   = iI (+) (iI (+) (result 4) [2;3] Ii ) (result 10: _ list) Ii   // *1

    // *1 These lines fails when Apply.Invoke has no 'or ^'``Applicative<'U>`` ' (output) constraint.
    // *2 F# 4.1 regression