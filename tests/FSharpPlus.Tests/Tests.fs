module FSharpPlus.Tests

open System
open FSharpPlus.Operators
open FSharpPlus.Builders
open FSharpPlus.Data
open NUnit.Framework


type WrappedListA<'s> = WrappedListA of 's list with
    static member ToSeq    (WrappedListA lst) = List.toSeq lst
    static member OfSeq  lst = WrappedListA (Seq.toList lst)

type WrappedListB<'s> = WrappedListB of 's list with
    static member Return   (x) = WrappedListB [x]
    static member Append  (WrappedListB l, WrappedListB x) = WrappedListB (l @ x)
    static member Empty   = WrappedListB List.empty
    static member ToSeq    (WrappedListB lst)     = List.toSeq lst
    static member FoldBack (WrappedListB x, f, z) = List.foldBack f x z

type WrappedListC<'s> = WrappedListC of 's list with
    static member Append  (WrappedListC l, WrappedListC x) = WrappedListC (l @ x)
    static member Empty   = WrappedListC List.empty
    static member Concat  (lst: seq<WrappedListC<_>>)  = Seq.head lst

type WrappedListD<'s> = WrappedListD of 's list with
    interface Collections.Generic.IEnumerable<'s> with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator()
    interface Collections.IEnumerable             with member x.GetEnumerator() = (let (WrappedListD x) = x in x :> _ seq).GetEnumerator() :> Collections.IEnumerator
    static member Return  (x) = WrappedListD [x]
    static member Bind ((WrappedListD x):WrappedListD<'T>, f) = WrappedListD (List.collect (f >> (fun (WrappedListD x) -> x)) x)

type WrappedListE<'s> = WrappedListE of 's list with
    static member Return  (x) = WrappedListE [x]
    static member Bind  (WrappedListE x: WrappedListE<'T>, f) = WrappedListE (List.collect (f >> (fun (WrappedListE x) -> x)) x)
    static member get_MZero() = WrappedListE List.empty
    static member MPlus (WrappedListE l, WrappedListE x) = WrappedListE (l @ x)
    

[<TestFixture>]
type Monoid() =
    [<Test>]
    member x.mconcat_Default_Custom() = 
        let (WrappedListB x) = concat [WrappedListB [10] ;WrappedListB [15]]
        let (WrappedListC y) = concat [WrappedListC [10] ;WrappedListC [15]]
        Assert.AreEqual (x, [10;15])
        Assert.AreEqual (y, [10])


[<TestFixture>]
type Functor() =
    [<Test>]
    member x.map_Default_Custom() = 
        let testVal = map ((+) 1) {Head = 10; Tail = [20;30]}
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal)


[<TestFixture>]
type Foldable() = 
    [<Test>]
    member x.filter_Default_Custom() = 
        let wlA1 = WrappedListA [1..10]
        let testVal = filter ((=)2) wlA1
        Assert.AreEqual (testVal, WrappedListA [2])
        Assert.IsInstanceOf<Option<WrappedListA<int>>> (Some testVal)

    [<Test>]
    member x.foldAternatives() = 
        let x = choice [None; Some 3; Some 4; None]
        let y = choice [| []; [3]; [4]; [] |]
        Assert.AreEqual (x, Some 3)
        Assert.AreEqual (y, [3;4])

    [<Test>]
    member x.FromToSeq() =
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
    member x.SortBy() =
        let l  = [10;4;6;89]
        let l' = sortBy id l
        let s  = WrappedListB [10;4;6;89]
        let s' = sortBy id s
        Assert.AreEqual (l', [4;6;10;89])
        Assert.AreEqual (s', WrappedListB [4;6;10;89])


[<TestFixture>]
type Monad() = 
    [<Test>]
    member x.WorkFlow() =       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        Assert.IsInstanceOf<WrappedListD<int>>(testVal)

    [<Test>]
    member x.DelayForCont() = 
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


[<TestFixture>]
type Traversable() = 
    [<Test>]
    member x.sequenceA_Default_Primitive() = 
        let testVal = sequenceA [|Some 1; Some 2|]
        Assert.AreEqual (Some [|1;2|], testVal)
        Assert.IsInstanceOf<Option<array<int>>> testVal

    member x.sequenceA_Specialization() =
        let inline seqSeq (x:_ seq ) = sequenceA x
        let inline seqArr (x:_ []  ) = sequenceA x
        let inline seqLst (x:_ list) = sequenceA x

        let a = seqSeq (seq [[1];[3]])
        Assert.AreEqual ([seq [1; 3]], a)
        Assert.IsInstanceOf<list<seq<int>>> a
        let b = seqArr ( [|[1];[3]|])
        Assert.AreEqual ([[|1; 3|]], b)
        Assert.IsInstanceOf<list<array<int>>> b
        let c = seqLst ( [ [1];[3] ])
        Assert.AreEqual ([[1; 3]], c)
        Assert.IsInstanceOf<list<list<int>>> c

        
type ZipList<'s> = ZipList of 's seq with
    static member Map    (ZipList x, f:'a->'b)               = ZipList (Seq.map f x)
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) :ZipList<'b>
    
type ZipList'<'s> = ZipList' of 's seq with
    static member Return (x:'a)                                = ZipList' (Seq.initInfinite (konst x))
    static member (<*>) (ZipList' (f:seq<'a->'b>), ZipList' x) = ZipList' (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) :ZipList'<'b>

[<TestFixture>]
type Applicative() = 
    [<Test>]
    member x.ApplicativeMath() = 
        let inline (+) (a:'T) (b:'T) :'T = a + b
        let inline ( |+  ) (x :'Functor't)     (y :'t)             = map ((+)/> y) x :'Functor't
        let inline (  +| ) (x :'t)             (y :'Functor't)     = map ((+)   x) y :'Functor't
        let inline ( |+| ) (x :'Applicative't) (y :'Applicative't) = (+) <!> x <*> y :'Applicative't

        let testVal = [1;2] |+| [10;20] |+| [100;200] |+  2
        Assert.AreEqual ([113; 213; 123; 223; 114; 214; 124; 224], testVal)
        Assert.IsInstanceOf<Option<list<int>>> (Some testVal)


    [<Test>]
    member x.Applicatives() = 

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

type Applicative with
    [<Test>]
    member x.IdiomBrackets() =    
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
 

[<TestFixture>]
type MonadPlus() = 
    [<Test>]
    member x.ZeroAndPlus() = 
        let v = WrappedListE [1;2]
        let x = v <|> getMZero()
        let y = getMZero() <|> v
        Assert.AreEqual (v, x)
        Assert.AreEqual (v, y)

    [<Test>]
    member x.WorkFlows() = 
        let effects = ResizeArray()
        let zero = monadPlus {
            effects.Add(1)
            do! Async.Sleep 10
            effects.Add(2) }
        Assert.AreEqual(effects |> toList, [])

        let combine = monadPlus { 
            if true then do! Async.Sleep 10
            return! zero }
        Assert.AreEqual(effects |> toList, [])
        Async.RunSynchronously combine
        Assert.AreEqual(effects |> toList, [1;2])

        let lst: _ list = monadPlus {
            effects.Add(3)
            return 5;
            return 6; }
        Assert.AreEqual(effects |> toList, [1;2;3])
        Assert.AreEqual(lst, [5;6])

        let effects = ResizeArray()
        let seq3: seq<_> = monadPlus { 
            effects.Add "Start"
            try
                try
                    10 / 0 |> ignore
                finally
                    effects.Add "execute this"
            with
            | e -> 
                effects.Add (sprintf "Exception! %s" e.Message)
                return 42 }
        Assert.AreEqual(effects |> toList, [])
        let seqValue = seq3 |> Seq.toList
        Assert.AreEqual(effects |> toList, ["Start"; "execute this"; "Exception! Attempted to divide by zero."])
        Assert.AreEqual(seqValue, [42])


module NumericLiteralG =
    open FsControl
    let inline FromZero() = Zero.Invoke()
    let inline FromOne () = One.Invoke()
    let inline FromInt32  (i:int   ) = FromInt32.Invoke i
    let inline FromInt64  (i:int64 ) = FromInt64.Invoke i
    let inline FromString (i:string) = fromBigInt <| System.Numerics.BigInteger.Parse i

open MathNet.Numerics

[<TestFixture>]
type Numerics() = 
    [<Test>]
    member x.GenericMath() = 
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
    static member inline get_Empty() = Sum 0G
    static member inline Append (Sum (x:'n), Sum(y:'n)) = Sum (x + y)


[<TestFixture>]
type Splits() = 
    [<Test>]
    member x.SplitArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|]  |> Seq.map System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|] |> Seq.map System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((toList a1 = toList a2))
        Assert.IsTrue((toList b1 = toList b2))

    member x.ReplaceArraysAndStrings() = 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B  |> System.Text.Encoding.ASCII.GetString

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B |> System.Text.Encoding.ASCII.GetString

        Assert.IsTrue((a1 = a2))
        Assert.IsTrue((b1 = b2))

    member x.IntercalateArraysAndStrings() = 
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