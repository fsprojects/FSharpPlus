module GeneralTests

open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"

open System
open System.Collections.ObjectModel
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
#if !FABLE_COMPILER
open FSharpPlus.Math.Applicative
#endif
open System.Collections.Generic
open System.Collections
open System.Threading.Tasks




// Idiom brackets from http://www.haskell.org/haskellwiki/Idiom_brackets
type Ii = Ii
type Ji = Ji
type J = J
type Idiomatic = Idiomatic with
    static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
    static member        ($) (Idiomatic, Ii) = id

let idiomBrackets = testList "IdiomBrackets" [
    #if !FABLE_COMPILER
    testCase "idiomBrackets" (fun () ->    
        let inline idiomatic a b = (Idiomatic $ b) a
        let inline iI x = (idiomatic << result) x

        let res3n4''  = iI ((+) 2) [1;2] Ii
        let res3n4''' = iI (+) (result 2) [1;2] Ii   // fails to compile when constraints are not properly defined
        Assert.AreEqual ([3;4], res3n4'' )
        Assert.AreEqual ([3;4], res3n4''')


        let output = System.Text.StringBuilder ()
        let append (x: string) = output.Append x |> ignore

        let v5: Lazy<_> = lazy (append "5"; 5)
        Assert.AreEqual (0, output.Length)
        let fPlus10 x   = lazy (append " + 10"; x + 10)
        Assert.AreEqual (0, output.Length)
        let v5plus10    = v5 >>= fPlus10
        Assert.AreEqual (0, output.Length)
        let v15 = v5plus10.Force ()
        Assert.AreEqual ("5 + 10", string output)
        Assert.AreEqual (15, v15)

        output.Clear () |> ignore

        let v4ll: Lazy<_> = lazy (append "outer"; lazy (append "inner"; 4))
        Assert.AreEqual (0, output.Length)
        let v4l = join v4ll
        Assert.AreEqual (0, output.Length)
        let v4  = v4l.Force()
        Assert.AreEqual ("outerinner", string output)
        Assert.AreEqual (4, v4))
    #endif
    ]

type RErrors = | NegativeValue

let monadTransformers = testList "MonadTransformers" [
    #if !FABLE_COMPILER
    testCase "testCompileResultT" (fun () ->
        // Test MonadError
        let _err1Layers = catch (Error "Invalid Value") (fun s -> Error ["the error was: " + s]) : Result<int, _>


        let someResultFunction foo = if foo = "foo" then Result.Ok foo else Result.Error "not good"

        let doSomeOperation x = ResultT <| async {
            if x < 10 then return Result.Ok 10
            else return Result.Error "failure" }

        let okFoo10Comp: ResultT<_, _, _> =
            monad {
                let! resFoo = ResultT.hoist <| someResultFunction "foo"
                let! res10  = doSomeOperation 0
                return (resFoo, res10) } 
            </catch/> (fun s -> throw ("The error was: " + s))

        let _okFoo10 = okFoo10Comp |> ResultT.run |> Async.RunSynchronously

        ())
    #endif

    #if !FABLE_COMPILER
    testCase "testCompileChoiceT" (fun () ->
        // Test MonadError
        let _err1Layers = catch (Choice2Of2 "Invalid Value") (fun s -> Choice2Of2 ["the error was: " + s]) : Choice<int, _>


        let someErrorFunction foo = if foo = "foo" then Choice1Of2 foo else Choice2Of2 "not good"

        let doSomeOperation x = ChoiceT <| async {
            if x < 10 then return Choice1Of2 10
            else return Choice2Of2 "failure"   }

        let okFoo10Comp: ChoiceT<_, _, _> =
            monad {
                let! resFoo = ChoiceT.hoist <| someErrorFunction "foo"
                let! res10  = doSomeOperation 0
                return (resFoo, res10) }
            </catch/> (fun s -> throw ("The error was: " + s))

        let _okFoo10 = okFoo10Comp |> ChoiceT.run |> Async.RunSynchronously

        // test generic put (no unknown(1,1): error FS0073: internal error: Undefined or unsolved type variable:  ^_?51242)
        let initialState = -1
        let _ = put initialState : ListT<State<int, __>, unit>
        let _ = put initialState : ChoiceT<string, State<int, __>, unit>

        ())
    #if !NETSTANDARD3_0
    testCase "testStateT" (fun () ->
        let lst1: StateT<string, _, _> = StateT.lift [1;2]
        let lst2: StateT<string, _, _> = StateT.lift [4;5]

        let m = monad { 
            let! x =  lst1
            let! y =  lst2
            do! modify String.toUpper
            let! st = gets String.length
            return (x, y +  st)
            }

        equalSeq [((1, 6), "OK"); ((1, 7), "OK"); ((2, 6), "OK"); ((2, 7), "OK")] (StateT.run m "ok")

        ())

    testCase "testCompilationMT1" (fun () ->

        let fn : ResultT<RErrors, Reader<int, __>, _> =
            monad {
               let! x1 = ask
               let! x2 = 
                   if x1 > 0 then result 1
                   else ResultT (result (Error NegativeValue)) 
               return x1 + x2
            }

        let x = (fn |> ResultT.run |> Reader.run) 10
        equal (Ok 11) x
        let y = (fn |> ResultT.run |> Reader.run) -1
        equal (Error NegativeValue) y)
    #endif
    #endif
    ]

#if !FABLE_COMPILER || FABLE_COMPILER_3
module ProfunctorDefaults =
    type Fun<'T,'U> = Fun of ('T -> 'U) with
        static member Dimap ((Fun f): Fun<'B,'C>, g: 'A->'B, h:'C->'D) = Fun (g >> f >> h)

    let a = lmap id (Fun int)
    let b = rmap id (Fun float)
    let b' = map id (Fun float)
    ()
#endif

#if !FABLE_COMPILER || FABLE_COMPILER_3
module BifunctorDefaults =
    type Tup<'a,'b> = Tup of ('a * 'b) with
        static member Bimap (Tup (a, b), f, g) = Tup (f a, g b)

    let a = first  string (Tup (1, '2'))
    let b = second string (Tup (1, '2'))
    let b' =  map  string (Tup (1, '2'))
    ()
#endif

#if !FABLE_COMPILER || FABLE_COMPILER_3
type StringCodec<'t> = StringCodec of ReaderT<string, Result<__, string>, 't> * ('t -> Const<string, unit>) with
    static member Invmap (StringCodec (d, e), f: 'T -> 'U, g: 'U -> 'T) = StringCodec (map f d, contramap g e)
module StringCodec =
    let decode (StringCodec (d,_)) x = ReaderT.run d x
    let encode (StringCodec (_,e)) x = Const.run (e x)
#endif

let invariant = testList "Invariant" [
#if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "testStringToIntDerivedFromFloat" (fun () ->
        let floatCodec = StringCodec (ReaderT (tryParse >> Option.toResultWith "Parse error"), string<float> >> Const)
        let floatParsed  = StringCodec.decode floatCodec "1.8"
        let floatEncoded = StringCodec.encode floatCodec 1.5
        Assert.AreEqual (Result<float, string>.Ok 1.8, floatParsed)
        Assert.AreEqual ("1.5", floatEncoded)

        let intCodec = invmap int<float> float<int> floatCodec
        let oneParsed  = StringCodec.decode intCodec "1"
        let tenEncoded = StringCodec.encode intCodec 10
        Assert.AreEqual (Result<int, string>.Ok 1, oneParsed)
        Assert.AreEqual ("10", tenEncoded))
#endif
    ]

type Sum<'a> = Sum of 'a with
    static member inline get_Zero () = Sum 0
    static member inline (+) (Sum (x:'n), Sum (y:'n)) = Sum (x + y)


let splits = testList "Splits" [

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "splitArraysAndStrings" (fun () ->
        let a1 = "this.isABa.tABCest"  |> split [|"AT" ; "ABC" |]
        let a2 = "this.isABa.tABCest"B |> split [|"AT"B; "ABC"B|]

        let b1 = "this.is.a.t...est"  |> split [|"." ; "..." |]
        let b2 = "this.is.a.t...est"B |> split [|"."B; "..."B|]
        
        Assert.IsTrue ((toList a1 = ["this.isABa.t"; "est"]))
        Assert.IsTrue ((toList a2 = [[|116uy; 104uy; 105uy; 115uy; 46uy; 105uy; 115uy; 65uy; 66uy; 97uy; 46uy; 116uy|]; [|101uy; 115uy; 116uy|]]))
        Assert.IsTrue ((toList b1 = ["this"; "is"; "a"; "t"; ""; ""; "est"]))
        Assert.IsTrue ((toList b2 = [[|116uy; 104uy; 105uy; 115uy|]; [|105uy; 115uy|]; [|97uy|]; [|116uy|]; [||]; [||]; [|101uy; 115uy; 116uy|]]))

        #if !FABLE_COMPILER
        Assert.IsInstanceOf<Option<string []>> (Some a1)
        #endif
        )

    testCase "replaceArraysAndStrings" (fun () -> 
        let a1 = "this.isABa.tABCest"  |> replace "AT"  "ABC"
        let a2 = "this.isABa.tABCest"B |> replace "AT"B "ABC"B

        let b1 = "this.is.a.t...est"  |> replace "."  "..."
        let b2 = "this.is.a.t...est"B |> replace "."B "..."B

        Assert.IsTrue ((a1 = "this.isABa.tABCest"))
        Assert.IsTrue ((a2 = [|116uy; 104uy; 105uy; 115uy; 46uy; 105uy; 115uy; 65uy; 66uy; 97uy; 46uy; 116uy; 65uy; 66uy; 67uy; 101uy; 115uy; 116uy|]))
        Assert.IsTrue ((b1 = "this...is...a...t.........est"))
        Assert.IsTrue ((b2 = [|116uy; 104uy; 105uy; 115uy; 46uy; 46uy; 46uy; 105uy; 115uy; 46uy; 46uy; 46uy; 97uy; 46uy; 46uy; 46uy; 116uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 46uy; 101uy; 115uy; 116uy|]))
        )

    testCase "intercalateArraysAndStrings" (fun () -> 
        let a1 = [|"this" ; "is" ; "a" ; "test" |] |> intercalate " "
        let a2 = [|"this"B; "is"B; "a"B; "test"B|] |> intercalate " "B

        //let b = [WrappedListB [1;2]; WrappedListB [3;4]; WrappedListB [6;7]] |> intercalate (WrappedListB [0;1])

        // /Control/Monoid.fs(..): (..) error FABLE: Cannot resolve trait call + - Inline call from ./Collection.fs(..) < ../../../tests/FSharpPlusFable.Tests/FSharpTests/General.fs(..)
        #if !FABLE_COMPILER
        let _c = [| Sum 1; Sum 2 |] |> intercalate (Sum 10)
        let d  = WrappedListB [Sum 1; Sum 2] |> intercalate (Sum 10)
        let _e = intercalate 10 (seq [1; 2; 3])
        #endif

        Assert.IsTrue((a1 = "this is a test"))
        Assert.IsTrue((a2 = [|116uy; 104uy; 105uy; 115uy; 32uy; 105uy; 115uy; 32uy; 97uy; 32uy; 116uy; 101uy; 115uy; 116uy|]))
        //Assert.IsTrue((b = WrappedListB [1; 2; 0; 1; 3; 4; 0; 1; 6; 7]))
        // Assert.IsTrue((c = Sum 13))
        #if !FABLE_COMPILER
        Assert.IsTrue((d = Sum 13))
        #endif
        )
    #endif

    ]

let bitConverter = testList "BitConverter" [
#if !FABLE_COMPILER
    testCase "roundtrips" (fun () ->
        let a0 = -79800210978L
        let b0 = -798021978
        let c0 = -79872.8
        let d0 = -79872.7969f
        let e0 = 43u
        let f0 = -45s
        let g0 = "this is just a String"
        let h0 = true
        let i0 = 'h'

        Assert.IsTrue ((a0 = (a0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((b0 = (b0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((c0 = (c0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((d0 = (d0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((e0 = (e0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((f0 = (f0 |> toBytes |> ofBytes)))
        Assert.IsTrue (("74-68-69-73-20-69-73-20-6A-75-73-74-20-61-20-53-74-72-69-6E-67" = (g0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((h0 = (h0 |> toBytes |> ofBytes)))
        Assert.IsTrue ((i0 = (i0 |> toBytes |> ofBytes))))
#endif
    ]


let curry = testList "Curry" [

#if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "curryTest" (fun () ->
    
        #if !FABLE_COMPILER
        let f1  (x: Tuple<_>) = [x.Item1]
        #endif
        
        let f2  (x, y)    = [x + y]
        let f3  (x, y, z) = [x + y + z]
        let f7  (t1, t2, t3, t4, t5, t6, t7) = [t1+t2+t3+t4+t5+t6+t7]
        
        #if !FABLE_COMPILER
        let f8  (t1, t2, t3, t4, t5, t6, t7: float, t8: char) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8]
        let f9  (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9]
        let f15 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15]
        let f16 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15, t16) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16]
        let f17 (t1, t2, t3, t4, t5, t6, t7: float, t8: char, t9: decimal, t10, t11, t12, t13, t14, t15, t16, t17) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16+t17]

        let _x1  = curryN f1 100
        #endif
        
        let _x2  = curryN f2 1 2
        let _x3  = curryN f3 1 2 3
        let _x7  = curryN f7 1 2 3 4 5 6 7
        
        #if !FABLE_COMPILER
        let _x8  = curryN f8 1 2 3 4 5 6 7. '8'
        let _x9  = curryN f9 1 2 3 4 5 6 7. '8' 9M
        let _x15 = curryN f15 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15
        let _x16 = curryN f16 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15 16
        let _x17 = curryN f17 1 2 3 4 5 6 7. '8' 9M 10 11 12 13 14 15 16 17
        #endif

        ())
#endif

#if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "uncurryTest" (fun () ->
        let g2  x y   = [x + y]
        let g3  x y z = [x + y + z]
        let g7  a b c d e f g = [a + b + c + d + e + f + g]
        
        #if !FABLE_COMPILER
        let g8  t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) = [t1+t2+t3+t4+t5+t6+ int t7 + int t8]
        let g9  t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal)  = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9]
        let g12 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12]
        let g15 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 t13 t14 t15 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15]
        let g16 t1 t2 t3 t4 t5 t6 (t7: float) (t8: char) (t9: decimal) t10 t11 t12 t13 t14 t15 t16 = [t1+t2+t3+t4+t5+t6+ int t7 + int t8+ int t9+t10+t11+t12+t13+t14+t15+t16]

        let _y1  = uncurryN string (Tuple<_> 1)
        #endif
        
        let _y2  = uncurryN g2 (1, 2)
        let _y3  = uncurryN g3 (1, 2, 3)
        let _y7  = uncurryN g7 (1, 2, 3, 4, 5, 6, 7)
        
        #if !FABLE_COMPILER
        let _y8  = uncurryN g8 (1, 2, 3, 4, 5, 6, 7. , '8')
        let _y9  = uncurryN g9 (1, 2, 3, 4, 5, 6, 7. , '8', 9M)
        let _y12 = uncurryN g12 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12)
        let _y15 = uncurryN g15 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12, 13, 14, 15)
        let _y16 = uncurryN g16 (1, 2, 3, 4, 5, 6, 7. , '8', 9M, 10 , 11, 12, 13, 14, 15, 16)
        #endif

        ())
#endif
    ]


let memoization = testList "Memoization" [

#if !FABLE_COMPILER
    testCase "memoization" (fun () ->
        let effs = ResizeArray ()

        let f x                       = printfn "calculating"; effs.Add "f"; string x
        let g x (y:string) z : uint32 = printfn "calculating"; effs.Add "g"; uint32 (x * int y + int z)
        let h x y z                   = printfn "calculating"; effs.Add "h"; new System.DateTime (x, y, z)
        let sum2 (a:int)       = printfn "calculating"; effs.Add "sum2"; (+) a
        let sum3 a (b:int) c   = printfn "calculating"; effs.Add "sum3"; a + b + c
        let sum4 a b c d : int = printfn "calculating"; effs.Add "sum4"; a + b + c + d

        // memoize them
        let msum2 = memoizeN sum2
        let msum3 = memoizeN sum3
        let msum4 = memoizeN sum4
        let mf    = memoizeN f
        let mg    = memoizeN g
        let mh    = memoizeN h

        // check memoization really happens
        let _v1  = msum2 1 1
        let _v2  = msum2 1 1
        let _v3  = msum2 2 1
        let _v4  = msum3 1 2 3
        let _v5  = msum3 1 2 3
        let _v6  = msum4 3 1 2 3
        let _v7  = msum4 3 1 2 3
        let _v8  = msum4 3 5 2 3
        let _v9  = mf 3M
        let _v10 = mf 3M
        let _v11 = mg 4 "2" 3M
        let _v12 = mg 4 "2" 3M
        let _v13 = mh 2010 1 1
        let _v14 = mh 2010 1 1

        Assert.AreEqual ([|"sum2"; "sum2"; "sum3"; "sum4"; "sum4"; "f"; "g"; "h"|], effs.ToArray ()))


    testCase "memoizeAcceptsNullArgument" (fun () ->
        let f x y = ""
        let mf = memoizeN f
        let _ = mf null null  // should not throw
        ())
#endif
    ]

open General.Alternative
open General.Applicative
open General.Collections
open General.Foldable
open General.Functor
open General.Indexable
open General.Monad
open General.Monoid
open General.Parsing
open General.Traversable
open General.Lensing
open General.Numeric

let generalTests = testList "General" [
    idiomBrackets
    monadTransformers
    splits
    bitConverter
    curry
    memoization
    alternative
    applicative
    collections
    foldable
    functor
    indexable
    monad
    monoid
    parsing
    traversable
    lensing
    numeric
]
