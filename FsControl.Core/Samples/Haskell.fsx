#nowarn "3186"
#r @"..\bin\Release\FsControl.dll"

// This sample code mimics Haskell functions.
// It is based in an initial exploratory project  at http://code.google.com/p/fsharp-typeclasses/ no longer maintained.


let flip f x y = f y x
let const' k _ = k

let (</) = (|>)
let (/>) = flip
let (++) = (@)
let (==) = (=)
let (=/) x y = not (x = y)

type DeReference = DeReference with
    static member ($) (DeReference, a:'a ref     ) = !a
    static member ($) (DeReference, a:string     ) = a.ToCharArray() |> Array.toList
    static member ($) (DeReference, a:DeReference) = DeReference

let inline (!) a = DeReference $ a

type Maybe<'t> = Option<'t>
let  Just x :Maybe<'t> = Some x
let  Nothing:Maybe<'t> = None
let  (|Just|Nothing|) = function Some x -> Just x | _ -> Nothing
let maybe  n f = function | Nothing -> n | Just x -> f x

type Either<'a,'b> = Choice<'b,'a>
let  Right x :Either<'a,'b> = Choice1Of2 x
let  Left  x :Either<'a,'b> = Choice2Of2 x
let  (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 x -> Left x
let either f g = function Left x -> f x | Right y -> g y

// Numerics
type Integer = bigint
open System.Numerics
open FsControl
// open FsControl.Core.Types.Ratio

let inline fromInteger  (x:Integer)   :'Num    = FsControl.Operators.fromBigInt x
let inline toInteger    (x:'Integral) :Integer = FsControl.Operators.toBigInt   x
let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

module NumericLiteralG =
    let inline FromZero() = Zero.Invoke()
    let inline FromOne () = One.Invoke()
    let inline FromInt32  (i:int   ) = FromInt32.Invoke i
    let inline FromInt64  (i:int64 ) = FromInt64.Invoke i
    let inline FromString (i:string) = fromInteger <| BigInteger.Parse i

let inline abs    (x:'Num) :'Num = FsControl.Operators.abs    x
let inline signum (x:'Num) :'Num = FsControl.Operators.signum x

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

let inline negate (x:'Num) :'Num = FsControl.Operators.negate x
let inline (~-)   (x:'Num) :'Num = FsControl.Operators.negate x

let inline whenIntegral a = let _ = if false then toInteger a else 0I in ()

let inline div (a:'Integral) b :'Integral =
    whenIntegral a
    let (a,b) = if b < 0G then (-a,-b) else (a,b)
    (if a < 0G then (a - b + 1G) else a) / b

let inline quot (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a / b
let inline rem  (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a % b
let inline quotRem a b :'Integral * 'Integral = whenIntegral a; FsControl.Operators.divRem a b
let inline mod'   a b :'Integral = whenIntegral a; ((a % b) + b) % b  
let inline divMod D d :'Integral * 'Integral =
    let q, r = quotRem D d
    if (r < 0G) then
        if (d > 0G) then (q - 1G, r + d)
        else             (q + 1G, r - d)
    else (q, r)

// type Rational = Ratio<Integer>

let inline G0() = fromIntegral 0
let inline G1() = fromIntegral 1

let inline gcd x y :'Integral =
    let zero = G0()
    let rec gcd' a = function
        | b when b = zero -> a
        | b -> gcd' b (rem a b)
    match(x,y) with
    | t when t = (zero,zero) -> failwith "Prelude.gcd: gcd 0 0 is undefined"
    | _                      -> gcd' (abs x) (abs y)


// let inline ratio (a:'Integral) (b:'Integral) :Ratio<'Integral> =
//     whenIntegral a
//     let zero = G0()
//     if b = zero then failwith "Ratio.%: zero denominator"
//     let (a,b) = if b < zero then (negate a, negate b) else (a, b)
//     let gcd = gcd a b
//     Ratio (quot a gcd, quot b gcd)
// 
// let inline (%) (a:'Integral) (b:'Integral) :Ratio<'Integral> = a </ratio/> b
// let inline fromRational (x:Rational) :'Fractional = FsControl.Operators.fromRational x
// let inline whenFractional a = let _ = if false then fromRational (1I % 1I) else a in ()
let inline (/) (a:'Fractional) (b:'Fractional) :'Fractional = (* whenFractional a;*) a / b
let inline recip x :'Fractional = 1G / x

// Exp functions
let inline ( **^ ) (x:'Num) (n:'Integral)  = 
    whenIntegral n
    let rec f a b n = if n == 0G then a else f (b * a) b (n - 1G)
    if (n < 0G) then failwith "Negative exponent" else f 1G x n
let inline ( **^^ ) (x:'Fractional) (n:'Integral) = if n >= 0G then x**^n else recip (x**^(negate n))

// let inline properFraction (x:'RealFrac) : 'Integral * 'RealFrac =
//     let (a, b:'RealFrac) = FsControl.Operators.properFraction x
//     (fromIntegral a, b)

// let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x
// let inline toRational (x:'Real) :Rational = FsControl.Operators.toRational x
let inline pi() :'Floating = FsControl.Operators.getPi()

let inline ( **) a (b:'Floating) :'Floating = a ** b
let inline sqrt    (x:'Floating) :'Floating = sqrt x

let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))

let inline logBase x y  :'Floating =  log y / log x


// Test Numerics
// let res5_55:Integer * _ = properFraction 5.55M
// let res111_20 = toRational 5.55
// let res4_3    = toRational (12 % 9)
// let res17_1   = toRational 17uy
let divisions = List.map ( quot/> 5G) [5;8;10;15;20]

let inline quadratic a b c =
    let root1 = ( -b + sqrt (  b ** 2G - 4G * a * c) )  / (2G * a)
    let root2 = ( -b - sqrt (  b ** 2G - 4G * a * c) )  / (2G * a)
    (root1,root2)

let res30_15  = quadratic 2.0  -3G -9G
let res30_15f = quadratic 2.0f -3G -9G
let resCmplx:System.Numerics.Complex * _ = quadratic 2G -3G 9G



// Monads

let inline return' x = FsControl.Operators.result x
let inline (>>=) x (f:_->'R) : 'R = FsControl.Operators.(>>=) x f
let inline join (x:'Monad'Monad'a) : 'Monad'a = FsControl.Operators.join x

let inline sequence ms =
    let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (return' :list<'a> -> 'M) (List.Cons(x,xs))
    List.foldBack k ms ((return' :list<'a> -> 'M) [])

let inline mapM f as' = sequence (List.map f as')
let inline liftM  f m1    = m1 >>= (return' << f)
let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> return' (f x1 x2)
let inline ap     x y     = liftM2 id x y
let inline (>=>)  f g x   = f x >>= g
let inline (<=<)  g f x   = f x >>= g

type DoNotationBuilder() =
    member inline b.Return(x)    = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member        b.Let (p,rest) = rest p
    member    b.ReturnFrom(expr) = expr
let do' = new DoNotationBuilder()


// Test return
let resSome2 :option<_> = return' 2
let resSing2 :list<_>   = return' 2
let resLazy2 :Lazy<_>   = return' 2


// Test List Monad
// F#                           // Haskell
let result = 
    do' {                       // do {
        let! x1 = [1;2]         //   x1 <- [1;2]
        let! x2 = [10;20]       //   x2 <- [10;20]
        return ((+) x1 x2) }    //   return ((+) x1 x2) }

// desugared version
let lst11n21n12n22 = [1;2]  >>= (fun x1 -> [10;20] >>= (fun x2 ->  return'((+) x1 x2 )))


// IO
type IO<'a> = Async<'a>
let runIO  f   = Async.RunSynchronously f
let getLine    = async { return System.Console.ReadLine() }
let putStrLn x = async { printfn "%s" x}
let print    x = async { printfn "%A" x}

// Test IO
let action = do' {
    do! putStrLn  "What is your first name?"
    let! fn = getLine
    do! putStrLn  ("Thanks, " + fn) 
    do! putStrLn  ("What is your last name?")
    let! ln = getLine
    let  fullname = fn + " " + ln
    do! putStrLn  ("Your full name is: " + fullname)
    return fullname }
// try -> runIO action ;;


// Functors

let inline fmap   f x = FsControl.Operators.map f x

// Test Functors
let times2,minus3 = (*) 2, (-)/> 3
let resJust1      = fmap minus3 (Some 4G)
let noValue       = fmap minus3 None
let lstTimes2     = fmap times2 [1;2;3;4]
let fTimes2minus3 = fmap minus3 times2
let res39         = fTimes2minus3 21G
let getChars      = fmap (fun (x:string) -> x.ToCharArray() |> Seq.toList ) action
// try -> runIO getChars ;;

// Define a type Tree
type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Leaf of 'a
    static member map f (t:Tree<'a>  )  =
        match t with
        | Leaf x -> Leaf (f x)
        | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1, Tree.map f t2)

// add ìnstance for Functor class
    static member Map (x:Tree<_>, f, _:Map) = Tree.map f x

let myTree = Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9)
let mappedTree = fmap fTimes2minus3 myTree


// Comonads

let inline internal extend g s  = FsControl.Operators.extend g s
let inline internal duplicate x = FsControl.Operators.duplicate x
let inline internal (=>>)  s g  = fmap g (duplicate s)

let ct1 = duplicate [1;2;3;4] // val it : List<List<int>> = [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]]
let ct2 = duplicate ("a", 10) // val it : string * (string * int) = ("a", ("a", 10))
let ct3 = duplicate (fun (x:string) -> System.Int32.Parse x) // r3 "80" "100"  val it : int = 80100

let ct1' = extend id [1;2;3;4]
let ct2' = extend id ("a", 10)
let ct3' = extend id (fun (x:string) -> System.Int32.Parse x)

let ct1'' = (=>>) [1;2;3;4] id
let ct2'' = (=>>) ("a", 10) id
let ct3'' = (=>>) (fun (x:string) -> System.Int32.Parse x) id


// Monoids

let inline mempty() = FsControl.Operators.getEmpty ()
let inline mappend (x:'a) (y:'a): 'a = FsControl.Operators.append x y
let inline mconcat (x:seq<'a>) : 'a = FsControl.Operators.concat x

type Ordering = LT|EQ|GT with
    static member        Empty = EQ
    static member        Append (x:Ordering, y) = 
        match x, y with
        | LT, _ -> LT
        | EQ, a -> a
        | GT, _ -> GT

let inline compare' x y =
    match compare x y with
    | a when a > 0 -> GT
    | a when a < 0 -> LT
    | _            -> EQ

type Sum<'a> = Sum of 'a with
    static member inline get_Empty() = Sum 0G
    static member inline Append (Sum (x:'n), Sum(y:'n)) = Sum (x + y)

type Product<'a> = Product of 'a with
    static member inline get_Empty() = Product 1G
    static member inline Append (Product (x:'n), Product(y:'n)) = Product (x * y)

type Dual<'T> = Dual of 'T with
    static member inline get_Empty() = Dual (mempty())
    static member inline Append (Dual x, Dual y) = Dual (mappend y x)

type Endo<'T> = Endo of ('T -> 'T) with
    static member get_Empty() = Endo id
    static member Append (Endo f, Endo g) = Endo (f << g)

type All = All of bool with
    static member Empty = All true
    static member Append (All x, All y) = All (x && y)

type Any = Any of bool with
    static member Empty = Any false
    static member Append (Any x, Any y ) = Any (x || y)


// Test Monoids
let emptyLst:list<int> = mempty()
let zeroInt:Sum<int>   = mempty()
let res10 = mappend (mempty()) (Sum 10)
let res6  = mconcat <| fmap Sum [0.4; 5.6]
let res8:Sum<Integer>  = mconcat [mempty(); Sum 2G; Sum 6G]
let res8n4 = [mempty(); [8;4]]
let res15 = mappend (Product 15) (mempty()) 
let resTrue = mconcat [mempty(); Any true]
let resFalse = mconcat (fmap All [true;false])
let resHi = mappend (mempty()) "Hi"
let resGT = mappend (mempty()) GT
let resLT = mconcat [mempty(); LT ; EQ ;GT]
let res9823 = mconcat (fmap Dual [mempty();"3";"2";"8";"9"])
let resBA = (Dual "A" ) </mappend/> (Dual "B" )
let resEl00:list<int>*Sum<float> = mempty()
let resS3P20     = mappend (Sum 1G,Product 5.0) (Sum 2,Product 4G)
let res230       = mappend (mempty(),mempty()) ([2],[3.0])
let res243       = mappend  ([2;4],[3]) (mempty())
let res23        = mappend (mempty()) ([2],"3")
let resLtDualGt  = mappend  (LT,Dual GT) (mempty())
let res230hiSum2 = mappend (mempty(), mempty(), Sum 2) ([2], ([3.0], "hi"), mempty())
let res230hiS4P3 = mappend (mempty(), mempty()       ) ([2], ([3.0], "hi", Sum 4, Product (6 % 2)))
let tuple5 :string*(Any*string)*(All*All*All)*Sum<int>*string = mempty()


// Monad Plus

let inline mzero () = FsControl.Operators.getMZero ()
let inline mplus (x:'a) (y:'a) : 'a = FsControl.Operators.(<|>) x y
let inline guard x = if x then return' () else mzero()
type DoPlusNotationBuilder() =
    member inline b.Return(x) = return' x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p
    member b.ReturnFrom(expr) = expr
    member inline x.Zero() = mzero()
    member inline x.Combine(a, b) = mplus a b
let doPlus = new DoPlusNotationBuilder()

// Test MonadPlus
let nameAndAddress = mapM (fun x -> putStrLn x >>= fun _ -> getLine) ["name";"address"]

let a:list<int> = mzero()
let res123      = mplus (mempty()) ([1;2;3])

let inline mfilter p ma = do' {
  let! a = ma
  if p a then return a else return! mzero()}

let mfilterRes2 = mfilter ((=)2) (Just 2)

// sample code from http://en.wikibooks.org/wiki/Haskell/MonadPlus
let pythags = do'{
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  do! guard (x*x + y*y == z*z)
  return (x, y, z)}

let pythags' = doPlus{
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  if (x*x + y*y == z*z) then return (x, y, z)}

let allCombinations = sequence [!"abc"; !"12"]


// Kleisli

type Kleisli<'t, '``monad<'u>``> = Kleisli of ('t -> '``monad<'u>``) with

    // Profunctor
    static member inline Dimap (Kleisli bmc :Kleisli<'B,'``Monad<'C>``>, ab:'A->'B, cd:'C->'D) = let cmd = fmap cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
    static member        LMap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, k:'A->'B            ) = Kleisli (k >> f)       : Kleisli<'A,'``Monad<'C>``>
    static member inline RMap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, cd:'C->'D           ) = Kleisli (fmap cd << f) : Kleisli<'B,'``Monad<'D>``>
    
    // Category
    static member inline Id (_:Kleisli<'a,'b>, _:Id) = Kleisli return' :Kleisli<'a,'b>
    static member inline Comp (Kleisli f, Kleisli g, _, _:Comp) = Kleisli (g >=> f)

    // Arrow
    static member inline Arr (f, _:Kleisli<_,_>, _:Arr) = Kleisli ((<<) return' f)
    static member inline ArrFirst (Kleisli f, _:Kleisli<_,_>, _:ArrFirst  ) = Kleisli (fun (b,d) -> f b >>= fun c -> return' (c,d))
    static member inline ArrSecond (Kleisli f, _:Kleisli<_,_>, _:ArrSecond) = Kleisli (fun (d,b) -> f b >>= fun c -> return' (d,c))
    static member inline Fanin (Kleisli f, Kleisli g, _:Kleisli<_,_>, _:Fanin) = Kleisli (either f g)

    static member inline AcMerge (Kleisli (f:'T->'u), Kleisli (g:'v->'w), _:Kleisli<Choice<'v,'T>,'z>, _:AcMerge) =
        Fanin.Invoke (Kleisli (f >=> ((<<) return' Choice2Of2))) (Kleisli (g >=> ((<<) return' Choice1Of2))) :Kleisli<Choice<'v,'T>,'z>

    static member inline AcLeft (Kleisli f, _, _:AcLeft) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Kleisli f) (Arr.Invoke (Id.Invoke()))
    static member inline AcRight (Kleisli f, _, _:AcRight) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke())) (Kleisli f)
    static member App (_: Kleisli<Kleisli<'a,'b> * 'a,'b>, _:App) = Kleisli (fun (Kleisli f, x) -> f x)
    
    // ArrowPlus
    static member inline MZero (output :Kleisli<'T,'``Monad<'U>``>, mthd :MZero) = Kleisli (fun _ -> MZero.Invoke ())
    static member inline MPlus (Kleisli f, Kleisli g, mthd:MPlus) = Kleisli (fun x -> MPlus.Invoke (f x) (g x))

let runKleisli (Kleisli f) = f
let runFunc (f : System.Func<_,_>) = f.Invoke

// Contravariants

module Predicate = let run (p:System.Predicate<_>) x = p.Invoke(x)

let inline contramap (f:'T->'U) (x:'Contravariant'U) :'Contravariant'T = Contramap.Invoke f x

let intToString (x:int) = string x
let resStr54 = contramap (fun (x:float) -> int x) intToString <| 54.
let isEven      = System.Predicate(fun x -> x </mod'/> 2 = 0)
let fstIsEven   = contramap List.head isEven
let resBoolTrue = Predicate.run fstIsEven [0..10]


// BiFunctors

let inline bimap f g x = FsControl.Operators.bimap f g x
let inline first   f x = FsControl.Operators.first f x
let inline second  f x = FsControl.Operators.second f x

let rInt10Str10 = bimap  int string (10.0, 10)
let resR11      = bimap  string ((+) 1) (Right 10)
let rStrTrue    = first  string (true, 10)
let rStr10      = second string (true, 10)

// Profunctors

let inline dimap f g x = FsControl.Operators.dimap f g x
let inline lmap f x = FsControl.Operators.lmap f x
let inline rmap f x = FsControl.Operators.rmap f x

let resStrFalse  = dimap int string (Predicate.run isEven) 99.0

let lx x = System.Char.GetNumericValue x + 100.
let rx x = string (x + 100)
let kl = Kleisli (fun (y:float) -> [int y; int y * 2 ; int y * 3])

let resl = lmap lx kl
let r105n210n315 = runKleisli resl '5'
let resr = rmap rx kl
let r105n110n115 = runKleisli resr 5.0
let resd = dimap lx rx kl
let r205n310n415 = runKleisli resd '5'


// Arrows

let inline id'() = FsControl.Operators.getCatId()
let inline (<<<) f g = FsControl.Operators.catComp f g
let inline (>>>) f g = FsControl.Operators.catComp g f
let inline arr   f = FsControl.Operators.arr    f
let inline arrFirst  f = FsControl.Operators.arrFirst f
let inline arrSecond f = FsControl.Operators.arrSecond f
let inline ( *** ) f g = FsControl.Operators.( *** ) f g
let inline ( &&& ) f g = arr (fun b -> (b, b)) >>> f *** g
let inline (|||) f g = FsControl.Operators.fanin f g
let inline (+++) f g = FsControl.Operators.(+++) f g
let inline left  f = FsControl.Operators.left  f
let inline right f = FsControl.Operators.right f
let inline app() = FsControl.Operators.getApp()
let inline zeroArrow() = FsControl.Operators.getMZero()
let inline (<+>)   f g = FsControl.Operators.(<|>) f g

// Test Arrows
let r5:List<_>  = (runKleisli (id'())) 5
let k = Kleisli (fun y -> [y; y * 2 ; y * 3]) <<< Kleisli (fun x -> [x + 3; x * 2])
let r8n16n24n10n20n30 = runKleisli k  <| 5

let res1 = (System.Func<_,_>string >>> System.Func<_,_>int).Invoke '1'

let r20n5n30n5   = runKleisli (arrFirst  <| Kleisli (fun y -> [y * 2; y * 3])) (10,5) 
let r10n10n10n15 = runKleisli (arrSecond <| Kleisli (fun y -> [y * 2; y * 3])) (10,5)

let resStr6 =          arr (fun x -> string (x * 2 ))  3
let resStr8 = runFunc (arr (fun x -> string (x * 2 ))) 4
let resSome2n4n6:option<_> = runKleisli (arr (fun y -> [y; y * 2 ; y * 3])) 2

let res500n19 = ( (*) 100) *** ((+) 9)  <| (5,10)
let res500n14 = ( (*) 100) &&& ((+) 9)  <| 5
let (res10x13n10x20n15x13n15x20:list<_>) = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) *** Kleisli (fun x -> [x + 3; x *  2] )) (5,10)
let (res10x8n10x10n15x8n15x10  :list<_>) = runKleisli (Kleisli (fun y -> [y * 2; y * 3]) &&& Kleisli (fun x -> [x + 3; x *  2] )) 5

// Test Arrow Choice
let resLeft7       = ( (+) 2) +++ ( (*) 10)   <| Left  5
let res7n50        = runKleisli (Kleisli (fun y -> [y; y * 2 ; y * 3]) ||| Kleisli (fun x -> [x + 2; x * 10] )) (Right 5)
let resLeft5n10n15 = runKleisli (Kleisli (fun y -> [y; y * 2 ; y * 3]) +++ Kleisli (fun x -> [x + 3; x *  2] )) (Left  5)

// Test Arrow Apply
let res7      = app() ( (+) 3 , 4)
let res4n8n12 = runKleisli (app()) (Kleisli (fun y -> [y; y * 2 ; y * 3]) , 4)

// Test Arrow Plus
let resSomeX = Kleisli(fun x -> Some x)
let (resSomeXPlusZero:option<_>) = runKleisli (resSomeX <+> zeroArrow()) 10

// Applicative functors

let inline pure' x   = FsControl.Operators.result x
let inline (<*>) x y = FsControl.Operators.(<*>) x y
let inline empty()   = FsControl.Operators.getMZero()
let inline (<|>) x y = FsControl.Operators.(<|>) x y


let inline (<<|>) f a   = fmap f a
let inline liftA2 f a b = f <<|> a <*> b
let inline (  *>)   x   = x |> liftA2 (const' id)
let inline (<*  )   x   = x |> liftA2 const'
let inline (<**>)   x   = x |> liftA2 (|>)

let inline optional v = Just <<|> v <|> pure' Nothing

type ZipList<'s> = ZipList of 's seq with
    static member Map    (ZipList x, f:'a->'b)               = ZipList (Seq.map f x)
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (const' x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f, x) -> f x)) :ZipList<'b>

// Test Applicative (lists)
let res3n4   = pure' ((+) 2) <*> [1;2]
let res2n4n8 = pure' ( **^) </ap/> pure' 2. <*> [1;2;3]

// Test Applicative (functions)
let res3 = pure' 3 "anything"
let res607 = fmap (+) ( (*) 100 ) 6 7
let res606 = ( (+) <*>  (*) 100 ) 6
let res508 = (fmap (+) ((+) 3 ) <*> (*) 100) 5

// Test Applicative (ZipList)
let res9n5   = fmap ((+) 1) (ZipList(seq [8;4]))
let res18n24 = pure' (+) <*> ZipList(seq [8;4]) <*> ZipList(seq [10;20])
let res6n7n8 = pure' (+) <*> pure' 5G <*> ZipList [1;2;3]
let res18n14 = pure' (+) <*> ZipList(seq [8;4]) <*> pure' 10

// Idiom brackets from http://www.haskell.org/haskellwiki/Idiom_brackets
type Ii = Ii
type Ji = Ji
type J = J
type Idiomatic = Idiomatic with
    static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
    static member        ($) (Idiomatic, Ii) = id
let inline idiomatic a b = (Idiomatic $ b) a
let inline iI x = (idiomatic << pure') x

let res3n4''  = iI ((+) 2) [1;2] Ii
let res3n4''' = iI (+) (pure' 2) [1;2] Ii                               // *1
let res18n24' = iI (+) (ZipList(seq [8;4])) (ZipList(seq [10;20])) Ii
let res6n7n8' = iI (+) (pure' 5G          ) (ZipList [1;2;3]     ) Ii   // *1
let res18n14' = iI (+) (ZipList(seq [8;4])) (pure' 10            ) Ii

type Idiomatic with static member inline ($) (Idiomatic, Ji) = fun xii -> join xii

let safeDiv x y = if y == 0 then Nothing else Just (x </div/> y)
let resJust3    = join (iI safeDiv (Just 6) (Just 2) Ii)
let resJust3'   =       iI safeDiv (Just 6) (Just 2) Ji

let safeDivBy y = if y == 0 then Nothing else Just (fun x -> x </div/> y)
let resJust2  = join (pure' safeDivBy  <*> Just 4G) <*> Just 8G
let resJust2' = join (   iI safeDivBy (Just 4G) Ii) <*> Just 8G

type Idiomatic with static member inline ($) (Idiomatic, J ) = fun fii x -> (Idiomatic $ x) (join fii)

let resJust2'' = iI safeDivBy (Just 4G) J (Just 8G) Ii
let resNothing = iI safeDivBy (Just 0G) J (Just 8G) Ii
let res16n17  = iI (+) (iI (+) (pure' 4) [2;3] Ii) (pure'  10) Ii   // *1

// *1 These lines fails when Apply.Invoke has no 'or ^'``Applicative<'U>`` ' (output) constraint.


// Foldable

let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = FsControl.Operators.foldBack f x z
let inline foldl (f: 'b -> 'a -> 'b) (z:'b) x :'b = FsControl.Operators.fold     f z x
let inline foldMap (f:'T->'Monoid) (x:'Foldable'T) :'Monoid = FsControl.Operators.foldMap f x

// Test Foldable
let resGt = foldMap (compare' 2) [1;2;3]
let resHW = foldMap (fun x -> Just ("hello " + x)) (Just "world")

module FoldableTree =
    type Tree<'a> =
        | Empty 
        | Leaf of 'a 
        | Node of (Tree<'a>) * 'a * (Tree<'a>)

        // add instance for Foldable class
        static member inline FoldMap (t:Tree<_>, f, _:FoldMap) =
            let rec _foldMap x f =
                match x with
                | Empty        -> mempty()
                | Leaf n       -> f n
                | Node (l,k,r) -> mappend (_foldMap l f) (mappend (f k) (_foldMap r f) )
            _foldMap t f
        static member inline FoldBack (x:Tree<_>, f, z, _:FoldBack) = FoldBack.FromFoldMap f z x
        static member inline ToSeq    (x:Tree<_>) = Tree.FoldBack (x, (fun x y -> seq {yield x; yield! y}), Seq.empty, Unchecked.defaultof<FoldBack>)
    
    let myTree = Node (Node (Leaf(1), 6, Leaf(3)), 2 , Leaf(9))
    let resSum21      = foldMap Sum     myTree
    let resProduct324 = foldMap Product myTree
    let res21         = foldr   (+) 0   myTree
    let res21'        = foldl   (+) 0   myTree      // <- Uses the default method.


// Traversable

let inline traverse f t = FsControl.Operators.traverse f t
let inline sequenceA  t = FsControl.Operators.sequenceA t

// Test Traversable
let f x = if x < 200 then [3 - x] else []
let g x = if x < 200 then Just (3 - x) else Nothing

let resSomeminus100 = traverse f (Just 103)
let resLstOfNull    = traverse f Nothing 
let res210          = traverse f [1;2;3]  
let resSome210      = traverse g [1;2;3]  
let resEmptyList    = traverse f [1000;2000;3000] 
let resEListOfElist = traverse f []
let resSome321  = sequenceA [Some 3;Some 2;Some 1]
let resNone     = sequenceA [Some 3;None  ;Some 1]
let res654      = sequenceA [ (+)3 ; (+)2 ; (+) 1] 3
let resCombined = sequenceA [ [1;2;3] ; [4;5;6]  ]
let resLstOfArr = sequenceA [|[1;2;3] ; [4;5;6] |]  // <- Uses the default method.
let resArrOfLst = sequenceA [[|1;2;3|];[|4;5;6 |]]
let get3strings = sequenceA [getLine;getLine;getLine]





// Monad Transformers

let inline lift (x:'ma) = FsControl.Operators.lift x
let inline liftIO (x: Async<'a>) = FsControl.Operators.liftAsync x
let inline callCC f = FsControl.Operators.callCC f
let inline get< ^T when ^T : (static member Get : ^T)> : ^T = FsControl.Operators.get
let inline put x = FsControl.Operators.put x
let inline ask< ^T when ^T : (static member Ask : ^T)> : ^T = FsControl.Operators.ask
let inline local f m = FsControl.Operators.local f m
let inline tell   x = FsControl.Operators.tell x
let inline listen m = FsControl.Operators.listen m
let inline pass   m = FsControl.Operators.pass   m




// MonadError
let inline throwError x   = FsControl.Operators.throw x
let inline catchError v h = FsControl.Operators.catch v h

// Test MonadError
let err1Layers   = catchError (Left "Invalid Value") (fun s -> Left ["the error was: " + s]) : Either<_,int>