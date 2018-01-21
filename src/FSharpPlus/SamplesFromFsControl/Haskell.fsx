#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module SamplesFromFsControl.Haskell
#endif


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

type Either<'a,'b> = Result<'b,'a>
let  Right x :Either<'a,'b> = Ok x
let  Left  x :Either<'a,'b> = Error x
let  (|Right|Left|) = function Ok x -> Right x | Error x -> Left x
let either f g = function Left x -> f x | Right y -> g y

// Numerics
type Integer = bigint
open System.Numerics
open FsControl
// open FsControl.Core.Types.Ratio

let inline fromInteger  (x:Integer)   :'Num    = FSharpPlus.Operators.fromBigInt x
let inline toInteger    (x:'Integral) :Integer = FSharpPlus.Operators.toBigInt   x
let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

module NumericLiteralG =
    let inline FromZero() = Zero.Invoke()
    let inline FromOne () = One.Invoke()
    let inline FromInt32  (i:int   ) = FromInt32.Invoke i
    let inline FromInt64  (i:int64 ) = FromInt64.Invoke i
    let inline FromString (i:string) = fromInteger <| BigInteger.Parse i

let inline abs    (x:'Num) :'Num = FSharpPlus.Operators.abs    x
let inline signum (x:'Num) :'Num = FSharpPlus.Operators.signum x

let inline (+) (a:'Num) (b:'Num) :'Num = a + b
let inline (-) (a:'Num) (b:'Num) :'Num = a - b
let inline (*) (a:'Num) (b:'Num) :'Num = a * b

let inline negate (x:'Num) :'Num = FSharpPlus.Operators.negate x
let inline (~-)   (x:'Num) :'Num = FSharpPlus.Operators.negate x

let inline whenIntegral a = let _ = if false then toInteger a else 0I in ()

let inline div (a:'Integral) b :'Integral =
    whenIntegral a
    let (a,b) = if b < 0G then (-a,-b) else (a,b)
    (if a < 0G then (a - b + 1G) else a) / b

let inline quot (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a / b
let inline rem  (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a % b
let inline quotRem a b :'Integral * 'Integral = whenIntegral a; FSharpPlus.Operators.divRem a b
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
// let inline fromRational (x:Rational) :'Fractional = FSharpPlus.Operators.fromRational x
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
//     let (a, b:'RealFrac) = FSharpPlus.Operators.properFraction x
//     (fromIntegral a, b)

// let inline truncate (x:'RealFrac) :'Integral = fst <| properFraction x
// let inline toRational (x:'Real) :Rational = FSharpPlus.Operators.toRational x
let inline pi() :'Floating = FSharpPlus.Operators.getPi()

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

let inline return' x = FSharpPlus.Operators.result x
let inline (>>=) x (f:_->'R) : 'R = FSharpPlus.Operators.(>>=) x f
let inline join (x:'Monad'Monad'a) : 'Monad'a = FSharpPlus.Operators.join x

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
    member        b.ReturnFrom(expr) = expr
    member inline b.Delay(expr:unit -> 't) = FsControl.Delay.Invoke(expr) : 't
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




let inline fmap   f x = FSharpPlus.Operators.map f x


// Kleisli

type Kleisli<'t, '``monad<'u>``> = Kleisli of ('t -> '``monad<'u>``) with

    // Profunctor
    static member inline Dimap (Kleisli bmc :Kleisli<'B,'``Monad<'C>``>, ab:'A->'B, cd:'C->'D) = let cmd = fmap cd in Kleisli (ab >> bmc >> cmd) : Kleisli<'A,'``Monad<'D>``>
    static member        Contramap (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, k:'A->'B            ) = Kleisli (k >> f)       : Kleisli<'A,'``Monad<'C>``>
    static member inline Map (Kleisli f    :Kleisli<'B,'``Monad<'C>``>, cd:'C->'D           ) = Kleisli (fmap cd << f) : Kleisli<'B,'``Monad<'D>``>
    
    // Category
    static member inline get_Id () = Kleisli return' :Kleisli<'a,'b>
    static member inline (<<<) (Kleisli f, Kleisli g) = Kleisli (g >=> f)

    // Arrow
    static member inline Arr f = Kleisli ((<<) return' f)
    static member inline First  (Kleisli f) = Kleisli (fun (b, d) -> f b >>= fun c -> return' (c, d))
    static member inline Second (Kleisli f) = Kleisli (fun (d, b) -> f b >>= fun c -> return' (d, c))
    static member inline (|||) (Kleisli f, Kleisli g) = Kleisli (FSharpPlus.Choice.either g f)

    static member inline (+++) (Kleisli (f:'T->'u), Kleisli (g:'v->'w)) =
        Fanin.InvokeOnInstance (Kleisli (f >=> ((<<) return' Choice2Of2))) (Kleisli (g >=> ((<<) return' Choice1Of2))) :Kleisli<Choice<'v,'T>,'z>

    static member inline Left (Kleisli f) =
        let inline (+++) a b = AcMerge.Invoke a b
        AcMerge.Invoke (Kleisli f) (Arr.Invoke (Id.Invoke()))
    static member inline Right (Kleisli f) =
        let inline (+++) a b = AcMerge.Invoke a b
        (+++) (Arr.Invoke (Id.Invoke())) (Kleisli f)
    static member get_App () = Kleisli (fun (Kleisli f, x) -> f x)
    
    // ArrowPlus
    static member inline Empty (output :Kleisli<'T,'``Monad<'U>``>, mthd :Empty) = Kleisli (fun _ -> Empty.Invoke ())
    static member inline Append (Kleisli f, Kleisli g, mthd:Append) = Kleisli (fun x -> Append.Invoke (f x) (g x))

let runKleisli (Kleisli f) = f
let runFunc (f : System.Func<_,_>) = f.Invoke









// Arrows

let inline id'() = FSharpPlus.Operators.getCatId()
let inline (<<<) f g = FSharpPlus.Operators.catComp f g
let inline (>>>) f g = FSharpPlus.Operators.catComp g f
let inline arr   f = FSharpPlus.Operators.arr    f
let inline arrFirst  f = FSharpPlus.Operators.arrFirst f
let inline arrSecond f = FSharpPlus.Operators.arrSecond f
let inline ( *** ) f g = FSharpPlus.Operators.( *** ) f g
let inline ( &&& ) f g = FSharpPlus.Operators.fanout f g
let inline (|||) f g = FSharpPlus.Operators.fanin f g
let inline (+++) f g = FSharpPlus.Operators.(+++) f g
let inline left  f = FSharpPlus.Operators.left  f
let inline right f = FSharpPlus.Operators.right f
let inline app() = FSharpPlus.Operators.getApp()
let inline zeroArrow() = FSharpPlus.Operators.getEmpty()
let inline (<+>)   f g = FSharpPlus.Operators.(<|>) f g

// Test Categories
let r5:List<_>  = (runKleisli (id'())) 5
let k = Kleisli (fun y -> [y; y * 2 ; y * 3]) <<< Kleisli (fun x -> [x + 3; x * 2])
let r8n16n24n10n20n30 = runKleisli k  5

let res1 = (System.Func<_,_>string >>> System.Func<_,_>int).Invoke '1'

type MapTuple = MapTuple with
    static member inline (?<-) (MapTuple, f, (x,y))   = (Invoke.Invoke (f, x), Invoke.Invoke (f, y)) 
    static member inline (?<-) (MapTuple, f, (x,y,z)) = (Invoke.Invoke (f, x), Invoke.Invoke (f, y), Invoke.Invoke (f, z))
let inline mapTuple f t = (?<-) MapTuple f t

let tupInt5nInt5  = mapTuple (      List.max           >>>      List.min           ) ([[7;5;8]; [4;5;3]], [   [7;5;8]   ;    [4;5;3]]   )
let tupInt5nChar5 = mapTuple (Unchecked.defaultof<Max> >>> Unchecked.defaultof<Min>) ([[7;5;8]; [4;5;3]], [['7';'5';'8']; ['4';'5';'3']])


// Test Arrows
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
let resLeft7       = ( (+) 2) +++ ( (*) 10)   <| Choice2Of2 5
let res7n50        = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) ||| Kleisli (fun x -> [x + 2; x * 10] )) (Choice1Of2 5)
let resLeft5n10n15 = runKleisli (Kleisli (fun y -> [y; y * 2; y * 3]) +++ Kleisli (fun x -> [x + 3; x *  2] )) (Choice2Of2 5)

// Test Arrow Apply
let res7      = app() ( (+) 3 , 4)
let res4n8n12 = runKleisli (app()) (Kleisli (fun y -> [y; y * 2 ; y * 3]) , 4)

// Test Arrow Plus
let resSomeX = Kleisli(fun x -> Some x)
let (resSomeXPlusZero:option<_>) = runKleisli (resSomeX <+> zeroArrow()) 10




// Monad Transformers

let inline lift (x:'ma) = FSharpPlus.Operators.lift x
let inline liftIO (x: Async<'a>) = FSharpPlus.Operators.liftAsync x
let inline callCC f = FSharpPlus.Operators.callCC f
let inline get< ^T when ^T : (static member Get : ^T)> : ^T = FSharpPlus.Operators.get
let inline put x = FSharpPlus.Operators.put x
let inline ask< ^T when ^T : (static member Ask : ^T)> : ^T = FSharpPlus.Operators.ask
let inline local f m = FSharpPlus.Operators.local f m
let inline tell   x = FSharpPlus.Operators.tell x
let inline listen m = FSharpPlus.Operators.listen m
let inline pass   m = FSharpPlus.Operators.pass   m




// MonadError
let inline throwError x   = FSharpPlus.Operators.throw x
let inline catchError v h = FSharpPlus.Operators.catch v h

// Test MonadError
let err1Layers   = catchError (Left "Invalid Value") (fun s -> Left ["the error was: " + s]) : Either<_,int>