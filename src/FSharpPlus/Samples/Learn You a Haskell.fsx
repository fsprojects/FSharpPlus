#nowarn "3186"
#r @"../bin/Release/net45/FSharpPlus.dll"

open System
open FSharpPlus
open FSharpPlus.Operators
open FSharpPlus.Data

(* --------------------------------------------------
    Functors and applicative functors
   --------------------------------------------------*)

let res1 = map ((+) 2) (Some 2)
let res2 = map ((*) 3) ((+) 100) 1                                    // 303
let res3 = map (List.replicate 3) [1;2;3;4]
let res4 = map (List.replicate 3) (Some 3)
let res5 = map id (Some 3)
let res6 = map (*) (Some 3)

let res7 = Some ((+) 3) <*> (Some 9)
let res8 = result (+) <*> (Some 3) <*> (Some 5)
let res9 = (+) <!> (Some 3) <*> (Some 5)
let res10 = [(*) 0; (+) 10; fun x -> x * x] <*> [1;2;3]               // [0; 0; 0; 11; 12; 13; 1; 4; 9]
let res11= [(+); (*)] <*> [1;2] <*> [3;4]                             // [1+3; 1+4; 2+3; 2+4; 1*3; 1*4; 2*3; 2*4] => [4; 5; 5; 6; 3; 4; 6; 8]
let res12 = (+) <!> ["ha"; "heh"; "hmm"] <*> ["?"; "!"; "."]          // ["ha?"; "ha!"; "ha."; "heh?"; "heh!"; "heh."; "hmm?"; "hmm!"; "hmm."]
let res13 = (*) <!> [2;5;10] <*> [8;10;11] |> List.filter ((<) 50)    // [55; 80; 100; 110]
let res14 = (+) <!> ((+) 3) <*> ((*) 100) <| 5                        // 508

let res15 = (+) <!> (ZipList <| seq { 1..3 } ) <*> (ZipList <| Seq.init 3 (fun _ -> 100)) |> ZipList.run  // seq [101; 102; 103]
let res16 = (fun x -> [x]) <!> (Some 4)                               // Some [4]
let res17 = liftA2 (fun x xs -> x::xs) (Some 3) (Some [4])            // [3; 4]

let res18 = List.sequence [Some 3; Some 2; Some 1]                    // Some [3; 2; 1]


(* --------------------------------------------------
    Monoid    
   --------------------------------------------------*)

open FsControl

type Pair<'a, 'b> = Pair of ('a * 'b)
type Pair with
    static member runPair (Pair tuple : Pair<'a, 'b>) : ('a * 'b) = tuple
    static member map f (Pair (a, b) : Pair<'a, 'a>) : Pair<'b, 'b> = Pair ((f a), (f b))
    static member Map (x:Pair<'a,'a>, f) = Pair.map f x

let res19 = Pair (10, "hello")
let res20 = map ((*) 100) (Pair (2, 3))                               // Pair (200, 300)

let res21 = plus    [1;4] [3;6]
let res22 = plus    "Hello " "World"
let res23 = plus    "pang"  (getZero())                               // "pang"
let res24 = Seq.sum [[1;2]; [3;6]; [9]]                               // [1; 2; 3; 6; 9]

let res25 = plus    (Any true) (Any false)                            // Any true
let res26 = [false; false; false; true] |> List.map Any |> Seq.sum
let res27 = plus    (getZero()) (All false)                           // All false
let res28 = plus    (Some "some") None                                // Some "some"

let res29 = foldBack (*) [1;2;3] 1
let res30 = fold     (+) 2 (Some 9)
let res31 = foldBack (||) (Some true) false                           // true


(* --------------------------------------------------
    Foldable    
   --------------------------------------------------*)


type Tree<'a> =
    | MEmpty
    | Node of ('a * Tree<'a> * Tree<'a>)
    
type Tree with
    static member treeFold f tree z =
        match tree with
        | MEmpty -> z
        | Node (x, left, right) -> Tree<_>.treeFold f right (Tree<_>.treeFold f left (f x z))
    static member inline FoldBack (x:Tree<'a>, f, z) = Tree<'a>.treeFold f x z
    static member inline FoldMap  (x:Tree<'a>, f, impl:FoldMap) = Tree<'a>.FoldBack(x, plus << f, getZero())

let testTree =
    let one = Node (1, MEmpty, MEmpty)
    let six = Node (6, MEmpty, MEmpty)
    let three = Node (3, one, six)
    let eight = Node (8, MEmpty, MEmpty)
    let ten = Node (10, MEmpty, MEmpty)
    let nine = Node (9, eight, ten)
    let five = Node (5, three, nine)
    five

let res32 = foldBack (+) testTree 0
let res33 = foldBack (*) testTree 1
let res34 = foldMap (fun x -> Any (x > 15)) testTree                            // Any false


(* --------------------------------------------------
    Monads    
   --------------------------------------------------*)

let res35 = Some 9 >>= (fun x -> Some (x * 10))                                 // Some 90
let res36 = None >>= (fun x -> Some (x * 10))                                   // None

type Birds = int
type Pole = (Birds * Birds)

//landLeft :: Birds -> Pole -> Pole  
let landLeft (n) ((left, right): Pole) = (left + n, right)  
//landRight :: Birds -> Pole -> Pole  
let landRight n ((left, right) : Pole) = (left, right + n)

let res37 = landLeft 2 (0,0)                                                    // (2,0)  
let res38 = landRight 1 (1,2)                                                   // (1,3)  
let res39 = landRight (-1) (1,2)                                                // (1,1)

let (|~) x f = f x
  
let res40 = 100 |~ ((*)3)
let res41 = (0,0) |~ (landLeft 2)                                               // (2,0)
let res42 = (0,0) |~ landLeft 1 |~ landRight 1 |~ landLeft 2                    // (3,1)

let landLeft1 n ((left, right) : Pole) =  
    match abs ( (left + n) - right) with
    | x when x < 4 -> Some (left + n, right)  
    | _ -> None

let landRight1 n ((left, right) : Pole) =
    match abs ( left - (right + n) ) with
    | x when x < 4 -> Some (left, right + n)  
    | _ -> None

let res43 = landLeft1 2 (0,0)
let res44 = landRight1 1 (0,0) >>= landLeft1 2                                                    // Some (2, 1)
let res45 = Some (0,0) >>= landRight1 2 >>= landLeft1 2 >>= landRight1 2                          // Some (2,4)
let res46 = Some (0,0) >>= landLeft1 1 >>= landRight1 4 >>= landLeft1 (-1) >>= landRight1 (-2)    // None

let banana _ = None

let res47 = Some (0,0) >>= landLeft1 1 >>= banana >>= landRight1 1                                // None

let res48 = Some 9 >>= (fun x -> Some (x > 8))

let res49 =
    monad {
        let! a = Some 9
        return a < 8
    }

let res50 = 
    monad {
        let! start = Some (0,0)
        let! first = landLeft1 2 start
        let! second = landRight1 2 first
        return! landLeft1 1 second
    }

let res51 = 
    monad {
        let! start = Some (0,0)
        let! first = landLeft1 2 start
        do! None//do! banana first
        let! second = landRight1 2 first
        return! landLeft1 1 second
    }

let res52 = [3;4;5] >>= (fun x -> [x;-x])                                 // [3; -3; 4; -4; 5; -5]
let res53 = [] >>= (fun x -> ["bad","mad","rad"])                         // []
let res54 = [1;2] >>= (fun n -> [(n, 'a'); (n, 'b')])                     // [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]

let res55 =
    monad {
        let! x = [1;2]
        let! y = [(x, 'a'); (x, 'b')]
        return y
    }

let res56 : option<unit> = guard (5 > 2)                                  // Some (None)
let res57 : option<unit> = guard (1 > 2)                                  // None
let res58 : list<unit> = guard (5 > 2)                                    // [None]
let res59 : list<unit> = guard (1 > 2)                                    // []

// (>>) :: (Monad m) => m a -> m b -> m b  
// m >> n = m >>= \_ -> n
let inline (>>~) (ma : 'Monad'a) (mb : 'Monad'b) : 'Monad'b =
    ma >>= fun _ -> mb

let res60 = None >>~ Some 3                                              // None
let res61 = Some 3 >>~ Some 4                                            // Some 4  
let res62 : option<int> = Some 3 >>~ None                                // None
let res63 = guard (5 > 2) >>~ ["cool"]                                   // ["cool"]  
let res64 = guard (1 > 2) >>~ ["cool"]                                   // []

let res65 = [1..50] >>= (fun x -> guard (x < 7) >>~ [x])

let res66 = 
    monad {
        let! x = [1..50]
        do! guard (x < 7)
        return x
    }

let res67 = 
    monad {
        let! z = [1..50]
        let! x = [1..z]
        let! y = [x..z]
        do! guard (x*x + y*y = z*z)
        return (x, y, z)
   } 
 
type KnightPos = (int * int)

let moveKnight (c,r) : List<KnightPos> = 
    monad {  
        let! (c',r') = [(c+2,r-1);(c+2,r+1);(c-2,r-1);(c-2,r+1);  
                        (c+1,r-2);(c+1,r+2);(c-1,r-2);(c-1,r+2)  
                       ]  
        do! guard ( List.exists (fun x -> x = c' && x = r') [1..8] )  
        return (c',r')
    }

let res68 = moveKnight (6,2)

let in3 start =
    monad {   
        let! first = moveKnight start  
        let! second = moveKnight first  
        return! moveKnight second  
    }

let in3b start = [start] >>= moveKnight >>= moveKnight >>= moveKnight

let canReachIn3 start end_ = List.exists (fun x -> x = end_) (in3b start)

let res69 = canReachIn3 (6,2) (7,3)


(* --------------------------------------------------
    Writer monad    
   --------------------------------------------------*)

let isBigGang x = (x > 9, "Compared gang size to 9.")

let applyLog (x,log) f = let (y,newLog) = f x in (y,log + newLog)

let applyLog1 (x,log) f =
    let (y, newLog) = f x
    (y,log + newLog)

let res70 = applyLog (3, "Smallish gang.") isBigGang

let res71: Writer<List<string>, unit> = tell ["Something gonna happend"]     // Writer (None, ["Something gonna happend"])

let logNumber (x:int) = Writer (x, ["Got number: " + (x |> string)])

let multWithLog =
    monad {  
        let! a = logNumber 3  
        let! b = logNumber 5  
        do! tell ["Gonna multiply these two"]  
        return (a*b)
    }

let multWithLog1 : Writer<List<string>, int> =
    logNumber 3 >>= fun a ->
    logNumber 5 >>= fun b ->
    tell ["Gonna multiply these two"] >>= fun c ->
    Writer ((a * b), [])
    
let rec gcd' a b : Writer<List<string>, int> =    
    monad {
        match b = 0 with
        | true -> 
            do! tell ["Finished with " + (a |> string)]
            return a        
        | false ->
            do! tell [ (a |> string) + " mod " + (b |> string)  + " = " + ( (a % b) |> string ) ]
            return! gcd' b (a % b)
    }

let res72 = gcd' 8 3


(* --------------------------------------------------
    Reader monad    
   --------------------------------------------------*)

let addStuff =
    monad {  
        let! a = (*)2  
        let! b = (+)10  
        return (a+b)
    } <| 3

let addStuff2 =
    (   (*)2 >>= fun x ->
        (+)10 >>= fun y ->
        fun _ -> (x + y)
    ) <| 3 

let addStuff3 = 
    monad {
        let! a = Reader ((*)2)
        let! b = Reader ((+)10)
        return (a + b)
    } |> Reader.run <| 3

let addStuff4 =
    (   Reader ((*) 2) >>= fun x ->
        Reader ((+) 10) >>= fun y ->
        Reader (fun _ -> x + y)
    ) |> Reader.run <| 3
    

(* --------------------------------------------------
    State monad    
   --------------------------------------------------*)

type Stack = List<int>
  
let pop (x::xs) = (x,xs)  
let push a xs = ((),a::xs)

let stackManip stack =
    let ((),newStack1) = push 3 stack  
    let (a,newStack2) = pop newStack1  
    in pop newStack2  
    
let res73 = stackManip [5;8;2;1]                                                      // (5, [8; 2; 1])

let pop1 = State ( fun (x::xs) -> (x,xs) ) 
let push1 a = State ( fun xs -> ((),a::xs) )  

let stackManip1 =
    monad {
        do! push1 3
        let! a = pop1
        return! pop1
    }

let res74 = stackManip1 |> State.run <| [5;8;2;1]                                     // (5, [8; 2; 1])

let stackStuff : State<List<int>, unit> =
    monad {  
        let! a = pop1
        if a = 5 then
            do! push1 5  
        else  
            do! push1 3  
            do! push1 8
    }

let res75 = stackStuff |> State.run <| [9;0;2;1;0]                                    // (null, [8; 3; 0; 2; 1; 0])

let stackyStack =
    monad {  
        let! stackNow = State.get
        if stackNow = [1;2;3] then
            do! State.put [8;3;1]  
        else
            do! State.put [9;2;1]
    }

let _11_r69 = stackyStack |> State.run <| [9;0;2;1;0]                                 // (null, [9; 2; 1])


(* --------------------------------------------------
    Error monad    
   --------------------------------------------------*)

let res76 : Choice<int, string> = Choice2Of2 "boom" >>= (fun x -> Choice1Of2 (x+1))   // Choice2Of2 "boom"
let res77 : Choice<int, string> = Choice1Of2 10 >>= (fun x -> Choice1Of2 (x+1))       // Choice1Of2 11
let res78 : Choice<int, string> = Choice1Of2 100 >>= (fun x -> Choice2Of2 "no way!")  // Choice2Of2 "no way!"


(* --------------------------------------------------
    Monad functions    
   --------------------------------------------------*)

let inline liftM x = map x
let inline ap x = (<*>) x

let res79 = liftM ((*)3) (Some 8)                                                     // Some 24
let res80 = map ((*)3) (Some 8)                                                       // Some 24
let res81 = ((*)3) <!> (Some 8)                                                       // Some 24
let res82 = liftM not (Writer (true, "chickpeas")) |> Writer.run                      // (false,"chickpeas")
let res83 = liftM ((+)100) (pop1) |> State.run <| [1;2;3;4]                           // (101, [2; 3; 4])
let res84 = (Some ((+)3)) <*> (Some 4)                                                // Some 7
let res85 = ap (Some ((+)3)) (Some 4)                                                 // Some 7
let res86 = [(+)1; (+)2; (+)3] <*> [10;11]                                            // [11; 12; 12; 13; 13; 14]
let res87 = ap [(+)1; (+)2; (+)3] [10;11]                                             // [11; 12; 12; 13; 13; 14]

let res88 = join (Some (Some 9))
let res89 : Option<int> = join (Some (None))                                          // explicit return type required!
let res90 = join [[1;2;3];[4;5;6]]
let res91 = join (Writer (Writer (1,"aaa"), "bbb")) |> Writer.run                     // (1, "bbbaaa")
let res92 : Choice<int, string> = join (Choice1Of2 (Choice1Of2 9))                    // Choice1Of2 9
let res93 : Choice<int, string> = join (Choice1Of2 (Choice2Of2 "error"))              // Choice2Of2 "error"
let res94 = join ( State (fun s -> (push1 10, 1::2::s)) ) |> State.run <| [0;0;0]     // (null, [10; 1; 2; 0; 0; 0])

//filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
//filterM _ []     =  return []
//filterM p (x:xs) =  do
//   flg <- p x
//   ys  <- filterM p xs
//   return (if flg then x:ys else ys)
let inline filterM (f : 'a -> 'Monad'Bool) (xs : List<'a>) : 'Monad'List'a =
    let rec loopM (f : 'a -> 'Monad'Bool) (xs : List<'a>) : 'Monad'List'a =
         monad {
            match xs with
            | h::t -> let! flg = f h
                      let! ys = loopM f t
                      return if flg then (h::ys) else ys 
            | [] -> return []
         }
    loopM f xs

// keepSmall :: Int -> Writer [String] Bool
let keepSmall x : Writer<List<string>, bool> = 
    monad {
        match x < 4 with
        | true -> do! tell ["Keeping " + (x |> string) ]
                  return true
        | false -> do! tell [ (x |> string) + " is too large, throwing it away"]
                   return false
    }

let keepSmallSome x : Option<bool> =
    match x < 4 with
    | true -> Some true 
    | false -> None

let res95 = keepSmall 10
let res96 = keepSmall 3
let res97 = filterM keepSmall [9;1;5;2;10;3]                                        // Writer ([1; 2; 3], ["9 is too large, throwing it away"; "Keeping 1"; "5 is too large, throwing it away"; "Keeping 2"; "10 is too large, throwing it away"; "Keeping 3"])
let res98 = filterM keepSmallSome [1;2;3]                                           // Some [1; 2; 3]
let res99 = filterM keepSmallSome [9;1;5;2;10;3]                                    // None

// foldM             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
// foldM _ a []      =  return a
// foldM f a (x:xs)  =  f a x >>= \fax -> foldM f fax xs
let inline foldM (f:'a->'b->'Monad'a) (a:'a) (bx:List<'b>) : 'Monad'a =
    let rec loopM (f:'a->'b->'Monad'a) (a:'a) (bx:List<'b>) : 'Monad'a =
        match bx with
        | x::xs -> (f a x) >>= fun fax -> loopM f fax xs 
        | [] -> result a
    loopM f a bx

// binSmalls :: Int -> Int -> Maybe Int  
let binSmalls acc x =
    match x with
    | n when n > 9 -> None  
    | _ -> Some (acc + x)

let res100 = foldM binSmalls 0 [2;8;3;1]                                              // Some 14  
let res101 = foldM binSmalls 0 [2;11;3;1]                                             // None

let f = (+)1 << (*)100  
let res102 = f 4                                                                      // 401
let g = ( fun x -> Some (x+1) ) <=< ( fun x -> Some (x*100) )
let res103 = Some 4 >>= g                                                             // Some 401

let fb = List.foldBack id [(+)1;(*)100;(+)1]  
let res104 = fb 1                                                                     // 201
let fs = [ (fun x -> Some (x+1)); (fun x -> Some (x*100))]
let res105 = fs |> List.head <| 3                                                     // Some 4
let res106 = List.foldBack (<=<) fs (fun _ -> Some 4) <| 10                           // Some 401

let experiment (x:KnightPos) = 
    match x with
    | (10, 10) -> None
    | (v1, v2) as v -> Some (v1 + 10, v2 + 10)

let res107 = Some (11,11) >>= experiment >>= experiment >>= experiment                 // Some (41,41)
let exps = [experiment; experiment; experiment]
let res108 = List.foldBack (<=<) exps (experiment) <| (11, 11)                         // Some (51,51)
let res109 = List.reduce (<=<) exps <| (11,11)                                         // Some (41,41)
    
let inMany (n:int) (start: KnightPos) =
    let xs = List.replicate n (moveKnight) 
    let x = List.reduce (<=<) xs <| start in x

let res110 = inMany 4 (2,3)
let canReachIn n start end_ = List.exists (fun x -> x = end_) (inMany n start)
let res111 = canReachIn 3 (6,2) (7,3)

// Safe RPN calculator
let readMaybe st =
    match Double.TryParse(st) with
    | true, v -> Some (v |> int) 
    | false, _ -> None

//foldingFunction :: [Double] -> String -> Maybe [Double]
let foldingFunction xs numberString =
    match xs, numberString with
    | x::y::ys, "*" -> Some ((x*y)::ys)
    | x::y::ys, "+" -> Some ((x+y)::ys)
    | x::y::ys, "-" -> Some ((y-x)::ys)
    | xs, numberString -> liftM (fun x -> x::xs) (readMaybe numberString)

let res112 = foldingFunction [3;2] "*"
let res113 = foldingFunction [3;2] "-"
let res114 = foldingFunction [] "*"
let res115 = foldingFunction [] "1"

let words (st:string) = st.Split(' ') |> Array.toList
  
let solveRPN st =
    monad {
        let! result = foldM foldingFunction [] (words st)
        match result with
        | h::[] -> return result
        | _ -> return []
    }
    
let res116 = solveRPN "1 2 * 4 +"                                                 // Some [6]
let res117 = solveRPN "1 2 * 4 + 5 *"                                             // Some [30]
let res118 = solveRPN "1 2 * 4"                                                   // Some []
let res119 = solveRPN "1 8 wharglbllargh"                                         // Some []


(* --------------------------------------------------
    Custom type classes
   --------------------------------------------------*)

    (* --------------------------------------------------
        Probability    
       --------------------------------------------------*)

module Probability =
    
    type Prob<'a> = Prob of List<'a * float>
    
    type Prob with
        static member probMap f (Prob prob : Prob<'a>) : Prob<'b> = List.map (fun (x, p) -> (f x, p)) prob |> Prob
        static member flatten (Prob xs : Prob<Prob<'a>>) : Prob<'a> =
            let multAll (Prob innerxs, p) = List.map (fun (x, r) -> (x, p * r)) innerxs
            Prob (List.map multAll xs |> List.concat)
        static member Map (prob:Prob<'a>, f) =
            Prob.probMap f prob : Prob<'b>
        static member Return (x) =
            Prob [(x, 1.0)] : Prob<'a>
        static member Bind (prob:Prob<'a>, f:'a -> Prob<'b>) =
            Prob.flatten (map f prob) : Prob<'b>

open Probability

let res120 = map (fun x -> -x) (Prob [(3, 0.5); (5, 0.25); (9, 0.25)])

type Coin =
    | Heads
    | Tails
  
let coin = Prob [(Heads, 0.5); (Tails, 0.5)]  
let loadedCoin = Prob [(Heads, 0.1); (Tails, 0.9)]
  
let flipThree : Prob<bool> =
    monad {  
        let! a = coin  
        let! b = coin  
        let! c = loadedCoin  
        return [a;b;c] |> List.forall (fun x -> x = Tails)
    }   // Prob [(false, 0.025); (false, 0.225); (false, 0.025); (false, 0.225); (false, 0.025); (false, 0.225); (false, 0.025); (true, 0.225)]


    (* --------------------------------------------------
        MathLib    
       --------------------------------------------------*)

module MathLibTypeClasses =
    (* One way to define custom type class *)
    type PlusClass =
        static member Plus (x:int) = fun y -> x + y
        static member Plus (x:float) = fun y -> x + y 
        
    type MinusClass =
        static member Minus (x:int) = fun y -> x - y
        static member Minus (x:float) = fun y -> x - y 
      
    type DivideClass =
        static member Divide (x:int) = fun y -> x / y
        static member Divide (x:float) = fun y -> x / y

    type LengthClass =
        static member Length (xs: List<int>  , _:int)   = xs.Length
        static member Length (xs: List<float>, _:float) = xs.Length |> float
    
    let inline internal plus   x y = let inline Invoke (a: ^a, b: ^b) = ((^a or ^b) : (static member Plus  : ^b -> _) b) in Invoke (Unchecked.defaultof<PlusClass>  , x) y
    let inline internal minus  x y = let inline Invoke (a: ^a, b: ^b) = ((^a or ^b) : (static member Minus : ^b -> _) b) in Invoke (Unchecked.defaultof<MinusClass> , x) y
    let inline internal divide x y = let inline Invoke (a: ^a, b: ^b) = ((^a or ^b) : (static member Divide: ^b -> _) b) in Invoke (Unchecked.defaultof<DivideClass>, x) y
    let inline internal length xs :^R  = let inline Invoke (a: ^a, b: ^b, r:^r) = ((^a or ^b or ^r) : (static member Length: _*_ -> _) b, r) in Invoke (Unchecked.defaultof<LengthClass>, xs, Unchecked.defaultof<'R>) 

open MathLibTypeClasses

let res121 = plus 12 34
let res122 = plus 12.0 34.0
let res123 = divide 12 34
let res124 = divide 12.0 34.0

module StatisticsLib =
    let inline mean (xs : 'Math'a list) =
        let sum = xs |> List.reduce plus in divide sum (length xs)

open StatisticsLib

let res125 = mean [13.; 23.; 42.; 45.; 61.; 73.; 96.; 100.; 199.; 420.; 900.; 3839.]
let res126 = mean [13; 23; 42; 45; 61; 73; 96; 100; 199; 420; 900; 3839]
    
type Tree with
    static member inline Plus   (x:Tree<'a>      ) = fun (_ : Tree<'a>) -> x
    static member inline Minus  (x:Tree<'a>      ) = fun (_ : Tree<'a>) -> x
    static member inline Divide (x:Tree<'a>      ) = fun (_ : Tree<'a>) -> x
    static member inline Length (x:List<Tree<'a>>, _:Tree<'a>) = MEmpty : Tree<'a>

(* defined above
let testTree =
    let one = Node (1, MEmpty, MEmpty)
    let six = Node (6, MEmpty, MEmpty)
    let three = Node (3, one, six)
    let eight = Node (8, MEmpty, MEmpty)
    let ten = Node (10, MEmpty, MEmpty)
    let nine = Node (9, eight, ten)
    let five = Node (5, three, nine)
    five
 *)
    
let res127 : Tree<int> = plus (testTree) (MEmpty)
let res128 : Tree<int> = minus (testTree) (MEmpty)
let res129 : Tree<int> = divide (testTree) (MEmpty)
let res130 : Tree<int> = length [testTree; MEmpty]
let res132 : Tree<int> = mean [testTree; testTree]


    (* --------------------------------------------------
        YesNo    
       --------------------------------------------------*)

module YesNoTypeClass =
    (* Other way to define custom type class *)
    type YesNoClass = YesNoClass with
        static member YesNo (x:int) =
            match x with
            | 0 -> false
            | _ -> true
        static member YesNo (xs:List<_>) = 
            match xs with
            | [] -> false
            | _ -> true
        static member YesNo (b:bool) = b
        static member YesNo (op:option<_>) = 
            match op with
            | Some _ -> true
            | None -> false

    module internal YesNoOverloads =
        let inline Invoke (a: ^a, b: ^b) =
            ( (^a or ^b) : (static member YesNo: ^b -> bool) b )

    let inline yesno (x) : bool = YesNoOverloads.Invoke (YesNoClass, x)

open YesNoTypeClass

let inline yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult
    else noResult

let res133 = yesno []
let res134 = yesno [12;32]
let res135 = yesno ["cat";"dog"]
let res136 = yesno 10
let res137 = yesno (Some 10.2)
let res138 = yesno None
let res139 = yesnoIf [] "empty" "not"

type TrafficLight =
    | Red
    | Green
    | Orange

type TrafficLight with
    static member YesNo (tl:TrafficLight) =
        match tl with
        | Red -> false
        | _ -> true

let res140 = yesno Red
let res141 = yesno Green