#nowarn "3186"
#r @"..\bin\Release\FsControl.Core.dll"

open System
open FsControl.Core.Types
open FsControl.Core.TypeMethods
open FsControl.Operators

let flip f x y = f y x
let konst k _ = k
let (</) = (|>)
let (/>) = flip

type Tree<'a> =
    | Empty 
    | Leaf of 'a 
    | Node of (Tree<'a>) * 'a * (Tree<'a>)

    // add instance for Foldable abstraction (ToSeq is the minimal definition).
    static member ToSeq x =        
        let rec loop t = seq {
            match t with
            | Empty        -> ()
            | Leaf n       -> yield n
            | Node (l,k,r) -> yield k; yield! loop l; yield! loop r}
        loop x
       
    static member inline FoldBack (x, f, z) = 
        let rec _foldMap x f =
            match x with
            | Empty        -> mempty()
            | Leaf n       -> f n
            | Node (l,k,r) -> mappend (_foldMap l f) (mappend (f k) (_foldMap r f))
        let appEndo (Endo f) = f
        appEndo (_foldMap x (Endo << f )) z

    
let tree = Node (Node (Leaf 1, 6, Leaf 3), 2 , Leaf 9)
let res21  = foldBack   (+) tree 0
// Uses the default method:
let res21' = fold   (+) 0   tree      
let resTr  = exists ((=) 3) tree
let resS3  = tryPick (fun x -> if x = 3 then Some x else None) tree

type ZipList<'s> = ZipList of 's seq with
    static member Return (x:'a)                              = ZipList (Seq.initInfinite (konst x))
    static member Map   (ZipList x, f:'a->'b)                = ZipList (Seq.map f x)
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>
    static member inline Mempty() = result (mempty())                                :ZipList<'a>
    static member inline Mappend (x:ZipList<'a>, y:ZipList<'a>) = liftA2 mappend x y :ZipList<'a>
    // try also commenting/uncommenting the following method.
    static member inline Mconcat (x:seq<ZipList<'a>>) = printfn "ZipList mconcat optimized (in theory)"; List.foldBack mappend (Seq.toList x) (mempty()):ZipList<'a>
    static member ToSeq    (ZipList lst)     = lst

type WrappedList<'s> = WrappedList of 's list with
    static member Return   (_:WrappedList<'a>, _:Return ) = fun (x:'a)     -> WrappedList [x]
    static member Mappend  (WrappedList l, WrappedList x) = WrappedList (l @ x)
    static member Mempty   (_:WrappedList<'a>, _:Mempty) = WrappedList List.empty
    static member ToSeq    (WrappedList lst)     = List.toSeq lst
    static member FoldBack (WrappedList x, f, z) = List.foldBack f x z

let wl = WrappedList  [2..10]

let threes = filter ((=) 3) [ 1;2;3;4;5;6;1;2;3;4;5;6 ]
let fours  = filter ((=) 4) [|1;2;3;4;5;6;1;2;3;4;5;6|]
let twos   = filter ((=) (box 2)) (([1;2;3;4;3;2;1;2;3] |> fromSeq) : Collections.ArrayList)
let five   = filter ((=) 5) (WrappedList [1;2;3;4;5;6])   // <- Uses the default method for filter.
let sorted = sortBy (~-)    (WrappedList [1;2;3;4;5;6])
let optionFilter = filter ((=) 3) (Some 4)

let arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
let listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
let seqGroup   = groupBy ((%)/> 2) (seq [11;2;3;9;5;6;7;8;9;10])

let arrayGroupAdj   = groupAdjBy ((%)/> 2) [11;2;3;9;5;6;7;8;9;10]

let sortedList = sortBy string     [ 11;2;3;9;5;6;7;8;9;10 ]
let sortedSeq  = sortBy string (seq [11;2;3;9;5;6;7;8;9;10])

let bigSeq = seq {1..10000000}
let bigLst = [ 1..10000000 ]
let bigArr = [|1..10000000|]
let bigMut = ResizeArray(seq {1..10000000})

let x = head bigSeq
let y = head bigLst
let z = head bigArr

let a = skip 1000 bigSeq
let b = skip 1000 bigLst
let c = skip 1000 bigArr
let d = skip 1000 bigMut
let e = "hello world" |> skip 6 |> toList
let h = fromList ['h';'e';'l';'l';'o';' '] + "world"
let i = item 2 bigSeq
let j = item 2 "hello"


// Monoids

let asQuotation = mappend <@ ResizeArray(["1"]) @> <@ ResizeArray(["2;3"]) @>
let quot123     = mappend <@ ResizeArray([1])   @> <@ ResizeArray([2;3])   @>
let quot1       = mappend <@ ResizeArray([1])   @>      (mempty())
let quot23      = mappend    (mempty())            <@ ResizeArray([2;3])   @>
let quot13      = mappend    (mempty())            <@ ("1","3") @>
let quotLst123  = mappend    (mempty())            (ZipList [ [1];[2];[3] ])
let quotLst123' = mconcat    [mempty();  mempty();  ZipList [ [1];[2];[3] ]]

let lzy1 = mappend (lazy [1]) (lazy [2;3])
let lzy2 = mappend (mempty()) lzy1
let asy1 = mappend (async.Return [1]) (async.Return [2;3])
let asy2 = mappend (mempty()) asy1

let mapA = Map.empty 
            |> Map.add 1 (async.Return "Hey")
            |> Map.add 2 (async.Return "Hello")

let mapB = Map.empty 
            |> Map.add 3 (async.Return " You")
            |> Map.add 2 (async.Return " World")

let mapAB = mappend mapA mapB
let greeting1 = Async.RunSynchronously mapAB.[2]
let greeting2 = Async.RunSynchronously (mconcat [mapA; mempty(); mapB]).[2]

open System.Collections.Generic
open System.Threading.Tasks

let dicA = new Dictionary<string,Task<string>>()
dicA.["keya"] <- (result "Hey"  : Task<_>)
dicA.["keyb"] <- (result "Hello": Task<_>)

let dicB = new Dictionary<string,Task<string>>()
dicB.["keyc"] <- (result " You"  : Task<_>)
dicB.["keyb"] <- (result " World": Task<_>)

let dicAB = mappend dicA dicB

let greeting3 = extract dicAB.["keyb"]
let greeting4 = extract (mconcat [dicA; mempty(); dicB]).["keyb"]

let res2 = mconcat [ async {return (+) 2 } ; async {return (*) 10 } ; async {return id } ;  async {return (%) 3 } ; async {return mempty() } ] </Async.RunSynchronously/> 3


// Functors, Monads

let quot7 = map ((+)2) <@ 5 @>
let (quot5:Microsoft.FSharp.Quotations.Expr<int>) = result 5

// Do notation
type MonadBuilder() =
    member inline b.Return(x)    = result x
    member inline b.Bind(p,rest) = p >>= rest
    member        b.Let (p,rest) = rest p
    member    b.ReturnFrom(expr) = expr

let monad     = new MonadBuilder()



// Seq

let stack = new Collections.Generic.Stack<_>([1;2;3])

let twoSeqs = mappend (seq [1;2;3]) (seq [4;5;6])
let sameSeq = mappend (mempty()   ) (seq [4;5;6])

let seqFromLst:_ seq = fromList [1;2;3;4]
let seqFromLst' = toSeq [1;2;3;4]
let seqFromOpt  = toSeq (Some 1)

// This should not compile 
(*
let twoStacks = mappend stack stack
let twoSeqs'  = mappend (seq [1;2;3]) [4;5;6]
let twoSeqs'' = mappend [1;2;3] (seq [4;5;6])
let (stackFromLst:_ Collections.Generic.Stack) = fromList [1;2;3;4]
*)

let singletonList: _ list = result 1
let singletonSeq : _ seq  = result 1


// This should not compile (but it does)
(*
let sortedStack = sortBy  string    stack  // <- cool, now it fails
*)
let mappedstack = map string stack
let stackGroup  = groupBy ((%)/> 2) stack


// Test Seq Monad
                        
let rseq =
    monad {
        let! x1 = seq [1;2]
        let! x2 = seq [10;20]
        return ((+) x1 x2) }


// Test Seq Comonad

let lst   = seq [1;2;3;4;5]
let elem1 = head        lst
let tails = duplicate   lst
let lst'  = extend head lst

(* Should fail
let tails' = duplicate stack
let stk'  = extend head stack
*)

// Test foldable

let r10  = foldBack (+) (seq [1;2;3;4]) 0
let r323 = toList (seq [3;2;3])
let r03  = filter ((=) 3) (seq [1;2;3])

// This should not compile ??? (but it does)
let r10' = foldBack (+) stack 0
let r123 = toList stack

let r03' = filter ((=) 3) stack

// Test traversable

let resNone   = traverse (fun x -> if x > 4 then Some x else None) (Seq.initInfinite id) // optimized method, otherwise it doesn't end
let resNone'  = sequenceA (seq [Some 3;None ;Some 1])

// This should not compile (but it does)
let resNone'' = sequenceA (new Collections.Generic.Stack<_>([Some 3;None  ;Some 1]))



let getLine    = async { return System.Console.ReadLine() }
let putStrLn x = async { printfn "%s" x}

let inline sequence ms =
    let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
    Array.foldBack k (Seq.toArray ms) ((result :seq<'a> -> 'M) (Seq.empty))

let inline mapM f as' = sequence (Seq.map f as')

type DoPlusNotationBuilder() =
    member inline b.Return(x) = result x
    member inline b.Bind(p,rest) = p >>= rest
    member b.Let(p,rest) = rest p
    member b.ReturnFrom(expr) = expr
    member inline x.Zero() = zero()
    member inline x.Combine(a, b) = a <|> b
let doPlus = new DoPlusNotationBuilder()

// Test MonadPlus
let nameAndAddress = mapM (fun x -> putStrLn x >>= fun _ -> getLine) (seq ["name";"address"])

// this compiles but it requires a type annotation to tell between
// seq and other monadplus #seq types
let pythags = monad {
  let! z = seq [1..50]
  let! x = seq [1..z]
  let! y = seq [x..z]
  do! (guard (x*x + y*y = z*z) : _ seq)
  return (x, y, z)}


let pythags' = doPlus{
  let! z = seq [1..50]
  let! x = seq [1..z]
  let! y = seq [x..z]
  if (x*x + y*y = z*z) then return (x, y, z)}

let res123123 = (seq [1;2;3]) <|> (seq [1;2;3])
let allCombinations = sequence (seq [seq ['a';'b';'c']; seq ['1';'2']]) //|> Seq.map Seq.toList |> Seq.toList 



// Test SeqT Monad Transformer

open FsControl.Core.Types

let listT  = ListT (Some [2;4]      ) >>= fun x -> ListT (Some [x; x+10]      )
let seqT   = SeqT  (Some (seq [2;4])) >>= fun x -> SeqT  (Some (seq [x; x+10]))
let resListTSome2547 = (SeqT (Some (seq [2;4]) )) >>=  (fun x -> SeqT ( Some (seq [x;x+3])) )
let apSeqT  = SeqT.run ((SeqT  (Some [(+) 3]) ) <*> ( SeqT  (Some [3]) ))