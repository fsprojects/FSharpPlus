#r @"..\bin\Release\FsControl.Core.dll"

open System
open FsControl.Core.TypeMethods
open FsControl.Core.TypeMethods.Collection
open FsControl.Core.TypeMethods.Functor
open FsControl.Core.TypeMethods.Applicative
open FsControl.Core.TypeMethods.Comonad
open FsControl.Core.TypeMethods.Foldable
open FsControl.Core.TypeMethods.Monoid

let flip f x y = f y x
let konst k _ = k
let (</) = (|>)
let (/>) = flip

let inline skip (n:int) (x) = Inline.instance (Skip, x) n
let inline take (n:int) (x) = Inline.instance (Take, x) n
let inline fromList (value:list<'t>) = Inline.instance FromList value
let inline toList value :list<'t> = Inline.instance (ToList, value) ()
let inline extract x = Inline.instance (Comonad.Extract, x) ()
let inline result  x = Inline.instance Pure x
let inline mempty() = Inline.instance Monoid.Mempty ()
let inline mappend (x:'a) (y:'a) :'a = Inline.instance (Mappend, x) y
let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldr, x) (f,z)
let inline foldMap f x = Inline.instance (FoldMap, x) f
let inline filter (p:_->bool) (x:'t) = (Inline.instance (Filter, x) p) :'t

let inline groupBy (f:'a->'b) (x:'t) = (Inline.instance (GroupBy, x) f)
let inline splitBy (f:'a->'b) (x:'t) = (Inline.instance (SplitBy, x) f)
let inline sortBy  (f:'a->'b) (x:'t) = (Inline.instance (SortBy , x) f) :'t

type ZipList<'s> = ZipList of 's seq with
    static member instance (_:Map,   ZipList x  , _:ZipList<'b>) = fun (f:'a->'b) -> ZipList (Seq.map f x)
    static member instance (_:Pure, _:ZipList<'a>  ) = fun (x:'a)     -> ZipList (Seq.initInfinite (konst x))
    static member instance (_:Apply  ,   ZipList (f:seq<'a->'b>), ZipList x ,_:ZipList<'b>) = fun () ->
        ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>
    static member instance (_:Mempty, _:ZipList<'a>  ) = fun () -> ZipList Seq.empty   : ZipList<'a>
    static member instance (_:Mappend, ZipList(x) , _) = fun (ZipList(y)) -> ZipList (Seq.append x y)
    static member instance (_:Skip   , (ZipList s):ZipList<'a> , _:ZipList<'a>) = fun n -> ZipList (Seq.skip n s) :ZipList<'a>
    static member instance (_:Take   , (ZipList s):ZipList<'a> , _:ZipList<'a>) = fun n -> ZipList (Seq.take n s) :ZipList<'a>
    static member instance (_:Extract, (ZipList s):ZipList<'a> , _:'a) = fun () -> Seq.head s

let threes = filter ((=) 3) [ 1;2;3;4;5;6;1;2;3;4;5;6 ]
let fours  = filter ((=) 4) [|1;2;3;4;5;6;1;2;3;4;5;6|]
// let five   = filter ((=) 5) (set [1;2;3;4;5;6])             // <- Uses the default method.

let arrayGroup = groupBy ((%)/> 2) [|11;2;3;9;5;6;7;8;9;10|]
let listGroup  = groupBy ((%)/> 2) [ 11;2;3;9;5;6;7;8;9;10 ]
let sortedList = sortBy  string    [ 11;2;3;9;5;6;7;8;9;10 ]

let bigSeq = ZipList (seq {1..10000000})
let bigLst = [ 1..10000000 ]
let bigArr = [|1..10000000|]
let bigMut = new ResizeArray<_>(seq {1..10000000})

let x = extract bigSeq
let y = extract bigLst
let z = extract bigArr

let a = skip 1000 bigSeq
let b = skip 1000 bigLst
let c = skip 1000 bigArr
let d = skip 1000 bigMut
let e = "hello world" |> skip 6 |> toList
let h = fromList ['h';'e';'l';'l';'o';' '] + "world"

let asQuotation = mappend <@ new ResizeArray<_>(["1"]) @> <@ new ResizeArray<_>(["2;3"]) @>

let inline internal map   f x = Inline.instance (Map, x) f
let inline internal (>>=) x (f:_->'R) : 'R = Inline.instance (Monad.Bind, x) f

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
let sameSeq = mappend (mempty()) (seq [4;5;6])

// This should not compile
// twoStacks = mappend stack stack
// let twoSeqs'  = mappend (seq [1;2;3]) [4;5;6]
// let twoSeqs'' = mappend [1;2;3] (seq [4;5;6])


let singletonList: _ list = result 1
let singletonSeq : _ seq  = result 1


// This should not compile (but it does)
let mappedstack = map string stack

// Test Seq Monad
                        
let rseq =
    monad {
        let! x1 =  [1;2]
        let! x2 = seq [10;20]
        return ((+) x1 x2) }


// Test Seq Comonad
let inline duplicate (x)                = Inline.instance (Comonad.Duplicate, x) ()
let inline extend  (g:'Comonad'a->'b) (s:'Comonad'a): 'Comonad'b = Inline.instance (Comonad.Extend, s) g
let inline (=>>)   (s:'Comonad'a) (g:'Comonad'a->'b): 'Comonad'b = extend g s

let lst   = seq [1;2;3;4;5]
let elem1 = extract   lst
let tails = duplicate lst
let lst'  = extend extract lst

// This should not compile (but it does)
let elem3  = extract   stack
let tails' = duplicate stack

// This should not compile
// let stk'  = extend extract stack


// Test foldable

let r10  = foldr (+) 0 (seq [1;2;3;4])
let r323 = toList (seq [3;2;3])
let r03  = filter ((=) 3) (seq [1;2;3])

// This should not compile ??? (but it does)
let r10' = foldr (+) 0 stack
let r123 = toList stack

// This should not compile
// let r03' = filter ((=) 3) stack


// Test traversable
let inline sequenceA  t = Inline.instance (Traversable.SequenceA, t) ()

let resNone  = sequenceA (seq [Some 3;None ;Some 1])

// This should not compile (but it does)
let resNone' = sequenceA (new Collections.Generic.Stack<_>([Some 3;None  ;Some 1]))


// Test SeqT Monad Transformer

open FsControl.Core.Types

let listT  = ListT (Some [2;4]      ) >>= fun x -> ListT (Some [x; x+10]      )
let seqT   = SeqT  (Some (seq [2;4])) >>= fun x -> SeqT  (Some (seq [x; x+10]))
let resListTSome2547 = (SeqT (Some (seq [2;4]) )) >>=  (fun x -> SeqT ( Some (seq [x;x+3])) )