#r @"..\bin\Release\FsControl.Core.dll"

open System
open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Collection
open FsControl.Core.Abstractions.Functor
open FsControl.Core.Abstractions.Applicative
open FsControl.Core.Abstractions.Comonad
open FsControl.Core.Abstractions.Foldable
open FsControl.Core.Abstractions.Monoid

let flip f x y = f y x
let konst k _ = k
let (</) = (|>)
let (/>) = flip

let inline skip (n:int) (x) = Inline.instance (Skip, x) n
let inline take (n:int) (x) = Inline.instance (Take, x) n
let inline fromList (value:list<'t>) = Inline.instance FromList value
let inline toList    value :list<'t> = Inline.instance (ToList, value) ()
let inline extract x = Inline.instance (Comonad.Extract, x) ()
let inline result  x = Inline.instance Pure x
let inline mempty() = Inline.instance Monoid.Mempty ()
let inline mappend (x:'a) (y:'a) :'a = Inline.instance (Mappend, x) y
let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldr, x) (f,z)
let inline foldMap f x = Inline.instance (FoldMap, x) f
let inline filter p : 't->'t = foldMap (fun a -> if p a then result a else mempty())

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

let e1 = filter ((=) '2')  [ '2';'3';'4' ]
let e2 = filter ((=) '2')  [|'2';'3';'4'|]

let bigSeq = ZipList (seq {1..10000000})
let bigLst = [ 1..10000000 ]
let bigArr = [|1..10000000|]
let bigMut = new Collections.Generic.List<_>(seq {1..10000000})

let x = extract bigSeq
let y = extract bigLst
let z = extract bigArr

let a = skip 1000 bigSeq
let b = skip 1000 bigLst
let c = skip 1000 bigArr
let d = skip 1000 bigMut
let e = "hello world" |> skip 6 |> toList
let h = fromList ['h';'e';'l';'l';'o';' '] + "world"