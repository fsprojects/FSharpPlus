namespace FSharpPlus

open System
open FsControl
open FSharpPlus.Operators

module Lens =

    // Basic operations
    let set  lens v = Identity.run << lens (fun _ -> Identity v)
    let over lens f = Identity.run << lens (Identity << f)
    let view lens   = Const.run << lens Const
    let preview prism = First.run << Const.run << prism (fun x -> Const (FSharpPlus.First (Some x)))

    /// Build a 'Lens' from a getter and a setter.
    let inline lens sa sbt afb s = sbt s <!> afb (sa s)


    /// Build a Prism usign Choice instead of Option a to permit the types of s and t to differ.
    let inline prism (bt:'b->'t) (seta:'s->Choice<'a,'t>) = dimap seta (choice result (map bt)) << (fun g -> choice Choice2Of2 (Choice1Of2 << g))
    let inline prism' (bs:'b->'s) (sma:'s->Option<'a>) = prism bs (fun s -> option (Choice2Of2 s) Choice1Of2 (sma s))

    /// Build an iso from a pair of inverse functions.
    let inline iso (sa:'s->'a) (bt:'b->'t) = dimap sa (map bt)

    /// Merge two lenses, getters, setters, folds or traversals.
    let inline choosing l r f = function
        | Choice2Of2 x -> Choice2Of2 <!> l f x
        | Choice1Of2 x -> Choice1Of2 <!> r f x

    // Some common Lens
    let inline _1 f t = map (fun x -> mapItem1 (fun _ -> x) t) (f (item1 t))
    let inline _2 f t = map (fun x -> mapItem2 (fun _ -> x) t) (f (item2 t))
    let inline _3 f t = map (fun x -> mapItem3 (fun _ -> x) t) (f (item3 t))
    let inline _4 f t = map (fun x -> mapItem4 (fun _ -> x) t) (f (item4 t))
    let inline _5 f t = map (fun x -> mapItem5 (fun _ -> x) t) (f (item5 t))

    // Prism
    let inline _Choice1Of2 x = (prism Choice1Of2 <| choice (Choice2Of2 << Choice2Of2) Choice1Of2) x
    let inline _Choice2Of2 x = (prism Choice2Of2 <| choice Choice1Of2 (Choice2Of2 << Choice1Of2)) x
    let inline _Some x = (prism Some <| option (Choice2Of2 None) Choice1Of2) x
    let inline _None x = (prism' (konst None) <| option (Some ()) (konst None)) x

    // Traversal
    let inline _all ref f s =
        let update old = if old = ref then f old else result old
        traverse update s

    // functions
    let inline to' k = dimap k (contramap k)

    let inline foldMapOf l f = Const.run </rmap/> l (Const </rmap/> f)
    let inline foldOf    l   = Const.run </rmap/> l Const
    let inline foldrOf l f z = flip Endo.run z << foldMapOf l (Endo </rmap/> f)
    let inline foldlOf l f z = (flip Endo.run z </lmap/> Dual.run) </rmap/> foldMapOf l (Dual </rmap/> Endo </rmap/> flip f)
    let        toListOf  l   = let cons x y = x :: y in foldrOf l cons []

    let inline anyOf  l f = let getAny (Any p) = p in getAny </rmap/> foldMapOf l (Any </rmap/> f)
    let inline allOf  l f = let getAll (All p) = p in getAll </rmap/> foldMapOf l (All </rmap/> f)
    let inline elemOf l = anyOf l << (=)
    let inline items x = traverse x

    let inline filtered p f s = if p s then f s else result s
    let inline both f (a, b) = liftA2 tuple2 (f a) (f b)

    type Exchange<'T,'U> = Exchange of 'T * 'U with static member Dimap (Exchange (sa, bt), f, g) = Exchange (sa << f, g << bt)
    let inline withIso ai k = let (Exchange (sa, bt)) = ai (Exchange (id, Identity)) in k sa (Identity.run </rmap/> bt)
    let inline from l    = withIso l <| fun sa bt -> iso bt sa
    let inline mapping k = withIso k <| fun sa bt -> iso (map sa) (map bt)

    // Operators
    let (^.)  s l = view l s
    let (.->) l s = set  l s
    let (%->) l s = over l s
    let (^?)  s l = preview  l s
    let (^..) s l = toListOf l s


   