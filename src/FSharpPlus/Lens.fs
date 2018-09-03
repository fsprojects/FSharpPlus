namespace FSharpPlus

open FSharpPlus.Operators
open FSharpPlus.Data

/// Lens functions and operators
module Lens =

    /// [omit]
    module Internals =
        let lmap' ab p = ab >> p
        let rmap' cd p = p >> cd
        let getAny (Any p) = p
        let getAll (All p) = p

        [<Struct>]
        type Exchange<'T,'U> = Exchange of 'T * 'U with
            static member Dimap (Exchange (sa, bt), f, g) = Exchange (sa << f, g << bt)

    open Internals

    // Basic operations

    /// Write to a lens
    let setl lens v = Identity.run << lens (fun _ -> Identity v)

    /// Update a value in a lens
    let over lens f = Identity.run << lens (Identity << f)

    /// Read from a lens
    let view lens   = Const.run << lens Const

    /// Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). See also (^?).
    let preview prism = First.run << Const.run << prism (fun x -> Const (FSharpPlus.Data.First (Some x)))

    /// Build a 'Lens' from a getter and a setter.
    let inline lens sa sbt afb s = sbt s <!> afb (sa s)


    /// Build a Prism using Result instead of Option to permit the types of 's and 't to differ.
    let inline prism (bt: 'b->'t) (seta: 's->Result<'a,'t>) = dimap seta (either (map bt) result) << (fun g -> either (Ok << g) Error)
    let inline prism' (bs: 'b->'s) (sma: 's->Option<'a>) = prism bs (fun s -> option Ok (Error s) (sma s))

    /// Build an iso from a pair of inverse functions.
    let inline iso (sa: 's->'a) (bt: 'b->'t) = dimap sa (map bt)

    /// Merge two lenses, getters, setters, folds or traversals.
    let inline choosing l r f = function
        | Error x -> Error <!> l f x
        | Ok    x -> Ok    <!> r f x

    // Some common Lens

    /// Lens for the first element of a tuple
    let inline _1 f t = map (fun x -> mapItem1 (fun _ -> x) t) (f (item1 t))

    /// Lens for the second element of a tuple
    let inline _2 f t = map (fun x -> mapItem2 (fun _ -> x) t) (f (item2 t))

    /// Lens for the third element of a tuple
    let inline _3 f t = map (fun x -> mapItem3 (fun _ -> x) t) (f (item3 t))

    /// Lens for the fourth element of a tuple
    let inline _4 f t = map (fun x -> mapItem4 (fun _ -> x) t) (f (item4 t))

    /// Lens for the fifth element of a tuple
    let inline _5 f t = map (fun x -> mapItem5 (fun _ -> x) t) (f (item5 t))

    // Prism
    let inline _Ok    x = (prism Ok    <| either Ok (Error << Error)) x
    let inline _Error x = (prism Error <| either (Error << Ok) Ok) x
    let inline _Some x = (prism Some <| option Ok (Error None)) x
    let inline _None x = (prism' (konst None) <| option (konst None) (Some ())) x

    // Traversal
    let inline _all ref f s =
        let update old = if old = ref then f old else result old
        traverse update s

    // functions
    let inline to' k = dimap k (contramap k)

    let foldMapOf l f = Const.run </rmap'/> l (Const </rmap'/> f)
    let foldOf    l   = Const.run </rmap'/> l Const
    let foldrOf l f z = flip Endo.run z << foldMapOf l (Endo </rmap'/> f)
    let foldlOf l f z = (flip Endo.run z </lmap'/> Dual.run) </rmap'/> foldMapOf l (Dual </rmap'/> Endo </rmap'/> flip f)

    /// Extract a list of the targets of a Fold. See also (^..).
    let toListOf  l   = let cons x y = x :: y in foldrOf l cons []

    let anyOf  l f = getAny </rmap'/> foldMapOf l (Any </rmap'/> f)
    let allOf  l f = getAll </rmap'/> foldMapOf l (All </rmap'/> f)
    let elemOf l = anyOf l << (=)
    let inline items x = traverse x

    let inline filtered p f s = if p s then f s else result s
    let inline both f (a, b) = tuple2 <!> f a <*> f b

    let inline withIso ai k = let (Exchange (sa, bt)) = ai (Exchange (id, Identity)) in k sa (Identity.run </rmap'/> bt)
    let inline from' l   = withIso l <| fun sa bt -> iso bt sa
    let inline mapping k = withIso k <| fun sa bt -> iso (map sa) (map bt)

    // Operators

    /// Read from a lens. Same as ``view``.
    let (^.)  s l = view l s

    /// Write to a lens. Same as ``setl``.
    let (.->) l s = setl l s

    /// Update a value in a lens. Same as ``over``.
    let (%->) l s = over l s

    /// Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). Same as ``preview``.
    let (^?)  s l = preview  l s

    /// Extract a list of the targets of a Fold. Same as ``toListOf``.
    let (^..) s l = toListOf l s