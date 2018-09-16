namespace FSharpPlus

open FSharpPlus.Operators
open FSharpPlus.Data

/// Lens functions and operators
module Lens =

    /// [omit]
    module Internals =
        let lmap' ab p = ab >> p
        let rmap' cd p = p >> cd
        let dimap' ab cd  p = ab >> p >> cd
        let getAny (Any p) = p
        let getAll (All p) = p

        [<Struct>]
        type Exchange<'T,'U> = Exchange of 'T * 'U with
            static member Dimap (Exchange (sa, bt), f, g) = Exchange (sa << f, g << bt)

        type ConstIdType =
            | TUndefined
            | TIdentity
            | TConst
            with static member (&&&) (x, y) = 
                    match x, y with
                    | v         , v' when v = v'    -> v
                    | TUndefined, v | v, TUndefined -> v
                    | _         , _                 -> failwithf "Unexpected %A %A" x y

        type ConstId<'monoid,'t> = ConstId of ('t * 'monoid * ConstIdType) with
            static member inline Return x = ConstId (x, getZero (), TUndefined)   : ConstId<'Monoid,'T>
            static member inline (<*>) (ConstId (f, wf, vf), ConstId (t, wx, vx)) : ConstId<'Monoid,'U> =
                let v = vf &&& vx
                ConstId ((if v = TConst then Unchecked.defaultof<_> else f t), (if v = TIdentity then Unchecked.defaultof<_> else plus wf wx), v)

        module ConstId =
            let map f (ConstId (t: 'T, w, v)) : ConstId<'Monoid,'U> = ConstId ((if v = TConst then Unchecked.defaultof<_> else f t), w, v)
            let asId x = ConstId (x, (), TIdentity) : ConstId<unit, 't>
            let runId (ConstId (t, _, v) : ConstId<'Monoid,'T>) = if v = TConst then failwith "Identity expected, but got Const" else t
            let asConst x = ConstId (Unchecked.defaultof<_>, x, TConst)
            let runConst (ConstId (_, w, v)) : 'T = if v = TIdentity then failwith "Const expected, but got Identity" else w

    open Internals
    open FSharpPlus.Control

    // Basic operations

    /// <summary>Write to a lens.</summary>
    /// <param name="optic">The lens.</param>
    /// <param name="value">The value we want to write in the part targeted by the lens.</param>
    /// <param name="source">The original object.</param>
    /// <returns>The new object with the value modified.</returns>
    let setl optic value source = ConstId.runId (optic (fun _ -> ConstId.asId value) source)

    /// <summary>Update a value in a lens.</summary>
    /// <param name="optic">The lens.</param>
    /// <param name="updater">A function that converts the value we want to write in the part targeted by the lens.</param>
    /// <param name="source">The original object.</param>
    /// <returns>The new object with the value modified.</returns>
    let over optic updater source = ConstId.runId (optic (ConstId.asId << updater) source)

    /// <summary>Read from a lens.</summary>
    /// <param name="optic">The lens.</param>
    /// <param name="source">The object.</param>
    /// <returns>The part the lens is targeting.</returns>
    let view optic source = ConstId.runConst (optic ConstId.asConst source)

    /// <summary>Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). See also (^?).</summary>
    /// <param name="optic">The prism.</param>
    /// <param name="source">The object.</param>
    /// <returns>The value (if any) the prism is targeting.</returns>
    let preview optic source = source |> optic (fun x -> ConstId.asConst (FSharpPlus.Data.First (Some x))) |> ConstId.runConst |> First.run

    /// <summary>Build a 'Lens' from a getter and a setter.</summary>
    /// <remarks>The lens should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
    /// <param name="getter">The getter function.</param>
    /// <param name="setter">The setter function, having as first parameter the object and second the value to set.</param>
    /// <param name="f">The free parameter.</param>
    /// <returns>The lens.</returns>
    let lens getter setter f = fun s -> setter s </ConstId.map/> f (getter s)


    /// <summary>Build a 'Prism' from a constructor and a getter.</summary>
    /// <remarks>The prism should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
    /// <remarks>Using Result instead of Option to permit the types of 's and 't to differ.</remarks>
    /// <param name="constructor">The getter function.</param>
    /// <param name="getter">The getter function, having as first parameter the object and second the value to set.</param>
    /// <param name="f">The free parameter.</param>
    /// <returns>The prism.</returns>
    let inline prism (constructor: 'b -> 't) (getter: 's -> Result<'a,'t>) f = f |> (fun g -> either (Ok << g) Error) |> dimap' getter (either (ConstId.map constructor) ConstId.Return)

    /// <summary>Build a 'Prism' from a constructor and a getter.</summary>
    /// <remarks>The prism should be assigned as an inline function of the free parameter, not a value, otherwise compiler will fail with a type constraint mismatch.</remarks>
    /// <remarks>Using Option which makes 's and 't the same type.</remarks>
    /// <param name="constructor">The getter function.</param>
    /// <param name="getter">The getter function, having as first parameter the object and second the value to set.</param>
    /// <param name="f">The free parameter.</param>
    /// <returns>The prism.</returns>
    let inline prism' (constructor: 'b -> 's) (getter: 's -> Option<'a>) f = prism constructor (fun s -> option Ok (Error s) (getter s)) f

    /// <summary>Build an 'Iso' from a pair of inverse functions.</summary>
    /// <param name="func">The transform function.</param>
    /// <param name="inv">The inverse of the transform function.</param>
    /// <returns>The iso.</returns>
    let inline iso (func: 's -> 'a) (inv: 'b -> 't) = dimap func (ConstId.map inv)

    /// Merge two lenses, getters, setters, folds or traversals.
    /// <param name="optic1">The first optic.</param>
    /// <param name="optic2">The second optic.</param>
    /// <param name="f">The free parameter.</param>
    /// <returns>An optic for a Result which uses the first optic for the Ok and the second for the Error.</returns>
    let choosing optic1 optic2 f = function
        | Error x -> Error </ConstId.map/> optic1 f x
        | Ok    x -> Ok    </ConstId.map/> optic2 f x

    // Some common Lens

    /// Lens for the first element of a tuple
    let inline _1 f t = ConstId.map (fun x -> mapItem1 (fun _ -> x) t) (f (item1 t))

    /// Lens for the second element of a tuple
    let inline _2 f t = ConstId.map (fun x -> mapItem2 (fun _ -> x) t) (f (item2 t))

    /// Lens for the third element of a tuple
    let inline _3 f t = ConstId.map (fun x -> mapItem3 (fun _ -> x) t) (f (item3 t))

    /// Lens for the fourth element of a tuple
    let inline _4 f t = ConstId.map (fun x -> mapItem4 (fun _ -> x) t) (f (item4 t))

    /// Lens for the fifth element of a tuple
    let inline _5 f t = ConstId.map (fun x -> mapItem5 (fun _ -> x) t) (f (item5 t))

    // Prism
    let inline _Ok    x = (prism Ok    <| either Ok (Error << Error)) x
    let inline _Error x = (prism Error <| either (Error << Ok) Ok) x
    let inline _Some x = (prism Some <| option Ok (Error None)) x
    let inline _None x = (prism' (konst None) <| option (konst None) (Some ())) x

    // Traversal
    let inline _all ref f s =
        let update old = if old = ref then f old else ConstId.Return old
        traverse update s

    // functions
    let inline to' k = dimap k (Contramap.InvokeOnInstance k)

    let foldMapOf l f = ConstId.runConst </rmap'/> l (ConstId.asConst </rmap'/> f)
    let foldOf    l   = ConstId.runConst </rmap'/> l ConstId.asConst
    let foldrOf l f z = flip Endo.run z << foldMapOf l (Endo </rmap'/> f)
    let foldlOf l f z = (flip Endo.run z </lmap'/> Dual.run) </rmap'/> foldMapOf l (Dual </rmap'/> Endo </rmap'/> flip f)

    /// Extract a list of the targets of a Fold. See also (^..).
    let toListOf  l   = let cons x y = x :: y in foldrOf l cons []

    let anyOf  l f = getAny </rmap'/> foldMapOf l (Any </rmap'/> f)
    let allOf  l f = getAll </rmap'/> foldMapOf l (All </rmap'/> f)
    let elemOf l = anyOf l << (=)
    let inline items x = traverse x

    let inline filtered p f s = if p s then f s else ConstId.Return s
    let inline both f (a, b) = tuple2 </ConstId.map/> f a </curry ConstId.(<*>)/> f b

    let inline withIso ai k = let (Exchange (sa, bt)) = ai (Exchange (id, ConstId.asId)) in k sa (ConstId.runId </rmap'/> bt)
    let inline from' l   = withIso l <| fun sa bt -> iso bt sa
    let inline mapping k = withIso k <| fun sa bt -> iso (Map.InvokeOnInstance sa) (Map.InvokeOnInstance bt)

    // Operators

    /// <summary>Read from a lens. Same as ``view`` but with the arguments flipped.</summary>
    /// <param name="lens">The lens.</param>
    /// <param name="source">The object.</param>
    /// <returns>The part the lens is targeting.</returns>
    let (^.) source lens = view lens source

    /// <summary>Write to a lens. Same as ``setl``.</summary>
    /// <param name="lens">The lens.</param>
    /// <param name="value">The value we want to write in the part targeted by the lens.</param>
    /// <returns>The new object with the value modified.</returns>
    let (.->) lens value = setl lens value

    /// <summary>Update a value in a lens. Same as ``over``.</summary>
    /// <param name="lens">The lens.</param>
    /// <param name="updater">A function that converts the value we want to write in the part targeted by the lens.</param>
    /// <returns>The new object with the value modified.</returns>
    let (%->) lens updater = over lens updater

    /// <summary>Retrieve the first value targeted by a Prism, Fold or Traversal (or Some result from a Getter or Lens). Same as ``preview`` but with the arguments flipped.</summary>
    /// <param name="prism">The prism.</param>
    /// <param name="source">The object.</param>
    /// <returns>The value (if any) the prism is targeting.</returns>
    let (^?) source prism = preview  prism source

    /// Extract a list of the targets of a Fold. Same as ``toListOf`` but with the arguments flipped.
    let (^..) s l = toListOf l s

    /// <summary>An infix flipped map, restricted to non-primitive types.</summary>
    /// <param name="x">The functor.</param>
    /// <param name="f">The mapper function.</param>
    /// <returns>The mapped Functor.</returns>
    let (<&>) x (f: 'T->'U) = ConstId.map f x