namespace FSharpPlus.Data

open System
open System.Runtime.InteropServices
open System.ComponentModel
open System.Collections.Generic
open FSharpPlus
open FSharpPlus.Control

/// A type-safe set that contains at least one element.
type NonEmptySet<[<EqualityConditionalOn>]'a when 'a: comparison> = private { Value: Set<'a> } with
    interface Collections.IEnumerable with member x.GetEnumerator () = (x.Value :> _ seq).GetEnumerator() :> _
    interface IEnumerable<'a> with member x.GetEnumerator() = (x.Value :> _ seq).GetEnumerator()
    interface IReadOnlyCollection<'a> with member x.Count = x.Value.Count

    static member Create (first: 'a, [<ParamArray>] rest: 'a[]) : NonEmptySet<'a> =
        { Value = Set.ofArray rest |> Set.add first }

    member x.Add(value) = { Value = Set.add value x.Value }
    member x.Count = x.Value.Count
    member x.Contains(value) = x.Value.Contains(value)
    member x.IsSubsetOf(other: _ NonEmptySet) = x.Value.IsSubsetOf(other.Value)
    member x.IsProperSubsetOf(other: _ NonEmptySet) = x.Value.IsProperSubsetOf(other.Value)
    member x.IsSupersetOf(other: _ NonEmptySet) = x.Value.IsSupersetOf(other.Value)
    member x.IsProperSupersetOf(other: _ NonEmptySet) = x.Value.IsProperSupersetOf(other.Value)
    member x.MinimumElement = x.Value.MinimumElement
    member x.MaximumElement = x.Value.MaximumElement

/// Basic operations on NonEmptySet
[<RequireQualifiedAccess>]
module NonEmptySet =
    /// <summary>Builds a non empty set.</summary>
    let create x xs = { Value = Set.ofSeq xs |> Set.add x }
    /// <summary>Builds a non empty set with a single element.</summary>
    let singleton x = { Value = Set.singleton x }
    /// <summary>Builds a list from the given non empty set.</summary>
    let toList { Value = v } = Set.toList v
    /// <summary>Builds a sequence from the given non empty set.</summary>
    let toSeq { Value = v } = Set.toSeq v
    /// <summary>Builds an array from the given non empty set.</summary>
    let toArray { Value = v } = Set.toArray v
    /// <summary>Builds a set from the given non empty set.</summary>
    let toSet { Value = v } = v
    /// <summary>Builds a non empty set from the given array.</summary>
    /// <param name="array">The input array.</param>
    /// <returns>Non empty set containing the elements of the array.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input array is empty.</exception>
    /// <remarks>Throws exception for empty array</remarks>
    let ofArray (array : _ array) =
        match array |> Array.toList with
        | []    -> invalidArg "array" "The input array was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty set from the given list.</summary>
    /// <param name="list">The input list.</param>
    /// <returns>Non empty set containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty list</remarks>
    let ofList (list : _ list) =
        match list with
        | []    -> invalidArg "list" "The input list was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty set from the given sequence.</summary>
    /// <param name="seq">The input list.</param>
    /// <returns>Non empty set containing the elements of the list.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input list is empty.</exception>
    /// <remarks>Throws exception for empty sequence</remarks>
    let ofSeq (seq : _ seq) =
        match seq |> Seq.toList with
        | []    -> invalidArg "seq" "The input sequence was empty."
        | x::xs -> create x xs
    /// <summary>Builds a non empty set from the given set.</summary>
    /// <param name="set">The input set.</param>
    /// <returns>Non empty set containing the elements of the set.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the input set is empty.</exception>
    /// <remarks>Throws exception for empty set</remarks>
    let ofSet (set: Set<_>) =
      if Set.isEmpty set then invalidArg "seq" "The input sequence was empty."
      else { Value = set }
    /// Transforms a set to a NonEmptySet, returning an option to signal when the original set was empty.
    let tryOfSet (set: Set<_>) =
      if Set.isEmpty set then None
      else Some { Value = set }
    /// Returns the count of a non empty set. You can also use property nes.Count
    let count (nes: _ NonEmptySet) = Set.count nes.Value
    /// <summary>Returns a new set with an element added to the set. No exception is raised if
    /// the set already contains the given element.</summary>
    /// <param name="value">The value to add.</param>
    /// <param name="set">The input set.</param>
    /// <returns>A new set containing <c>value</c>.</returns>
    let add value (nes: _ NonEmptySet) = { Value = Set.add value nes.Value }
    /// <summary>Evaluates to "true" if the given element is in the given set.</summary>
    /// <param name="element">The element to test.</param>
    /// <param name="set">The input set.</param>
    /// <returns>True if <c>element</c> is in <c>set</c>.</returns>
    let contains element (nes: _ NonEmptySet) = nes.Value |> Set.contains element
    /// <summary>Evaluates to "true" if all elements of the first set are in the second</summary>
    /// <param name="set1">The potential subset.</param>
    /// <param name="set2">The set to test against.</param>
    /// <returns>True if <c>set1</c> is a subset of <c>set2</c>.</returns>
    let isSubset (set1: _ NonEmptySet) (set2: _ NonEmptySet) = Set.isSubset set1.Value set2.Value
    /// <summary>Evaluates to "true" if all elements of the first set are in the second, and at least
    /// one element of the second is not in the first.</summary>
    /// <param name="set1">The potential subset.</param>
    /// <param name="set2">The set to test against.</param>
    /// <returns>True if <c>set1</c> is a proper subset of <c>set2</c>.</returns>
    let isProperSubset (set1: _ NonEmptySet) (set2: _ NonEmptySet) = Set.isProperSubset set1.Value set2.Value
    /// <summary>Evaluates to "true" if all elements of the second set are in the first.</summary>
    /// <param name="set1">The potential superset.</param>
    /// <param name="set2">The set to test against.</param>
    /// <returns>True if <c>set1</c> is a superset of <c>set2</c>.</returns>
    let isSuperset (set1: _ NonEmptySet) (set2: _ NonEmptySet) = Set.isSuperset set1.Value set2.Value
    /// <summary>Evaluates to "true" if all elements of the second set are in the first, and at least
    /// one element of the first is not in the second.</summary>
    /// <param name="set1">The potential superset.</param>
    /// <param name="set2">The set to test against.</param>
    /// <returns>True if <c>set1</c> is a proper superset of <c>set2</c>.</returns>
    let isProperSuperset (set1: _ NonEmptySet) (set2: _ NonEmptySet) = Set.isProperSuperset set1.Value set2.Value
    /// <summary>Tests if any element of the collection satisfies the given predicate.
    /// If the input function is <c>predicate</c> and the elements are <c>i0...iN</c>
    /// then computes <c>p i0 or ... or p iN</c>.</summary>
    /// <param name="predicate">The function to test set elements.</param>
    /// <param name="set">The input set.</param>
    /// <returns>True if any element of <c>set</c> satisfies <c>predicate</c>.</returns>
    let exists predicate (set: _ NonEmptySet) = set.Value |> Set.exists predicate
    /// <summary>Returns a new collection containing the results of applying the
    /// given function to each element of the input set.</summary>
    /// <param name="mapping">The function to transform elements of the input set.</param>
    /// <param name="set">The input set.</param>
    /// <returns>A set containing the transformed elements.</returns>
    let map mapping (set: _ NonEmptySet) = { Value = set.Value |> Set.map mapping }
    /// <summary>Tests if all elements of the collection satisfy the given predicate.
    /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
    /// then computes <c>p i0 &amp;&amp; ... &amp;&amp; p iN</c>.</summary>
    /// <param name="predicate">The function to test set elements.</param>
    /// <param name="set">The input set.</param>
    /// <returns>True if all elements of <c>set</c> satisfy <c>predicate</c>.</returns>
    let forall predicate (set: _ NonEmptySet) = set.Value |> Set.forall predicate
    /// <summary>Computes the union of the two sets.</summary>
    /// <param name="set1">The first input set.</param>
    /// <param name="set2">The second input set.</param>
    /// <returns>The union of <c>set1</c> and <c>set2</c>.</returns>
    let union (set1: _ NonEmptySet) (set2: _ NonEmptySet) = { Value = Set.union set1.Value set2.Value }
    /// <summary>Computes the union of a non empty list of sets.</summary>
    /// <param name="sets">The sequence of sets to union.</param>
    /// <returns>The union of the input sets.</returns>
    let unionMany (sets: _ NonEmptySet NonEmptyList) =
      { Value = Set.unionMany (NonEmptyList.toSeq sets |> Seq.map toSet)}
    /// <summary>Applies the given function to each element of the set, in order according
    /// to the comparison function.</summary>
    /// <param name="action">The function to apply to each element.</param>
    /// <param name="set">The input set.</param>
    let iter action (set: _ NonEmptySet) = set.Value |> Set.iter action
    /// <summary>Returns the lowest element in the set according to the ordering being used for the set.</summary>
    /// <param name="set">The input set.</param>
    /// <returns>The min value from the set.</returns>
    let minElement (set: _ NonEmptySet) = Set.minElement set.Value
    /// <summary>Returns the highest element in the set according to the ordering being used for the set.</summary>
    /// <param name="set">The input set.</param>
    /// <returns>The max value from the set.</returns>
    let maxElement (set: _ NonEmptySet) = Set.maxElement set.Value
    /// <summary>Applies the given accumulating function to all the elements of the set</summary>
    /// <param name="folder">The accumulating function.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="set">The input set.</param>
    /// <returns>The final state.</returns>
    let fold folder state (set: _ NonEmptySet) = Set.fold folder state set.Value
    /// <summary>Applies the given accumulating function to all the elements of the set.</summary>
    /// <param name="folder">The accumulating function.</param>
    /// <param name="set">The input set.</param>
    /// <param name="state">The initial state.</param>
    /// <returns>The final state.</returns>
    let foldBack folder (set: _ NonEmptySet) state = Set.foldBack folder set.Value state
    let reduce reduction (set: _ NonEmptySet) = Seq.reduce reduction (toSeq set)

type NonEmptySet<[<EqualityConditionalOn>]'a when 'a: comparison> with
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Map (x: NonEmptySet<'a>, f: 'a->'b) = NonEmptySet.map f x
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Return (x: 'a) = NonEmptySet.singleton x
    static member (+) (set1: NonEmptySet<'a>, set2: NonEmptySet<'a>) = NonEmptySet.union set1 set2
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member FoldBack (set: NonEmptySet<'a>, f, z) = Set.foldBack f set.Value z

    #if !FABLE_COMPILER
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToList (s: NonEmptySet<'a>, [<Optional>]_impl: ToList) = NonEmptySet.toList s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member ToSeq (s: NonEmptySet<'a>, [<Optional>]_impl: ToSeq) = NonEmptySet.toSeq s

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member Reduce (s: NonEmptySet<'a>, reduction: 'a -> 'a -> 'a) = NonEmptySet.reduce reduction s
    #endif
