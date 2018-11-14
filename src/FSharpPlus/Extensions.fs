namespace FSharpPlus

open System

/// Additional operations on Option
[<RequireQualifiedAccess>]
module Option =
    /// <summary>If both value are Some, returns the result of applying the second value to the first one. Otherwise it returns None.</summary>
    /// <param name="f">The function.</param>
    /// <param name="x">The value.</param>
    /// <returns>The result sequence.</returns>
    let apply f x =
        match f, x with
        | Some f, Some x -> Some (f x)
        | _              -> None

    /// <summary>If value is Some, returns both of them tupled. Otherwise it returns None tupled.</summary>
    /// <param name="v">The value.</param>
    /// <returns>The resulting tuple.</returns>
    let unzip v =
        match v with
        | Some (x, y) -> Some x, Some y
        | _           -> None  , None

    /// <summary>If both value are Some, returns both of them tupled. Otherwise it returns None.</summary>
    /// <param name="x">The first value.</param>
    /// <param name="y">The second value.</param>
    /// <returns>The resulting option.</returns>
    let zip x y =
        match x, y with
        | Some x, Some y -> Some (x, y)
        | _              -> None

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResult source = match source with Some x -> Ok x | None -> Error ()

    /// <summary>Converts an option to a Result.</summary>
    /// <param name="errorValue">The error value to be used in case of None.</param>
    /// <param name="source">The option value.</param>
    /// <returns>The resulting Result value.</returns>
    let toResultWith (errorValue: 'Error) (source: 'T option) = match source with Some x -> Ok x | None -> Error errorValue

    /// <summary>Converts a Result to an option.</summary>
    /// <remarks>The error value (if any) is lost.</remarks>
    /// <param name="source">The Result value.</param>
    /// <returns>The resulting option value.</returns>
    let ofResult source = match source with Ok x -> Some x | Error _ -> None


/// Additional operations on Result<'Ok,'Error>
[<RequireQualifiedAccess>]
module Result =
    let result x = Ok x
    let throw  x = Error x
    let apply f x = match f, x with   Ok a, Ok b      -> Ok (a b) | Error e, _ | _, Error e -> Error e      : Result<'b,'e>
    let flatten                  = function Ok (Ok v) -> Ok v     | Ok (Error e)  | Error e -> Error e
    let inline catch f           = function Ok v      -> Ok v     | Error e                 -> (f: 't->_) e : Result<'v,'e>
    let inline either f g        = function Ok v      -> f v      | Error e                 -> g e


/// Additional operations on Choice
[<RequireQualifiedAccess>]
module Choice =
    let result x = Choice1Of2 x
    let throw  x = Choice2Of2 x
    let apply f x = match f, x with Choice1Of2 a, Choice1Of2 b              -> Choice1Of2 (a b) | Choice2Of2 e, _ | _, Choice2Of2 e        -> Choice2Of2 e : Choice<'b,'e>
    let map   f                        = function Choice1Of2 v              -> Choice1Of2 (f v) | Choice2Of2 e                             -> Choice2Of2 e
    let flatten                        = function Choice1Of2 (Choice1Of2 v) -> Choice1Of2 v     | Choice1Of2 (Choice2Of2 e) | Choice2Of2 e -> Choice2Of2 e
    let bind (f: 't -> _)              = function Choice1Of2 v              -> f v              | Choice2Of2 e                             -> Choice2Of2 e : Choice<'v,'e>
    let inline catch (f: 't -> _)      = function Choice1Of2 v              -> Choice1Of2 v     | Choice2Of2 e                             -> f e          : Choice<'v,'e>
    let inline either f g              = function Choice1Of2 v              -> f v              | Choice2Of2 e                             -> g e


/// Additional operations on Seq
[<RequireQualifiedAccess>]
module Seq =
    let bind (f: 'a->seq<'b>) x = Seq.collect f x
    let apply f x = bind (fun f -> Seq.map ((<|) f) x) f
    let foldBack f x z = Array.foldBack f (Seq.toArray x) z

    let chunkBy projection (source: _ seq) = seq {
        use e = source.GetEnumerator ()
        if e.MoveNext () then
            let mutable g = projection e.Current
            let mutable members = ResizeArray ()
            members.Add e.Current
            while e.MoveNext () do
                let key = projection e.Current
                if g = key then members.Add e.Current
                else
                    yield g, members
                    g <- key
                    members <- ResizeArray ()
                    members.Add e.Current
            yield g, members }

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    let intersperse sep list = seq {
        let mutable notFirst = false
        for element in list do
            if notFirst then yield sep
            yield element
            notFirst <- true }

    /// Inserts a separator between each element in the source sequence.
    let intercalate separator source = seq {
        let mutable notFirst = false
        for element in source do
            if notFirst then yield! separator
            yield! element
            notFirst <- true }

    /// Creates a sequence of sequences by splitting the source sequence on any of the given separators.
    let split separators source =
        let split options = seq {
            match separators |> Seq.map Seq.toList |> Seq.toList with
            | []         -> yield source
            | separators ->
                let buffer = ResizeArray ()
                let candidate = separators |> List.map List.length |> List.max |> ResizeArray
                let mutable i = 0
                for item in source do
                    candidate.Add item
                    match separators |> List.filter (fun sep -> sep.Length > i && item = sep.[i]) with
                    | [] ->
                        i <- 0
                        buffer.AddRange candidate
                        candidate.Clear ()
                    | seps ->
                        if seps |> List.exists (fun sep -> sep.Length = i + 1) then
                            i <- 0
                            if options = StringSplitOptions.None || buffer.Count > 0 then yield buffer.ToArray () :> seq<_>
                            buffer.Clear ()
                            candidate.Clear ()
                        else i <- i + 1
                if candidate.Count > 0 then buffer.AddRange candidate
                if options = StringSplitOptions.None || buffer.Count > 0 then yield buffer :> seq<_> }
        split StringSplitOptions.None

    let replace (oldValue: seq<'t>) (newValue: seq<'t>) (source: seq<'t>) : seq<'t> = seq {
        let old = oldValue |> Seq.toList
        if old.Length = 0 then
            yield! source
        else
            let candidate = ResizeArray old.Length
            let mutable sindex = 0
            for item in source do
                candidate.Add item
                if item = old.[sindex] then
                    sindex <- sindex + 1
                    if sindex >= old.Length then
                        sindex <- 0
                        yield! newValue
                        candidate.Clear ()
                else
                    sindex <- 0
                    yield! candidate
                    candidate.Clear ()
            yield! candidate }

    /// <summary>Returns a sequence that drops N elements of the original sequence and then yields the
    /// remaining elements of the sequence.</summary>
    /// <remarks>When count exceeds the number of elements in the sequence it
    /// returns an empty sequence instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    let drop i (source: seq<_>) =
        let mutable count = i
        use e = source.GetEnumerator ()
        while (count > 0 && e.MoveNext ()) do count <- count-1
        seq { while e.MoveNext () do yield e.Current }

    let replicate count initial = Linq.Enumerable.Repeat (initial, count)

    open System.Collections.ObjectModel
    open System.Collections.Generic
    let toIReadOnlyList (x: seq<_>) = x |> ResizeArray |> ReadOnlyCollection :> IReadOnlyList<_>


/// Additional operations IList<'T>
[<RequireQualifiedAccess>]
module IList =
    open System.Collections.ObjectModel
    open System.Collections.Generic
    let toIReadOnlyList (source: IList<_>) = ReadOnlyCollection source :> IReadOnlyList<_>


/// Additional operations on List
[<RequireQualifiedAccess>]
module List =

    /// Creates a list with a single element.
    let singleton x = [x]

    let cons x y = x :: y
    let apply f x = List.collect (fun f -> List.map ((<|) f) x) f

    /// Returns a list with all possible tails of the source list.
    let tails x = let rec loop = function [] -> [] | _::xs as s -> s::(loop xs) in loop x

    let take i list = Seq.take i list |> Seq.toList

    let skip i list =
        let rec listSkip lst = function 
            | 0 -> lst 
            | n -> listSkip (List.tail lst) (n-1)
        listSkip list i


    /// <summary>Returns a list that drops N elements of the original list and then yields the
    /// remaining elements of the list.</summary>
    /// <remarks>When count exceeds the number of elements in the list it
    /// returns an empty list instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The result list.</returns>
    let drop i list = 
        let rec loop i lst = 
            match lst, i with
            | [] as x, _ | x, 0 -> x
            | x, n -> loop (n-1) (List.tail x)
        if i > 0 then loop i list else list

    let intercalate (separator: list<_>) (source: seq<list<_>>) = source |> Seq.intercalate separator |> Seq.toList

    /// Inserts a separator between each element in the source list.
    let intersperse element source = source |> List.toSeq |> Seq.intersperse element |> Seq.toList : list<'T>

    /// Creates a sequence of lists by splitting the source list on any of the given separators.
    let split (separators: seq<list<_>>) (source: list<_>) = source |> List.toSeq |> Seq.split separators |> Seq.map Seq.toList

    /// Replace a subsequence of the source list with the given replacement list.
    let replace oldValue (newValue: _ list) (source: _ list) = source |> List.toSeq |> Seq.replace oldValue newValue |> Seq.toList : list<'T>

    let toIReadOnlyList (source: _ list) =
        { new System.Collections.Generic.IReadOnlyList<_> with
            member __.Count = source.Length
            member __.Item with get index = source.[index]
            member __.GetEnumerator () = (source :> _ seq).GetEnumerator ()
            member __.GetEnumerator () = (source :> System.Collections.IEnumerable).GetEnumerator () }


/// Additional operations on Array
[<RequireQualifiedAccess>]
module Array =
    let intercalate (separator: _ []) (source: seq<_ []>) = source |> Seq.intercalate separator |> Seq.toArray

    /// Inserts a separator between each element in the source array.
    let intersperse element source = source |> Array.toSeq |> Seq.intersperse element |> Seq.toArray : 'T []

    /// Creates a sequence of arrays by splitting the source array on any of the given separators.
    let split (separators: seq<_ []>) (source: _ []) = source |> Array.toSeq |> Seq.split separators |> Seq.map Seq.toArray

    /// Replace a subsequence of the source array with the given replacement array.
    let replace (oldValue: _ []) (newValue: _ []) source = source |> Array.toSeq |> Seq.replace oldValue newValue |> Seq.toArray : 'T []


/// Additional operations on String
[<RequireQualifiedAccess>]
module String =
    open System.Text
    open System.Globalization

    /// Concatenates all elements, using the specified separator between each element.
    let intercalate (separator: string) (source: seq<string>) = String.Join (separator, source)

    /// Inserts a separator between each char in the source string.
    let intersperse (element: char) (source: string) = String.Join ("", Array.ofSeq (source |> Seq.intersperse element))

    /// Creates a sequence of strings by splitting the srouce string on any of the given separators.
    let split (separators: seq<string>) (source: string) = source.Split (Seq.toArray separators, StringSplitOptions.None) :> seq<_>

    /// Replace a substring with the given replacement string.
    let replace (oldValue: string) newValue (source: string) = if oldValue.Length = 0 then source else source.Replace (oldValue, newValue)

    let isSubString subString (source: string) = source.Contains subString
    let startsWith  subString (source: string) = source.StartsWith (subString, false, CultureInfo.InvariantCulture)
    let endsWith    subString (source: string) = source.EndsWith   (subString, false, CultureInfo.InvariantCulture)
    let contains    char      (source: string) = Seq.contains char source
    let toUpper (source: string) = if isNull source then source else source.ToUpperInvariant ()
    let toLower (source: string) = if isNull source then source else source.ToLowerInvariant ()
    let trimWhiteSpaces (source: string) = source.Trim ()

    let normalize normalizationForm (source: string) = if isNull source then source else source.Normalize normalizationForm
    let removeDiacritics (source: string) =
        if isNull source then source
        else
            source
            |> normalize NormalizationForm.FormD
            |> String.filter (fun ch -> CharUnicodeInfo.GetUnicodeCategory ch <> UnicodeCategory.NonSpacingMark)
            |> normalize NormalizationForm.FormC


/// Additional operations on IReadOnlyCollection<'T>
[<RequireQualifiedAccess>]
module IReadOnlyCollection =
    open System.Collections.Generic

    let ofArray (source: 'T[]   ) = source                :> IReadOnlyCollection<'T>
    let ofList  (source: 'T list) = source                :> IReadOnlyCollection<'T>
    let ofSeq   (source: seq<'T>) = source |> Array.ofSeq :> IReadOnlyCollection<'T>
    let map mapping (source: IReadOnlyCollection<'T>) = Seq.map mapping source |> Seq.toArray :> IReadOnlyCollection<'U>


/// Additional operations on ReadOnlyList<'T>
[<RequireQualifiedAccess>]
module IReadOnlyList =
    open System.Collections.Generic

    let ofArray (source: _ array) = IList.toIReadOnlyList source
    let toArray (source: IReadOnlyList<_>) = Array.ofSeq source

    /// Returns a new IReadOnlyList from a given IReadOnlyList, with replaced binding for index.
    let add i value (source: IReadOnlyList<_>) =
        let setNth i v (source: _ array) = source.[i] <- v; source
        if 0 <= i && i < source.Count then
            source |> Array.ofSeq |> setNth i value |> ofArray |> Some
        else None

    let tryItem i (source: IReadOnlyList<_>) =
        if 0 <= i && i < source.Count then Some source.[i]
        else None


/// Additional operations on Map<'Key, 'Value>
[<RequireQualifiedAccess>]
module Map =
    open System.Collections.Generic
    open System.Linq

    let keys   (source: Map<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: Map<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    /// <summary>Map values of the original Map.</summary>
    /// <remarks>Keys remain unchanged.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues f (x: Map<'Key, 'T>) = Map.map (fun _ -> f) x

    /// <summary>Map values of two Maps.</summary>
    /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">The first input Map.</param>
    /// <param name="y">The second input Map.</param>
    ///
    /// <returns>The mapped Map.</returns>
    let mapValues2 f (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) = Map <| seq {
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield (k, f.Invoke (vx, vy))
            | None    -> () }

    /// <summary>Tuple values of two Maps.</summary>
    /// <remarks>Keys that are not present on both Maps are dropped.</remarks>
    /// <param name="x">The first input Map.</param>
    /// <param name="y">The second input Map.</param>
    ///
    /// <returns>The tupled Map.</returns>
    let zip (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) = Map <| seq {
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield (k, (vx, vy))
            | None    -> () }

    let unzip (source: Map<'Key, 'T1 * 'T2>) = mapValues fst source, mapValues snd source

    /// Returns the union of two maps, using the combiner function for duplicate keys.
    let unionWith combiner (source1: Map<'Key, 'Value>) (source2: Map<'Key, 'Value>) =
        Map.fold (fun m k v' -> Map.add k (match Map.tryFind k m with Some v -> combiner v v' | None -> v') m) source1 source2

    /// Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: Map<'Key, 'T>) (altSource: Map<'Key, 'T>) = unionWith (fun x _ -> x) source altSource

/// Additional operations on IDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module Dict =
    open System.Collections.Generic
    open System.Collections.ObjectModel
    open System.Linq

    let toIReadOnlyDictionary source = ReadOnlyDictionary source :> IReadOnlyDictionary<_,_>

    let tryGetValue k (dct: IDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None

    let keys   (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: IDictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    let map f (x: IDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IDictionary<'Key, 'U>

    let map2 f (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'U>

    /// <summary>Tuple values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'T1 * 'T2>

    let unzip (source: IDictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1 :> IDictionary<'Key, 'T1>, dct2 :> IDictionary<'Key, 'T2>

    /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: IDictionary<'Key, 'Value>) (source2: IDictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d :> IDictionary<'Key,'Value>

    // Returns the union of two maps, preferring values from the first in case of duplicate keys.
    let union (source: IDictionary<'Key, 'T>) (altSource: IDictionary<'Key, 'T>) = 
        Enumerable
          .Union(
            source, 
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with 
                      member __.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member __.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>

/// Additional operations on IReadOnlyDictionary<'Key, 'Value>
[<RequireQualifiedAccess>]
module IReadOnlyDictionary =
    open System.Linq
    open System.Collections.Generic

    let add key value (table: IReadOnlyDictionary<'Key, 'Value>) = table |> Seq.map (|KeyValue|) |> Map |> Map.add key value :> IReadOnlyDictionary<_,_>

    let tryGetValue k (dct: IReadOnlyDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None

    let keys   (source: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(k, _)) -> k) source
    let values (source: IReadOnlyDictionary<_,_>) = Seq.map (fun (KeyValue(_, v)) -> v) source

    let map f (x: IReadOnlyDictionary<'Key, 'T>) =
        let dct = Dictionary<'Key, 'U> ()
        for KeyValue(k, v) in x do
            dct.Add (k, f v)
        dct :> IReadOnlyDictionary<'Key, 'U>

    let map2 f (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'U> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, f.Invoke (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'U>

    /// <summary>Tuple values of two dictionaries.</summary>
    /// <remarks>Keys that are not present on both dictionaries are dropped.</remarks>
    /// <param name="x">The first input dictionary.</param>
    /// <param name="y">The second input dictionary.</param>
    ///
    /// <returns>The tupled dictionary.</returns>
    let zip (x: IReadOnlyDictionary<'Key, 'T1>) (y: IReadOnlyDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IReadOnlyDictionary<'Key, 'T1 * 'T2>

    let unzip (source: IReadOnlyDictionary<'Key, 'T1 * 'T2>) =
        let dct1 = Dictionary<'Key, 'T1> ()
        let dct2 = Dictionary<'Key, 'T2> ()
        for KeyValue(k, (vx, vy)) in source do
            dct1.Add (k, vx)
            dct2.Add (k, vy)
        dct1 :> IReadOnlyDictionary<'Key, 'T1>, dct2 :> IReadOnlyDictionary<'Key, 'T2>

    /// Returns the union of two dictionaries, using the combiner function for duplicate keys.
    let unionWith combiner (source1: IReadOnlyDictionary<'Key, 'Value>) (source2: IReadOnlyDictionary<'Key, 'Value>) =
        let d = Dictionary<'Key,'Value> ()
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt combiner
        for KeyValue(k, v ) in source1 do d.[k] <- v
        for KeyValue(k, v') in source2 do d.[k] <- match d.TryGetValue k with true, v -> f.Invoke (v, v') | _ -> v'
        d :> IReadOnlyDictionary<'Key,'Value>

    /// Returns the union of two dictionaries, preferring values from the first in case of duplicate keys.
    let union (source: IReadOnlyDictionary<'Key, 'T>) (altSource: IReadOnlyDictionary<'Key, 'T>) = 
        Enumerable
          .Union(
            source, 
            altSource,
            { new IEqualityComparer<KeyValuePair<'Key,'T>> with 
                      member __.Equals ((a:KeyValuePair<'Key,'T>),(b:KeyValuePair<'Key,'T>)) : bool = a.Key = b.Key
                      member __.GetHashCode (a:KeyValuePair<'Key,'T>) = a.Key.GetHashCode () })
          .ToDictionary((fun x -> x.Key), (fun y -> y.Value)) :> IDictionary<'Key, 'T>


/// Additional operations on IEnumerator
[<RequireQualifiedAccess>]
module Enumerator =
        
    let inline invalidArgFmt (arg: string) (format: string) paramArray =    
        let msg = String.Format (format,paramArray)
        raise (new ArgumentException (msg, arg))
    
    let noReset ()         = raise (new System.NotSupportedException ("Reset is not supported on this enumerator."))
    let notStarted ()      = invalidOp "Enumeration has not started. Call MoveNext."
    let alreadyFinished () = invalidOp "Enumeration already finished."
    let check started = if not started then notStarted ()
    let dispose (r: System.IDisposable) = r.Dispose ()
    
    open System.Collections
    open System.Collections.Generic
    
    /// A concrete implementation of an enumerator that returns no values
    [<Sealed>]
    type EmptyEnumerator<'T> () =
        let mutable started = false
        interface IEnumerator<'T> with
            member __.Current =
                check started
                (alreadyFinished () : 'T)
    
        interface IEnumerator with
            member __.Current =
                check started
                (alreadyFinished () : obj)
            member __.MoveNext () =
                if not started then started <- true
                false
            member __.Reset() = noReset ()
        interface System.IDisposable with
            member __.Dispose () = ()
              
    let Empty<'T> () = new EmptyEnumerator<'T>() :> IEnumerator<'T>

    let singleton x = (Seq.singleton x).GetEnumerator()

    type IFinallyEnumerator = abstract AppendFinallyAction : (unit -> unit) -> unit

    [<Sealed>]
    type ConcatEnumerator<'T> (sources: IEnumerator<IEnumerator<'T>>) =
        let mutable outerEnum = sources
        let mutable currInnerEnum = Empty ()
        let mutable started  = false
        let mutable finished = false
        let mutable compensations = []

        [<DefaultValue(false)>]
        val mutable private currElement : 'T

        member __.Finish () =
            finished <- true
            try
                match currInnerEnum with
                | null -> ()
                | _ ->
                    try
                        currInnerEnum.Dispose ()
                    finally
                        currInnerEnum <- null
            finally
                try
                    match outerEnum with
                    | null -> ()
                    | _ ->
                        try
                            outerEnum.Dispose ()
                        finally
                            outerEnum <- null
                finally
                    let rec iter comps =
                        match comps with
                        | [] -> ()
                        | h::t -> try h () finally iter t
                    try
                        compensations |> List.rev |> iter
                    finally
                        compensations <- []

        member x.GetCurrent () =
            check started
            if finished then alreadyFinished () else x.currElement

        interface IFinallyEnumerator with
            member __.AppendFinallyAction f =
                compensations <- f :: compensations

        interface IEnumerator<'T> with
            member x.Current = x.GetCurrent ()

        interface IEnumerator with
            member x.Current = box (x.GetCurrent ())

            member x.MoveNext () =
                if not started then (started <- true)
                if finished then false
                else
                  let rec takeInner () =
                    // check inner
                    if currInnerEnum.MoveNext () then
                        x.currElement <- currInnerEnum.Current
                        true
                    else
                        // check outer
                        let rec takeOuter () =
                            if outerEnum.MoveNext () then
                                let ie = outerEnum.Current
                                // Optimization to detect the statically-allocated empty IEnumerators
                                match box ie with
                                | :? EmptyEnumerator<'T> ->
                                        // This one is empty, just skip, don't call GetEnumerator, try again
                                        takeOuter ()
                                | _ ->
                                        // OK, this one may not be empty.
                                        // Don't forget to dispose of the inner enumerator now we're done with it
                                        currInnerEnum.Dispose ()
                                        currInnerEnum <- ie
                                        takeInner ()
                            else
                                // We're done
                                x.Finish ()
                                false
                        takeOuter ()
                  takeInner ()

            member __.Reset () = noReset ()

        interface System.IDisposable with
            member x.Dispose () = if not finished then x.Finish ()

    let concat sources = new ConcatEnumerator<_> (sources) :> IEnumerator<_>
    
    let rec tryItem index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then None
        elif index = 0 then Some e.Current
        else tryItem (index-1) e
    
    let rec nth index (e: IEnumerator<'T>) =
        if not (e.MoveNext ()) then
            let shortBy = index + 1
            invalidArgFmt "index"
                "{0}\nseq was short by {1} {2}"
                [|"The input sequence has an insufficient number of elements."; shortBy; (if shortBy = 1 then "element" else "elements")|]
        if index = 0 then e.Current
        else nth (index-1) e
    
    [<NoEquality; NoComparison>]
    type MapEnumeratorState =
        | NotStarted
        | InProcess
        | Finished
    
    [<AbstractClass>]
    type MapEnumerator<'T> () =
        let mutable state = NotStarted
        [<DefaultValue(false)>]
        val mutable private curr : 'T
    
        member this.GetCurrent () =
            match state with
            | NotStarted -> notStarted ()
            | Finished   -> alreadyFinished ()
            | InProcess  -> ()
            this.curr
    
        abstract DoMoveNext : byref<'T> -> bool
        abstract Dispose : unit -> unit
    
        interface IEnumerator<'T> with
            member this.Current = this.GetCurrent ()
    
        interface IEnumerator with
            member this.Current = box (this.GetCurrent ())
            member this.MoveNext () =
                state <- InProcess
                if this.DoMoveNext (&this.curr) then
                    true
                else
                    state <- Finished
                    false
            member __.Reset () = noReset ()
        interface System.IDisposable with
            member this.Dispose () = this.Dispose ()
    
    let map f (e: IEnumerator<_>) : IEnumerator<_> =
        upcast
            { new MapEnumerator<_>() with
                  member __.DoMoveNext (curr: byref<_>) =
                      if e.MoveNext () then
                          curr <- f e.Current
                          true
                      else
                          false
                  member __.Dispose () = e.Dispose ()
            }
    
    let mapi f (e: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        let i = ref -1
        upcast {  
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    i := !i + 1
                    if e.MoveNext () then
                        curr <- f.Invoke (!i, e.Current)
                        true
                    else false
                member __.Dispose () = e.Dispose () }
    
    let map2 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    if n1 && n2 then
                        curr <- f.Invoke (e1.Current, e2.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }
    
    let mapi2 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        let i = ref -1
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    i := !i + 1
                    if e1.MoveNext () && e2.MoveNext () then
                        curr <- f.Invoke (!i, e1.Current, e2.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }
    
    let map3 f (e1: IEnumerator<_>) (e2: IEnumerator<_>) (e3: IEnumerator<_>) : IEnumerator<_> =
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt f
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    let n3 = e3.MoveNext ()
                   
                    if n1 && n2 && n3 then
                        curr <- f.Invoke (e1.Current, e2.Current, e3.Current)
                        true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally
                        try e2.Dispose ()
                        finally e3.Dispose () }
    
    let choose f (e: IEnumerator<'T>) =
        let started = ref false
        let curr = ref None
        let get () =  check !started; (match !curr with None -> alreadyFinished () | Some x -> x)
        { new IEnumerator<'U> with
              member __.Current = get ()
          interface IEnumerator with
              member __.Current = box (get ())
              member __.MoveNext () =
                  if not !started then started := true
                  curr := None
                  while (!curr).IsNone && e.MoveNext () do
                      curr := f e.Current
                  Option.isSome !curr
              member __.Reset() = noReset ()
          interface System.IDisposable with
              member __.Dispose () = e.Dispose () }
    
    let filter f (e: IEnumerator<'T>) =
        let started = ref false
        { new IEnumerator<'T> with
                member __.Current = check !started; e.Current
            interface IEnumerator with
                member __.Current = check !started; box e.Current
                member __.MoveNext () =
                    let rec next () =
                        if not !started then started := true
                        e.MoveNext () && (f e.Current || next ())
                    next ()
                member __.Reset () = noReset ()
            interface System.IDisposable with
                member __.Dispose () = e.Dispose () }
    
    let unfold f x : IEnumerator<_> =
        let state = ref x
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    match f !state with
                    |   None -> false
                    |   Some (r, s) ->
                            curr <- r
                            state := s
                            true
                member __.Dispose () = () }
    
    let upto lastOption f =
        match lastOption with
        | Some b when b < 0 -> Empty ()    // a request for -ve length returns empty sequence
        | _ ->
            let unstarted   = -1  // index value means unstarted (and no valid index)
            let completed   = -2  // index value means completed (and no valid index)
            let unreachable = -3  // index is unreachable from 0,1,2,3,...
            let finalIndex  = 
                match lastOption with
                | Some b -> b             // here b>=0, a valid end value.
                | None   -> unreachable   // run "forever", well as far as Int32.MaxValue since indexing with a bounded type.
            // The Current value for a valid index is "f i".
            // Lazy<_> values are used as caches, to store either the result or an exception if thrown.
            // These "Lazy<_>" caches are created only on the first call to current and forced immediately.
            // The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
            // For example, the full enumeration of Seq.initInfinite in the tests.
            // state
            let index = ref unstarted
            // a Lazy node to cache the result/exception
            let current = ref Unchecked.defaultof<_>
            let setIndex i = index := i; current := Unchecked.defaultof<_> // cache node unprimed, initialized on demand.
            let getCurrent () =
                if !index = unstarted then notStarted ()
                if !index = completed then alreadyFinished ()
                match box !current with
                | null -> current := Lazy<_>.Create (fun () -> f !index)
                | _    ->  ()
                // forced or re-forced immediately.
                (!current).Force ()
            { new IEnumerator<'U> with
                  member __.Current = getCurrent ()
              interface IEnumerator with
                  member __.Current = box (getCurrent ())
                  member __.MoveNext () =
                      if !index = completed then false
                      elif !index = unstarted then
                          setIndex 0
                          true
                      else (
                          if !index = System.Int32.MaxValue then raise <| System.InvalidOperationException ("Enumeration based on System.Int32 exceeded System.Int32.MaxValue.")
                          if !index = finalIndex then false
                          else
                              setIndex (!index + 1)
                              true )
                  member __.Reset () = noReset ()
              interface System.IDisposable with
                  member __.Dispose () = () }

    let zip (e1: IEnumerator<_>) (e2: IEnumerator<_>) : IEnumerator<_> =
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    if n1 && n2 then curr <- (e1.Current, e2.Current); true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally e2.Dispose () }

    let zip3 (e1: IEnumerator<_>) (e2: IEnumerator<_>) (e3: IEnumerator<_>) : IEnumerator<_> =
        upcast {
            new MapEnumerator<_> () with
                member __.DoMoveNext curr =
                    let n1 = e1.MoveNext ()
                    let n2 = e2.MoveNext ()
                    let n3 = e3.MoveNext ()
                    if n1 && n2 && n3 then curr <- (e1.Current, e2.Current, e3.Current); true
                    else false
                member __.Dispose () =
                    try e1.Dispose ()
                    finally
                        try e2.Dispose ()
                        finally e3.Dispose () }


/// Additional operations on IEnumerator
module Async =

    /// <summary>Creates an async workflow from another workflow 'x', mapping its result with 'f'.</summary>
    let map f x = async.Bind (x, async.Return << f)

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping it results with 'f'.</summary>
    /// <remarks>Workflows are run in sequence.</remarks>
    /// <param name="f">The mapping function.</param>
    /// <param name="x">First async workflow.</param>
    /// <param name="y">Second async workflow.</param>
    let map2 f x y = async {
        let! a = x
        let! b = y
        return f a b}

    /// <summary>Creates an async workflow from two workflows 'x' and 'y', tupling it results.</summary>
    let zip x y = async {
        let! a = x
        let! b = y
        return a, b}

    /// Flatten two nested asyncs into one.
    let join x = async.Bind (x, id)

    /// <summary>Create an async workflow that is the result of applying the resulting function of an async workflow to the resulting value of another async workflow.</summary>
    /// <param name="f">Async workflow returning a function.</param>
    /// <param name="x">Async workflow returning a value.</param>
    let apply f x = async.Bind (f, fun x1 -> async.Bind (x, fun x2 -> async {return x1 x2}))

    /// Raise an exception in the async workflow
    let raise<'T> (ex: exn) : Async<'T> = Async.FromContinuations (fun (_, errK, _) -> errK ex)


/// Module containing F#+ Extension Methods  
module Extensions =

    type Collections.Generic.IEnumerable<'T>  with
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   -> this |> Seq.skip a
            | None  , Some b -> this |> Seq.take b
            | Some a, Some b -> this |> Seq.skip a |> Seq.take (b-a+1)


    type List<'T> with
        
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   when a < 0 -> this |> List.skip (this.Length + a)
            | Some a, None              -> this |> List.skip                a 
            | None  , Some b when b < 0 -> this |> List.take (this.Length + b)
            | None  , Some b            -> this |> List.take                b
            | Some a, Some b when a >= 0 && b >= 0 -> this |> List.skip a |> List.take b
            | Some a, Some b -> 
                let l = this.Length
                let f i = if i < 0 then l + i else i
                let a = f a
                this |> List.skip a |> List.take (f b - a + 1)

         

    // http://msdn.microsoft.com/en-us/library/system.threading.tasks.task.whenall.aspx 

    open System.Threading
    open System.Threading.Tasks

    let private (|Canceled|Faulted|Completed|) (t: Task<'a>) =
        if t.IsCanceled then Canceled
        else if t.IsFaulted then Faulted t.Exception
        else Completed t.Result

    type Task<'t> with
        static member WhenAll (tasks: Task<'a>[], ?cancellationToken: CancellationToken) =
            let tcs = TaskCompletionSource<'a[]> ()
            let cancellationToken = defaultArg cancellationToken CancellationToken.None
            cancellationToken.Register (fun () -> tcs.TrySetCanceled () |> ignore) |> ignore
            let results = Array.zeroCreate<'a> tasks.Length
            let pending = ref results.Length
            tasks 
            |> Seq.iteri (fun i t ->
                let continuation = function
                | Canceled    -> tcs.TrySetCanceled () |> ignore
                | Faulted e   -> tcs.TrySetException e |> ignore
                | Completed r -> 
                    results.[i] <- r
                    if Interlocked.Decrement pending = 0 then 
                        tcs.SetResult results
                t.ContinueWith (continuation, cancellationToken, TaskContinuationOptions.ExecuteSynchronously, TaskScheduler.Default) |> ignore)
            tcs.Task


    type Async<'t> with

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: seq<Async<_>>) : Async<seq<_>> = async {
            use enum = t.GetEnumerator ()
            let rec loop () =
                if enum.MoveNext () then async.Bind (enum.Current, fun x -> async.Bind (loop (), fun y -> async.Return (seq {yield x; yield! y})))
                else async.Return Seq.empty
            return! loop () }

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: list<Async<_>>) : Async<list<_>> =
            let rec loop acc = function
                | []    -> async.Return (List.rev acc)
                | x::xs -> async.Bind (x, fun x -> loop (x::acc) xs)
            loop [] t

        /// Combine all asyncs in one, chaining them in sequence order.
        static member Sequence (t: array<Async<_>>) : Async<array<_>> = async {
            let siz = Array.length t
            let arr = Array.zeroCreate siz
            for i in 0 .. siz-1 do
                let! v = t.[i]
                arr.[i] <- v
            return arr }


    type Option<'t> with

        /// Returns None if it contains a None element, otherwise a list of all elements
        static member Sequence (t: seq<option<'T>>) =
            let mutable ok = true
            let res = Seq.toArray (seq {
                use e = t.GetEnumerator ()
                while e.MoveNext () && ok do
                    match e.Current with
                    | Some v -> yield v
                    | None   -> ok <- false })
            if ok then Some (Array.toSeq res) else None
