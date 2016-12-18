namespace FsControl.Internals

type Default5 = class end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

open System.Collections.Generic

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline option n f = function None -> n | Some x -> f x
    let inline isNull (value : 'T) =  match value with null -> true | _ -> false

[<RequireQualifiedAccess>]
module internal Option =
    let inline apply f x =
        match (f,x) with 
        | Some f, Some x -> Some (f x) 
        | _              -> None

[<RequireQualifiedAccess>]
module internal List =
    let inline singleton x = [x]
    let inline cons x y = x :: y
    let inline apply f x = List.collect (fun f -> List.map ((<|) f) x) f
    let inline tails x = let rec loop = function [] -> [] | x::xs as s -> s::(loop xs) in loop x
    let inline drop i list = 
        let rec loop i lst = 
            match (lst, i) with
            | ([] as x, _) | (x, 0) -> x
            | x, n -> loop (n-1) (List.tail x)
        if i > 0 then loop i list else list

[<RequireQualifiedAccess>]
module internal Seq =
    let inline bind (f:'a->seq<'b>) x = Seq.collect f x
    let inline apply f x = bind (fun f -> Seq.map ((<|) f) x) f
    let inline foldBack f x z = Array.foldBack f (Seq.toArray x) z

    let inline chunkBy projection (source : _ seq) = seq {
        use e = source.GetEnumerator()
        if e.MoveNext() then
            let g = ref (projection e.Current)
            let members = ref (List())
            (!members).Add(e.Current)
            while (e.MoveNext()) do
                let key = projection e.Current
                if !g = key then (!members).Add(e.Current)
                else
                    yield (!g, !members)
                    g := key
                    members := List()
                    (!members).Add(e.Current)
            yield (!g, !members)}

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    let inline intersperse sep list = seq {
        let notFirst = ref false
        for element in list do 
            if !notFirst then yield sep
            yield element
            notFirst := true}

    let inline split options separators source = seq {
        match separators |> Seq.map Seq.toList |> Seq.toList with
        | []         -> yield source
        | separators ->
            let buffer = ResizeArray()
            let candidate = separators |> List.map List.length |> List.max |> ResizeArray
            let mutable i = 0
            for item in source do
                candidate.Add item
                match separators |> List.filter (fun sep -> sep.Length > i && item = sep.[i]) with
                | [] ->
                    i <- 0
                    buffer.AddRange candidate
                    candidate.Clear()                    
                | seps ->
                    match seps |> List.tryFind (fun sep -> sep.Length = i + 1) with
                    | Some sep ->
                        i <- 0
                        if options = System.StringSplitOptions.None || buffer.Count > 0 then yield buffer.ToArray() :> seq<_>
                        buffer.Clear()
                        candidate.Clear()                        
                    | _ -> i <- i + 1
            if candidate.Count > 0 then buffer.AddRange candidate
            if options = System.StringSplitOptions.None || buffer.Count > 0 then yield buffer :> seq<_> }

    let inline replace (oldValue:seq<'t>) (newValue:seq<'t>) (source:seq<'t>) :seq<'t> = seq {
        let old = oldValue |> Seq.toList
        if (old.Length = 0) then
            yield! source
        else
            let candidate = ResizeArray(old.Length)
            let mutable sindex = 0
            for item in source do
                candidate.Add(item)
                if (item = old.[sindex]) then
                    sindex <- sindex + 1
                    if (sindex >= old.Length) then
                        sindex <- 0
                        yield! newValue
                        candidate.Clear()                    
                else
                    sindex <- 0
                    yield! candidate
                    candidate.Clear()                
            yield! candidate}

    let inline drop i (source:seq<_>) =
        let mutable count = i
        use e = source.GetEnumerator()
        while (count > 0 && e.MoveNext()) do count <- count-1
        seq {while (e.MoveNext()) do yield e.Current}

[<RequireQualifiedAccess>]
module internal Error =
    let inline map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
    let inline apply f x =
        match (f,x) with
        | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
        | (Choice2Of2 a, _)            -> Choice2Of2 a
        | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>
    let inline result x = Choice1Of2 x
    let inline throw  x = Choice2Of2 x
    let inline bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> f v | Choice2Of2 e -> Choice2Of2 e
    let inline catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> Choice1Of2 v | Choice2Of2 e -> f e

[<RequireQualifiedAccess>]
module internal Implicit = let inline Invoke (x : ^t) = ((^R or ^t) : (static member op_Implicit : ^t -> ^R) x) :^R

module Errors =
    let exnDivByZero      = new System.DivideByZeroException() :> exn
    let exnNoDivision     = new System.Exception "These numbers are not divisible in this domain."
    let exnSqrtOfNegative = new System.Exception "Cannot calculate square root of a negative number"
    let exnNoSqrt         = new System.Exception "No square root defined for this value in this domain."
    let exnNoSubtraction  = new System.Exception "No subtraction defined for these values in this domain."

module Decimal =
    let inline trySqrt x =
        match sign x with
        | -1 -> Choice2Of2 Errors.exnSqrtOfNegative
        |  0 -> Choice1Of2 0.M
        | _  ->
            let rec loop previous =
                let current = (previous + x / previous) / 2.0M
                if previous - current = 0.0M then current else loop current
            x |> float |> sqrt |> decimal |> loop |> Choice1Of2

module Rational =
    let inline numerator   x = (^F: (member Numerator  : 'R) x)
    let inline denominator x = (^F: (member Denominator: 'R) x)

module BigInteger =
    open System.Numerics
    let trySqrtRem x =
        if sign x = -1 then Choice2Of2 Errors.exnSqrtOfNegative
        else
            let rec loop previous =
                let current = (previous + x / previous) >>> 1
                if abs (previous - current) < 2I then current else loop current
            let guess = 10I ** (((int (BigInteger.Log10 (x + 1I))) + 1) >>> 1)
            let r = loop guess
            let r2 = r * r
            match compare r2 x with
            | 0 -> Choice1Of2 (r, 0I)
            | 1 -> let root = r - 1I in Choice1Of2 (r, x - r2)
            | _ -> Choice1Of2 (r, x - r2)


// Dummy types

type Id<'t>(v:'t) =
   let value = v
   member this.getValue = value

[<RequireQualifiedAccess>]
module Id =
    let run   (x:Id<_>) = x.getValue
    let map f (x:Id<_>) = Id (f x.getValue)
    let create x = Id (x)

type Id0(v:string) =
   let value = v
   member this.getValue = value

type Either<'L,'R> = L of 'L | R of 'R

type DmStruct = struct end