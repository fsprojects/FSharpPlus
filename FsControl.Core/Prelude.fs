namespace FsControl.Core

open System.Collections.Generic

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline option n f = function None -> n | Some x -> f x

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

[<RequireQualifiedAccess>]
module internal Seq =
    let inline bind (f:'a->seq<'b>) x = Seq.collect f x
    let inline apply f x = bind (fun f -> Seq.map ((<|) f) x) f

    let inline groupAdjBy projection (source : _ seq) = seq {
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
            yield (!g, !members)
    }

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

namespace FsControl.Core.Types
type Id<'t>(v:'t) =
   let value = v
   member this.getValue = value

[<RequireQualifiedAccess>]
module Id =
    let run   (x:Id<_>) = x.getValue
    let map f (x:Id<_>) = Id (f x.getValue)
    let create x = Id (x)