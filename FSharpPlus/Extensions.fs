namespace FSharpPlus

open System
open FSharpPlus.Operators

module Extensions =

    type Collections.Generic.IEnumerable<'T>  with
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   -> this |> Seq.skip a
            | None  , Some b -> this |> Seq.take b
            | Some a, Some b -> this |> Seq.skip a |> Seq.take (b-a+1)

    [<AutoOpen>]
    module Seq =
        
        let groupAdjBy keyMapper (source:_ seq) = seq {
            use e = source.GetEnumerator()
            if (e.MoveNext()) then
                let groupKey = ref (keyMapper e.Current)
                let values   = ref (new ResizeArray<_>())
                (!values).Add(e.Current)
                while (e.MoveNext()) do
                    let key = keyMapper e.Current
                    if !groupKey = key then (!values).Add(e.Current)
                    else
                        yield (!groupKey, !values :> seq<_>)
                        groupKey := key
                        values   := new ResizeArray<_>()
                        (!values).Add(e.Current)
                yield (!groupKey, !values :> seq<_>)}

        // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
        let intersperse sep list =
            seq {
                let notFirst = ref false
                for element in list do 
                    if !notFirst then yield sep
                    yield element
                    notFirst := true}

    type List<'T> with
        static member singleton x = [x]
        member this.GetSlice = function
            | None  , None   -> this
            | Some a, None   when a < 0 -> this |> skip (this.Length + a)
            | Some a, None              -> this |> skip                a 
            | None  , Some b when b < 0 -> this |> take (this.Length + b)
            | None  , Some b            -> this |> take                b
            | Some a, Some b when a >= 0 && b >= 0 -> this |> skip a |> take b
            | Some a, Some b -> 
                let l = this.Length
                let f i = if i < 0 then l + i else i
                let a = f a
                this |> skip a |> take (f b - a + 1)


    [<RequireQualifiedAccess>]
    module Error =
        let inline map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
        let inline result x = Choice1Of2 x
        let inline throw  x = Choice2Of2 x
        let inline bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> f v | Choice2Of2 e -> Choice2Of2 e
        let inline catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> Choice1Of2 v | Choice2Of2 e -> f e

    [<Runtime.CompilerServices.Extension>]
    module ValueOrException =
        [<Runtime.CompilerServices.Extension>]
        let IsValue     :Choice<_,exn> -> _ = function Choice1Of2 _ -> true | _ -> false

        [<Runtime.CompilerServices.Extension>]
        let IsException :Choice<_,exn> -> _ = function Choice2Of2 _ -> true | _ -> false

        [<Runtime.CompilerServices.Extension>]
        let Value :Choice<_,exn>       -> _ = function Choice1Of2 v -> v | Choice2Of2 e -> raise e

        [<Runtime.CompilerServices.Extension>]
        let Exception :Choice<_,exn>   -> _ = function Choice2Of2 e -> e | _ -> new Exception()
