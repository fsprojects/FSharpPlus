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
            | Some a, None   -> this |> skip a
            | None  , Some b -> this |> take b
            | Some a, Some b -> this |> skip a |> take (b-a+1)