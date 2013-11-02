namespace FSharpPlus

open FSharpPlus.Operators

module Extensions =

    type List<'T> with
        static member singleton x = [x]
        member this.GetSlice = function
            | None  , None -> this
            | Some a, None -> skip a this
            | None  , Some b -> take b this
            | Some a, Some b -> skip a this |> take (b-a+1)