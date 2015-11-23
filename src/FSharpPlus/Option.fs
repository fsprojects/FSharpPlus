namespace FSharpPlus

open FsControl

[<RequireQualifiedAccess>]
module internal Option =
    let inline apply f x =
        match (f,x) with 
        | Some f, Some x -> Some (f x) 
        | _              -> None