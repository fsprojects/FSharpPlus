namespace FSharpPlus

[<RequireQualifiedAccess>]
module Validations =
    open System
    open FSharpPlus.Data
    
    let inline validate error f v =
        if f v then
            Success v
        else
            Failure [ error ]

    let inline requireString error =
        validate error (String.IsNullOrWhiteSpace >> not)

    let inline requireGreaterThan error min =
        validate error (flip (>) min)
        
    let inline requireGreaterOrEqualThan error min =
        validate error (flip (>=) min)

    let inline requireEmail error =
        let check (v: string) =
            try
                let _ = Net.Mail.MailAddress(v)
                true
            with
            | ex -> false

        validate error check

    let inline requireGuid error =
        validate error (fun v -> v <> Guid.Empty)

    let inline requireObject error =
        let check value = box value <> null
        validate error check

    let inline requireWhenSome value checkWhenSome =
        match value with
        | Some v -> checkWhenSome v |> Validation.map Some
        | _ -> Success None

    let inline requireArrayValues values check =
        let validated : Validation<_,_> [] =
            values
            |> Array.map check
        validated
        |> sequence
        |> Validation.map Seq.toArray

    let inline requireListValues values check =
        let validated : List<Validation<_,_>> =
            values
            |> List.map check
        validated
        |> sequence
        |> Validation.map Seq.toArray

    let inline requireAtLeastOne error =
        let check values =
            Seq.isEmpty values |> not

        validate error check