namespace FSharpPlus

[<RequireQualifiedAccess>]
module Validations =
    open System
    open FSharpPlus.Data
    
    let inline validate errorMessage f v =
        if f v then
            Success v
        else
            Failure [ errorMessage ]

    let inline requireString propName =
        let errorMessage =
            sprintf "%s cannot be null, empty or whitespace." propName

        validate errorMessage (String.IsNullOrWhiteSpace >> not)

    let inline requireGreaterThan propName min =
        let errorMessage =
            sprintf "%s have to be greater or equal to '%d'." propName min

        validate errorMessage (flip (>) min)
        
    let inline requireGreaterOrEqualThan propName min =
        let errorMessage =
            sprintf "%s have to be greater or equal to '%d'." propName min

        validate errorMessage (flip (>=) min)

    let inline requireEmail propName =
        let errorMessage =
            sprintf "%s is not a valid email address." propName

        let check (v: string) =
            try
                let _ = Net.Mail.MailAddress(v)
                true
            with
            | ex -> false

        validate errorMessage check

    let inline requireGuid propName =
        validate (sprintf "%s is required" propName) (fun v -> v <> Guid.Empty)

    let inline requireObject propName =
        let check value = box value <> null
        validate (sprintf "%s is required" propName) check

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

    let inline requireAtLeastOne propName =
        let check values =
            Seq.isEmpty values |> not

        let errorMessage =
            sprintf "%s should have at least one element'." propName

        validate errorMessage check
