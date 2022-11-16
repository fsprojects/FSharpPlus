namespace FSharpPlus

[<RequireQualifiedAccess>]
module RequiredValidation =
    open System
    open FSharpPlus.Data
    
    let inline validate error f v =
        if f v then
            Success v
        else
            Failure [ error ]

    let inline string error value =
        validate (error value) (String.IsNullOrWhiteSpace >> not) value

    let inline greaterThan error min value =
        validate (error value) (flip (>) min) value
        
    let inline greaterOrEqualThan error min value =
        validate (error value) (flip (>=) min) value

    let inline email error value =
        let check (v: string) =
            try
                let _ = Net.Mail.MailAddress(v)
                true
            with
            | ex -> false

        validate (error value) check value

    let inline guid error value =
        validate (error value) (fun v -> v <> Guid.Empty) value

    let inline object error value =
        let check value = box value <> null
        validate (error value) check

    let inline whenSome value checkWhenSome =
        match value with
        | Some v -> checkWhenSome v |> Validation.map Some
        | _ -> Success None

    let inline arrayValues values check =
        let validated : Validation<_,_> [] =
            values
            |> Array.map check
        validated
        |> sequence
        |> Validation.map Seq.toArray

    let inline listValues values check =
        let validated : List<Validation<_,_>> =
            values
            |> List.map check
        validated
        |> sequence
        |> Validation.map Seq.toArray

    let inline atLeastOne error =
        let check values =
            Seq.isEmpty values |> not

        validate error check
        
    let RequiredValidation.isSuccess =
        function
        | Success _ -> true
        | Failure _ -> false
      
    let isFailure =
        function
        | Success _ -> false
        | Failure _ -> true