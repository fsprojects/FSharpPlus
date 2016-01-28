#nowarn "3186"
#r @"..\..\build\FsControl.dll"
#r @"..\..\build\FSharpPlus.dll"

open System
open FsControl
open FSharpPlus

let getLine    = async { return System.Console.ReadLine()}
let putStrLn x = async { printfn "%s" x}
let isValid s = 
    String.length s >= 8 
        && String.exists System.Char.IsLetter s 
        && String.exists System.Char.IsNumber s 
        && String.exists Char.IsPunctuation s

let decodeError = function
    | -1 -> "Password not valid"
    | _  -> "Unknown"

let getValidPassword : ErrorT<_> =
    monad {
        let! s = lift getLine
        if isValid s then return s
        else return! throw -1}
    </catch/>
        (fun s -> throw ("The error was: " + decodeError s))
    
let askPassword = monad {
    do! lift <| putStrLn "Insert your new password:"
    let! value = getValidPassword
    do! lift <| putStrLn "Storing in database..."
    return value}

//try -> Async.RunSynchronously (ErrorT.run askPassword)