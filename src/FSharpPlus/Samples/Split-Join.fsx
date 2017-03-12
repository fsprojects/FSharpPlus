#nowarn "3186"
#r @"..\bin\Release\FSharpPlus.dll"

open FSharpPlus

let text = "This is a sample text, showing how to split and join collections"

let words = String.split [" "; ", "] text
let textWithSpaces = String.intercalate " " words   // [|"This"; "is"; "a"; "sample"; "text"; "showing"; "how"; "to"; "split"; "and"; "join"; "collections"|]

// Rule: intercalate sep >> split (seq [sep]) = id
let areEqual = words = (String.intercalate "sep" >> String.split ["sep"]) words

// The same functions are defined for other collections
let numbers = seq [ [1; 2; 3]; [5; 8]; [13; 21]]
let intercalated = List.intercalate [100;999] numbers       // [1; 2; 3; 100; 999; 5; 8; 100; 999; 13; 21]
let originalNumbers = List.split [[100;999]] intercalated   // seq [[1; 2; 3]; [5; 8]; [13; 21]]

// And they have their corresponding polymorphic version
let words' = split [" "; ", "] text
let textWithSpaces' = intercalate " " words'
let intercalated' = intercalate [100;999] numbers
let originalNumbers' = split [[100;999]] intercalated'