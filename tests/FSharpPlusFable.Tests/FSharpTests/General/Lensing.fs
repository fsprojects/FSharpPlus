module General.Lensing


open Testing
open FSharpPlus
open FSharpPlus.Data
open System
#if !FABLE_COMPILER || FABLE_COMPILER_3
open FSharpPlus.Lens
type Person = { Name: string; DateOfBirth: DateTime }
module Person =
    let inline _name f { Name = a; DateOfBirth = b } = f a <&> fun a' -> { Name = a'; DateOfBirth = b }
type Book = { Title: string; Author: Person }
module Book =
    let inline _author f { Author = a; Title = b } = f a <&> fun a' -> { Author = a'; Title = b }
    let inline _authorName b = _author << Person._name <| b
let rayuela =
    { Book.Title = "Rayuela"
      Author = { Person.Name = "Julio Cortázar"
                 DateOfBirth = DateTime (1914, 8, 26) } }
#endif
let lensing = testList "Lensing" [
#if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "Lens" (fun () ->
               equal (view Book._authorName rayuela) "Julio Cortázar")
    testCase "Lens view 1, 2" (fun () ->
               equal (view _1 (1, '2')) 1
               equal (view _2 ('1', 2)) 2 )
#endif
#if !FABLE_COMPILER
    testCase "Lens preview _Ok, Error" (fun () -> equal None (preview _Ok (Error 1)))
    testCase "Lens preview _Ok, OK" (fun () -> equal (Some 1) (preview _Ok (Ok 1)))
    testCase "Lens preview _Error, Error" (fun () -> equal (Some 1) (preview _Error (Error 1)))
    testCase "Lens preview _Error, Ok" (fun () -> equal None (preview _Error (Ok 1)))
    testCase "Lens preview _Ok Error" (fun () -> equal None (preview _Ok (Error 1)))
    testCase "Lens preview _Ok Ok" (fun () -> equal (Some 1) (preview _Ok (Ok 1)))
    testCase "Lens preview _Error, Error" (fun () -> equal (Some 1) (preview _Error (Error 1)))
    testCase "Lens preview _Error, Ok" (fun () -> equal None (preview _Error (Ok 1)))
    testCase "prism1" (fun () -> equal true (Option.isNone (preview _Some None)))
    testCase "prism2" (fun () -> equal (Some 1) (preview _Some (Some 1)))
    testCase "prism3" (fun () -> equal (Some ()) (preview _None None))
    testCase "prism4" (fun () -> equal true (Option.isNone (preview _None (Some 1))))
#endif
    ]
