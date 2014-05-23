#r @"..\bin\Release\FsControl.Core.dll"

// FsControl does not automatically export any function, just the 'Type Methods'.
// However in the FsControl.Operators module there are some function and operator definitions.
// The main purpose of that module is to make tests easier, without having to reapeat code or rely on another library to make simple tests.

open FsControl.Operators

let inline flip f x y = f y x
let inline konst k _ = k
let inline (</) x = (|>) x
let inline (/>) x = flip x
let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
let inline option n f = function None -> n | Some x -> f x


// Test Functors
let times2,minus3 = (*) 2, (-)/> 3
let resJust1      = map minus3 (Some 4)
let noValue       = map minus3 None
let lstTimes2     = map times2 [1;2;3;4]
let fTimes2minus3 = map minus3 times2
let res39         = fTimes2minus3 21
let res3n4   = result ((+) 2) <*> [1;2]