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


// test lazy
let v5: Lazy<_> = lazy (printfn "5 !!! "; 5)
let fPlus10 x   = lazy (printfn "add 10 !!! ";x + 10)
let v5plus10    = v5 >>= fPlus10
let v15 = v5plus10.Force()

let v4ll: Lazy<_> = lazy (printfn "outer !!! ";lazy (printfn "inner !!! "; 4))
let v4l = join v4ll
let v4  = v4l.Force()


// test numbers
let inline findMin (lst: 'a list) =
    let minValue, maxValue = minValue(), maxValue()
    let rec loop acc = function
        | [] -> acc
        | x::_ when x = minValue -> x
        | x::xs -> loop (if x < acc then x else acc) xs
    loop maxValue lst
    
let minInt  = findMin [1;0;12;2]
let minUInt = findMin [1u;0u;12u;2u]  // loops only twice


open FsControl.Core.Types

// Test functor and applicatives for ReaderT

let readerTf = ReaderT (fun x -> [(+)x])
let readerTx = ReaderT (fun x -> [x;2*x])
let readerTr = ReaderT.run (readerTf <*> readerTx) 7
let readerTm = ReaderT.run (map ((+) 100) readerTx) 7

(* Why these lines work (after 30 secs) ONLY if run line by line?
let readerTf' = ReaderT (fun x -> Cont (fun k -> k ((+)x))) 
let readerTx' = ReaderT (fun x -> Cont (fun k -> k (2*x)))
let readerTm' = Cont.run (ReaderT.run (map ((+) 100) readerTx') 7) id
let readerTr' = Cont.run (ReaderT.run (readerTf' <*> readerTx') 7) id
*)

// Test functor and applicatives for WriterT

let writerTf = WriterT [(+) 5, "Function"]
let writerTx = WriterT [10   , "Argument"]
let writerTr = writerTf <*> writerTx
let writerTm = map ((+) 100) writerTx

// Test functor and applicatives for StateT

let stateTf = StateT (fun s -> [(+)10, s; (-) 10, s])
let stateTx = StateT (fun s -> [4, s; 3, s])
let stateTr = StateT.run (stateTf <*> stateTx) "state"
let stateTm = StateT.run (map ((+) 100) stateTx) "state"

let d = System.Collections.Generic.Dictionary()
d.Add("first", Choice1Of2 1)
d.Add("second", Choice2Of2 "Error")
let errorT4x6xN = map ((+) 2) (ErrorT d)