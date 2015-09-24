#nowarn "3186"
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
let res3n4        = result ((+) 2) <*> [1;2]



// test numbers

let qr0  = divRem 7  3  //val qr0 : int * int = (2, 1)
let qr1  = divRem 7I 3I //val qr1 : System.Numerics.BigInteger * System.Numerics.BigInteger = (2, 1)
let qr2  = divRem 7. 3. //val qr2 : float * float = (2.333333333, 0.0) -> using default method.

let inline findMin (lst: 'a list) =
    let minValue, maxValue = minValue(), maxValue()
    let rec loop acc = function
        | [] -> acc
        | x::_ when x = minValue -> x
        | x::xs -> loop (if x < acc then x else acc) xs
    loop maxValue lst
    
let minInt  = findMin [1;0;12;2]
let minUInt = findMin [1u;0u;12u;2u]  // loops only twice




// Test Extension Methods
open FsControl.Core.TypeMethods
let lst11n21n12n22  = [1;2]  >>=  (fun x1 -> [10;20]  >>=  (fun x2 -> result((+) x1 x2 )))
let lst11n21n12n22' = [1;2] .Bind (fun x1 -> [10;20] .Bind (fun x2 -> result((+) x1 x2 )))

let lst5  = item5 (true, 2, "3", '4', [5], 6.0)
let lst5' = (true, 2, "3", '4', [5], 6.0).Item5()

let str3  = mapItem3 string (true, ['s'], 3)
let str3' = (true, ['s'], 3).MapItem3(string)

let mapp1 = [1..3] </mappend/> [4..8]
let mapp2 = [1..3]  .Mappend   [4..8]
let mcon1 = [|[|1..3|];[|4..5|]|] |> join
let mcon2 = [|[|1..3|];[|4..5|]|] .Join(null, Join.Instance) // optional arguments don't work from F#
// but in C# you can write (new[] {new[] {1, 2, 3}, new[] {4, 5, 6}}).Join();


// Test functor and applicatives for ReaderT
open FsControl.Core.Types

let readerTf = ReaderT (fun x -> [(+)x])
let readerTx = ReaderT (fun x -> [x;2*x])
let readerTr = ReaderT.run (readerTf <*> readerTx) 7
let readerTm = ReaderT.run (map ((+) 100) readerTx) 7

let readerTf' = ReaderT (fun x -> Cont (fun k -> k ((+)x))) 
let readerTx' = ReaderT (fun x -> Cont (fun k -> k (2*x)))
let readerTm' = Cont.run (ReaderT.run (map ((+) 100) readerTx') 7) id
(* takes minutes to compile
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