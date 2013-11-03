#r @"..\..\packages\FsControl.1.0.3\lib\net40\FsControl.Core.dll"
#load @"..\Operators.fs"
#load @"..\ZipList.fs"

open FSharpPlus

let seq1 = seq { 1..100000}
let seq2 = seq {10..100000}

let seq1_plus_seq2  = (+) <!> ZipList seq1 <*> ZipList seq2
let seq1_plus_seq2' = ZipList seq1 |+| ZipList seq2
let arrCombined     = 10 *| ZipList seq1 |+| ZipList seq2 |- 5
let asMonoid        = result 1 </mappend/> ZipList []

// try ZipList.run the results
let infOnes = ZipList.run asMonoid.[0..10] |> Seq.toList