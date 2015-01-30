#r @"..\..\build\FsControl.Core.dll"
#r @"..\..\build\FSharpPlus.dll"

open FSharpPlus
open GenericMath

let inline areaOfCircle radio =
    let pi = 
        314159265358979323846264338I 
            </ratio/> 
        100000000000000000000000000I
    fromRational pi * radio * radio

let area1 = areaOfCircle 5.
let area2 = areaOfCircle 5.0f
let area3 = areaOfCircle 5.0M