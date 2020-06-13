namespace FSharpPlus

/// Additional operations on Tuple (,)
[<RequireQualifiedAccess>]
module Tuple2 =
    let mapItem1 f (x, y) = (f x, y)
    let mapItem2 f (x, y) = (x, f y)

/// Additional operations on Tuple (,,)
[<RequireQualifiedAccess>]
module Tuple3 =
    let mapItem1 f (x, y, z) = (f x, y, z)
    let mapItem2 f (x, y, z) = (x, f y, z)
    let mapItem3 f (x, y, z) = (x, y, f z)
