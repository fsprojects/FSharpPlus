namespace FSharpPlus

/// Additional operations on ValueTuple (,)
[<RequireQualifiedAccess>]
module ValueTuple2 =
    let mapItem1 f (x, y) = (f x, y)
    let mapItem2 f (x, y) = (x, f y)

/// Additional operations on Tuple (,,)
[<RequireQualifiedAccess>]
module ValueTuple3 =
    let mapItem1 f (x, y, z) = (f x, y, z)
    let mapItem2 f (x, y, z) = (x, f y, z)
    let mapItem3 f (x, y, z) = (x, y, f z)
