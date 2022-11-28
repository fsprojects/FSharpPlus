namespace FSharpPlus

/// Additional operations on ValueTuple (,)
[<RequireQualifiedAccess>]
module ValueTuple2 =
    let mapItem1 f (struct (x, y)) = struct (f x, y)
    let mapItem2 f (struct (x, y)) = struct (x, f y)

/// Additional operations on ValueTuple (,,)
[<RequireQualifiedAccess>]
module ValueTuple3 =
    let mapItem1 f (struct (x, y, z)) = struct (f x, y, z)
    let mapItem2 f (struct (x, y, z)) = struct (x, f y, z)
    let mapItem3 f (struct (x, y, z)) = struct (x, y, f z)
