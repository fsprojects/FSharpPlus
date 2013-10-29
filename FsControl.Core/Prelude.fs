namespace FsControl.Core

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline (</) x = (|>) x
    let inline (/>) x = flip x
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline maybe  n f = function | None -> n | Some x -> f x

    let inline  internal map x = List.map x
    let inline  internal singleton x = [x]
    let inline  internal concat (x:List<List<'a>>) :List<'a> = List.concat x
    let inline  internal cons x y = x :: y


namespace FsControl.Core.Types

[<RequireQualifiedAccess>]
module Error =
    let map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
    let result = Choice1Of2
    let throw  = Choice2Of2
    let bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> f v | Choice2Of2 e -> Choice2Of2 e
    let catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> Choice1Of2 v | Choice2Of2 e -> f e








