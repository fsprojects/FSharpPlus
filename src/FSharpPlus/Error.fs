namespace FSharpPlus

open System

[<RequireQualifiedAccess>]
module Error =
    let map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
    let apply f x =
        match (f,x) with
        | (Choice1Of2 a, Choice1Of2 b) -> Choice1Of2 (a b)
        | (Choice2Of2 a, _)            -> Choice2Of2 a
        | (_, Choice2Of2 b)            -> Choice2Of2 b :Choice<'b,'e>
    let inline result x = Choice1Of2 x
    let inline throw  x = Choice2Of2 x
    let inline bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v -> f v | Choice2Of2 e -> Choice2Of2 e
    let inline catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v -> Choice1Of2 v | Choice2Of2 e -> f e

/// Choice<'TSuccess,'TFailure> specialized in 'TFailure = Exception 
[<Runtime.CompilerServices.Extension>]
module ResultOrException =
    [<Runtime.CompilerServices.Extension>]
    let IsResult  :Choice<_,exn>   -> _ = function Choice1Of2 _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let IsException :Choice<_,exn> -> _ = function Choice2Of2 _ -> true | _ -> false

    [<Runtime.CompilerServices.Extension>]
    let Result :Choice<_,exn>      -> _ = function Choice1Of2 v -> v | Choice2Of2 e -> raise e

    [<Runtime.CompilerServices.Extension>]
    let Exception :Choice<_,exn>   -> _ = function Choice2Of2 e -> e | _ -> new Exception()