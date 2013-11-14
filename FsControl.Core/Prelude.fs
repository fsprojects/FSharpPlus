namespace FsControl.Core

module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline option n f = function None -> n | Some x -> f x

[<RequireQualifiedAccess>]
module internal List =
    let inline singleton x = [x]
    let inline cons x y = x :: y

[<RequireQualifiedAccess>]
module internal Seq =
    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    let inline intersperse sep list = seq {
        let notFirst = ref false
        for element in list do 
            if !notFirst then yield sep
            yield element
            notFirst := true}

    let inline splitBy keyMapper (source:_ seq) = seq {
        use e = source.GetEnumerator()
        if (e.MoveNext()) then
            let groupKey = ref (keyMapper e.Current)
            let values   = ref (new ResizeArray<_>())
            (!values).Add(e.Current)
            while (e.MoveNext()) do
                let key = keyMapper e.Current
                if !groupKey = key then (!values).Add(e.Current)
                else
                    yield (!groupKey, !values)
                    groupKey := key
                    values   := new ResizeArray<_>()
                    (!values).Add(e.Current)
            yield (!groupKey, !values)}


[<RequireQualifiedAccess>]
module internal Error =
    let inline map f = function Choice1Of2 x -> Choice1Of2(f x) | Choice2Of2 x -> Choice2Of2 x
    let inline result x = Choice1Of2 x
    let inline throw  x = Choice2Of2 x
    let inline bind  (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> f v | Choice2Of2 e -> Choice2Of2 e
    let inline catch (f:'t -> Choice<'v,'e>) = function Choice1Of2 v  -> Choice1Of2 v | Choice2Of2 e -> f e