namespace FSharpPlus

module Seq =

    let apply f x = Seq.collect (fun f -> Seq.map ((<|) f) x) f

    let foldBack f (s:seq<_>) z = Array.foldBack f (Seq.toArray s) z

    let inline sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
        foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

    let inline mapM f as' = sequence (Seq.map f as')
              
    let groupAdjBy keyMapper (source:_ seq) = seq {
        use e = source.GetEnumerator()
        if (e.MoveNext()) then
            let groupKey = ref (keyMapper e.Current)
            let values   = ref (new ResizeArray<_>())
            (!values).Add(e.Current)
            while (e.MoveNext()) do
                let key = keyMapper e.Current
                if !groupKey = key then (!values).Add(e.Current)
                else
                    yield (!groupKey, !values :> seq<_>)
                    groupKey := key
                    values   := new ResizeArray<_>()
                    (!values).Add(e.Current)
            yield (!groupKey, !values :> seq<_>)}

    // http://codebetter.com/matthewpodwysocki/2009/05/06/functionally-implementing-intersperse/
    let intersperse sep list =
        seq {
            let notFirst = ref false
            for element in list do 
                if !notFirst then yield sep
                yield element
                notFirst := true}

    let replicate count initial = System.Linq.Enumerable.Repeat(initial, count)

    let inline replicateM count (initial:'Monad'T) : 'Monad'Seq'T = sequence (replicate count initial)