namespace FSharpPlus

module Seq =

    let apply f x = Seq.collect (fun f -> Seq.map ((<|) f) x) f

    let foldBack f (s:seq<_>) z = Array.foldBack f (Seq.toArray s) z

    let inline sequence (ms:seq<'``Applicative<'T>``>) : '``Applicative<seq<'T>>`` = sequenceA ms

    let inline traverse (f:'T->'``Applicative<'U>``) (xs:seq<'T>) :'``Applicative<seq<'U>>`` = traverse f xs
              
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

    let inline replicateM count (initial:'``Applicative<'T>``) = sequence (replicate count initial)

open FsControl

type SeqT<'``monad<seq<'t>>``> = SeqT of '``monad<seq<'t>>``

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline bind (f:'T-> SeqT<'``Monad<seq<'U>``>) (SeqT m : SeqT<'``Monad<seq<'T>``>)            = SeqT (m >>= (mapM:_->seq<_>->_) (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) 
    let inline apply (SeqT f : SeqT<'``Monad<seq<('T -> 'U)>``>) (SeqT x : SeqT<'``Monad<seq<'T>``>) = SeqT (map (Seq.apply:seq<_->_>->seq<_>->seq<_>) f <*> x)          : SeqT<'``Monad<seq<'U>``>       
    let inline map  (f:'T->'U) (SeqT m : SeqT<'``Monad<seq<'T>``>)                                   = SeqT <| map (Seq.map f: (seq<_>->_)) m      : SeqT<'``Monad<seq<'U>``>

type SeqT with
    static member inline Map    (x : SeqT<'``Monad<seq<'T>``>, f : 'T->'U , impl:Map)                                              = SeqT.map f x                      : SeqT<'``Monad<seq<'U>``>
    static member inline Return (output : SeqT<'``Monad<seq<'T>``>, impl:Return)                                                   = SeqT << result << Seq.singleton   : 'T -> SeqT<'``Monad<seq<'T>``>
    static member inline Apply  (f : SeqT<'``Monad<seq<('T -> 'U)>``>, x : SeqT<'``Monad<seq<'T>``>, output:SeqT<'r>, impl:Apply ) = SeqT.apply f x                    : SeqT<'``Monad<seq<'U>``>
    static member inline Bind   (x : SeqT<'``Monad<seq<'T>``>, f: 'T -> SeqT<'``Monad<seq<'U>``>)                                  = SeqT.bind f x

    static member inline MZero (output: SeqT<'``MonadPlus<seq<'T>``>, impl:MZero) = SeqT <| result Seq.empty                                                                 : SeqT<'``MonadPlus<seq<'T>``>
    static member inline MPlus (SeqT x, SeqT y, impl:MPlus)                       = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b)))) : SeqT<'``MonadPlus<seq<'T>``>

    static member inline Lift (x:'``Monad<'T>``) = x |> map Seq.singleton |> SeqT : SeqT<'``Monad<seq<'T>>``>
    
    static member inline LiftAsync (x : Async<'T>) = lift (liftAsync x) : '``SeqT<'MonadAsync<'T>>``
    
    static member inline Throw (x:'E) = x |> throw |> lift
    static member inline Catch (m:SeqT<'``MonadError<'E1,'T>``>, h:'E1 -> SeqT<'``MonadError<'E2,'T>``>) = SeqT ((fun v h -> catch v h) (SeqT.run m) (SeqT.run << h)) : SeqT<'``MonadError<'E2,'T>``>
    
    static member inline CallCC (f:(('T -> SeqT<'``MonadCont<'R,seq<'U>>``>) -> _)) = SeqT (callCC <| fun c -> SeqT.run (f (SeqT  << c << Seq.singleton ))) : SeqT<'``MonadCont<'R, seq<'T>>``>
    
    static member inline get_Get()  = lift get                                          : '``SeqT<'MonadState<'S,'S>>``
    static member inline Put (x:'T) = x |> put |> lift                                  : '``SeqT<'MonadState<unit,'S>>``
    
    static member inline get_Ask() = lift ask                                           : '``SeqT<'MonadReader<'R,seq<'R>>>``
    static member inline Local (SeqT (m:'``MonadReader<'R2,'T>``), f:'R1->'R2) = SeqT (local f m)