namespace FSharpPlus

open FsControl

type SeqT<'``monad<seq<'t>>``> = SeqT of '``monad<seq<'t>>``

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline map  (f:'T->'U) (SeqT m : SeqT<'``Monad<seq<'T>``>)                                            = SeqT <| map (Seq.map f: (seq<_>->_)) m      : SeqT<'``Monad<seq<'U>``>
    let inline bind (f:'T-> SeqT<'``Monad<seq<'U>``>) (SeqT m : SeqT<'``Monad<seq<'T>``>)               = SeqT (m >>= (mapM:_->seq<_>->_) (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) 
    let inline apply (SeqT f : SeqT<'``Monad<seq<('T -> 'U)>``>) (SeqT x : SeqT<'``Monad<seq<'T>``>) = SeqT (Map.Invoke (Seq.apply:seq<_->_>->seq<_>->seq<_>) f <*> x)          : SeqT<'``Monad<seq<'U>``>       

type SeqT with
    static member inline Map    (x : SeqT<'``Monad<seq<'T>``>, f : 'T->'U , impl:Map)                                                       =  SeqT.map f x                                                        : SeqT<'``Monad<seq<'U>``>
    static member inline Return (output : SeqT<'``Monad<seq<'T>``>, impl:Return)                                                            = SeqT << result << Seq.singleton                                      : 'T -> SeqT<'``Monad<seq<'T>``>
    static member inline Apply  (f : SeqT<'``Monad<seq<('T -> 'U)>``>, x : SeqT<'``Monad<seq<'T>``>, output:SeqT<'r>, impl:Apply ) = SeqT.apply f x                                                       : SeqT<'``Monad<seq<'U>``>
    static member inline Bind   (x  : SeqT<'``Monad<seq<'T>``>, f: 'T -> SeqT<'``Monad<seq<'U>``>)                                    = SeqT.bind f x

    static member inline MZero (output: SeqT<'``MonadPlus<seq<'T>``>, impl:MZero)                                                           = SeqT <| result Seq.empty                                             : SeqT<'``MonadPlus<seq<'T>``>
    static member inline MPlus (SeqT x, SeqT y, impl:MPlus)                                                                                 = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b)))) : SeqT<'``MonadPlus<seq<'T>``>

    static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad Seq.singleton ) |> SeqT    :  SeqT<'``Monad<seq<'T>>``>
    
    static member inline LiftAsync (x : Async<'T>) = lift (liftAsync x)
    
    static member inline ThrowError (x:'E) = x |> throw |> lift
    static member inline CatchError (m:SeqT<'``MonadError<'E1,'T>``>   , h:'E1 -> SeqT<'``MonadError<'E2,'T>``>)    = SeqT    ((fun v h -> catch v h) (SeqT.run    m) (SeqT.run    << h)) : SeqT<'``MonadError<'E2,'T>``>
    
    static member CallCC (f:(('T -> SeqT<Cont<'R,'U>>   ) -> _)) = SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))) :SeqT<Cont< 'R,  seq<'T>>>
    
    static member get_Get() = lift State.get :   SeqT<State<'S,_>>  
    static member Put (x:'T) = x |> State.put |> lift :   SeqT<_>  
    
    static member get_Ask() = lift Reader.ask :   SeqT<Reader<'R,   seq<'R>>>
    static member Local (  SeqT  (m:Reader<'R2,'T>), f:'R1->'R2) =   SeqT  <| Reader.local f m