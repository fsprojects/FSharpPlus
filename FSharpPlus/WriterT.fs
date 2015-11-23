namespace FSharpPlus

open FsControl

type WriterT<'``monad<'t * 'monoid>``> = WriterT of '``monad<'t * 'monoid>``

[<RequireQualifiedAccess>]
module WriterT =

    let run (WriterT x) = x : '``Monad<'T * 'Monoid>``

    let inline map (f:'T->'U) (WriterT m:WriterT<'``Monad<'T * 'Monoid>``>) =
        let mapWriter f (a, m) = (f a, m)
        WriterT (Map.Invoke (mapWriter f) m) : WriterT<'``Monad<'U * 'Monoid>``>

    let inline apply (WriterT f : WriterT<'``Monad<('T -> 'U) * 'Monoid>``>) (WriterT x : WriterT<'``Monad<'T * 'Monoid>``>) =
        let applyWriter (a, w) (b, w') = (a b, Append.Invoke w w')
        WriterT (result applyWriter <*> f <*> x) : WriterT<'``Monad<'U * 'Monoid>``>
        
    let inline bind (f:'T->WriterT<'``Monad<'U * 'Monoid>``>) (WriterT (m:'``Monad<'T * 'Monoid>``)) = 
        WriterT (m >>= (fun (a, w) -> run (f a) >>= (fun (b, w') -> result (b, Append.Invoke w w'))))  : WriterT<'``Monad<'U * 'Monoid>``>
    

type WriterT with

    static member inline Map      (x:WriterT<'``Monad<'T * 'Monoid>``>, f:'T->'U, _:Map) = WriterT.map f x
    static member inline Return   (_:WriterT<'``Monad<'T * 'Monoid>``>, _:Return) :'T -> WriterT<'``Monad<'T * 'Monoid>``> = fun a -> WriterT (result (a, Empty.Invoke()))
    static member inline Apply    (f:WriterT<'``Monad<('T -> 'U) * 'Monoid>``>, x: WriterT<'``Monad<'T * 'Monoid>``>, output: WriterT<'``Monad<'U * 'Monoid>``>, impl:Apply ) = WriterT.apply f x : WriterT<'``Monad<'U * 'Monoid>``>
    static member inline Bind     (x:WriterT<'``Monad<'T * 'Monoid>``>, f :'T -> _)  = WriterT.bind f x       : WriterT<'``Monad<'U * 'Monoid>``>

    static member inline MZero (output:WriterT<'``MonadPlus<'T * 'Monoid>``>, impl:MZero) = WriterT (MZero.Invoke()) : WriterT<'``MonadPlus<'T * 'Monoid>``>
    static member inline MPlus (  WriterT m, WriterT n                      , impl:MPlus) = WriterT (m <|> n)        : WriterT<'``MonadPlus<'T * 'Monoid>``>

    static member inline Tell   (w:'Monoid) = WriterT (result ((), w))                                                                                          : WriterT<'``Monad<unit * 'Monoid>``>
    static member inline Listen (WriterT m: WriterT<'``Monad<('T * ('Monoid'T -> 'Monoid)) * 'Monoid>``>) = WriterT (m >>= (fun (a, w) -> result ((a, w), w)))  : WriterT<'``Monad<('T * 'Monoid) * 'Monoid>``>
    static member inline Pass   (WriterT m: WriterT<'``Monad<'T * 'Monoid>``>) = WriterT (m >>= (fun ((a, f), w) -> result (a, f w)))                           : WriterT<'``Monad<'T * 'Monoid>``>

    static member inline Lift (m:'``Monad<'T>``) : WriterT<'``Monad<'T * 'Monoid>``> = WriterT (m >>= (fun a -> result (a, Empty.Invoke())))
    
    static member inline LiftAsync (x: Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)

    static member inline ThrowError (x: 'E) = x |> ThrowError.Invoke |> Lift.Invoke
    static member inline CatchError (m:WriterT<Choice<'T * 'Monoid, 'E2>> , h:'E2 -> _) = 
            WriterT (CatchError.Invoke (WriterT.run m) (WriterT.run << h)) : WriterT<Choice<'T * 'Monoid, 'E2>>

    static member inline CallCC (f : ('a->WriterT<Cont<'r,'t>>)->_)  :WriterT<Cont<'r,'a*'b>> = 
        WriterT (Cont.callCC <| fun c -> WriterT.run (f (fun a -> WriterT <| c (a, Empty.Invoke()))))
       
    static member inline get_Ask()                     = Lift.Invoke Reader.ask               : WriterT<Reader<'R,'R*'Monoid>>
    static member        Local (WriterT m, f:'R1->'R2) = WriterT (Reader.local f m)           : WriterT<Reader<'R1,'T*'Monoid>>

    static member inline get_Get()  = Lift.Invoke State.get                 : WriterT<State<'S,'S*'Monoid>>  
    static member inline Put (x:'S) = x |> State.put |> Lift.Invoke         : WriterT<State<'S,unit*'Monoid>>