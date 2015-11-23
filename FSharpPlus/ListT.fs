namespace FSharpPlus

open FsControl

type ListT<'``monad<list<'t>>``> = ListT of '``monad<list<'t>>``

[<RequireQualifiedAccess>]
module ListT =
    let run (ListT m) = m : '``Monad<list<'T>>``

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (result :list<'a> -> 'M) (x::xs)
        List.foldBack k ms ((result :list<'a> -> 'M) [])
    
    let inline internal mapM f as' = sequence (List.map f as')

    let inline map  (f:'T->'U) (ListT m: ListT<'``Monad<list<'T>``>) =  ListT (Map.Invoke (List.map f) m) : ListT<'``Monad<list<'U>``>
    let inline bind (f:'T-> ListT<'``Monad<list<'U>``>) (ListT m : ListT<'``Monad<list<'T>``>) = (ListT (m >>= mapM (run << f) >>= ((List.concat:list<_>->_) >> result)))
    let inline apply  (ListT f : ListT<'``Monad<list<('T -> 'U)>``>) (ListT x : ListT<'``Monad<list<'T>``>) = ListT (Map.Invoke List.apply f <*> x)  : ListT<'``Monad<list<'U>``>

type ListT with
    static member inline Map    (x : ListT<'``Monad<list<'T>``>, f : 'T->'U , impl:Map)                                                       = ListT.map f x                                         : ListT<'``Monad<list<'U>``>
    static member inline Return (output : ListT<'``Monad<list<'T>``>, impl:Return)                                                            = ListT << result << List.singleton                     : 'T -> ListT<'``Monad<list<'T>``>
    static member inline Apply  (f : ListT<'``Monad<list<('T -> 'U)>``>, x : ListT<'``Monad<list<'T>``>, output:ListT<'r>, impl:Apply ) = ListT.apply f x                                             : ListT<'``Monad<list<'U>``>
    static member inline Bind   (x  : ListT<'``Monad<list<'T>``>, f: 'T -> ListT<'``Monad<list<'U>``>)                                    = ListT.bind f x

    static member inline MZero (output: ListT<'``MonadPlus<list<'T>``>, impl:MZero)                                                           = ListT <| result []                                    : ListT<'``MonadPlus<list<'T>``>
    static member inline MPlus (ListT x, ListT y, impl:MPlus) = ListT <| (x >>= (fun a -> y >>= (fun b ->  result (a @ b ))))   : ListT<'``MonadPlus<list<'T>``>
    static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad List.singleton) |> ListT   :  ListT<'``Monad<list<'T>>``> 
    
    static member inline LiftAsync (x : Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)
    
    static member inline ThrowError (x:'E) = x |> ThrowError.Invoke |> Lift.Invoke
    static member inline CatchError (m:ListT<'``MonadError<'E1,'T>``>  , h:'E1 -> ListT<'``MonadError<'E2,'T>``>)   = ListT   ((fun v h -> CatchError.Invoke v h) (ListT.run   m) (ListT.run   << h)) : ListT<'``MonadError<'E2,'T>``>
    
    static member CallCC (f:(('T -> ListT<Cont<'R,'U>>  ) -> _)) = ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))) :ListT<Cont<'R, list<'T>>>
    
    static member get_Get() = Lift.Invoke State.get :  ListT<State<'S,_>>  
    static member Put (x:'T) = x |> State.put |> Lift.Invoke :  ListT<_>  
    
    static member get_Ask() = Lift.Invoke Reader.ask :  ListT<Reader<'R,  list<'R>>>
    static member Local ( ListT  (m:Reader<'R2,'T>), f:'R1->'R2) =  ListT  <| Reader.local f m