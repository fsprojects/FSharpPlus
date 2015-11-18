namespace FsControl

open FsControl.Core.Internals
open FsControl.Core.Internals.Prelude
open FsControl.Core.Internals.MonadOps

type OptionT<'``monad<option<'t>>``> = OptionT of '``monad<option<'t>>``

[<RequireQualifiedAccess>]
module OptionT =
    let run   (OptionT m) = m : '``Monad<option<'T>>``
    let inline map  (f:'T->'U) (OptionT m : OptionT<'``Monad<option<'T>``>)                                            = OptionT (Map.Invoke (Option.map f) m) : OptionT<'``Monad<option<'U>``>
    let inline bind (f:'T-> OptionT<'``Monad<option<'U>``>) (OptionT m : OptionT<'``Monad<option<'T>``>)               = (OptionT <| (m  >>= (fun maybe_value -> match maybe_value with Some value -> run (f value) | _ -> result None)))
    let inline apply (OptionT f : OptionT<'``Monad<option<('T -> 'U)>``>) (OptionT x : OptionT<'``Monad<option<'T>``>) = OptionT (Map.Invoke Option.apply f <*> x)  : OptionT<'``Monad<option<'U>``>

type OptionT with
    static member inline Map    (x : OptionT<'``Monad<option<'T>``>, f : 'T->'U , impl:Map)                                                       = OptionT.map f x                                                                             : OptionT<'``Monad<option<'U>``>
    static member inline Return (output : OptionT<'``Monad<option<'T>``>, impl:Return)                                                            = OptionT << result << Some                                                                   : 'T -> OptionT<'``Monad<option<'T>``>
    static member inline Apply  (f : OptionT<'``Monad<option<('T -> 'U)>``>, x : OptionT<'``Monad<option<'T>``>, output:OptionT<'r>, impl:Apply ) = OptionT.apply f x                                                                           : OptionT<'``Monad<option<'U>``>
    static member inline Bind   (x  : OptionT<'``Monad<option<'T>``>, f: 'T -> OptionT<'``Monad<option<'U>``>)                                    = OptionT.bind f x

    static member inline MZero (output: OptionT<'``MonadPlus<option<'T>``>, impl:MZero)                                                           = OptionT <| result None                                                                      : OptionT<'``MonadPlus<option<'T>``>
    static member inline MPlus (OptionT x, OptionT y, impl:MPlus)                                                                                 = OptionT <| (x  >>= (fun maybe_value -> match maybe_value with Some value -> x | _ -> y))    : OptionT<'``MonadPlus<option<'T>``>


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
    static member inline Map    (x : ListT<'``Monad<list<'T>``>, f : 'T->'U , impl:Map)                                                       = ListT.map f x                                                   : ListT<'``Monad<list<'U>``>
    static member inline Return (output : ListT<'``Monad<list<'T>``>, impl:Return)                                                            = ListT << result << List.singleton                              : 'T -> ListT<'``Monad<list<'T>``>
    static member inline Apply  (f : ListT<'``Monad<list<('T -> 'U)>``>, x : ListT<'``Monad<list<'T>``>, output:ListT<'r>, impl:Apply ) = ListT.apply f x                                                 : ListT<'``Monad<list<'U>``>
    static member inline Bind   (x  : ListT<'``Monad<list<'T>``>, f: 'T -> ListT<'``Monad<list<'U>``>)                                    = ListT.bind f x

    static member inline MZero (output: ListT<'``MonadPlus<list<'T>``>, impl:MZero)                                                           = ListT <| result []                                              : ListT<'``MonadPlus<list<'T>``>
    static member inline MPlus (ListT x, ListT y, impl:MPlus)                                                                                 = ListT <| (x >>= (fun a -> y >>= (fun b ->  result (a @ b ))))   : ListT<'``MonadPlus<list<'T>``>


type SeqT<'``monad<seq<'t>>``> = SeqT of '``monad<seq<'t>>``

[<RequireQualifiedAccess>]
module SeqT =
    let run (SeqT m) = m

    let inline internal sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun (xs:seq<'a>) -> (result :seq<'a> -> 'M) (seq {yield x; yield! xs})
        Seq.foldBack k ms ((result :seq<'a> -> 'M) Seq.empty)

    let inline internal mapM f as' = sequence (Seq.map f as')

    let inline map  (f:'T->'U) (SeqT m : SeqT<'``Monad<seq<'T>``>)                                            = SeqT <| Map.Invoke (Seq.map f: (seq<_>->_)) m      : SeqT<'``Monad<seq<'U>``>
    let inline bind (f:'T-> SeqT<'``Monad<seq<'U>``>) (SeqT m : SeqT<'``Monad<seq<'T>``>)               = SeqT (m >>= (mapM:_->seq<_>->_) (run << f) >>= ((Seq.concat:seq<seq<_>>->_) >> result)) 
    let inline apply (SeqT f : SeqT<'``Monad<seq<('T -> 'U)>``>) (SeqT x : SeqT<'``Monad<seq<'T>``>) = SeqT (Map.Invoke (Seq.apply:seq<_->_>->seq<_>->seq<_>) f <*> x)          : SeqT<'``Monad<seq<'U>``>       

type SeqT with
    static member inline Map    (x : SeqT<'``Monad<seq<'T>``>, f : 'T->'U , impl:Map)                                                       =  SeqT.map f x                                                        : SeqT<'``Monad<seq<'U>``>
    static member inline Return (output : SeqT<'``Monad<seq<'T>``>, impl:Return)                                                            = SeqT << result << Seq.singleton                                      : 'T -> SeqT<'``Monad<seq<'T>``>
    static member inline Apply  (f : SeqT<'``Monad<seq<('T -> 'U)>``>, x : SeqT<'``Monad<seq<'T>``>, output:SeqT<'r>, impl:Apply ) = SeqT.apply f x                                                       : SeqT<'``Monad<seq<'U>``>
    static member inline Bind   (x  : SeqT<'``Monad<seq<'T>``>, f: 'T -> SeqT<'``Monad<seq<'U>``>)                                    = SeqT.bind f x

    static member inline MZero (output: SeqT<'``MonadPlus<seq<'T>``>, impl:MZero)                                                           = SeqT <| result Seq.empty                                             : SeqT<'``MonadPlus<seq<'T>``>
    static member inline MPlus (SeqT x, SeqT y, impl:MPlus)                                                                                 = SeqT <| (x >>= (fun a -> y >>= (fun b ->  result ((Seq.append:seq<_>->seq<_>->_) a b)))) : SeqT<'``MonadPlus<seq<'T>``>




// MonadTrans

type OptionT with static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad Some)           |> OptionT : OptionT<'``Monad<option<'T>>``>
type ListT   with static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad List.singleton) |> ListT   :  ListT<'``Monad<list<'T>>``> 
type SeqT    with static member inline Lift (x:'``Monad<'T>``) = x |> (Map.FromMonad Seq.singleton ) |> SeqT    :  SeqT<'``Monad<seq<'T>>``>

type Lift =
    static member inline Invoke (x:'``Monad<'T>``) = (^R : (static member Lift: _ -> ^R) x)


// MonadAsync

type LiftAsync =

    static member inline Invoke (x:Async<'T>) :'``MonadAsync<'T>`` =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member LiftAsync: _ -> _) b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<LiftAsync> x

    static member inline LiftAsync (_:'R         ) = fun (x :Async<'T>) -> (^R : (static member LiftAsync: _ -> ^R) x)
    static member inline LiftAsync (_:^t when ^t: null and ^t: struct) = ()
    static member        LiftAsync (_:Async<'T>  ) = fun (x :Async<'T>) -> x

type OptionT with static member inline LiftAsync (x : Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)
type ListT   with static member inline LiftAsync (x : Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)
type SeqT    with static member inline LiftAsync (x : Async<'T>) = Lift.Invoke (LiftAsync.Invoke x)


type ThrowError =

    static member inline Invoke (x:'e) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ThrowError: _ -> _) b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Unchecked.defaultof<ThrowError> x

    static member inline ThrowError (_:OptionT<'``MonadError<'E,'T>``>) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:ListT<'``MonadError<'E,'T>``>  ) = Lift.Invoke << ThrowError.Invoke
    static member inline ThrowError (_:SeqT<'``MonadError<'E,'T>``>   ) = Lift.Invoke << ThrowError.Invoke
    static member        ThrowError (_:Choice<'T,'E>) = Error.throw


type CatchError =
    static member inline Invoke (v:'ma) (h:'e->'mb) :'mb =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member CatchError: _*_ -> _) b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Unchecked.defaultof<CatchError>, v) h

    static member inline CatchError (m:OptionT<'``MonadError<'E1,'T>``>, output:OptionT<'``MonadError<'E2,'T>``>) = fun (h:'E1 -> OptionT<'``MonadError<'E2,'T>``>) -> OptionT ((fun v h -> CatchError.Invoke v h) (OptionT.run m) (OptionT.run << h)) : OptionT<'``MonadError<'E2,'T>``>
    static member inline CatchError (m:ListT<'``MonadError<'E1,'T>``>  , output:ListT<'``MonadError<'E2,'T>``>  ) = fun (h:'E1 -> ListT<'``MonadError<'E2,'T>``>)   -> ListT   ((fun v h -> CatchError.Invoke v h) (ListT.run   m) (ListT.run   << h)) : ListT<'``MonadError<'E2,'T>``>
    static member inline CatchError (m:SeqT<'``MonadError<'E1,'T>``>   , output:SeqT<'``MonadError<'E2,'T>``>   ) = fun (h:'E1 -> SeqT<'``MonadError<'E2,'T>``>)    -> SeqT    ((fun v h -> CatchError.Invoke v h) (SeqT.run    m) (SeqT.run    << h)) : SeqT<'``MonadError<'E2,'T>``>
    static member        CatchError (m:Choice<'T,'E1>, output:Choice<'T,'E2>) = fun (h:'E1 -> Choice<'T,'E2>) -> Error.catch h m
    static member        CatchError (m:'E1 * 'T      , output:'E2 * 'T      ) = fun (h:'E1 -> 'E2 * 'T) -> h (fst m)



// MonadCont =
type OptionT with static member CallCC (f:(('T -> OptionT<Cont<'R,'U>>) -> _)) = OptionT(Cont.callCC <| fun c -> OptionT.run(f (OptionT << c << Some)))      :OptionT<Cont<'R,option<'T>>>
type ListT   with static member CallCC (f:(('T -> ListT<Cont<'R,'U>>  ) -> _)) = ListT  (Cont.callCC <| fun c ->   ListT.run(f (ListT << c << List.singleton))) :ListT<Cont<'R, list<'T>>>
type SeqT    with static member CallCC (f:(('T -> SeqT<Cont<'R,'U>>   ) -> _)) = SeqT   (Cont.callCC <| fun c ->   SeqT.run (f (SeqT  << c << Seq.singleton ))) :SeqT<Cont< 'R,  seq<'T>>>

type CallCC =
    static member inline Invoke f = (^R : (static member CallCC: _ -> ^R) f)


// MonadState =
type OptionT with static member get_Get() = Lift.Invoke State.get :OptionT<State<'S,_>>
type ListT   with static member get_Get() = Lift.Invoke State.get :  ListT<State<'S,_>>  
type SeqT    with static member get_Get() = Lift.Invoke State.get :   SeqT<State<'S,_>>  

type Get =
    static member inline Invoke() :^R = (^R : (static member Get: ^R) ())


type OptionT with static member Put (x:'T) = x |> State.put |> Lift.Invoke :OptionT<_>
type ListT   with static member Put (x:'T) = x |> State.put |> Lift.Invoke :  ListT<_>  
type SeqT    with static member Put (x:'T) = x |> State.put |> Lift.Invoke :   SeqT<_>  
   
type Put =
    static member inline Invoke (x:'s) :^R = (^R : (static member Put: _ -> ^R) x)


// MonadReader =
type OptionT with static member get_Ask() = Lift.Invoke Reader.ask :OptionT<Reader<'R,option<'R>>>
type ListT   with static member get_Ask() = Lift.Invoke Reader.ask :  ListT<Reader<'R,  list<'R>>>
type SeqT    with static member get_Ask() = Lift.Invoke Reader.ask :   SeqT<Reader<'R,   seq<'R>>>

type Ask =
    static member inline Invoke() :^R = (^R : (static member Ask: ^R) ())


type OptionT with static member Local (OptionT (m:Reader<'R2,'T>), f:'R1->'R2) = OptionT <| Reader.local f m
type ListT   with static member Local ( ListT  (m:Reader<'R2,'T>), f:'R1->'R2) =  ListT  <| Reader.local f m
type SeqT    with static member Local (  SeqT  (m:Reader<'R2,'T>), f:'R1->'R2) =   SeqT  <| Reader.local f m   

type Local =
    static member inline Invoke (f:'R1->'R2) (m:^R) :^R = (^R : (static member Local: _*_ -> ^R) m, f)


// MonadWriter =
type OptionT with static member inline Tell (w:'Monoid) = w |> Writer.tell |> Lift.Invoke :OptionT<_>
    
type Tell =
    static member inline Invoke (w:'Monoid) :^R = (^R : (static member Tell: _ -> ^R) w)


type OptionT with
    static member inline Listen (m : OptionT<Writer<'Monoid, option<'T>>>) =
        let liftMaybe (m, w) = Option.map (fun x -> (x, w)) m
        OptionT (Writer.listen (OptionT.run m) >>= (result << liftMaybe))

type Listen =
    static member inline Invoke (m:'ma) :^R = (^R : (static member Listen: _ -> ^R) m)


type OptionT with static member inline Pass m : OptionT<Writer<'Monoid, option<'T>>> = 
                    OptionT (OptionT.run m >>= option (result None) (Map.Invoke Some << Writer.pass << result))

type Pass =
    static member inline Invoke (m:'maww) :^R = (^R : (static member Pass: _ -> ^R) m)