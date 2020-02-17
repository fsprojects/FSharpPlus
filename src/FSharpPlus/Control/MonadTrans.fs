namespace FSharpPlus.Control

open FSharpPlus.Internals
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Internals.MonadOps

// MonadTrans

type Lift = static member inline Invoke (x: '``Monad<'T>``) = (^``MonadTrans<'Monad<'T>>`` : (static member Lift : _ -> ^``MonadTrans<'Monad<'T>>``) x)


// MonadAsync

type LiftAsync =
    static member inline Invoke (x: Async<'T>) : '``MonadAsync<'T>`` =
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member LiftAsync : _ -> _) b)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<LiftAsync> x

    static member inline LiftAsync (_: 'R) = fun (x: Async<'T>) -> (^R : (static member LiftAsync : _ -> ^R) x)
    #if !FABLE_COMPILER
    static member inline LiftAsync (_: ^t when ^t: null and ^t: struct) = ()
    #endif
    static member        LiftAsync (_: Async<'T>) = fun (x: Async<'T>) -> x


// MonadError

type Throw =
    static member inline Invoke (x: 'E) : '``'MonadError<'E,'T>`` =
        let inline call_2 (_: ^a, b: ^R, x) = ((^a or ^R) : (static member Throw : _*_->'R) (b, x))
        let inline call (a: 'a, x: 'x) = call_2 (a, Unchecked.defaultof<'r>, x) : 'r
        call (Unchecked.defaultof<Throw>, x)

    #if !FABLE_COMPILER
    static member inline Throw (_: 'R, x: 'E) = (^R : (static member Throw : _ -> ^R) x)
    static member inline Throw (_: ^t when ^t: null and ^t: struct, _) = id
    #endif
    static member        Throw (_: Result<'T,'E>, x: 'E) = Error x     : Result<'T,'E>
    static member        Throw (_: Choice<'T,'E>, x: 'E) = Choice2Of2 x: Choice<'T,'E>

type Catch =
    static member        Catch (x: Result<'a,'e1>, k: 'e1->Result<'a,'e2>) = Result.bindError k x
    static member        Catch (x: Choice<'a,'e1>, k: 'e1->Choice<'a,'e2>) = Choice.bindChoice2Of2 k x

    static member inline Invoke (x: '``MonadError<'E1,'T>``) (f: 'E1->'``MonadError<'E2,'T>``) : '``MonadError<'E2,'T>`` =
        let inline call_3 (_: ^a, b: ^b, _: ^c, f: ^f) = ((^a or ^b or ^c) : (static member Catch : _*_ -> _) b, f)
        call_3 (Unchecked.defaultof<Catch>, x, Unchecked.defaultof<'``MonadError<'E2,'T>``>, f)


// MonadCont

type CallCC = static member inline Invoke (f: (('T -> '``MonadCont<'U>``) ->'``MonadCont<'T>``)) = (^``MonadCont<'T>`` : (static member CallCC : _ -> '``MonadCont<'T>``) f)


// MonadState

type DuState<'s,'t> = DuState of ('s->('t * 's))

type Get =
    inherit Default1
    static member inline Get (_: ^``MonadState<'S * 'S>``, _: Default1) = (^``MonadState<'S * 'S>`` : (static member Get : _) ()) : ^``MonadState<'S * 'S>``
    static member        Get (_: DuState<'S,'S>          , _: Get     ) = DuState (fun (s: 'S) -> (s, s))                         : DuState<'S,'S>
    static member        Get (_: State<'S,'S>            , _: Get     ) = State.get                                               : State<'S,'S>

    static member inline Invoke () =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Get : _*_ -> _) b, a)
        let inline call (a: 'a) = call_2 (a, Unchecked.defaultof<'r>) :'r
        call Unchecked.defaultof<Get>


type Put =
    static member inline Invoke (x: 'E) : '``'MonadError<'E,'T>`` =
        let inline call_2 (_: ^a, b: ^R, x) = ((^a or ^R) : (static member Put : _*_->'R) (b, x))
        let inline call (a: 'a, x: 'x) = call_2 (a, Unchecked.defaultof<'r>, x) : 'r
        call (Unchecked.defaultof<Put>, x)

    #if !FABLE_COMPILER
    static member inline Put (_: 'R, x: 'E) = (^R : (static member Put : _ -> ^R) x)
    static member inline Put (_: ^t when ^t: null and ^t: struct, _) = id
    #endif
    static member        Put (_: State<'S,unit>  , x: 'S) = State.put x                : State<'S,unit>
    static member        Put (_: DuState<'S,unit>, x: 'S) = DuState (fun _ -> ((), x)) : DuState<'S,unit>


// MonadReader

type Ask   = static member inline Invoke ()                                          : '``MonadReader<'R,'T>``  = (^``MonadReader<'R,'T>``  : (static member Ask   : _) ())
type Local = static member inline Invoke (f: 'R1->'R2) (m: ^``MonadReader<'R2,'T>``) : '``MonadReader<'R1,'T>`` = (^``MonadReader<'R1,'T>`` : (static member Local : _*_ -> _) m, f)


// MonadWriter
    
type Tell   = static member inline Invoke (w: 'Monoid)                                               : '``MonadWriter<'Monoid,unit>``           = (^``MonadWriter<'Monoid,unit>``           : (static member Tell   : _ -> _) w)
type Listen = static member inline Invoke (m: '``MonadWriter<'Monoid,'T>``)                          : '``MonadWriter<'Monoid,('T * 'Monoid)>`` = (^``MonadWriter<'Monoid,('T * 'Monoid)>`` : (static member Listen : _ -> _) m)
type Pass   = static member inline Invoke (m: '``MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>``) : '``MonadWriter<'Monoid,'T>``             = (^``MonadWriter<'Monoid,'T>``             : (static member Pass   : _ -> _) m)