namespace FSharpPlus.Data

open FSharpPlus
open System.ComponentModel

/// Additional operations on List
module List =

    let inline sequence (ms: list<'``Applicative<'T>``>) : '``Applicative<list<'T>>`` = sequence ms

    let inline traverse (f: 'T->'``Applicative<'U>``) (xs:list<'T>) : '``Applicative<list<'U>>`` = traverse f xs
    
    let inline foldM (f: 'T->'U->'``Monad<'T>``) (a: 'T) (bx:list<'U>) : '``Monad<'T>`` =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
        let rec loopM a = function
            | x::xs -> (f.Invoke (a, x)) >>= fun fax -> loopM fax xs 
            | [] -> result a
        loopM a bx

    let inline filterM (f: 'T -> '``Monad<Bool>``) (xs: list<'T>) : '``Monad<list<'T>>`` =
        let rec loopM = function
            | []   -> result []
            | h::t -> 
                f h >>= (fun flg ->
                    loopM t >>= (fun ys ->
                        result (if flg then (h::ys) else ys)))
        loopM xs

    let inline replicateM count (initial: '``Applicative<'T>``)  = sequence (List.replicate count initial)


open FSharpPlus.Control

/// Monad Transformer for list<'T>
[<Struct>]
type ListT<'``monad<'t>``> = ListT of obj
type ListTNode<'``monad<'t>``,'t> = Nil | Cons of 't * ListT<'``monad<'t>``>

/// Basic operations on ListT
[<RequireQualifiedAccess>]
module ListT =

    let inline internal wrap (mit: 'mit) =
        let _mnil  = (result Unchecked.defaultof<'t> : 'mt) >>= fun (_:'t) -> (result ListTNode<'mt,'t>.Nil ) : 'mit
        ListT mit : ListT<'mt>

    let inline internal unwrap (ListT mit : ListT<'mt>) =
        let _mnil  = (result Unchecked.defaultof<'t> : 'mt) >>= fun (_:'t) ->  (result ListTNode<'mt,'t>.Nil ) : 'mit
        unbox mit : 'mit

    let inline empty () = wrap ((result ListTNode<'mt,'t>.Nil) : 'mit) : ListT<'mt>

    /// Concatenates the elements of two lists
    let inline concat l1 l2 =
        let rec loop (l1: ListT<'mt>) (lst2: ListT<'mt>) =
            let (l1, l2) = unwrap l1, unwrap lst2
            ListT (l1 >>= function Nil ->  l2 | Cons (x: 't, xs) -> ((result (Cons (x, loop xs lst2))) : 'mit))
        loop l1 l2 : ListT<'mt>

    let inline bind f (source: ListT<'mt>) : ListT<'mu> =
        let rec loop f input =
            wrap (    
                (unwrap input : 'mit) >>= function
                        | Nil -> result <| (Nil : ListTNode<'mu,'u>) : 'miu
                        | Cons (h:'t, t: ListT<'mt>) ->
                            let ( res) = concat (f h: ListT<'mu>) (loop f t )
                            unwrap res  : 'miu)
        loop f source : ListT<'mu>

    let inline unfold (f:'State -> '``M<('T * 'State) option>``) (s:'State) : ListT<'MT> =
        let rec loop f s = f s |> map (function
                | Some (a, s) -> Cons(a, loop f s)
                | None -> Nil) |> wrap
        loop f s

    let inline map f (input : ListT<'mt>) : ListT<'mu> =
        let rec collect f (input : ListT<'mt>) : ListT<'mu> =
            wrap (
                (unwrap input : 'mit) >>= function
                    | Nil -> result <| (Nil : ListTNode<'mu,'u>) : 'miu
                    | Cons (h: 't, t: ListT<'mt>) ->
                        let ( res) = Cons (f h, collect f t)
                        result res  : 'miu)
        collect f (input: ListT<'mt>) : ListT<'mu>

    let inline singleton (v:'t) =
        let mresult x = result x
        let _mnil  = (result Unchecked.defaultof<'t> : 'mt) >>= konst (mresult ListTNode<'mt,'t>.Nil ) : 'mit
        wrap ((mresult <| ListTNode<'mt,'t>.Cons (v, (wrap (mresult ListTNode<'mt,'t>.Nil): ListT<'mt> ))) : 'mit) : ListT<'mt>

    let inline apply f x = bind (fun (x1: _) -> bind (fun x2 -> singleton (x1 x2)) x) f

    let inline append (head: 't) tail = wrap ((result <| ListTNode<'mt,'t>.Cons (head, (tail: ListT<'mt> ))) : 'mit) : ListT<'mt>

    let inline head (x : ListT<'mt>) =
        unwrap x >>= function
        | Nil -> failwith "empty list"
        | Cons (head, _) -> result head : 'mt

    let inline tail (x: ListT<'mt>) : ListT<'mt> =
        (unwrap x >>= function
        | Nil -> failwith "empty list"
        | Cons (_: 't, tail) -> unwrap tail) |> wrap

    let inline iter action lst =
        let rec loop (seq: ListT<'MT>) (action: 'T -> '``M<unit>``) : '``M<unit>`` =
            unwrap seq >>= function
                | Nil         -> result ()
                | Cons (h, t) -> action h >>= (fun () -> loop t action)
        loop lst action

    let inline lift (x: '``Monad<'T>``) = wrap (x >>= (result << (fun x -> Cons (x, empty () )))) : ListT<'``Monad<'T>``>

    let inline take count (input : ListT<'MT>) : ListT<'MT> =
        let rec loop count (input : ListT<'MT>) : ListT<'MT> = wrap <| monad {
            if count > 0 then
                let! v = unwrap input
                match v with
                    | Cons (h, t) -> return Cons (h, loop (count - 1) t)
                    | Nil         -> return Nil
            else return Nil }
        loop count (input: ListT<'MT>)

    let inline run (lst: ListT<'MT>) : '``Monad<list<'T>>`` =
        let rec loop acc x = unwrap x >>= function
            | Nil          -> result (List.rev acc)
            | Cons (x, xs) -> loop (x::acc) xs
        loop [] lst


[<AutoOpen>]
module ListTPrimitives =
    let inline listT (al: '``Monad<list<'T>>``) : ListT<'``Monad<'T>``> =
        ListT.unfold (fun i -> map (fun (lst:list<_>) -> if lst.Length > i then Some (lst.[i], i+1) else None) al) 0


type ListT<'``monad<'t>``> with
    static member inline Return (x: 'T) = ListT.singleton x : ListT<'M>

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x, f) = ListT.map f x

    static member inline (<*>) (f, x) = ListT.apply f x

    static member inline (>>=) (x, f) = ListT.bind f x
    static member inline get_Empty () = ListT.empty ()
    static member inline (<|>) (x, y) = ListT.concat x y

    static member inline TryWith (source: ListT<'``Monad<'T>``>, f: exn -> ListT<'``Monad<'T>``>) = ListT (TryWith.Invoke (ListT.unwrap source) (ListT.unwrap << f))
    static member inline TryFinally (computation: ListT<'``Monad<'T>``>, f) = ListT (TryFinally.Invoke     (ListT.unwrap computation) f)
    static member inline Using (resource, f: _ -> ListT<'``Monad<'T>``>)    = ListT (Using.Invoke resource (ListT.unwrap << f))
    static member inline Delay (body : unit   ->  ListT<'``Monad<'T>``>) = ListT (Delay.Invoke (fun _ -> ListT.unwrap (body ()))) : ListT<'``Monad<'T>``>

    static member inline Lift (x:'``Monad<'T>``) = ListT.wrap (x >>= (result << (fun x -> Cons (x, ListT.empty () )))) : ListT<'``Monad<'T>``>

    static member inline LiftAsync (x: Async<'T>) = lift (liftAsync x) : '``ListT<'MonadAsync<'T>>``

    static member inline Throw (x: 'E) = x |> throw |> lift
    static member inline Catch (m: ListT<'``MonadError<'E1,'T>``>, h: 'E1 -> ListT<'``MonadError<'E2,'T>``>) = listT ((fun v h -> Catch.Invoke v h) (ListT.run m) (ListT.run << h)) : ListT<'``MonadError<'E2,'T>``>

    static member inline CallCC (f: (('T -> ListT<'``MonadCont<'R,list<'U>>``>) -> _)) = listT (callCC <| fun c -> ListT.run (f (listT << c << List.singleton))) : ListT<'``MonadCont<'R, list<'T>>``>

    static member inline get_Get ()  = lift get         : '``ListT<'MonadState<'S,'S>>``
    static member inline Put (x: 'T) = x |> put |> lift : '``ListT<'MonadState<unit,'S>>``

    static member inline get_Ask () = lift ask          : '``ListT<'MonadReader<'R,  list<'R>>>``
    static member inline Local (m: ListT<'``MonadReader<'R2,'T>``>, f: 'R1->'R2) = listT (local f (ListT.run m))

    static member inline Take (lst, c, _: Take) = ListT.take c lst
