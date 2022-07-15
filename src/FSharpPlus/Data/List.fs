namespace FSharpPlus.Data

#if !FABLE_COMPILER || FABLE_COMPILER_3

open FSharpPlus
open System.ComponentModel
open FSharpPlus.Internals.Prelude


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
type ListT<'monad, 't> = ListT of obj
type ListTNode<'monad, 't> = Nil | Cons of 't * ListT<'monad, 't>

/// Basic operations on ListT
[<RequireQualifiedAccess>]
module ListT =

    let inline internal wrap (mit: '``Monad<ListTNode<'Monad, 'T>>``) =
        if opaqueId false then
            let _: 'Monad = Unchecked.defaultof<'``Monad<ListTNode<'Monad, 'T>>``> |> map (fun (_: ListTNode<'Monad, 'T>) -> Unchecked.defaultof<__>)
            let _: '``Monad<ListTNode<'Monad, 'T>>`` = Unchecked.defaultof<'Monad> |> map (fun (_: __) -> Unchecked.defaultof<ListTNode<'Monad, 'T>>)
            ()
        ListT mit : ListT<'Monad, 'T>

    let inline internal unwrap (ListT mit: ListT<'Monad, 'T>) : '``Monad<ListTNode<'Monad, 'T>>`` =
        if opaqueId false then
            let _: 'Monad = Unchecked.defaultof<'``Monad<ListTNode<'Monad, 'T>>``> |> map (fun (_: ListTNode<'Monad, 'T>) -> Unchecked.defaultof<__>)
            let _: '``Monad<ListTNode<'Monad, 'T>>`` = Unchecked.defaultof<'Monad> |> map (fun (_: __) -> Unchecked.defaultof<ListTNode<'Monad, 'T>>)
            ()
        unbox mit

    let inline empty<'T, .. > () = wrap ((result ListTNode<'Monad, 'T>.Nil) : '``Monad<ListTNode<'Monad, 'T>>``) : ListT<'Monad, 'T>

    /// Concatenates the elements of two lists
    let inline concat<'T, .. > l1 l2 =
        let rec loop (l1: ListT<'Monad, 'T>) (lst2: ListT<'Monad, 'T>) =
            let (l1, l2) = (unwrap l1: '``Monad<ListTNode<'Monad, 'T>>``), unwrap lst2
            ListT (l1 >>= function Nil ->  l2 | Cons (x: 'T, xs) -> ((result (Cons (x, loop xs lst2))) : '``Monad<ListTNode<'Monad, 'T>>``))
        loop l1 l2 : ListT<'Monad, 'T>

    let inline bind<'T, 'U, .. > f (source: ListT<'Monad, 'T>) : ListT<'Monad, ' U> =
        let rec loop f input =
            ListT (
                (unwrap input: '``Monad<ListTNode<'Monad, 'T>>``) >>= function
                    | Nil -> result Nil
                    | Cons (h: 'T, t: ListT<'Monad, 'T>) ->
                        let res = concat<'U, _, '``Monad<ListTNode<'Monad, 'U>>``> (f h: ListT<'Monad, 'U>) (loop f t)
                        unwrap res  : '``Monad<ListTNode<'Monad, 'U>>``) 
        loop f source

    let inline unfold<'State, 'T, .. > (f: 'State -> '``Monad<('T * 'State) option>``) (s: 'State) : ListT<'Monad, 'T> =
        let rec loop f s =
            (f s |> map (function
                | Some (a, s) -> Cons(a, loop f s)
                | None -> Nil) : '``Monad<ListTNode<'Monad, 'T>>``) |> wrap
        loop f s

    let inline lift<'T, .. > (x: '``Monad<'T>``) : ListT<'Monad, 'T> =
        wrap ((x |> (if opaqueId false then liftM else map) (fun x -> Cons (x, empty<'T, 'Monad, '``Monad<ListTNode<'Monad, 'T>>``> () ))) : '``Monad<ListTNode<'Monad, 'T>>`` )

    let inline map<'T, 'U, .. > f (input: ListT<'Monad, 'T>) : ListT<'Monad, 'U> =
        let rec collect f (input : ListT<'Monad, 'T>) : ListT<'Monad, 'U> =
            wrap (
                (unwrap input: '``Monad<ListTNode<'Monad, 'T>>``) >>= function
                    | Nil -> result Nil
                    | Cons (h: 'T, t: ListT<'Monad, 'T>) ->
                        let res = Cons (f h, collect f t)
                        result res : '``Monad<ListTNode<'Monad, 'U>>``)
        collect f (input: ListT<'Monad, 'T>) : ListT<'Monad, 'U>

    let inline singleton<'T, .. > (v: 'T) =
        let mresult x = result x
        wrap ((mresult <| ListTNode<'Monad, 'T>.Cons (v, (wrap (mresult ListTNode<'Monad, 'T>.Nil): ListT<'Monad, 'T> ))) : '``Monad<ListTNode<'Monad, 'T>>``) : ListT<'Monad, 'T>

    let inline apply<'T, 'U, .. > (f: ListT<'Monad, ('T -> 'U)>) (x: ListT<'Monad, 'T>) : ListT<'Monad, 'U> =
        bind<_, _, _, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, ('T -> 'U)>>``> (fun (x1: _) ->
            bind<_, _, _, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``> (fun x2 ->
                singleton<_, _, '``Monad<ListTNode<'Monad, 'U>>``> (x1 x2)) x) f

    /// <summary>Safely builds a new list whose elements are the results of applying the given function
    /// to each of the elements of the two lists pairwise.</summary>
    /// <remark>If one list is shorter, excess elements are discarded from the right end of the longer list.</remark>
    let inline map2 (f: 'T -> 'U -> 'V) (x: ListT<'Monad, 'T>) (y : ListT<'Monad, 'U>) : ListT<'Monad, 'V> =
        let rec collect f x y =
            wrap (
                (lift2 tuple2<ListTNode<'Monad, 'T>, ListTNode<'Monad, 'U>>
                (unwrap x: '``Monad<ListTNode<'Monad, 'T>>``)
                (unwrap y: '``Monad<ListTNode<'Monad, 'U>>``)
                    : '``Monad<ListTNode<'Monad, 'T> * ListTNode<'Monad, 'U>>``)
                >>= function
                | Cons (t: 'T, ts: ListT<'Monad, 'T>), Cons (u: 'U, us: ListT<'Monad, 'U>) ->
                    let res = Cons (f t u, collect f ts us)
                    result res: '``Monad<ListTNode<'Monad, 'V>>``
                | _, _ -> result Nil)
        collect f x y

    /// Same as map2 but with 3 lists.
    let inline map3 (f: 'T -> 'U -> 'V -> 'W) (x: ListT<'Monad, 'T>) (y : ListT<'Monad, 'U>) (z: ListT<'Monad, 'V>) : ListT<'Monad, 'W> =
        let rec collect f x y z =
            wrap (
                (lift3
                    tuple3<ListTNode<'Monad, 'T>, ListTNode<'Monad, 'U>, ListTNode<'Monad, 'V>>
                    (unwrap x: '``Monad<ListTNode<'Monad, 'T>>``)
                    (unwrap y: '``Monad<ListTNode<'Monad, 'U>>``)
                    (unwrap z: '``Monad<ListTNode<'Monad, 'V>>``)
                        : '``Monad<ListTNode<'Monad, 'T> * ListTNode<'Monad, 'U>> * ListTNode<'Monad, 'V>>``)
                >>= function
                | Cons (t: 'T, ts: ListT<'Monad, 'T>), Cons (u: 'U, us: ListT<'Monad, 'U>), Cons (v: 'V, vs: ListT<'Monad, 'V>) ->
                    let res = Cons (f t u v, collect f ts us vs)
                    result res: '``Monad<ListTNode<'Monad, 'W>>``
                | _, _, _ -> result Nil)
        collect f x y z

    /// <summary>Combines values from two list and calls a mapping function on this combination.</summary>
    /// <param name="f">Mapping function taking three element combination as input.</param>
    /// <param name="x">First list.</param>
    /// <param name="y">Second list.</param>
    ///
    /// <returns>List with values returned from mapping function.</returns>
    let inline lift2<'T, 'U, 'V, .. > (f: 'T -> 'U -> 'V) (x: ListT<'Monad, 'T>) (y : ListT<'Monad, 'U>) : ListT<'Monad, 'V> =
        f </map<'T, 'U -> 'V, 'Monad, '``Monad<ListTNode<'Monad, 'U -> 'V>>``, '``Monad<ListTNode<'Monad, 'T>>``> /> x
          </apply<'U, 'V    , 'Monad, '``Monad<ListTNode<'Monad, 'U -> 'V>>``, '``Monad<ListTNode<'Monad, 'V>>``, '``Monad<ListTNode<'Monad, 'U>>``> /> y

    /// <summary>Combines values from three list and calls a mapping function on this combination.</summary>
    /// <param name="f">Mapping function taking three element combination as input.</param>
    /// <param name="x">First list.</param>
    /// <param name="y">Second list.</param>
    /// <param name="z">Third list.</param>
    ///
    /// <returns>List with values returned from mapping function.</returns>
    let inline lift3<'T, 'U, 'V, 'W, .. > (f: 'T -> 'U -> 'V -> 'W) (x: ListT<'Monad, 'T>) (y : ListT<'Monad, 'U>) (z: ListT<'Monad, 'V>) : ListT<'Monad, 'W> =
        f </map<'T, 'U -> 'V -> 'W, 'Monad, '``Monad<ListTNode<'Monad, 'U -> 'V -> 'W>>``, '``Monad<ListTNode<'Monad, 'T>>``> /> x
          </apply<'U, 'V -> 'W    , 'Monad, '``Monad<ListTNode<'Monad, 'U -> 'V -> 'W>>``, '``Monad<ListTNode<'Monad, 'V -> 'W>>``, '``Monad<ListTNode<'Monad, 'U>>``> /> y
          </apply<'V, 'W          , 'Monad, '``Monad<ListTNode<'Monad, 'V -> 'W>>``      , '``Monad<ListTNode<'Monad, 'W>>``      , '``Monad<ListTNode<'Monad, 'V>>``> /> z

    let inline append (head: 'T) tail = wrap ((result <| ListTNode<'Monad, 'T>.Cons (head, (tail: ListT<'Monad, 'T> ))) : '``Monad<ListTNode<'Monad, 'T>>``) : ListT<'Monad, 'T>

    let inline head (x: ListT<'Monad, 'T>) =
        (unwrap x: '``Monad<ListTNode<'Monad, 'T>>``) >>= function
        | Nil -> failwith "empty list"
        | Cons (head, _: ListT<'Monad, 'T>) -> result head : '``Monad<'T>``

    let inline tail (x: ListT<'Monad, 'T>) : ListT<'Monad, 'T> =
        ((unwrap x: '``Monad<ListTNode<'Monad, 'T>>``) >>= function
        | Nil -> failwith "empty list"
        | Cons (_: 'T, tail: ListT<'Monad, 'T>) -> (unwrap tail: '``Monad<ListTNode<'Monad, 'T>>``)) |> wrap

    let inline iterM<'T, .. > (action: 'T -> '``Monad<unit>``) (lst: ListT<'Monad, 'T>) : '``Monad<unit>`` =
        let rec loop lst action =
            (unwrap lst: '``Monad<ListTNode<'Monad, 'T>>``) >>= function
                | Nil         -> result ()
                | Cons (h, t) -> action h >>= (fun () -> loop t action)
        loop lst action
        
    let inline iter<'T, .. > (action: 'T -> unit) (lst: ListT<'Monad, 'T>) : '``Monad<unit>`` =
        iterM<'T, '``Monad<unit>``, '``Monad<ListTNode<'Monad, 'T>>``, 'Monad> (action >> result) lst

    let inline take<'T, .. > count (input: ListT<'Monad, 'T>) : ListT<'Monad, 'T> =
        let rec loop count input = wrap <| (monad {
            if count > 0 then
                let! v = unwrap input: '``Monad<ListTNode<'Monad, 'T>>``
                match v with
                    | Cons (h, t: ListT<'Monad, 'T>) -> return Cons (h, loop (count - 1) t)
                    | Nil         -> return Nil
            else return Nil } : '``Monad<ListTNode<'Monad, 'T>>``)
        loop count (input: ListT<'Monad, 'T>)
        
    let inline filterM<'T, .. > (f: 'T -> '``Monad<bool>``) (input: ListT<'Monad, 'T>) : ListT<'Monad, 'T> =
        input
        |> bind<_, _, _, '``Monad<ListTNode<'Monad, 'T>>``, '``Monad<ListTNode<'Monad, 'T>>``> (fun v ->
            lift<_, _, '``Monad<ListTNode<'Monad, bool>``, _> (f v) |> bind<_, _, _, '``Monad<ListTNode<'Monad, 'T>>``, '``Monad<ListTNode<'Monad, bool>``> (fun b ->
                if b then singleton<_, _, '``Monad<ListTNode<'Monad, 'T>>``> v else empty<'T, 'Monad, '``Monad<ListTNode<'Monad, 'T>>``> ()))

    let inline filter<'T, .. > (f: 'T -> bool) (input: ListT<'Monad, 'T>) : ListT<'Monad, 'T> =
        filterM<'T, '``Monad<bool>``, '``Monad<ListTNode<'Monad, bool>>``, 'Monad, '``Monad<ListTNode<'Monad, 'T>>``> (f >> result) input

    let inline run<'T, .. > (lst: ListT<'Monad, 'T>) : '``Monad<list<'T>>`` =
        let rec loop acc x =
            (unwrap x: '``Monad<ListTNode<'Monad, 'T>>``)
            >>= function
            | Nil          -> result (List.rev acc)
            | Cons (x, xs) -> loop (x::acc) xs
        loop [] lst


type [<AutoOpen>]ListTOperations =
    [<GeneralizableValue>]
    static member inline ListT<'T, .. > (source: '``Monad<list<'T>>``) : ListT<'Monad, 'T> =
        ListT.unfold<int, 'T, '``Monad<('T * int) option>``, '``Monad<ListTNode<'Monad, 'T>>``, 'Monad>
            (fun i -> map (fun (lst: list<'T>) -> if lst.Length > i then Some (lst.[i], i + 1) else None) source) 0


module [<AutoOpen>]ListTOperations =
    let inline listT<'T, .. > (source: '``Monad<list<'T>>``) : ListT<'Monad, 'T> = ListTOperations.ListT<_, _, '``Monad<('T * int) option>``, '``Monad<ListTNode<'Monad, 'T>>``, _> source



type ListT<'monad, 't> with
    static member inline Return (x: 'T) : ListT<'Monad, 'T> = ListT.singleton<_, _, '``Monad<ListTNode<'Monad, 'T>>``> x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Map (x : ListT<'Monad, 'T>, f: 'T -> 'U) : ListT<'Monad, 'U> =
        ListT.map<'T, 'U, 'Monad, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``> f x

    /// <summary>Lifts a function into a ListT. Same as map.
    /// To be used in Applicative Style expressions, combined with &lt;*&gt;
    /// </summary>
    /// <category index="1">Functor</category>
    static member inline (<!>) (x : ListT<'Monad, 'T>, f: 'T -> 'U) : ListT<'Monad, 'U> =
        ListT.map<'T, 'U, 'Monad, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``> f x

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift2 (f: 'T -> 'U -> 'V, x: ListT<'Monad, 'T>, y: ListT<'Monad, 'U>) : ListT<'Monad, 'V> =
        ListT.lift2<_, _, _, _, '``Monad<ListTNode<'Monad, 'U -> 'V>>``, '``Monad<ListTNode<'Monad, 'T>>``, '``Monad<ListTNode<'Monad, 'V>>``, '``Monad<ListTNode<'Monad, 'U>>``> f x y

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline Lift3 (f: 'T -> 'U -> 'V -> 'W, x: ListT<'Monad, 'T>, y: ListT<'Monad, 'U>, z: ListT<'Monad, 'V>) : ListT<'Monad, 'W> =
        ListT.lift3<_, _, _, _, _, '``Monad<ListTNode<'Monad, 'T>>``, '``Monad<ListTNode<'Monad, 'U -> 'V -> 'W>>``, '``Monad<ListTNode<'Monad, 'V -> 'W>>``, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'W>>``, '``Monad<ListTNode<'Monad, 'V>>``> f x y z

    static member inline (<*>) (f: ListT<'Monad, ('T -> 'U)>, x: ListT<'Monad, 'T>) : ListT<'Monad, 'U> =
        ListT.apply<_, _, _, '``Monad<ListTNode<'Monad, 'T -> 'U>>``, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``> f x

    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: ListT<'Monad, 'T>, y: ListT<'Monad, 'U>) : ListT<'Monad, 'U> =
        let (<!>) = ListT.map<_, _, 'Monad, '``Monad<ListTNode<'Monad, ('U -> 'U)>>``, '``Monad<ListTNode<'Monad, 'T>>``>
        let (<*>) = ListT.apply<_, _, 'Monad, '``Monad<ListTNode<'Monad, 'U -> 'U>>``, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'U>>``>
        ((fun (_: 'T) (k: 'U) -> k) <!> x: ListT<'Monad, ('U -> 'U)>) <*> y
    
    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: ListT<'Monad, 'U>, y: ListT<'Monad, 'T>) : ListT<'Monad, 'U> =
        let (<!>) = ListT.map<_, _, 'Monad, '``Monad<ListTNode<'Monad, 'T -> 'U>>``, '``Monad<ListTNode<'Monad, 'U>>``>
        let (<*>) = ListT.apply<_, _, 'Monad, '``Monad<ListTNode<'Monad, 'T -> 'U>>``, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``>
        ((fun (k: 'U) (_: 'T) -> k) <!> x: ListT<'Monad, ('T -> 'U)>) <*> y

    static member inline (>>=) (x: ListT<'Monad, 'T>, f: 'T -> ListT<'Monad, ' U>) : ListT<'Monad, ' U> =
        ListT.bind<_, _, _, '``Monad<ListTNode<'Monad, 'U>>``, '``Monad<ListTNode<'Monad, 'T>>``> f x

    static member inline get_Empty () : ListT<'Monad, 'T> = ListT.empty<_, _, '``Monad<ListTNode<'Monad, 'T>>``> ()
    static member inline (<|>) (x: ListT<'Monad, 'T>, y: ListT<'Monad, 'T>) : ListT<'Monad, 'T> = ListT.concat<_, _, '``Monad<ListTNode<'Monad, 'T>>``> x y

    static member inline TryWith (source: ListT<'Monad, 'T>, f: exn -> ListT<'Monad, 'T>) = ListT (TryWith.Invoke (ListT.unwrap source: '``Monad<ListTNode<'Monad, 'T>>``) (ListT.unwrap << f))
    static member inline TryFinally (computation: ListT<'Monad, 'T>, f) = ListT (TryFinally.Invoke     (ListT.unwrap computation: '``Monad<ListTNode<'Monad, 'T>>``) f)
    static member inline Using (resource, f: _ -> ListT<'Monad, 'T>)    = ListT (Using.Invoke resource (ListT.unwrap << f : 'R -> '``Monad<ListTNode<'Monad, 'T>>``))
    static member inline Delay (body : unit   ->  ListT<'Monad, 'T>) : ListT<'Monad, 'T> = ListT (Delay.Invoke (fun _ -> ListT.unwrap (body ()) : '``Monad<ListTNode<'Monad, 'T>>``))

    static member inline Lift (x: '``Monad<'T>``) = ListT.lift<_, _, '``Monad<ListTNode<'Monad, 'T>>``, _> x : ListT<'Monad, 'T>
    
    static member inline LiftAsync (x: Async<'T>) = ListT.lift<_, _, '``MonadAsync<ListTNode<'MonadAsync, 'T>>``, _> (liftAsync x: '``MonadAsync<'T>``) : ListT<'MonadAsync, 'T>
    
    static member inline Throw (x: 'E) : ListT<'``MonadError<'E>``, 'T> = x |> throw |> ListT.lift<_, '``MonadError<'E, 'T>``, '``Monad<ListTNode<'MonadError<'E>, 'T>>``, _>
    static member inline Catch (m: ListT<'``MonadError<'E1>``, 'T>, h: 'E1 -> ListT<'``MonadError<'E2>``, 'T>) : ListT<'``MonadError<'E2>``, 'T> =
        ListT (
            (fun v h -> Catch.Invoke v h)
                (ListT.run<'T, '``MonadError<'E1>``, '``MonadError<'E1, ListTNode<'MonadError<'E1>, 'T>>``, '``MonadError<'E1, list<'T>>``> m)
                (ListT.run<'T, '``MonadError<'E2>``, '``MonadError<'E2, ListTNode<'MonadError<'E2>, 'T>>``, '``MonadError<'E2, list<'T>>``> << h))
    
    static member inline CallCC (f: (('T -> ListT<'``MonadCont<'R>``, 'U>) -> _)) : ListT<'``MonadCont<'R>``, 'T> =
        ListT (callCC <| fun c -> ListT.run<'T, '``MonadCont<'R>``, '``MonadCont<'R, ListTNode<'MonadCont<'R>, 'T>>``, '``MonadCont<'R, list<'T>>``> (f (ListT << c << List.singleton)))
    
    static member inline get_Get ()  : ListT<'``MonadState<'S>``, 'S> = ListT.lift<'S, '``MonadState<'S, 'S>``, '``MonadState<'S, ListTNode<'MonadState<'S>, 'S>>``, '``MonadState<'S>``> get
    static member inline Put (x: 'T) : ListT<'``MonadState<unit>``, 'S> = x |> put |> ListT.lift<_, '``MonadState<'S, 'S>``, '``MonadState<'S, ListTNode<'MonadState<'S>, 'S>>``, _>
    
    static member inline get_Ask () : ListT<'``MonadReader<'R>``, 'R> = ListT.lift<_, '``MonadReader<'R, 'R>``, '``MonadReader<'R, ListTNode<'MonadReader<'R>, 'R>>``, _> ask
    static member inline Local (m: ListT<'``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : ListT<'``MonadReader<'R1>``, 'T> =
        listT<'T, '``MonadReader<'R1, list<'T>>``, '``MonadReader<'R1, ('T * int) option>``, '``MonadReader<'R1, ListTNode<MonadReader<'R1>, 'T>>``, _> (local f (ListT.run<'T, '``MonadReader<'R2>``, '``MonadReader<'R2, ListTNode<MonadReader<'R2>, 'T>>``, '``MonadReader<'R2, list<'T>>``> m))

    static member inline Take (lst : ListT<'Monad, 'T>, c: int, _: Take) : ListT<'Monad, 'T> = ListT.take<_, _,  '``Monad<ListTNode<'Monad, 'T>>``> c lst

#endif