namespace FSharpPlus.Data

#if !FABLE_COMPILER || FABLE_COMPILER_3

open System.ComponentModel
open FSharpPlus
open FSharpPlus.Internals.Prelude


/// Additional operations on Seq
module Seq =

    let inline sequence (ms: seq<'``Applicative<'T>``>) : '``Applicative<seq<'T>>`` = sequence ms

    let inline traverse (f: 'T->'``Applicative<'U>``) (xs: seq<'T>) : '``Applicative<seq<'U>>`` = traverse f xs

    let inline replicateM count (initial: '``Applicative<'T>``) = sequence (Seq.replicate count initial)

open System
open System.Collections.Generic
open FSharpPlus.Control

#endif
#nowarn "0193"
#if !FABLE_COMPILER

module Internal =
    let inline monomorphicBind (binder: 'T -> '``Monad<'T>``) (source: '``Monad<'T>``) : '``Monad<'T>`` =
        let inline call (_mthd: 'M, input: 'I, _output: 'I, f) = ((^M or ^I) : (static member (>>=) : _*_ -> _) input, f)
        call (Unchecked.defaultof<Bind>, source, Unchecked.defaultof<'``Monad<'T>``>, binder)

open Internal

type MonadFxStrictBuilderMod<'``monad<'t>``> () =
    inherit FSharpPlus.GenericBuilders.MonadFxStrictBuilder<'``monad<'t>``> ()
    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``

type MonadPlusStrictBuilderMod<'``monad<'t>``> () =
    inherit FSharpPlus.GenericBuilders.MonadPlusStrictBuilder<'``monad<'t>``> ()
    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``

type MonadFxStrictBuilderMod2<'``monad<'t>``, ^``monad<unit>``>
                                    when (Return or ^``monad<unit>``) : (static member Return: ^``monad<unit>`` * Return -> (unit -> ^``monad<unit>``)) 
                                    and  (Bind   or ^``monad<unit>``) : (static member (>>=): ^``monad<unit>`` * (unit -> ^``monad<unit>``) -> ^``monad<unit>``)
                                    and  (Using  or ^``monad<unit>``) : (static member Using: IDisposable * (IDisposable -> ^``monad<unit>``) * Using -> ^``monad<unit>``)
                                         () =
    inherit StrictBuilder<'``monad<'t>``> ()
    member inline _.Zero () = result ()                                       : '``monad<unit>``
    member inline _.Combine (a: '``Monad<unit>``, b) = a >>= (fun () -> b ()) : '``Monad<'T>``
    
    member inline _.While (guard, body: unit -> '``monad<unit>``)             : '``monad<unit>`` =
        let rec loop guard body =
            if guard () then body () |> monomorphicBind (fun () -> loop guard body)
            else result ()
        loop guard body
    
     member inline this.For (p: #seq<'T>, rest: 'T->'``monad<unit>``) =
         Using.Invoke (p.GetEnumerator () :> IDisposable) (fun enum ->
             let enum = enum :?> IEnumerator<_>
             this.While (enum.MoveNext, fun () -> rest enum.Current) : '``monad<unit>``)

    member inline _.Delay expr = (fun () -> Delay.Invoke expr) : unit -> '``Monad<'T>``


module SpecialBuilders =
    let innerMonad<'mt> = new MonadFxStrictBuilderMod<'mt> ()
    let inline innerMonad2<'mt, .. > () = new MonadFxStrictBuilderMod2<'mt, _> ()

open SpecialBuilders



type IEnumeratorM<'``Monad<bool>``, 'T> =
    abstract MoveNext : unit -> '``Monad<bool>``
    abstract Current : 'T with get
    inherit IDisposable

type IEnumerableM<'``Monad<bool>``, 'T> =
    abstract GetEnumerator : unit -> IEnumeratorM<'``Monad<bool>``, 'T>

// Monad Transformer for seq<'T>
[<Struct>]
type SeqT<'``monad``, 't> =
    | SeqT of IEnumerableM<'``monad``, 't>
    interface IEnumerableM<'``monad``, 't> with
        member x.GetEnumerator () = let (SeqT x) = x in x.GetEnumerator ()


module SeqT =

    let ofIEnumerableM x : SeqT<'``Monad<bool>``, 'T> = SeqT x

    [<RequireQualifiedAccess>]
    type MapState<'``Monad<seq<'T>>``, 'T> =
       | NotStarted    of '``Monad<seq<'T>>``
       | HaveEnumerator of IEnumerator<'T>
       | Finished

    let inline wrap (inp: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let state = ref (MapState.NotStarted inp)
                    let current = ref Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."                            
                        member x.MoveNext () = monad' {
                            match !state with
                            | MapState.NotStarted inp ->
                                let! (s: seq<'T>) = inp
                                let e1 = s.GetEnumerator ()
                                state := MapState.HaveEnumerator e1
                                return! (x.MoveNext ())
                            | MapState.HaveEnumerator e1 ->
                                let res1 = e1.MoveNext ()
                                if res1 then
                                    current := Some e1.Current
                                    return true
                                else
                                    x.Dispose ()
                                    return! x.MoveNext ()
                            | _ -> return false }
                        member _.Dispose () =
                            match !state with
                            | MapState.HaveEnumerator e1 ->
                                state := MapState.Finished
                                dispose e1
                            | _ -> () } }

    let inline hoist (source: seq<'T>) : SeqT<'``Monad<bool>``, 'T> = wrap (result source: '``Monad<seq<'T>>``)
    
    let inline toArrayM<'T, .. > (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T []>`` =
        let ra = new ResizeArray<_> ()
        Using.Invoke
            ((source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ())
            (fun ie ->
                ie.MoveNext () >>= fun (move: bool) ->
                    let b = ref move
                    let rec loop guard (body: unit -> '``Monad<unit>``) : '``Monad<unit>`` =
                        if guard () then body () >>= (fun () -> loop guard body)
                        else result ()
                    loop
                        (fun () -> b.Value)
                        (fun () ->
                            ra.Add ie.Current
                            ie.MoveNext () >>= fun moven ->
                                b := moven
                                result () )
                    >>= fun () -> result (ra.ToArray ()) )
    
    let inline toListM<'T, .. > (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T list>`` = toArrayM<_, _, '``Monad<'T []>``, '``Monad<unit>``> source |> map Array.toList<'T>
    let inline toSeqM<'T, .. >  (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T seq>``  = toArrayM<_, _, '``Monad<'T []>``, '``Monad<unit>``> source |> map Array.toSeq<'T>
    let inline run<'T, .. >     (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<'T seq>``  =   toSeqM<_, _, '``Monad<unit>``, '``Monad<'T []>``, '``Monad<'T seq>``> source

    [<GeneralizableValue>]
    let inline empty<'T, .. > : SeqT<'``Monad<bool>``, 'T> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                          member _.MoveNext () = result false
                          member _.Current = invalidOp "Enumeration has not started. Call MoveNext."
                          member _.Dispose () = () } }


    let inline singleton (v: 'T) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with 
                member _.GetEnumerator () = 
                    let stateStarted = ref false
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with 
                          member _.MoveNext () =
                              innerMonad {
                                  let res = not stateStarted.Value
                                  stateStarted := true
                                  return res }
                          member _.Current =
                              if stateStarted.Value then v
                              else invalidOp "Enumeration has not started. Call MoveNext."
                          member _.Dispose () = () } }




    let inline make (f: unit -> '``Monad<SeqT<'Monad<bool>, 'T>>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () = 
                    let state   = ref -1
                    let enum    = ref Unchecked.defaultof<IEnumeratorM<'``Monad<bool>``, 'T>>
                    let current = ref Unchecked.defaultof<'T>
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with 
                        member _.Current =
                            match !state with
                            | -1 -> invalidOp "Enumeration has not started. Call MoveNext."
                            | _  -> current.Value

                        member x.MoveNext () = 
                            innerMonad {
                                match !state with
                                    | -1 -> 
                                        let! (s: SeqT<'``Monad<bool>``, 'T>) = f ()
                                        return! (
                                            let e = (s :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                            enum := e
                                            state := 0
                                            x.MoveNext ())
                                    | 0 ->   
                                        let e = enum.Value
                                        let! (res: bool) = e.MoveNext ()
                                        do
                                            current := e.Current
                                            if not res then x.Dispose ()
                                        return res
                                    | _ -> return false }
                        member _.Dispose () = 
                            match !state with 
                            | 0 -> 
                                let e = enum.Value
                                state := 1
                                enum := Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }


    // let inline delayO (f: unit -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =  make (fun () -> monad { return f() })

    let delay (f: unit -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () = (f () :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator () }

    let inline bindM<'T, 'U, .. > (f: 'T -> SeqT<'``Monad<bool>``, 'U>) (inp: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'U> =
         make (fun () -> innerMonad<'``Monad<SeqT<'Monad<bool>, 'U>>``> { let! v = inp in return f v })

    let inline lift (source: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with 
                member _.GetEnumerator () = 
                    let stateStarted = ref None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.MoveNext () =
                            match stateStarted.Value with
                            | Some _ -> result false
                            | None -> source |> (if opaqueId false then liftM else map) (fun v -> stateStarted := Some v; true)
                        member _.Current =
                            match stateStarted.Value with
                            | Some v -> v
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."
                        member _.Dispose () = () } }


    [<RequireQualifiedAccess>]
    type CollectState<'T, 'U, '``Monad<bool>``> =
       | NotStarted    of SeqT<'``Monad<bool>``, 'T>
       | HaveInputEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | HaveInnerEnumerator of IEnumeratorM<'``Monad<bool>``, 'T> * IEnumeratorM<'``Monad<bool>``, 'U>
       | Finished


    let inline collect<'T, 'U, .. > (f: 'T -> SeqT<'``Monad<bool>``, 'U>) (inp: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let state = ref (CollectState.NotStarted inp)
                    let current = ref Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."                            
                        member x.MoveNext () =
                            monad' {
                                match !state with
                                    | CollectState.NotStarted inp ->
                                        return! (
                                            let e1 = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                            state := CollectState.HaveInputEnumerator e1
                                            x.MoveNext ())
                                    | CollectState.HaveInputEnumerator e1 ->
                                        let! res1 = e1.MoveNext ()
                                        return! (
                                            if res1 then
                                                let e2 = (f e1.Current :> IEnumerableM<'``Monad<bool>``, 'U>).GetEnumerator ()
                                                state := CollectState.HaveInnerEnumerator (e1, e2)
                                            else
                                                x.Dispose ()
                                            x.MoveNext () )
                                    | CollectState.HaveInnerEnumerator (e1, e2) ->
                                        let! (res2: bool) = e2.MoveNext ()
                                        if res2 then
                                            current := Some e2.Current
                                            return res2
                                        else
                                            state := CollectState.HaveInputEnumerator e1
                                            dispose e2
                                            return! x.MoveNext ()
                                    | _ ->
                                        return false }
                        member _.Dispose () =
                            match !state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state := CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state := CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }


    let inline apply<'T, 'U, .. > (f: SeqT<'``Monad<bool>``, 'T -> 'U>) (x1: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let state = ref (CollectState.NotStarted f)
                    let current = ref Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."                            
                        member x.MoveNext () =
                            monad' {
                                match !state with
                                    | CollectState.NotStarted f ->
                                        return! (
                                            let e1 = (f :> IEnumerableM<'``Monad<bool>``, ('T -> 'U)>).GetEnumerator ()
                                            state := CollectState.HaveInputEnumerator e1
                                            x.MoveNext ())
                                    | CollectState.HaveInputEnumerator e1 ->
                                        let! res1 = e1.MoveNext ()
                                        return! (
                                            if res1 then
                                                let e2 = (x1 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                                state := CollectState.HaveInnerEnumerator (e1, e2)
                                            else
                                                x.Dispose ()
                                            x.MoveNext () )
                                    | CollectState.HaveInnerEnumerator (e1, e2) ->
                                        let! (res2: bool) = e2.MoveNext ()
                                        if res2 then
                                            current := Some (e1.Current e2.Current)
                                            return res2
                                        else
                                            state := CollectState.HaveInputEnumerator e1
                                            dispose e2
                                            return! x.MoveNext ()
                                    | _ ->
                                        return false }
                        member _.Dispose () =
                            match !state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state := CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state := CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }

    let inline lift2<'T1, 'T2, 'U, .. > (f: 'T1 -> 'T2 -> 'U) (x1: SeqT<'``Monad<bool>``, 'T1>) (x2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let state = ref (CollectState.NotStarted x1)
                    let current = ref Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."                            
                        member x.MoveNext () =
                            monad' {
                                match !state with
                                    | CollectState.NotStarted x1 ->
                                        return! (
                                            let e1 = (x1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
                                            state := CollectState.HaveInputEnumerator e1
                                            x.MoveNext ())
                                    | CollectState.HaveInputEnumerator e1 ->
                                        let! res1 = e1.MoveNext ()
                                        return! (
                                            if res1 then
                                                let e2 = (x2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
                                                state := CollectState.HaveInnerEnumerator (e1, e2)
                                            else
                                                x.Dispose ()
                                            x.MoveNext () )
                                    | CollectState.HaveInnerEnumerator (e1, e2) ->
                                        let! (res2: bool) = e2.MoveNext ()
                                        if res2 then
                                            current := Some (f e1.Current e2.Current)
                                            return res2
                                        else
                                            state := CollectState.HaveInputEnumerator e1
                                            dispose e2
                                            return! x.MoveNext ()
                                    | _ ->
                                        return false }
                        member _.Dispose () =
                            match !state with
                            | CollectState.HaveInputEnumerator e1 ->
                                state := CollectState.Finished
                                dispose e1
                            | CollectState.HaveInnerEnumerator (e1, e2) ->
                                state := CollectState.Finished
                                dispose e2
                                dispose e1
                            | _ -> () } }
    
    [<RequireQualifiedAccess>]
    type AppendState<'``Monad<bool>``, 'T> =
       | NotStarted1     of SeqT<'``Monad<bool>``, 'T> * SeqT<'``Monad<bool>``, 'T>
       | HaveEnumerator1 of IEnumeratorM<'``Monad<bool>``, 'T> * SeqT<'``Monad<bool>``, 'T>
       | NotStarted2     of SeqT<'``Monad<bool>``, 'T>
       | HaveEnumerator2 of IEnumeratorM<'``Monad<bool>``, 'T> 
       | Finished        

    let inline append (inp1: SeqT<'``Monad<bool>``, 'T>) (inp2: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T>  with 
                member _.GetEnumerator () = 
                    let state = ref (AppendState.NotStarted1 (inp1, inp2) )
                    let current = ref Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."     
    
                        member x.MoveNext () = 
                            innerMonad {
                                match !state with 
                                    | AppendState.NotStarted1 (inp1, inp2) -> 
                                        return! (
                                            let enum1 = (inp1 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                            state := AppendState.HaveEnumerator1 (enum1, inp2)
                                            x.MoveNext ())
                                    | AppendState.HaveEnumerator1 (enum1, inp2) ->
                                        let! (res: bool) = enum1.MoveNext ()
                                        if res then
                                            current := Some enum1.Current
                                            return res
                                        else
                                            return! 
                                              (state := AppendState.NotStarted2 inp2
                                               dispose enum1
                                               x.MoveNext ())
                                    | AppendState.NotStarted2 inp2 ->
                                        return! (
                                            let enum2 = (inp2 :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                            state := AppendState.HaveEnumerator2 enum2
                                            x.MoveNext () )
                                    | AppendState.HaveEnumerator2 enum2 ->   
                                        let! (res: bool) = enum2.MoveNext ()
                                        return (
                                            if res then
                                                current := Some enum2.Current
                                            else
                                                state := AppendState.Finished
                                                dispose enum2
                                            res)
                                               
                                    | _ -> 
                                        return false }
                          member _.Dispose () = 
                              match !state with 
                              | AppendState.HaveEnumerator1 (enum, _) 
                              | AppendState.HaveEnumerator2 enum -> 
                                  state := AppendState.Finished
                                  dispose enum 
                              | _ -> () } }

    let inline mapM (f: 'T -> '``Monad<'U>``) (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        source |> collect (fun itm ->
            f itm |> bindM<_, _, _, '``Monad<SeqT<'Monad<bool>, 'U>>``, _> (fun v ->
                singleton v))

    let inline map (f: 'T -> 'U) (inp: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'U> with
                member _.GetEnumerator () =
                    let state = ref (CollectState.NotStarted inp)
                    let current = ref Option<'U>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'U>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."                            
                        member x.MoveNext () =
                              innerMonad {
                                  match !state with
                                      | CollectState.NotStarted inp ->
                                          return! (
                                              let e1 = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                              state := CollectState.HaveInputEnumerator e1
                                              x.MoveNext ())
                                      | CollectState.HaveInputEnumerator e1 ->
                                          let! res1 = e1.MoveNext ()
                                          if res1 then
                                              current := Some (f e1.Current)
                                              return true
                                          else
                                              x.Dispose ()
                                              return! x.MoveNext ()
                                      | _ ->
                                          return false }
                          member _.Dispose () =
                              match !state with
                              | CollectState.HaveInputEnumerator e1 ->
                                  state := CollectState.Finished
                                  dispose e1
                              | CollectState.HaveInnerEnumerator (e1, e2) ->
                                  state := CollectState.Finished
                                  dispose e2
                                  dispose e1
                              | _ -> () } }

    let inline lift3<'T1, 'T2, 'T3, 'U, .. > (f: 'T1 -> 'T2 -> 'T3 -> 'U) (x1: SeqT<'``Monad<bool>``, 'T1>) (x2: SeqT<'``Monad<bool>``, 'T2>) (x3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> =
        f </map/> x1 </apply/> x2 </apply/> x3

    let inline filter (f: 'T -> bool) (inp: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let state = ref (CollectState.NotStarted inp)
                    let current = ref Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."
                        member x.MoveNext () =
                              innerMonad {
                                  match !state with
                                      | CollectState.NotStarted inp ->
                                          return! (
                                              let e1 = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                              state := CollectState.HaveInputEnumerator e1
                                              x.MoveNext ())
                                      | CollectState.HaveInputEnumerator e1 ->
                                          let! res1 = e1.MoveNext ()
                                          if res1 && f e1.Current then
                                              current := Some e1.Current
                                              return true
                                          elif res1 then
                                              return! x.MoveNext ()
                                          else
                                              x.Dispose ()
                                              return! x.MoveNext ()
                                      | _ ->
                                          return false }
                          member _.Dispose () =
                              match !state with
                              | CollectState.HaveInputEnumerator e1 ->
                                  state := CollectState.Finished
                                  dispose e1
                              | CollectState.HaveInnerEnumerator (e1, e2) ->
                                  state := CollectState.Finished
                                  dispose e2
                                  dispose e1
                              | _ -> () } }

    let inline iteriM<'T, .. > (f: int -> 'T -> '``Monad<unit>``) (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = 
        innerMonad { 
            use ie = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
            let count = ref 0
            let! (move: bool) = ie.MoveNext ()
            let b = ref move
            while b.Value do
                do! f !count ie.Current
                let! moven = ie.MoveNext ()
                do incr count
                   b := moven
        }

    let inline iterM<'T, .. > (f: 'T -> '``Monad<unit>``) (inp: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iteriM (fun _ x -> f x) inp    
    let inline iteri<'T, .. > (f: int -> 'T -> unit)      (inp: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iteriM (fun i x -> result (f i x)) inp
    let inline iter<'T, .. > f (source: SeqT<'``Monad<bool>``, 'T>) : '``Monad<unit>`` = iterM (f >> result) source

    [<RequireQualifiedAccess>]
    type TryWithState<'``Monad<bool>``, 'T> =
       | NotStarted of SeqT<'``Monad<bool>``, 'T>
       | HaveBodyEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | HaveHandlerEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | Finished

    /// Implements the 'TryWith' functionality for computation builder
    let inline tryWith<'T, .. > (inp: SeqT<'``Monad<bool>``, 'T>) (handler : exn -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
          // Note: this is put outside the object deliberately, so the object doesn't permanently capture inp1 and inp2
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let state = ref (TryWithState.NotStarted inp)
                    let current = ref Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."
                        member x.MoveNext () =
                            innerMonad2<_, '``Monad<unit>``> () {
                              match !state with
                              | TryWithState.NotStarted inp ->
                                  let res = ref Unchecked.defaultof<_>
                                  try
                                      res := Choice1Of2 ((inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ())
                                  with exn ->
                                      res := Choice2Of2 exn
                                  match res.Value with
                                  | Choice1Of2 r ->
                                      return!
                                        (state := TryWithState.HaveBodyEnumerator r
                                         x.MoveNext ())
                                  | Choice2Of2 exn ->
                                      return!
                                         (x.Dispose ()
                                          let enum = (handler exn :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                          state := TryWithState.HaveHandlerEnumerator enum
                                          x.MoveNext ())
                              | TryWithState.HaveBodyEnumerator e ->
                                  let res = ref Unchecked.defaultof<Choice<bool, exn>>
                                  try
                                      let! r = e.MoveNext ()
                                      res := Choice1Of2 r
                                  with exn ->
                                      res := Choice2Of2 exn
                                  match res.Value with
                                  | Choice1Of2 res ->
                                      return
                                          (match res with
                                           | false -> x.Dispose ()
                                           | true  -> current := Some e.Current
                                           res)
                                  | Choice2Of2 exn ->
                                      return! (
                                         x.Dispose ()
                                         let e = (handler exn :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                         state := TryWithState.HaveHandlerEnumerator e
                                         x.MoveNext ())
                              | TryWithState.HaveHandlerEnumerator e ->
                                  let! res = e.MoveNext()
                                  return (
                                      if res then
                                          current := Some e.Current
                                          true
                                      else
                                          x.Dispose ()
                                          false)
                              | _ ->
                                  return false }
                                
                          member _.Dispose () =
                              match !state with
                              | TryWithState.HaveBodyEnumerator e | TryWithState.HaveHandlerEnumerator e ->
                                  state := TryWithState.Finished
                                  dispose e
                              | _ -> () } }


    [<RequireQualifiedAccess>]
    type TryFinallyState<'``Monad<bool>``, 'T> =
       | NotStarted    of SeqT<'``Monad<bool>``, 'T>
       | HaveBodyEnumerator of IEnumeratorM<'``Monad<bool>``, 'T>
       | Finished

    // This pushes the handler through all the async computations
    // The (synchronous) compensation is run when the Dispose () is called
    let inline tryFinally (inp: SeqT<'``Monad<bool>``, 'T>) (compensation : unit -> unit) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
          { new IEnumerableM<'``Monad<bool>``, 'T> with
                member _.GetEnumerator () =
                    let state = ref (TryFinallyState.NotStarted inp)
                    let current = ref Option<'T>.None
                    { new IEnumeratorM<'``Monad<bool>``, 'T> with
                        member _.Current =
                            match !current with
                            | Some c -> c
                            | None -> invalidOp "Enumeration has not started. Call MoveNext."
                        member x.MoveNext () =
                              innerMonad {
                                  match !state with
                                      | TryFinallyState.NotStarted inp ->
                                          return! (
                                              let e = (inp :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                                              state := TryFinallyState.HaveBodyEnumerator e
                                              x.MoveNext ())
                                      | TryFinallyState.HaveBodyEnumerator e ->
                                          let! (res: bool) = e.MoveNext ()
                                          return (
                                              if res then current := Some e.Current
                                              else x.Dispose ()
                                              res)
                                      | _ ->
                                          return false }
                          member _.Dispose () =
                              match !state with
                              | TryFinallyState.HaveBodyEnumerator e ->
                                  state := TryFinallyState.Finished
                                  dispose e
                                  compensation ()
                              | _ -> () } }

    let inline unfold (f: 'State -> '``Monad<('T * 'State) option>``) (s: 'State) : SeqT<'``Monad<bool>``, 'T> =
        SeqT
            { new IEnumerableM<'``Monad<bool>``, 'T> with
                  member _.GetEnumerator () =
                      let stateStarted = ref false
                      let currentState = ref s
                      let current = ref Option<'T>.None
                      { new IEnumeratorM<'``Monad<bool>``, 'T>  with
                          member _.Current =
                              match !current, !stateStarted with
                              | Some c, true -> c
                              | _     , false -> invalidOp "Enumeration has not started. Call MoveNext."
                              | None  , true  -> invalidOp "Enumeration finished."
                          member x.MoveNext () =
                                monad' {
                                    if not stateStarted.Value then
                                        stateStarted := true
                                        return! x.MoveNext ()
                                    else
                                        let! res = f currentState.Value
                                        match res with
                                        | None -> return false
                                        | Some (t, newState) ->
                                            current := Some t
                                            currentState := newState
                                            return true }
                          member _.Dispose () = () } }


type [<AutoOpen>]SeqTOperations =
    static member inline SeqT (source: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.wrap source

module [<AutoOpen>]SeqTOperations =
    let inline seqT<'T, .. > (source: '``Monad<seq<'T>>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.wrap source


type SeqT<'``monad<bool>``, 'T> with
    
    static member inline Return (x: 'T) : SeqT<'``Monad<bool>``, 'T> = SeqT.singleton x
    static member inline Map   (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> 'U) : SeqT<'``Monad<bool>``, 'U> = SeqT.map f x
    static member inline (<!>) (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> 'U) : SeqT<'``Monad<bool>``, 'U> = SeqT.map f x
    static member inline (<*>) (f: SeqT<'``Monad<bool>``, ('T -> 'U)>, x: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> = SeqT.apply f x

    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the first argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline ( *>) (x: SeqT<'``Monad<bool>``, 'T>, y: SeqT<'``Monad<bool>``, 'U>) : SeqT<'``Monad<bool>``, 'U> =
        let (<!>) = SeqT.map
        let (<*>) = SeqT.apply
        ((fun (_: 'T) (k: 'U) -> k) <!> x: SeqT<'``Monad<bool>``, ('U -> 'U)>) <*> y
    
    /// <summary>
    /// Sequences two lists left-to-right, discarding the value of the second argument.
    /// </summary>
    /// <category index="2">Applicative</category>
    static member inline (<* ) (x: SeqT<'``Monad<bool>``, 'U>, y: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'U> =
        let (<!>) = SeqT.map
        let (<*>) = SeqT.apply
        ((fun (k: 'U) (_: 'T) -> k) <!> x: SeqT<'``Monad<bool>``, ('T -> 'U)>) <*> y

    static member inline (>>=) (x: SeqT<'``Monad<bool>``, 'T>, f: 'T -> SeqT<'``Monad<bool>``, 'U>) : SeqT<'``Monad<bool>``, 'U> = SeqT.collect f x
    static member inline get_Empty () : SeqT<'``Monad<bool>``, 'T> = SeqT.empty
    static member inline (<|>) (x, y) : SeqT<'``Monad<bool>``, 'T> = SeqT.append x y

    static member inline Lift2 (f: 'T1 -> 'T2 -> 'U, x1: SeqT<'``Monad<bool>``, 'T1>, x2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> = SeqT.lift2 f x1 x2
    static member inline Lift3 (f: 'T1 -> 'T2 -> 'T3 -> 'U, x1: SeqT<'``Monad<bool>``, 'T1>, x2: SeqT<'``Monad<bool>``, 'T2>, x3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> = SeqT.lift3 f x1 x2 x3

    static member inline TryWith (source: SeqT<'``Monad<bool>``, 'T>, f: exn -> SeqT<'``Monad<bool>``, 'T>) = SeqT.tryWith<_, _, '``Monad<unit>``, _> source f
    static member inline TryFinally (computation: SeqT<'``Monad<bool>``, 'T>, f) = SeqT.tryFinally computation f
    static member inline Delay (body: unit -> SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> = SeqT.delay body
    static member inline Using (resource, f: _ -> SeqT<'``Monad<bool>``, 'T>) =
        SeqT.tryFinally (f resource) (fun () -> if box resource <> null then dispose resource)

    static member inline Lift (m: '``Monad<'T>``) : SeqT<'``Monad<bool>``, 'T> = SeqT.lift m
    static member inline LiftAsync (x: Async<'T>) = SeqT.lift (liftAsync x: '``MonadAsync<'T>``) : SeqT<'MonadAsync, 'T>

    static member inline Throw (x: 'E) : SeqT<'``MonadError<'E>``, 'T> = x |> throw |> SeqT.lift
    static member inline Catch (m: SeqT<'``MonadError<'E1>``, 'T>, h: 'E1 -> SeqT<'``MonadError<'E2>``, 'T>) : SeqT<'``MonadError<'E2>``, 'T> =
        seqT (
            (fun v h -> Catch.Invoke v h)
                (SeqT.run m)
                (SeqT.run << h))
    
    static member inline CallCC (f: (('T -> SeqT<'``MonadCont<'R>``, 'U>) -> _)) : SeqT<'``MonadCont<'R>``, 'T> =
        seqT (callCC <| fun c -> SeqT.run (f (seqT << c << Seq.singleton)))
    
    static member inline get_Get ()  : SeqT<'``MonadState<'S>``, 'S> = SeqT.lift get
    static member inline Put (x: 'T) : SeqT<'``MonadState<unit>``, 'S> = x |> put |> SeqT.lift
    
    static member inline get_Ask () : SeqT<'``MonadReader<'R>``, 'R> = SeqT.lift ask
    static member inline Local (m: SeqT<'``MonadReader<'R2>``, 'T>, f: 'R1 -> 'R2) : SeqT<'``MonadReader<'R1>``, 'T> =
        seqT (local f (SeqT.run m))
    
[<AutoOpen>]
module Extension =
    module SeqT =
        let inline take count (source: SeqT<'``Monad<bool>``, 'T>) : SeqT<'``Monad<bool>``, 'T> =
            if (count < 0) then invalidArg "count" "must be non-negative"
            monad.plus {
                use ie = (source :> IEnumerableM<'``Monad<bool>``, 'T>).GetEnumerator ()
                let n = ref count
                if n.Value > 0 then
                    let! (move: bool) = SeqT.lift (ie.MoveNext ())
                    let b = ref move
                    while b.Value do
                        yield ie.Current
                        n := n.Value - 1
                        if n.Value > 0 then
                            let! moven = SeqT.lift (ie.MoveNext ())
                            b := moven
                        else b := false }

        let inline map2M (f: 'T1 -> 'T2 -> '``Monad<'U>``) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> = monad.plus {
            use ie1 = (source1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
            use ie2 = (source2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
            let! move1 = SeqT.lift (ie1.MoveNext () : '``Monad<bool>``)
            let! move2 = SeqT.lift (ie2.MoveNext () : '``Monad<bool>``)
            let b1 = ref move1
            let b2 = ref move2
            while b1.Value && b2.Value do
                let! res = SeqT.lift (f ie1.Current ie2.Current)
                yield res
                let! move1n = SeqT.lift (ie1.MoveNext ())
                let! move2n = SeqT.lift (ie2.MoveNext ())
                b1 := move1n
                b2 := move2n }

        let inline map2 (f: 'T1 -> 'T2 -> 'U) (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, 'U> = monad.plus {
            use ie1 = (source1 :> IEnumerableM<'``Monad<bool>``, 'T1>).GetEnumerator ()
            use ie2 = (source2 :> IEnumerableM<'``Monad<bool>``, 'T2>).GetEnumerator ()
            let! move1 = SeqT.lift (ie1.MoveNext ())
            let! move2 = SeqT.lift (ie2.MoveNext ())
            let b1 = ref move1
            let b2 = ref move2
            while b1.Value && b2.Value do
                yield f ie1.Current ie2.Current
                let! move1n = SeqT.lift (ie1.MoveNext ())
                let! move2n = SeqT.lift (ie2.MoveNext ())
                b1 := move1n
                b2 := move2n }

        let inline zip (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, ('T1 * 'T2)> =
            map2 tuple2 source1 source2

        let inline map3<'T1, 'T2, 'T3, 'U, .. > (f: 'T1 -> 'T2 -> 'T3 -> 'U) (x1: SeqT<'``Monad<bool>``, 'T1>) (x2: SeqT<'``Monad<bool>``, 'T2>) (x3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, 'U> =
            map2 (<|) (map2 f x1 x2) x3

        let inline zip3 (source1: SeqT<'``Monad<bool>``, 'T1>) (source2: SeqT<'``Monad<bool>``, 'T2>) (source3: SeqT<'``Monad<bool>``, 'T3>) : SeqT<'``Monad<bool>``, ('T1 * 'T2 * 'T3)> =
            map3 tuple3 source1 source2 source3

type SeqT<'``monad<bool>``, 'T> with
    static member inline Take (source: SeqT<'``Monad<bool>``, 'T>, count, _: Take) : SeqT<'``Monad<bool>``, 'T> = SeqT.take count source
    static member inline Zip (source1: SeqT<'``Monad<bool>``, 'T1>, source2: SeqT<'``Monad<bool>``, 'T2>) : SeqT<'``Monad<bool>``, ('T1 * 'T2)> = SeqT.zip source1 source2

#endif