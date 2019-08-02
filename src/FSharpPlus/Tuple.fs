namespace FSharpPlus.Control

open System
open System.Text
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FSharpPlus.Internals


type Item1 = static member inline Invoke value = (^t : (member Item1 : _) value)
type Item2 = static member inline Invoke value = (^t : (member Item2 : _) value)
type Item3 = static member inline Invoke value = (^t : (member Item3 : _) value)
type Item4 = static member inline Invoke value = (^t : (member Item4 : _) value)
type Item5 = static member inline Invoke value = (^t : (member Item5 : _) value)


type MapItem1 =   
    static member inline MapItem1 (t: 't, fn) =
        let xr = (^t : (member Rest : 'tr) t)
        let x7 = (^t : (member Item7: 't7) t)
        let x6 = (^t : (member Item6: 't6) t)
        let x5 = (^t : (member Item5: 't5) t)
        let x4 = (^t : (member Item4: 't4) t)
        let x3 = (^t : (member Item3: 't3) t)
        let x2 = (^t : (member Item2: 't2) t)
        let x1 = (^t : (member Item1: 't1) t)
        Tuple<_,_,_,_,_,_,_,_> (fn x1, x2, x3, x4, x5, x6, x7, xr)

    static member MapItem1 ( x: Tuple<_>         , fn) = Tuple<_> (fn x.Item1)
    static member MapItem1 ((a, b)               , fn) = (fn a, b)
    static member MapItem1 ((a, b, c)            , fn) = (fn a, b, c)
    static member MapItem1 ((a, b, c, d)         , fn) = (fn a, b, c, d)
    static member MapItem1 ((a, b, c, d, e)      , fn) = (fn a, b, c, d, e)
    static member MapItem1 ((a, b, c, d, e, f)   , fn) = (fn a, b, c, d, e, f)
    static member MapItem1 ((a, b, c, d, e, f, g), fn) = (fn a, b, c, d, e, f, g)

    static member inline Invoke f value = 
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member MapItem1 : _ * _ -> _) b, f)
        let inline call   (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem1>, value)

type MapItem2 =   
    static member inline MapItem2 (t: 't, fn) =
        let xr = (^t : (member Rest : 'tr) t)
        let x7 = (^t : (member Item7: 't7) t)
        let x6 = (^t : (member Item6: 't6) t)
        let x5 = (^t : (member Item5: 't5) t)
        let x4 = (^t : (member Item4: 't4) t)
        let x3 = (^t : (member Item3: 't3) t)
        let x2 = (^t : (member Item2: 't2) t)
        let x1 = (^t : (member Item1: 't1) t)
        Tuple<_,_,_,_,_,_,_,_> (x1, fn x2, x3, x4, x5, x6, x7, xr)

    static member MapItem2 ( x: Id<_>            , fn) = Id<_> (fn x.getValue)
    static member MapItem2 ((a, b)               , fn) = (a, fn b)
    static member MapItem2 ((a, b, c)            , fn) = (a, fn b, c)
    static member MapItem2 ((a, b, c, d)         , fn) = (a, fn b, c, d)
    static member MapItem2 ((a, b, c, d, e)      , fn) = (a, fn b, c, d, e)
    static member MapItem2 ((a, b, c, d, e, f)   , fn) = (a, fn b, c, d, e, f)
    static member MapItem2 ((a, b, c, d, e, f, g), fn) = (a, fn b, c, d, e, f, g)

    static member inline Invoke f value = 
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member MapItem2 : _ * _ -> _) b, f)
        let inline call   (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem2>, value)

type MapItem3 =   
    static member inline MapItem3 (t: 't, fn) =
        let xr = (^t : (member Rest : 'tr) t)
        let x7 = (^t : (member Item7: 't7) t)
        let x6 = (^t : (member Item6: 't6) t)
        let x5 = (^t : (member Item5: 't5) t)
        let x4 = (^t : (member Item4: 't4) t)
        let x3 = (^t : (member Item3: 't3) t)
        let x2 = (^t : (member Item2: 't2) t)
        let x1 = (^t : (member Item1: 't1) t)
        Tuple<_,_,_,_,_,_,_,_> (x1, x2, fn x3, x4, x5, x6, x7, xr)

    static member MapItem3 ( x: Id<_>            , fn) = Id<_> (fn x.getValue)
    static member MapItem3 ((a, b, c)            , fn) = (a, b, fn c)
    static member MapItem3 ((a, b, c, d)         , fn) = (a, b, fn c, d)
    static member MapItem3 ((a, b, c, d, e)      , fn) = (a, b, fn c, d, e)
    static member MapItem3 ((a, b, c, d, e, f)   , fn) = (a, b, fn c, d, e, f)
    static member MapItem3 ((a, b, c, d, e, f, g), fn) = (a, b, fn c, d, e, f, g)

    static member inline Invoke f value = 
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member MapItem3 : _ * _ -> _) b, f)
        let inline call   (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem3>, value)

type MapItem4 =   
    static member inline MapItem4 (t: 't, fn) =
        let xr = (^t : (member Rest  : 'tr) t)
        let x7 = (^t : (member Item7 : 't7) t)
        let x6 = (^t : (member Item6 : 't6) t)
        let x5 = (^t : (member Item5 : 't5) t)
        let x4 = (^t : (member Item4 : 't4) t)
        let x3 = (^t : (member Item3 : 't3) t)
        let x2 = (^t : (member Item2 : 't2) t)
        let x1 = (^t : (member Item1 : 't1) t)
        Tuple<_,_,_,_,_,_,_,_> (x1, x2, x3, fn x4, x5, x6, x7, xr)

    static member MapItem4 ( x: Id<_>            , fn) = Id<_> (fn x.getValue)
    static member MapItem4 ((a, b, c, d)         , fn) = (a, b, c, fn d)
    static member MapItem4 ((a, b, c, d, e)      , fn) = (a, b, c, fn d, e)
    static member MapItem4 ((a, b, c, d, e, f)   , fn) = (a, b, c, fn d, e, f)
    static member MapItem4 ((a, b, c, d, e, f, g), fn) = (a, b, c, fn d, e, f, g)

    static member inline Invoke f value = 
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member MapItem4 : _ * _ -> _) b, f)
        let inline call   (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem4>, value)

type MapItem5 =   
    static member inline MapItem5 (t: 't, fn) =
        let xr = (^t : (member Rest  : 'tr) t)
        let x7 = (^t : (member Item7 : 't7) t)
        let x6 = (^t : (member Item6 : 't6) t)
        let x5 = (^t : (member Item5 : 't5) t)
        let x4 = (^t : (member Item4 : 't4) t)
        let x3 = (^t : (member Item3 : 't3) t)
        let x2 = (^t : (member Item2 : 't2) t)
        let x1 = (^t : (member Item1 : 't1) t)
        Tuple<_,_,_,_,_,_,_,_> (x1, x2, x3, x4, fn x5, x6, x7, xr)

    static member MapItem5 ( x: Id<_>            , fn) = Id<_> (fn x.getValue)
    static member MapItem5 ((a, b, c, d, e)      , fn) = (a, b, c, d, fn e)
    static member MapItem5 ((a, b, c, d, e, f)   , fn) = (a, b, c, d, fn e, f)
    static member MapItem5 ((a, b, c, d, e, f, g), fn) = (a, b, c, d, fn e, f, g)

    static member inline Invoke f value = 
        let inline call_2 (_: ^a, b: ^b) = ((^a or ^b) : (static member MapItem5 : _ * _ -> _) b, f)
        let inline call   (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem5>, value)


open FSharpPlus.Internals.Prelude

type Curry =
    static member inline Invoke f =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Curry: _*_ -> _) b, a)
        call_2 (Unchecked.defaultof<Curry>, Unchecked.defaultof<'t>) (f: 't -> 'r) : 'args

    static member inline Curry (t: 't, _: Curry) = fun f t1 t2 t3 t4 t5 t6 t7 ->
        Curry.Invoke (fun tr ->
            let _f _ = Constraints.whenNestedTuple t : ('t1*'t2*'t3*'t4*'t5*'t6*'t7*'tr)
            f (Tuple<'t1,'t2,'t3,'t4,'t5,'t6,'t7,'tr> (t1, t2, t3, t4, t5, t6, t7, tr) |> retype))

    static member Curry (_: Tuple<'t1>        , _: Curry) = fun f t1                   -> f (Tuple<_> t1)
    static member Curry ((_, _)               , _: Curry) = fun f t1 t2                -> f (t1, t2)
    static member Curry ((_, _, _)            , _: Curry) = fun f t1 t2 t3             -> f (t1, t2, t3)
    static member Curry ((_, _, _, _)         , _: Curry) = fun f t1 t2 t3 t4          -> f (t1, t2, t3, t4)
    static member Curry ((_, _, _, _, _)      , _: Curry) = fun f t1 t2 t3 t4 t5       -> f (t1, t2, t3, t4, t5)
    static member Curry ((_, _, _, _, _, _)   , _: Curry) = fun f t1 t2 t3 t4 t5 t6    -> f (t1, t2, t3, t4, t5, t6)
    static member Curry ((_, _, _, _, _, _, _), _: Curry) = fun f t1 t2 t3 t4 t5 t6 t7 -> f (t1, t2, t3, t4, t5, t6, t7)

type Uncurry =
    static member inline Invoke f t =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Uncurry: _*_ -> _) b, a) f
        call_2 (Unchecked.defaultof<Uncurry>, t) : 'r

    static member inline Uncurry (t: 't, _: Uncurry) = fun f  ->
        let (tr: 'tr) = (^t : (member Rest  : 'tr) t)
        let (t7: 't7) = (^t : (member Item7 : 't7) t)
        let (t6: 't6) = (^t : (member Item6 : 't6) t)
        let (t5: 't5) = (^t : (member Item5 : 't5) t)
        let (t4: 't4) = (^t : (member Item4 : 't4) t)
        let (t3: 't3) = (^t : (member Item3 : 't3) t)
        let (t2: 't2) = (^t : (member Item2 : 't2) t)
        let (t1: 't1) = (^t : (member Item1 : 't1) t)
        Uncurry.Invoke (f t1 t2 t3 t4 t5 t6 t7) tr

    static member Uncurry (x: Tuple<'t1>               , _: Uncurry) = fun f -> f x.Item1
    static member Uncurry ((t1, t2)                    , _: Uncurry) = fun f -> f t1 t2
    static member Uncurry ((t1, t2, t3)                , _: Uncurry) = fun f -> f t1 t2 t3
    static member Uncurry ((t1, t2, t3, t4)            , _: Uncurry) = fun f -> f t1 t2 t3 t4
    static member Uncurry ((t1, t2, t3, t4, t5)        , _: Uncurry) = fun f -> f t1 t2 t3 t4 t5
    static member Uncurry ((t1, t2, t3, t4, t5, t6)    , _: Uncurry) = fun f -> f t1 t2 t3 t4 t5 t6
    static member Uncurry ((t1, t2, t3, t4, t5, t6, t7), _: Uncurry) = fun f -> f t1 t2 t3 t4 t5 t6 t7