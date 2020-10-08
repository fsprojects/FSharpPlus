namespace FSharpPlus.Control

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.FSharp.Quotations

open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus
open FSharpPlus.Data

#if !FABLE_COMPILER

// Functor class ----------------------------------------------------------

type Iterate =
    static member Iterate (x: Lazy<'T>   , action) = action x.Value : unit
    static member Iterate (x: seq<'T>    , action) = Seq.iter action x
    static member Iterate (x: option<'T> , action) = match x with Some x -> action x | _ -> ()
    static member Iterate (x: list<'T>   , action) = List.iter action x
    static member Iterate ((_: 'W, a: 'T), action) = action a :unit
    static member Iterate (x: 'T []      , action) = Array.iter   action x
    static member Iterate (x: 'T [,]     , action) = Array2D.iter action x
    static member Iterate (x: 'T [,,]    , action) = Array3D.iter action x
    static member Iterate (x: 'T [,,,]   , action) =
                    for i = 0 to Array4D.length1 x - 1 do
                        for j = 0 to Array4D.length2 x - 1 do
                            for k = 0 to Array4D.length3 x - 1 do
                                for l = 0 to Array4D.length4 x - 1 do
                                    action x.[i,j,k,l]

    static member Iterate (x: Async<'T>            , action) = action (Async.RunSynchronously x) : unit
    static member Iterate (x: Result<'T, 'E>       , action) = match x with Ok x         -> action x | _ -> ()
    static member Iterate (x: Choice<'T, 'E>       , action) = match x with Choice1Of2 x -> action x | _ -> ()
    static member Iterate (KeyValue(_: 'Key, x: 'T), action) = action x : unit
    static member Iterate (x: Map<'Key,'T>         , action) = Map.iter (const' action) x 
    static member Iterate (x: Dictionary<'Key, 'T> , action) = Seq.iter action x.Values
    static member Iterate (x: _ ResizeArray        , action) = Seq.iter action x

    // Restricted
    static member Iterate (x:string         , action) = String.iter action x
    static member Iterate (x:StringBuilder  , action) = String.iter action (string x)
    static member Iterate (x:Set<'T>        , action) = Set.iter action x

    static member inline Invoke (action: 'T->unit) (source: '``Functor<'T>``) : unit =
        let inline call (_: ^M, source: ^I) =  ((^M or ^I) : (static member Iterate : _*_ -> _) source, action)
        call (Unchecked.defaultof<Iterate>, source)

#endif

type Map =
    inherit Default1

#if !FABLE_COMPILER
    static member Map ((x: Lazy<_>             , f: 'T->'U), _mthd: Map) = Lazy.map f x
    static member Map ((x: Task<'T>            , f: 'T->'U), _mthd: Map) = Task.map f x : Task<'U>
    static member Map ((x: option<_>           , f: 'T->'U), _mthd: Map) = Option.map  f x
    static member Map ((x: list<_>             , f: 'T->'U), _mthd: Map) = List.map    f x : list<'U>
    static member Map ((g: 'R->'T              , f: 'T->'U), _mthd: Map) = (>>) g f
    static member Map ((g: Func<'R, 'T>        , f: 'T->'U), _mthd: Map) = Func<'R, 'U> (g.Invoke >> f)
    static member Map (((m: 'Monoid, a)        , f: 'T->'U), _mthd: Map) = (m, f a)
    static member Map ((x: _ []                , f: 'T->'U), _mthd: Map) = Array.map   f x
    static member Map ((x: _ [,]               , f: 'T->'U), _mthd: Map) = Array2D.map f x
    static member Map ((x: _ [,,]              , f: 'T->'U), _mthd: Map) = Array3D.map f x
    static member Map ((x: _ [,,,]             , f: 'T->'U), _mthd: Map) = Array4D.init (x.GetLength 0) (x.GetLength 1) (x.GetLength 2) (x.GetLength 3) (fun a b c d -> f x.[a,b,c,d])
    static member Map ((x: Async<_>            , f: 'T->'U), _mthd: Map) = async.Bind (x, async.Return << f)
    static member Map ((x: Result<_,'E>        , f: 'T->'U), _mthd: Map) = Result.map f x
    static member Map ((x: Choice<_,'E>        , f: 'T->'U), _mthd: Map) = Choice.map f x
    static member Map ((KeyValue(k, x)         , f: 'T->'U), _mthd: Map) = KeyValuePair (k, f x)
    static member Map ((x: Map<'Key,'T>        , f: 'T->'U), _mthd: Map) = Map.map (const' f) x : Map<'Key,'U>
    static member Map ((x: Dictionary<_,_>     , f: 'T->'U), _mthd: Map) = Dictionary.map f x : Dictionary<'Key,'U>
    static member Map ((x: Expr<'T>            , f: 'T->'U), _mthd: Map) = Expr.Cast<'U> (Expr.Application (Expr.Value (f), x))
    static member Map ((x: ResizeArray<'T>     , f: 'T->'U), _mthd: Map) = ResizeArray.map f x

    // Restricted
    static member Map ((x: string              , f        ), _mthd: Map) = String.map f x
    static member Map ((x: StringBuilder       , f        ), _mthd: Map) = new StringBuilder (String.map f (string x))
    static member Map ((x: Set<_>              , f        ), _mthd: Map) = Set.map f x
    static member Map ((_: Set2<'T>            , _: 'T->'U), _mthd: Map) = Set2<'U>()


    static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

#endif

    static member inline InvokeOnInstance (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` = 
        (^``Functor<'T>`` : (static member Map : _ * _ -> _) source, mapping)

#if !FABLE_COMPILER


type Map with
    static member inline Map ((x: '``Monad<'T>``       when '``Monad<'T>`` : (static member (>>=)  : '``Monad<'T>`` * ('T -> '``Monad<'U>``) -> '``Monad<'U>``)
                                                       and  '``Monad<'U>`` : (static member Return : 'U -> '``Monad<'U>``)
                                                          , f: 'T->'U), [<Optional>]_mthd: Default4) = Bind.InvokeOnInstance x (f >> Return.InvokeOnInstance) : '``Monad<'U>``

    static member inline Map ((x: '``Applicative<'T>`` when '``Applicative<'T>``     : (static member (<*>)  : '``Applicative<'T->'U>`` * '``Applicative<'T>`` -> '``Applicative<'U>``)
                                                       and  '``Applicative<'T->'U>`` : (static member Return : ('T -> 'U) -> '``Applicative<'T->'U>``)
                                                          , f: 'T->'U), [<Optional>]_mthd: Default3) = Apply.InvokeOnInstance (Return.InvokeOnInstance f: '``Applicative<'T->'U>``) x : '``Applicative<'U>``

    static member        Map ((x: seq<_>                  , f: 'T->'U), _mthd: Default2) = Seq.map f x              : seq<'U>
    static member        Map ((x: NonEmptySeq<_>          , f: 'T->'U), _mthd: Default2) = NonEmptySeq.map f x      : NonEmptySeq<'U>
    static member        Map ((x: IEnumerator<_>          , f: 'T->'U), _mthd: Default2) = Enumerator.map f x       : IEnumerator<'U>
    static member        Map ((x: IDictionary<_,_>        , f: 'T->'U), _mthd: Default2) = let d = Dictionary () in Seq.iter (fun (KeyValue(k, v)) -> d.Add (k, f v)) x; d :> IDictionary<'Key,'U>
    static member        Map ((x: IReadOnlyDictionary<_,_>, f: 'T->'U), _mthd: Default2) = IReadOnlyDictionary.map f x : IReadOnlyDictionary<'Key,_>
    static member        Map ((x: IObservable<'T>         , f: 'T->'U), _mthd: Default2) = Observable.map f x       : IObservable<'U>
    static member        Map ((x: Nullable<_>             , f: 'T->'U), _mthd: Default2) = Nullable.map f x         : Nullable<'U>
    static member        Map ((x: IReadOnlyCollection<'T> , f: 'T->'U), _mthd: Default1) = IReadOnlyCollection.map f x : IReadOnlyCollection<'U>
    static member inline Map ((x: '``Functor<'T>``        , f: 'T->'U), _mthd: Default1) = Map.InvokeOnInstance f x : '``Functor<'U>``
    static member inline Map ((_: ^t when ^t: null and ^t: struct, _ ), _mthd: Default1) = ()


type Unzip =
    inherit Default1
    static member inline Unzip ((source: '``Functor<'T * 'U>``             , _output: '``Functor<'T>`` * '``Functor<'U>``                  ) , _mthd: Default2) = Map.InvokeOnInstance fst source, Map.Invoke snd source : '``Functor<'T>`` * '``Functor<'U>``
    static member inline Unzip ((source: '``Functor<'T * 'U>``             , _output: '``Functor<'T>`` * '``Functor<'U>``                  ) , _mthd: Default1) = (^``Functor<'T * 'U>``: (static member Unzip : _->_) source) : '``Functor<'T>`` * '``Functor<'U>``
    static member inline Unzip (( _    : ^t when ^t: null and ^t: struct   , _                                                             ) , _              ) = ()
    
    static member        Unzip ((source: Lazy<'T * 'U>                     , _output: Lazy<'T> * Lazy<'U>                                  ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source

    static member        Unzip ((source: Task<'T * 'U>                     , _output: Task<'T> * Task<'U>                                  ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: option<'T * 'U>                   , _output: option<'T> * option<'U>                              ) , _mthd: Unzip   ) = Option.unzip source

    static member        Unzip ((source: list<'T * 'U>                     , _output: list<'T> * list<'U>                                  ) , _mthd: Unzip   ) = List.unzip   source
    static member        Unzip ((source: 'R -> ('T * 'U)                   , _output: ('R -> 'T) * ('R -> 'U)                              ) , _mthd: Unzip   ) = (fun x -> fst (source x)), (fun x -> snd (source x))
    static member        Unzip ((source: Func<'R, ('T * 'U)>               , _output: Func<'R,'T> * Func<'R,'U>                            ) , _mthd: Unzip   ) = Func<_,_> (fun x -> fst (source.Invoke x)), Func<_,_> (fun x -> snd (source.Invoke x))
    static member        Unzip (((m: 'Monoid, t: ('T * 'U))                , _output: ('Monoid * 'T) * ('Monoid * 'U)                      ) , _mthd: Unzip   ) = (m, fst t), (m, snd t)
    static member        Unzip ((source: ('T * 'U) []                      , _output: 'T []    * 'U []                                     ) , _mthd: Unzip   ) = Array.unzip  source
    
    static member        Unzip ((source: ('T * 'U) [,]                     , _output: 'T [,]   * 'U [,]                                    ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: ('T * 'U) [,,]                    , _output: 'T [,,]  * 'U [,,]                                   ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: ('T * 'U) [,,,]                   , _output: 'T [,,,] * 'U [,,,]                                  ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source

    static member        Unzip ((source: Async<'T * 'U>                    , _output: Async<'T> * Async<'U>                                ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: Result<'T * 'U, 'E>               , _output: Result<'T,'E> * Result<'U,'E>                        ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: Choice<'T * 'U, 'E>               , _output: Choice<'T,'E> * Choice<'U,'E>                        ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: KeyValuePair<'Key, 'T * 'U>       , _output: KeyValuePair<_, 'T> * KeyValuePair<_, 'U>            ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: Map<'Key, 'T * 'U>                , _output: Map<_, 'T> * Map<_, 'U>                              ) , _mthd: Unzip   ) = Map.unzip    source
    static member        Unzip ((source: Dictionary<'Key, 'T * 'U>         , _output: Dictionary<_, 'T> * Dictionary<_, 'U>                ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source

    static member        Unzip ((source: Expr<'T * 'U>                     , _output: Expr<'T> * Expr<'U>                                  ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source

    static member        Unzip ((source: ResizeArray<'T * 'U>              , _output: ResizeArray<'T> * ResizeArray<'U>                    ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    
    static member        Unzip ((source: seq<'T * 'U>                      , _output: seq<'T> * seq<'U>                                    ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    static member        Unzip ((source: NonEmptySeq<'T * 'U>                      , _output: NonEmptySeq<'T> * NonEmptySeq<'U>            ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    
    static member        Unzip ((source: IEnumerator<'T * 'U>              , _output: IEnumerator<'T> * ResizeArray<'U>                    ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source
    
    static member        Unzip ((source: IDictionary<'Key, 'T * 'U>        , _output: IDictionary<_,'T> * IDictionary<_,'U>                ) , _mthd: Unzip   ) = Dict.unzip source
    static member        Unzip ((source: IReadOnlyDictionary<'Key,'T * 'U> , _output: IReadOnlyDictionary<_,'T> * IReadOnlyDictionary<_,'U>) , _mthd: Unzip   ) = IReadOnlyDictionary.unzip source
    static member        Unzip ((source: IObservable<'T * 'U>              , _output: IObservable<'T> * ResizeArray<'U>                    ) , _mthd: Unzip   ) = Map.Invoke fst source, Map.Invoke snd source

    [<Obsolete>]static member        Unzip (source: list<'T * 'U>         , [<Optional>]_output: list<'T> * list<'U>                 , [<Optional>]_mthd:Unzip   ) = List.unzip  source
    [<Obsolete>]static member        Unzip (source: ('T * 'U) []          , [<Optional>]_output: 'T [] * 'U []                       , [<Optional>]_mthd:Unzip   ) = Array.unzip source


    static member inline Invoke (source: '``Functor<'T1 * 'T2>``) =
        let inline call_3 (a: ^a, b: ^b, d: ^d) = ((^a or ^b or ^d) : (static member Unzip : (_*_)*_ -> _) (b, d), a)
        let inline call (a: 'a, b: 'b) = call_3 (a, b, Unchecked.defaultof<'r>) : 'r
        call (Unchecked.defaultof<Unzip>, source) : '``Functor<'T1>`` * '``Functor<'T2>``


type Zip =
    inherit Default1

    static member Zip ((x: IEnumerator<'T>            , y: IEnumerator<'U>           , _output: IEnumerator<'T*'U>           ), _mthd: Zip) = Enumerator.zip x y
    static member Zip ((x: seq<'T>                    , y: seq<'U>                   , _output: seq<'T*'U>                   ), _mthd: Zip) = Seq.zip        x y
    static member Zip ((x: NonEmptySeq<'T>            , y: NonEmptySeq<'U>           , _output: NonEmptySeq<'T*'U>           ), _mthd: Zip) = NonEmptySeq.zip        x y
    static member Zip ((x: IDictionary<'K, 'T>        , y: IDictionary<'K,'U>        , _output: IDictionary<'K,'T*'U>        ), _mthd: Zip) = Dict.zip       x y
    static member Zip ((x: IReadOnlyDictionary<'K, 'T>, y: IReadOnlyDictionary<'K,'U>, _output: IReadOnlyDictionary<'K,'T*'U>), _mthd: Zip) = IReadOnlyDictionary.zip x y
    static member Zip ((x: Dictionary<'K, 'T>         , y: Dictionary<'K,'U>         , _output: Dictionary<'K,'T*'U>         ), _mthd: Zip) = Dict.zip       x y :?> Dictionary<'K,'T*'U>
    static member Zip ((x: Map<'K, 'T>                , y: Map<'K,'U>                , _output: Map<'K,'T*'U>                ), _mthd: Zip) = Map.zip        x y
    static member Zip ((f: 'R -> 'T                   , g: 'R -> 'U                  , _output: 'R -> 'T * 'U                ), _mthd: Zip) = fun x -> (f x, g x)
    static member Zip ((f: Func<'R, 'T>               , g: Func<'R, 'U>              , _output: Func<'R, 'T * 'U>            ), _mthd: Zip) = Func<_,_> (fun x -> (f.Invoke x, g.Invoke x))
    static member Zip ((x: list<'T>                   , y: list<'U>                  , _output: list<'T*'U>                  ), _mthd: Zip) = List.zip       x y
    static member Zip ((x: 'T []                      , y: 'U []                     , _output: ('T*'U) []                   ), _mthd: Zip) = Array.zip      x y
    static member Zip ((x: option<'T>                 , y: option<'U>                , _output: option<'T*'U>                ), _mthd: Zip) = Option.zip     x y
    static member Zip ((x: Async<'T>                  , y: Async<'U>                 , _output: Async<'T*'U>                 ), _mthd: Zip) = Async.zip      x y
    static member Zip ((x: Task<'T>                   , y: Task<'U>                  , _output: Task<'T*'U>                  ), _mthd: Zip) = Task.zip       x y

    static member inline Invoke (source1: '``ZipFunctor<'T1>``) (source2: '``ZipFunctor<'T2>``) =
        let inline call_4 (a: ^a, b: ^b, c: ^c, d: ^d) = ((^a or ^b or ^c or ^d) : (static member Zip : (_*_*_)*_ -> _) (b, c, d), a)
        let inline call (a: 'a, b: 'b, c: 'c) = call_4 (a, b, c, Unchecked.defaultof<'r>) : 'r
        call (Unchecked.defaultof<Zip>, source1, source2) : '``ZipFunctor<'T1 * 'T2>``

    static member inline InvokeOnInstance (source1: '``ZipFunctor<'T1>``) (source2: '``ZipFunctor<'T2>``) : '``ZipFunctor<'T1 * 'T2>`` =
        ((^``ZipFunctor<'T1>`` or ^``ZipFunctor<'T2>`` or  ^``ZipFunctor<'T1 * 'T2>``) : (static member Zip : _*_ -> _) source1, source2)

type Zip with    
    static member inline Zip ((_: ^t when ^t : null and ^t: struct, _: ^u when ^u : null and ^u: struct, _output: ^r when ^r : null and ^r: struct), _mthd: Default1) = id
    static member inline Zip ((x: '``ZipFunctor<'T1>``            , y: '``ZipFunctor<'T2>``            , _output: '``ZipFunctor<'T1 * 'T2>``      ), _mthd: Default1) = Zip.InvokeOnInstance x y : '``ZipFunctor<'T1 * 'T2>``


// Bifunctor class --------------------------------------------------------

type Bimap =
    inherit Default1
       
    static member Bimap ((x: 'T1, y: 'T2)      , f: 'T1->'U1, g: 'T2->'U2, [<Optional>]_mthd: Bimap) = (f x, g y)
    static member Bimap (x: Result<'T2, 'T1>   , f: 'T1->'U1, g: 'T2->'U2, [<Optional>]_mthd: Bimap) = Result.either (Ok << g) (Error << f) x
    static member Bimap (KeyValue(k:'T1, x:'T2), f: 'T1->'U1, g: 'T2->'U2, [<Optional>]_mthd: Bimap) = KeyValuePair (f k, g x)
    static member Bimap (x: Choice<'T2, 'T1>   , f: 'T1->'U1, g: 'T2->'U2, [<Optional>]_mthd: Bimap) = Choice.either (Choice1Of2 << g) (Choice2Of2 << f) x

    static member inline Invoke (f: 'T->'U) (g: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Bimap : _*_*_*_ -> _) source, f, g, mthd)
        call (Unchecked.defaultof<Bimap>, source, Unchecked.defaultof<'``Bifunctor<'U,'W>``>)

    static member inline InvokeOnInstance (f: 'T->'U) (g: 'V->'W) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` =
        (^``Bifunctor<'T,'V>``: (static member Bimap : _*_*_ -> _) source, f, g)


type MapFirst =
    inherit Default1

    static member First ((x: 'T1, y: 'T2)        , f: 'T1->'U1, [<Optional>]_mthd: MapFirst) = (f x, y)
    static member First (x: Result<'T2, 'T1>     , f: 'T1->'U1, [<Optional>]_mthd: MapFirst) = Result.either Ok         (Error      << f) x
    static member First (x: Choice<'T2, 'T1>     , f: 'T1->'U1, [<Optional>]_mthd: MapFirst) = Choice.either Choice1Of2 (Choice2Of2 << f) x
    static member First (KeyValue(k: 'T1, x: 'T2), f: 'T1->'U1, [<Optional>]_mthd: MapFirst) = KeyValuePair (f k, x)

    static member inline Invoke (f: 'T->'U) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member First : _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<MapFirst>, source, Unchecked.defaultof<'``Bifunctor<'U,'V>``>)

    static member inline InvokeOnInstance (f: 'T->'V) (source: '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` =
        (^``Bifunctor<'T,'V>`` : (static member First : _*_ -> _) source, f)

type MapFirst with
    static member inline First (x: '``Bifunctor<'T,'V>``, f: 'T->'U, [<Optional>]_mthd: Default2) = Bimap.InvokeOnInstance f id x  : '``Bifunctor<'U,'V>``

    static member inline First (x: '``Bifunctor<'T,'V>``, f: 'T->'U, [<Optional>]_mthd: Default1) = MapFirst.InvokeOnInstance f x  : '``Bifunctor<'U,'V>``
    static member inline First (_: ^t when ^t: null and ^t: struct, _ : 'T->'U,  _mthd: Default1) = ()


type Map with
    static member inline Map ((x: '``Bifunctor<'T,'V>``, f: 'V->'W), [<Optional>]_mthd: Default6) = Bimap.InvokeOnInstance id f x


type Bimap with
    static member inline Bimap (x: '``Bifunctor<'T,'V>``, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Default2) = x |> MapFirst.InvokeOnInstance f |> Map.InvokeOnInstance g : '``Bifunctor<'U,'W>``

    static member inline Bimap (x: '``Bifunctor<'T,'V>``, f: 'T->'U, g: 'V->'W, [<Optional>]_mthd: Default1) = Bimap.InvokeOnInstance f g x                               : '``Bifunctor<'U,'W>``
    static member inline Bimap (_: ^t when ^t: null and ^t: struct, _: 'T->'U, _: 'V->'W,   _mthd: Default1) = ()


// Profunctor class -------------------------------------------------------

type Dimap =
    inherit Default1

    static member Dimap (f             , g: 'A->'B, h: 'C->'D, [<Optional>]_mthd: Dimap) = g >> f >> h : 'A->'D
    static member Dimap (f: Func<'B,'C>, g: 'A->'B, h: 'C->'D, [<Optional>]_mthd: Dimap) = Func<'A,'D> (g >> f.Invoke >> h)
    
    static member inline Invoke (ab: 'A->'B) (cd: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Dimap : _*_*_*_ -> _) source, ab, cd, mthd)
        call (Unchecked.defaultof<Dimap>, source, Unchecked.defaultof<'``Profunctor<'A,'D>``>)

    static member inline InvokeOnInstance (ab: 'A->'B) (cd: 'C->'D) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` =
        (^``Profunctor<'B,'C>`` : (static member Dimap : _*_*_ -> _) source, ab, cd)


// Contravariant class ----------------------------------------------------

type Contramap =
    inherit Default1

    static member inline Invoke (f: 'U -> 'T) (source: '``Contravariant<'T>``) : '``Contravariant<'U>`` = 
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Contramap : _*_*_ -> _) source, f, mthd)
        call (Unchecked.defaultof<Contramap>, source, Unchecked.defaultof<'``Contravariant<'U>``>)

    static member inline InvokeOnInstance (ab: 'A->'B) (source: '``Profunctor<'B,'C>``) : '``Profunctor<'A,'C>`` =
        (^``Profunctor<'B,'C>`` : (static member Contramap : _*_ -> _) source, ab)

    static member Contramap (k: 'T -> 'C            , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = f >> k : 'U->'C
    static member Contramap (k: Func<'T, 'C>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = Func<'U, 'C> (f >> k.Invoke)
    static member Contramap (p: Predicate<_>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = Predicate (fun x -> p.Invoke (f x))    
    static member Contramap (c: IComparer<_>        , f: 'U -> 'T, [<Optional>]_mthd: Contramap) = { new IComparer<'U> with member __.Compare (x, y) = c.Compare (f x, f y) }
    static member Contramap (c: IEqualityComparer<_>, f: 'U -> 'T, [<Optional>]_mthd: Contramap) = { 
                    new IEqualityComparer<'U> with
                        member __.Equals (x, y) = c.Equals (f x, f y)
                        member __.GetHashCode x = c.GetHashCode (f x) }
    
type Contramap with
    static member inline Contramap (x: '``Profunctor<'B,'C>``, f: 'A->'B, [<Optional>]_mthd: Default2) = Dimap.InvokeOnInstance f id x : '``Profunctor<'A,'C>``
    static member inline Contramap (x: '``Contravariant<'T>``, f: 'U->'T, [<Optional>]_mthd: Default1) = Contramap.InvokeOnInstance f x: '``Contravariant<'U>``
    static member inline Contramap (_: ^t when ^t: null and ^t: struct  , _: 'A->'B,  _mthd: Default1) = ()


type Map with
    static member inline Map ((x: '``Profunctor<'B,'C>``, cd: 'C->'D), [<Optional>]_mthd: Default5) = Dimap.InvokeOnInstance id cd x : '``Profunctor<'B,'D>``


type Dimap with
    static member inline Dimap (x: '``Profunctor<'B,'C>``, ab: 'A->'B, cd: 'C->'D, [<Optional>]_mthd: Default2) = x |> Map.InvokeOnInstance cd |> Contramap.InvokeOnInstance ab : '``Profunctor<'A,'D>``
    static member inline Dimap (x: '``Profunctor<'B,'C>``, ab: 'A->'B, cd: 'C->'D, [<Optional>]_mthd: Default1) = Dimap.InvokeOnInstance ab cd x                                : '``Profunctor<'A,'D>``
    static member inline Dimap (_: ^t when ^t: null and ^t: struct,     _: 'T->'U, _: 'V->'W,  _mthd: Default1) = ()

#endif

// Invariant functor

type Invmap = static member inline Invoke (f: 'T -> 'U) (g: 'U -> 'T) (source: '``InvariantFunctor<'T>``) = (^``InvariantFunctor<'T>`` : (static member Invmap : _*_*_ -> _) source, f, g) : '``InvariantFunctor<'U>``