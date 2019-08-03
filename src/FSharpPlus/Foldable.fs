#nowarn "77"
// Warn FS0077 -> Member constraints with the name 'get_Item' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in runtime failures if you attempt to invoke the member constraint from your own code.
// Those .NET types are string and array. String is explicitely handled here and array through the seq overload.

namespace FSharpPlus.Internals

open FSharpPlus.Control


[<Struct>]
type _Dual<'T> =
    struct
        val Value : 'T
        new (value: 'T) = {Value = value}
    end
    static member inline get_Zero () = _Dual (Zero.Invoke ())                                   : _Dual<'m>
    static member inline (+) (x: _Dual<'m>, y: _Dual<'m>) = _Dual (Plus.Invoke y.Value x.Value) : _Dual<'m>

[<Struct>]
type _Endo<'T> =
    struct
        val Value : 'T -> 'T
        new (value: 'T -> 'T) = {Value = value}
    end
    static member get_Zero () = _Endo id                                        : _Endo<'m>
    static member (+) (f: _Endo<'m>, g: _Endo<'m>) = _Endo (f.Value << g.Value) : _Endo<'m>

namespace FSharpPlus.Control

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text
open System.Collections.Generic
open FSharpPlus.Internals
open FSharpPlus.Internals.Prelude
open FSharpPlus



type ToSeq =
    inherit Default1
    static member ToSeq (x: seq<'T>   , [<Optional>]_impl: ToSeq) = x
    static member ToSeq (x: Text.StringBuilder,         _: ToSeq) = string x :> seq<char>
    static member ToSeq (x: string    ,                 _: ToSeq) = String.toSeq x
    static member ToSeq (x: option<'T>, [<Optional>]_impl: ToSeq) = match x with Some x -> Seq.singleton x | _ -> Seq.empty
    static member ToSeq (x: Id<'T>    , [<Optional>]_impl: ToSeq) = Seq.singleton x.getValue

    static member inline Invoke (source: '``Foldable<'T>``) : seq<'T>  =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member ToSeq : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToSeq>, source)

    static member inline InvokeOnInstance (source: '``Foldable<'T>``) : seq<'T> = (^``Foldable<'T>``: (static member ToSeq : _ -> _) source)

type ToSeq with
    static member inline ToSeq (x: 'S when 'S :> Collections.IEnumerable, [<Optional>]_impl: Default2) = let _f i x : 'T = (^S : (member get_Item : int -> 'T) x, i) in Seq.cast<'T> x : seq<'T>
    static member inline ToSeq (x: 'Foldable                            , [<Optional>]_impl: Default1) = ToSeq.InvokeOnInstance x
    static member inline ToSeq (_: 'T when 'T: null and 'T: struct      ,                 _: Default1) = ()


type ToList =
    inherit Default1    
    static member inline ToList (x                , [<Optional>]_impl: Default3) = x |> ToSeq.Invoke |> Seq.toList
    static member        ToList (x: seq<'a>       , [<Optional>]_impl: Default2) = Seq.toList x
    static member inline ToList (x                , [<Optional>]_impl: Default1) = (^Foldable : (static member ToList : 'Foldable->list<_>) x)
    static member        ToList (x: Set<'a>       , [<Optional>]_impl: ToList  ) = Set.toList x
    static member        ToList (x: string        , [<Optional>]_impl: ToList  ) = String.toList x
    static member        ToList (x: StringBuilder , [<Optional>]_impl: ToList  ) = x.ToString().ToCharArray() |> Array.toList
    static member        ToList (x: 'a []         , [<Optional>]_impl: ToList  ) = Array.toList x
    static member        ToList (x: 'a ResizeArray, [<Optional>]_impl: ToList  ) = Seq.toList x
    static member        ToList (x: list<'a>      , [<Optional>]_impl: ToList  ) = x

    static member inline Invoke value : 't list = 
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member ToList : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToList>, value)


type ToArray =
    inherit Default1
    static member inline ToArray (x                , [<Optional>]_impl: Default3) = x |> ToSeq.Invoke |> Seq.toArray
    static member        ToArray (x: seq<'a>       , [<Optional>]_impl: Default2) = Seq.toArray x
    static member inline ToArray (x                , [<Optional>]_impl: Default1) = (^Foldable : (static member ToArray : 'Foldable->array<_>) x)
    static member        ToArray (x: Set<'a>       , [<Optional>]_impl: ToArray ) = Set.toArray x
    static member        ToArray (x: string        , [<Optional>]_impl: ToArray ) = String.toArray x
    static member        ToArray (x: StringBuilder , [<Optional>]_impl: ToArray ) = x.ToString().ToCharArray ()
    static member        ToArray (x: 'a []         , [<Optional>]_impl: ToArray ) = x
    static member        ToArray (x: 'a ResizeArray, [<Optional>]_impl: ToArray ) = Seq.toArray x
    static member        ToArray (x: list<'a>      , [<Optional>]_impl: ToArray ) = List.toArray x

    static member inline Invoke value : 't [] = 
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member ToArray : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<ToArray>, value)


type FoldBack =
    inherit Default1
    static member inline FoldBack (x: 'F           , f: 'a->'b->'b, z: 'b , [<Optional>]_impl: Default2) = List.foldBack  f (ToList.Invoke x) z
    static member inline FoldBack (x: 'F           , f: 'a->'b->'b, z: 'b , [<Optional>]_impl: Default1) = (^F : (static member FoldBack : ^F -> _ -> _-> ^b) x, f, z)
    static member        FoldBack (x: seq<_>       , f            , z     , [<Optional>]_impl: FoldBack) = List.foldBack  f (Seq.toList x) z
    static member        FoldBack (x: option<_>    , f            , z     , [<Optional>]_impl: FoldBack) = match x with Some x -> f x z | _ -> z
    static member        FoldBack (x: list<_>      , f            , z     , [<Optional>]_impl: FoldBack) = List.foldBack          f x z
    static member        FoldBack (x: _ []         , f            , z     , [<Optional>]_impl: FoldBack) = Array.foldBack         f x z
    static member        FoldBack (x: Set<_>       , f            , z     , [<Optional>]_impl: FoldBack) = Set.foldBack           f x z
    static member        FoldBack (x: _ ResizeArray, f            , z     , [<Optional>]_impl: FoldBack) = Array.foldBack         f (x.ToArray ()) z
    static member        FoldBack (x: string       , f            , z     , [<Optional>]_impl: FoldBack) = Array.foldBack f (x.ToCharArray ()) z
    static member        FoldBack (x: StringBuilder, f            , z     , [<Optional>]_impl: FoldBack) = Array.foldBack f (x.ToString().ToCharArray ()) z   
    static member        FoldBack (x: Id<'a>       , f            , z     , [<Optional>]_impl: FoldBack) = f x.getValue z

    static member inline Invoke (folder: 'T->'State->'State) (state: 'State) (foldable: '``Foldable'<T>``) : 'State =
        let inline call_2 (a: ^a, b: ^b, f, z) = ((^a or ^b) : (static member FoldBack : _*_*_*_ -> _) b, f, z, a)
        let inline call (a: 'a, b: 'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<FoldBack>, foldable, folder, state)


type FoldMap =
    inherit Default1
    static member inline FromFoldFoldBack f x = FoldBack.Invoke (Plus.Invoke << f) (Zero.Invoke ()) x
    
    static member inline FoldMap (x: option<_>, f, [<Optional>]_impl: FoldMap ) = match x with Some x -> f x | _ -> Zero.Invoke ()
    static member inline FoldMap (x: list<_>  , f, [<Optional>]_impl: FoldMap ) = List.fold  (fun x y -> Plus.Invoke x (f y)) (Zero.Invoke ()) x
    static member inline FoldMap (x: Set<_>   , f, [<Optional>]_impl: FoldMap ) = Seq.fold   (fun x y -> Plus.Invoke x (f y)) (Zero.Invoke ()) x
    static member inline FoldMap (x: _ []     , f, [<Optional>]_impl: FoldMap ) = Array.fold (fun x y -> Plus.Invoke x (f y)) (Zero.Invoke ()) x

    static member inline Invoke (f: 'T->'Monoid) (x: '``Foldable'<T>``) : 'Monoid =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member FoldMap : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<FoldMap>, x, f)

type FoldMap with
    static member inline FoldMap (x: seq<_>          , f, [<Optional>]_impl: Default2) = Seq.fold   (fun x y -> Plus.Invoke x (f y)) (Zero.Invoke ()) x
    static member inline FoldMap (x                  , f, [<Optional>]_impl: Default1) = (^F : (static member FoldMap : ^F -> _ -> _) x, f)
    static member inline FoldMap (_: ^t when  ^t: null and ^t: struct, _, _: Default1) = ()

type FoldBack with
    static member inline FromFoldMap f z x = let (f: _Endo<'t>) = FoldMap.Invoke (_Endo << f) x in f.Value z


type Fold =
    inherit Default1

    static member inline FromFoldMap f z t = let (f: _Dual<_Endo<'t>>) = FoldMap.Invoke (_Dual << _Endo << flip f) t in f.Value.Value z

    static member inline Fold (x           , f,             z,     [<Optional>]_impl: Default2) = Seq.fold f z (ToSeq.Invoke x)
    static member inline Fold (x: 'F       , f: 'b->'a->'b, z: 'b, [<Optional>]_impl: Default1) = (^F : (static member Fold : ^F -> _ -> _-> ^b) x, f, z)
    static member        Fold (x: option<_>, f,             z    , [<Optional>]_impl: Fold    ) = match x with Some x -> f z x | _ -> z
    static member        Fold (x: Id<_>    , f,             z    , [<Optional>]_impl: Fold    ) = f z x.getValue
    static member        Fold (x: seq<_>   , f,             z    , [<Optional>]_impl: Fold    ) = Seq.fold               f z x
    static member        Fold (x: list<_>  , f,             z    , [<Optional>]_impl: Fold    ) = List.fold              f z x
    static member        Fold (x: Set<_>   , f,             z    , [<Optional>]_impl: Fold    ) = Set.fold               f z x
    static member        Fold (x:  _ []    , f,             z    , [<Optional>]_impl: Fold    ) = Array.fold             f z x

    static member inline Invoke (folder: 'State->'T->'State) (state: 'State) (foldable: '``Foldable'<T>``) : 'State =
        let inline call_2 (a: ^a, b: ^b, f, z) = ((^a or ^b) : (static member Fold : _*_*_*_ -> _) b, f, z, a)
        let inline call (a: 'a, b: 'b, f, z) = call_2 (a, b, f, z)
        call (Unchecked.defaultof<Fold>, foldable, folder, state)
    
 
type Exists =
    inherit Default1
    static member inline Exists (x                   , f          , [<Optional>]_impl: Default2) = Seq.exists f (ToSeq.Invoke x) : bool
    static member inline Exists (x: '``Foldable<'T>``, f: 'T->bool, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member Exists : '``Foldable<'T>``-> _ -> bool) (x, f))
    static member        Exists (x: Id<'T>           , f          , [<Optional>]_impl: Exists  ) = f x.getValue : bool
    static member        Exists (x: list<'a>         , f          , [<Optional>]_impl: Exists  ) = List.exists   f x
    static member        Exists (x: 'a []            , f          , [<Optional>]_impl: Exists  ) = Array.exists  f x
    static member        Exists (x: Set<'a>          , f          , [<Optional>]_impl: Exists  ) = Set.exists    f x
    static member        Exists (x: 'a ResizeArray   , f          , [<Optional>]_impl: Exists  ) = Seq.exists    f x
    static member        Exists (x: string           , f          , [<Optional>]_impl: Exists  ) = String.exists f x    
    static member        Exists (x: StringBuilder    , f          , [<Optional>]_impl: Exists  ) = x |> string |> String.exists f

    static member inline Invoke (predicate: 'T->bool) (source: '``Foldable'<T>``) =
        let inline call_3 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member Exists : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, f)
        call (Unchecked.defaultof<Exists>, source, predicate) : bool
 

type ForAll =
    inherit Default1
    static member inline ForAll (x                   , f          , [<Optional>]_impl: Default2) = Seq.forall    f (ToSeq.Invoke x) : bool
    static member inline ForAll (x: '``Foldable<'T>``, f: 'T->bool, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member ForAll : '``Foldable<'T>``-> _ -> bool) (x, f))
    static member        ForAll (x: Id<'T>           , f          , [<Optional>]_impl: ForAll  ) = f x.getValue : bool
    static member        ForAll (x: list<'a>         , f          , [<Optional>]_impl: ForAll  ) = List.forall   f x
    static member        ForAll (x: 'a []            , f          , [<Optional>]_impl: ForAll  ) = Array.forall  f x
    static member        ForAll (x: Set<'a>          , f          , [<Optional>]_impl: ForAll  ) = Set.forall    f x
    static member        ForAll (x: string           , f          , [<Optional>]_impl: ForAll  ) = String.forall f x
    static member        ForAll (x: 'a ResizeArray   , f          , [<Optional>]_impl: ForAll  ) = Seq.forall    f x
    static member        ForAll (x: StringBuilder    , f          , [<Optional>]_impl: ForAll  ) = x |> string |> String.forall f

    static member inline Invoke (predicate: 'T->bool) (source: '``Foldable'<T>``) =
        let inline call_3 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member ForAll : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_3 (a, b, f)
        call (Unchecked.defaultof<ForAll>, source, predicate) : bool


type Find =
    inherit Default1
    static member inline Find (x                   , f          , [<Optional>]_impl: Default2) = Seq.find   f (ToSeq.Invoke x) : 'T
    static member inline Find (x: '``Foldable<'T>``, f: 'T->bool, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member Find : '``Foldable<'T>``-> _ -> 'T) (x, f))
    static member        Find (x: Id<'T>           , f          , [<Optional>]_impl: Find    ) = List.find  f [x.getValue]
    static member        Find (x: ResizeArray<'T>  , f          , [<Optional>]_impl: Find    ) = Seq.find   f x
    static member        Find (x: list<'T>         , f          , [<Optional>]_impl: Find    ) = List.find  f x
    static member        Find (x: 'T []            , f          , [<Optional>]_impl: Find    ) = Array.find f x

    static member inline Invoke (predicate: 'T->bool) (source: '``Foldable'<T>``) =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member Find : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, x: 'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<Find>, source, predicate) : 'T


type TryFind =
    inherit Default1
    static member inline TryFind (x          , f, [<Optional>]_impl: Default1) = Seq.tryFind   f (ToSeq.Invoke x) : 'T option
    static member        TryFind (x: Id<'T>  , f, [<Optional>]_impl: TryFind ) = List.tryFind  f [x.getValue]
    static member        TryFind (x: seq<'T> , f, [<Optional>]_impl: TryFind ) = Seq.tryFind   f x
    static member        TryFind (x: list<'T>, f, [<Optional>]_impl: TryFind ) = List.tryFind  f x
    static member        TryFind (x: 'T []   , f, [<Optional>]_impl: TryFind ) = Array.tryFind f x

    static member inline Invoke (predicate: 'T->bool) (source: '``Foldable'<T>``) =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member TryFind : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, x: 'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<TryFind>, source, predicate) : 'T option


type Head =
    inherit Default1
    static member inline Head (x: '``Foldable<'T>``, [<Optional>]_impl: Default2) = Seq.head (ToSeq.Invoke x) : 'T
    static member inline Head (x: '``Foldable<'T>``, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (member Head : 'T) x)
    static member        Head (x: 'T option        , [<Optional>]_impl: Head    ) = x.Value
    static member        Head (x: 'T []            , [<Optional>]_impl: Head    ) = x.[0]    
    static member        Head (x: Id<'T>           , [<Optional>]_impl: Head    ) = x.getValue
    static member        Head (x: ResizeArray<'T>  , [<Optional>]_impl: Head    ) = x.[0]
    static member        Head (x: string           , [<Optional>]_impl: Head    ) = x.[0]
    static member        Head (x: StringBuilder    , [<Optional>]_impl: Head    ) = x.ToString().[0]

    static member inline Invoke (source: '``Foldable'<T>``)        =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Head : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Head>, source) : 'T


type TryHead =
    inherit Default1
    static member inline TryHead (x               , [<Optional>]_impl: Default1) = let x = ToSeq.Invoke x in if Seq.isEmpty x then None else Some (Seq.head x) : 'T option  
    static member        TryHead (x: 't list      , [<Optional>]_impl: TryHead ) = match x with [] -> None | _ -> Some (List.head x)
    static member        TryHead (x: 't []        , [<Optional>]_impl: TryHead ) = if Array.length x = 0 then None else Some x.[0]
    static member        TryHead (x: Id<'T>       , [<Optional>]_impl: TryHead ) = Some x.getValue
    static member        TryHead (x: string       , [<Optional>]_impl: TryHead ) = if String.length x = 0 then None else Some x.[0]   
    static member        TryHead (x: StringBuilder, [<Optional>]_impl: TryHead ) = if x.Length = 0 then None else Some (x.ToString().[0])
    static member        TryHead (x: 't seq       , [<Optional>]_impl: TryHead ) = if Seq.isEmpty x then None else Some (Seq.head x)

    static member inline Invoke (source: '``Foldable'<T>``)        =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member TryHead : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<TryHead>, source) : 'T option


type Pick =
    inherit Default1
    static member inline Pick (x: '``Foldable<'T>``, f: 'T->'U option, [<Optional>]_impl: Default2) = Seq.pick f (ToSeq.Invoke x) : 'U
    static member inline Pick (x: '``Foldable<'T>``, f: 'T->'U option, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member Pick : '``Foldable<'T>``-> _ -> 'T) (x, f))
    static member        Pick (x: Id<'T>           , f: 'T->'U option, [<Optional>]_impl: Pick    ) = List.pick  f [x.getValue]
    static member        Pick (x: ResizeArray<'T>  , f: 'T->'U option, [<Optional>]_impl: Pick    ) = Seq.pick   f x
    static member        Pick (x: list<'T>         , f: 'T->'U option, [<Optional>]_impl: Pick    ) = List.pick  f x
    static member        Pick (x: 'T []            , f: 'T->'U option, [<Optional>]_impl: Pick    ) = Array.pick f x

    static member inline Invoke (chooser: 'T->'U option) (source: '``Foldable'<T>``) =
        let inline call_2 (a: ^a, b: ^b, x) = ((^a or ^b ) : (static member Pick : _*_*_ -> _) b, x, a)
        let inline call (a: 'a, b: 'b, x: 'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<Pick>, source, chooser) : 'U


type TryPick =
    inherit Default1
    static member inline TryPick (x          , f: _->'U option, [<Optional>]_impl: Default1) = Seq.tryPick   f (ToSeq.Invoke x) : 'U option
    static member        TryPick (_: Id<'T>  , _: _->'U option, [<Optional>]_impl: TryPick ) = invalidOp "TryPick on ID"        : 'U option
    static member        TryPick (x: seq<'T> , f: _->'U option, [<Optional>]_impl: TryPick ) = Seq.tryPick   f x
    static member        TryPick (x: list<'T>, f: _->'U option, [<Optional>]_impl: TryPick ) = List.tryPick  f x
    static member        TryPick (x: 'T []   , f: _->'U option, [<Optional>]_impl: TryPick ) = Array.tryPick f x

    static member inline Invoke (chooser: 'T->'U option) (source: '``Foldable'<T>``) =
        let inline call_2 (a: ^a, b: ^b, x) = ((^a or ^b) : (static member TryPick : _*_*_ -> _) b, x, a)
        let inline call (a: 'a, b: 'b, x: 'x) = call_2 (a, b, x)
        call (Unchecked.defaultof<TryPick>, source, chooser) : 'U option
 

type Nth =
    inherit Default1
    static member inline Nth (x: '``Foldable<'T>``, n, [<Optional>]_impl: Default3) = x |> ToSeq.Invoke |> Seq.skip n |> Seq.head : 'T
    static member        Nth (x: IReadOnlyList<'a>, n, [<Optional>]_impl: Default2) = x.[n]
    static member        Nth (x: IList<'a>        , n, [<Optional>]_impl: Default1) = x.[n]
    static member        Nth (x: string           , n, [<Optional>]_impl: Nth     ) = x.[n]
    static member        Nth (x: StringBuilder    , n, [<Optional>]_impl: Nth     ) = x.ToString().[n]
    static member        Nth (x: 'a []            , n, [<Optional>]_impl: Nth     ) = x.[n] : 'a
    static member        Nth (x: 'a ResizeArray   , n, [<Optional>]_impl: Nth     ) = x.[n]
    static member        Nth (x: list<'a>         , n, [<Optional>]_impl: Nth     ) = x.[n]
    static member        Nth (x: 'a Id            , _, [<Optional>]_impl: Nth     ) = x.getValue

    static member inline Invoke (n: int) (source: '``Foldable<'T>``) : 'T =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member Nth : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<Nth>, source, n)


type Max =
    inherit Default1
    static member inline Max (x: '``Foldable<'T>``, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.max : 'T when 'T : comparison
    static member inline Max (x: '``Foldable<'T>``, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member Max : '``Foldable<'T>``-> 'T) x) : 'T when 'T : comparison
    static member        Max (x: Id<'T>           , [<Optional>]_impl: Max     ) = x.getValue  : 'T when 'T : comparison
    static member        Max (x: ResizeArray<'T>  , [<Optional>]_impl: Max     ) = Seq.max   x : 'T when 'T : comparison
    static member        Max (x: list<'T>         , [<Optional>]_impl: Max     ) = List.max  x : 'T when 'T : comparison
    static member        Max (x: 'T []            , [<Optional>]_impl: Max     ) = Array.max x : 'T when 'T : comparison

    static member inline Invoke (source: '``Foldable<'T>``) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Max : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Max>, source) : 'T when 'T : comparison


type MaxBy =
    inherit Default1
    static member inline MaxBy (x: '``Foldable<'T>``, f: 'T->'U when 'U : comparison, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.maxBy f : 'T
    static member inline MaxBy (x: '``Foldable<'T>``, f: 'T->'U when 'U : comparison, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member MaxBy : '``Foldable<'T>``-> _ -> 'T) (x, f))
    static member        MaxBy (x: Id<'T>           , _: 'T->'U when 'U : comparison, [<Optional>]_impl: MaxBy   ) = x.getValue
    static member        MaxBy (x: ResizeArray<'T>  , f: 'T->'U when 'U : comparison, [<Optional>]_impl: MaxBy   ) = Seq.maxBy   f x
    static member        MaxBy (x: list<'T>         , f: 'T->'U when 'U : comparison, [<Optional>]_impl: MaxBy   ) = List.maxBy  f x
    static member        MaxBy (x: 'T []            , f: 'T->'U when 'U : comparison, [<Optional>]_impl: MaxBy   ) = Array.maxBy f x

    static member inline Invoke (projection: 'T->'U when 'U : comparison) (source: '``Foldable<'T>``) =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member MaxBy : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MaxBy>, source, projection) : 'T


type Min =
    inherit Default1
    static member inline Min (x: '``Foldable<'T>``, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.min : 'T when 'T : comparison
    static member inline Min (x: '``Foldable<'T>``, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member Min : '``Foldable<'T>``-> 'T) x) : 'T when 'T : comparison
    static member        Min (x: Id<'T>           , [<Optional>]_impl: Min     ) = x.getValue  : 'T when 'T : comparison
    static member        Min (x: ResizeArray<'T>  , [<Optional>]_impl: Min     ) = Seq.min   x : 'T when 'T : comparison
    static member        Min (x: list<'T>         , [<Optional>]_impl: Min     ) = List.min  x : 'T when 'T : comparison
    static member        Min (x: 'T []            , [<Optional>]_impl: Min     ) = Array.min x : 'T when 'T : comparison

    static member inline Invoke (source: '``Foldable<'T>``) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Min : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Min>, source) : 'T when 'T : comparison


type MinBy =
    inherit Default1
    static member inline MinBy (x: '``Foldable<'T>``, f        , [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.minBy f : 'T
    static member inline MinBy (x: '``Foldable<'T>``, f: 'T->'U, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (static member MinBy : '``Foldable<'T>``-> _ -> 'T) (x, f))
    static member        MinBy (x: Id<'T>           , _: 'T->'U, [<Optional>]_impl: MinBy   ) = x.getValue
    static member        MinBy (x: ResizeArray<'T>  , f        , [<Optional>]_impl: MinBy   ) = Seq.minBy   f x
    static member        MinBy (x: list<'T>         , f        , [<Optional>]_impl: MinBy   ) = List.minBy  f x
    static member        MinBy (x: 'T []            , f        , [<Optional>]_impl: MinBy   ) = Array.minBy f x

    static member inline Invoke (projection: 'T->'U) (source: '``Foldable<'T>``) =
        let inline call_2 (a: ^a, b: ^b, f) = ((^a or ^b) : (static member MinBy : _*_*_ -> _) b, f, a)
        let inline call (a: 'a, b: 'b, f) = call_2 (a, b, f)
        call (Unchecked.defaultof<MinBy>, source, projection) : 'T


type Length =
    inherit Default1
    static member inline Length (x: '``Foldable<'T>``, [<Optional>]_impl: Default2) = x |> ToSeq.Invoke |> Seq.length
    static member inline Length (x: '``Foldable<'T>``, [<Optional>]_impl: Default1) = (^``Foldable<'T>`` : (member Length : int) x)
    static member        Length (_: Id<'T>           , [<Optional>]_impl: Length  ) = 1
    static member        Length (x: ResizeArray<'T>  , [<Optional>]_impl: Length  ) = x.Count
    static member        Length (x: option<'T>       , [<Optional>]_impl: Length  ) = if x.IsSome then 1 else 0
    static member        Length (x: 'T []            , [<Optional>]_impl: Length  ) = Array.length x

    static member inline Invoke (source: '``Foldable<'T>``) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Length : _*_ -> _) b, a)
        let inline call (a: 'a, b: 'b) = call_2 (a, b)
        call (Unchecked.defaultof<Length>, source) : int


type FindSliceIndex =
    inherit Default1
    static member inline FindSliceIndex (x: '``Foldable<'T>``, e: '``Foldable<T>`` , [<Optional>]_impl: Default1 ) = x |> ToSeq.Invoke |> Seq.findSliceIndex (ToSeq.Invoke e)
    static member        FindSliceIndex (x: string           , e                   , [<Optional>]_impl: FindSliceIndex) = String.findSliceIndex e x
    static member        FindSliceIndex (x: 'a []            , e                   , [<Optional>]_impl: FindSliceIndex) = Array.findSliceIndex e x
    static member        FindSliceIndex (x: 'a ResizeArray   , e: 'a ResizeArray   , [<Optional>]_impl: FindSliceIndex) = Seq.findSliceIndex e x
    static member        FindSliceIndex (x: list<'a>         , e                   , [<Optional>]_impl: FindSliceIndex) = List.findSliceIndex e x
    static member        FindSliceIndex (x: 'a Id            , e: 'a Id            , [<Optional>]_impl: FindSliceIndex) = List.findSliceIndex [e.getValue] [x.getValue]

    static member inline Invoke (slice: '``Foldable<'T>``) (source: '``Foldable<'T>``) : int =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member FindSliceIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<FindSliceIndex>, source, slice)

type TryFindSliceIndex =
    inherit Default1
    static member inline TryFindSliceIndex (x: '``Foldable<'T>``, e: '``Foldable<T>`` , [<Optional>]_impl: Default1 ) = x |> ToSeq.Invoke |> Seq.tryFindSliceIndex (ToSeq.Invoke e)
    static member        TryFindSliceIndex (x: string           , e                   , [<Optional>]_impl: TryFindSliceIndex) = String.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a []            , e                   , [<Optional>]_impl: TryFindSliceIndex) = Array.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a ResizeArray   , e: 'a ResizeArray   , [<Optional>]_impl: TryFindSliceIndex) = Seq.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: list<'a>         , e                   , [<Optional>]_impl: TryFindSliceIndex) = List.tryFindSliceIndex e x
    static member        TryFindSliceIndex (x: 'a Id            , e: 'a Id            , [<Optional>]_impl: TryFindSliceIndex) = List.tryFindSliceIndex [e.getValue] [x.getValue]

    static member inline Invoke (slice: '``Foldable<'T>``) (source: '``Foldable<'T>``) : int option =
        let inline call_2 (a: ^a, b: ^b, n) = ((^a or ^b) : (static member TryFindSliceIndex : _*_*_ -> _) b, n, a)
        let inline call (a: 'a, b: 'b, n) = call_2 (a, b, n)
        call (Unchecked.defaultof<TryFindSliceIndex>, source, slice)

