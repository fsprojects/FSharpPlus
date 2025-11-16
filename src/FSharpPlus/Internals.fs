namespace FSharpPlus.Internals

/// <namespacedoc>
/// <summary>
/// Internal to the library - please ignore
/// </summary>
/// </namespacedoc>
 
type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

#nowarn "0042" // retype

module internal Prelude =
    open System
    
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline tupleToOption x = match x with true, value -> Some value | _ -> None
    let inline opaqueId x = Unchecked.defaultof<_>; x
    
    let inline retype (x: 'T) : 'U =
    #if !FABLE_COMPILER
        (# "" x: 'U #)
    #else
        unbox<'U> x
    #endif

[<RequireQualifiedAccess>]
module internal Implicit = let inline Invoke (x: ^t) = ((^R or ^t) : (static member op_Implicit : ^t -> ^R) x) : ^R

module Constraints =
    /// Constrain 't to be a nested tuple of <'t1,'t2,'t3,'t4,'t5,'t6,'t7,'tr>
    let inline whenNestedTuple (t: 't) = 
        (^t: (member Item1: 't1) t), (^t: (member Item2: 't2) t), (^t: (member Item3: 't3) t), (^t: (member Item4: 't4) t), (^t: (member Item5: 't5) t), (^t: (member Item6: 't6) t), (^t: (member Item7: 't7) t), (^t: (member Rest: 'tr) t)

// Dummy types

type Id<'t> (v: 't) =
   let value = v
   member _.getValue = value

[<RequireQualifiedAccess>]
module Id =
    let run   (x: Id<_>) = x.getValue
    let map f (x: Id<_>) = Id (f x.getValue)
    let create x = Id x

type Id2<'t> (v: 't) =
   let value = v
   member _.getValue = value

[<RequireQualifiedAccess>]
module Id2 =
    let run   (x: Id2<_>) = x.getValue
    let map f (x: Id2<_>) = Id2 (f x.getValue)
    let create x = Id2 x

type Id0 (v: string) =
   let value = v
   member _.getValue = value

type Either<'t,'u> =
    | Left of 't
    | Right of 'u

type DmStruct = struct end
type DmStruct1<'T1> = struct end

type KeyValuePair2<'TKey, 'TValue> = struct
    val Key : 'TKey
    val Value : 'TValue
    new (key, value) = { Key = key; Value = value }
end

[<Sealed>]
type Set2<'T when 'T: comparison >() = class end

type NonEmptySeq2<'T> =
    inherit System.Collections.Generic.IEnumerable<'T>
    abstract member First: 'T
