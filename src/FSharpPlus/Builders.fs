namespace FSharpPlus

[<AutoOpenAttribute>]
module Builders =    

    open FSharpPlus.Operators

    // Idiom brackets
    type Ii = Ii
    type Ji = Ji
    type J = J
    type Idiomatic = Idiomatic with
        static member inline ($) (Idiomatic, si) = fun sfi x -> (Idiomatic $ x) (sfi <*> si)
        static member        ($) (Idiomatic, Ii) = id
    let inline idiomatic a b = (Idiomatic $ b) a
    let inline iI x = (idiomatic << result) x
    type Idiomatic with static member inline ($) (Idiomatic, Ji) = fun xii -> join xii
    type Idiomatic with static member inline ($) (Idiomatic, J ) = fun fii x -> (Idiomatic $ x) (join fii)


    // Do notation
    type MonadBuilder() =
        member inline b.Return(x)    = result x
        member inline b.Bind(p,rest) = p >>= rest
        member        b.Let (p,rest) = rest p
        member    b.ReturnFrom(expr) = expr

    type MonadPlusBuilder() =
        member inline b.Return(x) = result x
        member inline b.Bind(p,rest) = p >>= rest
        member b.Let(p,rest) = rest p
        member b.ReturnFrom(expr) = expr
        member inline x.Zero() = mzero()
        member inline x.Combine(a, b) = mplus a b
    
    let monad     = new MonadBuilder()
    let monadPlus = new MonadPlusBuilder()


    // Linq
    type LinqBuilder() =
        member inline __.Return(x)     = result x
        member inline __.Bind(p, rest) = p >>= rest
        member        __.Let (p, rest) = rest p
        member    __.ReturnFrom(expr)  = expr
        member inline __.For(p, rest)  = p >>= rest
        member inline __.Yield(x)      = result x
        member inline __.Zero()        = mzero()
        member inline __.Combine(a, b) = mplus a b

        [<CustomOperation("select", MaintainsVariableSpace=true)>]
        member inline __.Select(x, [<ProjectionParameter>] f) = map f x

        [<CustomOperation("where", MaintainsVariableSpace=true)>]
        member inline __.Where(x, [<ProjectionParameter>] p) = filter p x

    let linq = LinqBuilder()