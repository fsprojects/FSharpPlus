namespace FsControl

/// <summary> Computation type: Computations which read values from a shared environment.
/// <para/>   Binding strategy: Monad values are functions from the environment to a value. The bound function is applied to the bound value, and both have access to the shared environment.
/// <para/>   Useful for: Maintaining variable bindings, or other shared environment.</summary>
type Reader<'r,'t> = Reader of ('r->'t)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x                                                  : 'R->'T
    let map   (f:'T->_ ) (Reader m) = Reader (f << m)                       : Reader<'R,'U>
    let bind  (f:'T->_ ) (Reader m) = Reader (fun r -> run (f (m r)) r)     : Reader<'R,'U>
    let apply (Reader f) (Reader x) = Reader (fun a -> f a ((x:_->'T) a))   : Reader<'R,'U>

    /// Retrieves the monad environment.
    let ask = Reader id                                                     : Reader<'R,'R>

    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let local (f:'R1->'R2) m = let (Reader m) = m in Reader (m << f)        : Reader<'R1,'T>

type Reader with
    static member Map   (x:Reader<'R,'T>, f) = Reader.map f x   : Reader<'R,'U>
    static member Return x = Reader (fun _ -> x)                : Reader<'R,'T>
    static member Bind  (x:Reader<'R,'T>, f) = Reader.bind f x  : Reader<'R,'U>
    static member (<*>) (f, x:Reader<'R,'T>) = Reader.apply f x : Reader<'R,'U>
    static member get_Ask()    = Reader.ask                     : Reader<'R,'R>
    static member Local (m, f:'R1->'R2) = Reader.local f m      : Reader<'R1,'T>