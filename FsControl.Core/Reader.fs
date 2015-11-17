namespace FsControl

type Reader<'r,'t> = Reader of ('r->'t)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x                                                  : 'R->'T
    let map   (f:'T->_ ) (Reader m) = Reader (f << m)                       : Reader<'R,'U>
    let bind  (f:'T->_ ) (Reader m) = Reader (fun r -> run (f (m r)) r)     : Reader<'R,'U>
    let apply (Reader f) (Reader x) = Reader (fun a -> f a ((x:_->'T) a))   : Reader<'R,'U>
    let local (f:'R1->'R2) (Reader m) = Reader (m << f)                     : Reader<'R1,'T>
    let ask = Reader id                                                     : Reader<'R,'R>

type Reader with
    static member Map   (x:Reader<'R,'T>, f) = Reader.map f x   : Reader<'R,'U>
    static member Return x = Reader (fun _ -> x)                : Reader<'R,'T>
    static member Bind  (x:Reader<'R,'T>, f) = Reader.bind f x  : Reader<'R,'U>
    static member (<*>) (f, x:Reader<'R,'T>) = Reader.apply f x : Reader<'R,'U>
    static member get_Ask()    = Reader.ask                     : Reader<'R,'R>
    static member Local (m, f:'R1->'R2) = Reader.local f m      : Reader<'R1,'T>