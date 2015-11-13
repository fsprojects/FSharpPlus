namespace FsControl

type Reader<'R,'A> = Reader of ('R->'A)

[<RequireQualifiedAccess>]
module Reader =
    let run (Reader x) = x
    let map   f (Reader m) = Reader (f << m) :Reader<'r,_>
    let bind  f (Reader m) = Reader (fun r -> run (f (m r)) r) :Reader<'r,'b>
    let apply   (Reader f) (Reader x) = Reader (fun a -> f a (x a))
    let local f (Reader m) = Reader (m << f) :Reader<'r,_>
    let ask = Reader id

type Reader<'R,'A> with
    static member Map   (x:Reader<'r,'a>, f) = Reader.map f x   :Reader<'r,'b>
    static member Return a = Reader (fun _ -> a)                :Reader<'r,'a>
    static member Bind  (x, f) = Reader.bind f x                :Reader<'r,'b>
    static member (<*>) (f, x:Reader<'r,'a>) = Reader.apply f x :Reader<'r,'b>
    static member get_Ask()    = Reader.ask                     :Reader<'r,'r>
    static member Local (m, f) = Reader.local f m