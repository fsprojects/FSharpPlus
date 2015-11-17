namespace FsControl

type Writer<'monoid,'t> = Writer of ('t * 'monoid)

[<RequireQualifiedAccess>]
module Writer =
    let run (Writer x) = x                                                                                  : 'T * 'Monoid
    let map f (Writer (a:'T, w)) = Writer (f a, w)                                                          : Writer<'Monoid,'U>
    let inline bind  f (Writer (a:'T, w)) = Writer (let (b, w') = run (f a) in (b, MAppend.Invoke w w'))    : Writer<'Monoid,'U>
    let inline apply   (Writer (f, a)) (Writer (x:'T, b))       = Writer (f x, MAppend.Invoke a b)          : Writer<'Monoid,'U>
    let exec (Writer m:Writer<_,'T>) = snd m                                                                : Writer<'Monoid,'U>
    let tell               w         = Writer((),     w)                                                    : Writer<'Monoid,unit>
    let listen (Writer (a, w))       = Writer((a, w), w)                                                    : Writer<'Monoid,('T * 'Monoid)>
    let pass   (Writer((a, f), w:'Monoid)) = Writer(a, f w)                                                 : Writer<'Monoid,'T>

type Writer with
    static member        Map   (x, f:'T->_) = Writer.map f x            : Writer<'Monoid,'U>
    static member inline Return x = Writer (x, MEmpty.Invoke())         : Writer<'Monoid,'T>
    static member inline Bind  (x, f:'T->_) = Writer.bind f x           : Writer<'Monoid,'U>
    static member inline (<*>) (f, x:Writer<_,'T>) = Writer.apply f x   : Writer<'Monoid,'U>
    static member        Tell   w = Writer.tell w                       : Writer<'Monoid,unit>
    static member        Listen m = Writer.listen m                     : Writer<'Monoid,('T * 'Monoid)>
    static member        Pass   m = Writer.pass m                       : Writer<'Monoid,'T>