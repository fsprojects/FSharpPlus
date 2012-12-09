namespace FsControl.Core.Abstractions

// MonadPlus class ------------------------------------------------------------
module MonadPlus =
    type Mzero = Mzero with
        static member instance (Mzero, _:option<'a>) = fun () -> None
        static member instance (Mzero, _:List<'a>  ) = fun () -> []

    type Mplus = Mplus with
        static member instance (Mplus, x:option<_>, _) = fun y -> match x with None -> y | xs -> xs
        static member instance (Mplus, x:List<_>  , _) = fun y -> x @ y

    let inline internal mzero () = Inline.instance Mzero ()
    let inline internal mplus (x:'a) (y:'a) : 'a = Inline.instance (Mplus, x) y