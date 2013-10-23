namespace FSharpPlus

open FsControl.Core.Abstractions

[<AutoOpenAttribute>]
module Prelude =

    let inline flip f x y = f y x
    let inline konst k _ = k
    let inline (</) x = (|>) x
    let inline (/>) x = flip x
    let inline choice f g = function Choice2Of2 x -> f x | Choice1Of2 y -> g y
    let inline option n f = function None -> n | Some x -> f x


    // Functor ----------------------------------------------------------------
    let inline map f x = Inline.instance (Functor.Map, x) f


    // Applicative ------------------------------------------------------------
    let inline result x  = Inline.instance Applicative.Pure x
    let inline (<*>) x y = Inline.instance (Applicative.Ap, x, y) ()
    let inline empty() = Inline.instance Alternative.Empty ()
    let inline (<|>) (x:'a) (y:'a) :'a = Inline.instance (Alternative.Append, x) y
    let inline (<!>)  f a   = map f a
    let inline liftA2 f a b = f <!> a <*> b
    let inline (  *>)   x   = x |> liftA2 (konst id)
    let inline (<*  )   x   = x |> liftA2  konst
    let inline (<**>)   x   = x |> liftA2 (|>)
    let inline optional v = Some <<|> v <|> result None


    // ZipList
    type ZipList<'s> = ZipList of 's seq with
        static member instance (_:Functor.Map,    ZipList x   , _) = fun (f:'a->'b) -> ZipList (Seq.map f x)
        static member instance (_:Applicative.Pure, _:ZipList<'a>) = fun (x:'a)     -> ZipList (Seq.initInfinite (konst x))
        static member instance (_:Applicative.Ap  ,   ZipList (f:seq<'a->'b>), ZipList x ,_:ZipList<'b>) = fun () ->
            ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>

    [<RequireQualifiedAccess>]
    module ZipList =
        let run   (ZipList x) = x
        let map f (ZipList x) = ZipList (Seq.map f x)


    // NonEmptyList
    type NonEmptyList<'T> = {Head: 'T; Tail: 'T list }

    [<RequireQualifiedAccess>]
    module NonEmptyList =
        let toList {Head = x; Tail = xs} = x::xs
        let map f  {Head = x; Tail = xs} = {Head = f x; Tail = List.map f xs}
        let cons e {Head = x; Tail = xs} = {Head = e  ; Tail = x::xs}
        let rec tails s =
            let {Head = x; Tail = xs} = s
            match xs with
            | []   -> {Head = s; Tail = []}
            | h::t -> cons s (tails {Head = h; Tail = t})
         
    type NonEmptyList<'T> with
        static member instance (_:Functor.Map      , x:NonEmptyList<'a>, _:NonEmptyList<'b>) = fun (f:'a->'b) -> NonEmptyList.map f x
        
        static member instance (_:Monad.Return, _:NonEmptyList<'a>) = fun (x:'a)     -> {Head = x; Tail = []}                        
        static member instance (_:Monad.Bind, {Head = x; Tail = xs}, _:NonEmptyList<'b>   ) = fun (f:_->NonEmptyList<'b>  ) ->
            let {Head = y; Tail = ys} = f x
            let ys' = List.collect (NonEmptyList.toList << f) xs
            {Head = y; Tail = (ys @ ys')}

        static member instance (_:Applicative.Pure, _:NonEmptyList<'a>) = fun (x:'a)     -> {Head = x; Tail = []}
        static member instance (_:Applicative.Ap  , f:NonEmptyList<'a->'b>, x:NonEmptyList<'a> ,_:NonEmptyList<'b>) = fun () ->
             Applicative.DefaultImpl.ApFromMonad f x :NonEmptyList<'b>

        static member instance (_:Comonad.Extract  , {Head = h; Tail = _} ,_) = fun () -> h
        static member instance (_:Comonad.Duplicate, s:NonEmptyList<'a>, _:NonEmptyList<NonEmptyList<'a>>) = fun () -> NonEmptyList.tails s


    // Monad -----------------------------------------------------------
    let inline (>>=) x (f:_->'R) : 'R = Inline.instance (Monad.Bind, x) f
    let inline (=<<) (f:_->'R) x : 'R = Inline.instance (Monad.Bind, x) f
    let inline join x =  x >>= id


    // Monoid -----------------------------------------------------------------
    let inline mempty() = Inline.instance Monoid.Mempty ()
    let inline mappend (x:'a) (y:'a) :'a = Inline.instance (Monoid.Mappend, x) y
    let inline mconcat x =
        let foldR f s lst = List.foldBack f lst s
        foldR mappend (mempty()) x


    // Monad plus -------------------------------------------------------------
    let inline sequence ms =
        let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (result :list<'a> -> 'M) (List.Cons(x,xs))
        List.foldBack k ms ((result :list<'a> -> 'M) [])

    let inline mapM f as' = sequence (List.map f as')
    let inline liftM  f m1    = m1 >>= (result << f)
    let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline ap     x y     = liftM2 id x y

    let inline (>=>)  f g x   = f x >>= g
    let inline (<=<)  g f x   = f x >>= g

    let inline mzero() = Inline.instance MonadPlus.Mzero ()
    let inline mplus (x:'a) (y:'a) : 'a = Inline.instance (MonadPlus.Mplus, x) y
    let inline guard x = if x then result () else mzero()


    // Arrows -----------------------------------------------------------------
    let inline catId()    = Inline.instance  Category.Id ()
    let inline (<<<<) f g = Inline.instance (Category.Comp, f) g
    let inline (>>>>) g f = Inline.instance (Category.Comp, f) g
    let inline arr    f   = Inline.instance  Arrow.Arr    f
    let inline first  f   = Inline.instance (Arrow.First, f) ()
    let inline second f   = let swap (x,y) = (y,x) in arr swap >>>> first f >>>> arr swap
    let inline ( **** ) f g = first f >>>> second g
    let inline (&&&&) f g = arr (fun b -> (b,b)) >>>> f **** g
    let inline (||||) f g = Inline.instance  ArrowChoice.AcEither (f, g)
    let inline (++++) f g = Inline.instance  ArrowChoice.AcMerge  (f, g)
    let inline left   f   = Inline.instance (ArrowChoice.AcLeft , f) ()
    let inline right  f   = Inline.instance (ArrowChoice.AcRight, f) ()
    let inline arrAp()    = Inline.instance  ArrowApply.Apply ()


    // Foldable
    let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = Inline.instance (Foldable.Foldr, x) (f,z)
    let inline foldMap f x = Inline.instance (Foldable.FoldMap, x) f


    // Traversable
    let inline traverse f t = Inline.instance (Traversable.Traverse, t) f
    let inline sequenceA  x = traverse id x


    // Comonads
    let inline extract   x = Inline.instance (Comonad.Extract  , x) ()
    let inline duplicate x = Inline.instance (Comonad.Duplicate, x) ()
    let inline extend  g s = map g (duplicate s)
    let inline (=>>)   s g = map g (duplicate s)


    // Monad Transformers
    open FsControl.Core.Types
    let runOptionT   (OptionT m) = m
    let mapOptionT f (OptionT m) = OptionT (f m)

    let inline lift (x:'ma) = Inline.instance MonadTrans.Lift x
    let inline liftIO (x: Async<'a>) = Inline.instance MonadAsync.LiftAsync x
    let inline callCC f = Inline.instance MonadCont.CallCC f
    let inline get() = Inline.instance MonadState.Get ()
    let inline put x = Inline.instance MonadState.Put x
    let inline ask()     = Inline.instance  MonadReader.Ask ()
    let inline local f m = Inline.instance (MonadReader.Local, m) f
    let inline tell    x = Inline.instance  MonadWriter.Tell x
    let inline listen  m = Inline.instance (MonadWriter.Listen, m) ()
    let inline pass    m = Inline.instance (MonadWriter.Pass  , m) ()
    let inline throw x   = Inline.instance  MonadError.ThrowError x
    let inline catch v h = Inline.instance (MonadError.CatchError, v) h


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