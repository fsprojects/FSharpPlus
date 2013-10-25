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
    let inline map (f:'a->'b) (x:'Functor'a) :'Functor'b = Inline.instance (Functor.Map, x) f


    // Applicative ------------------------------------------------------------
    let inline result (x:'a): 'Functor'a = Inline.instance Applicative.Pure x
    let inline (<*>) (x:'Applicative'a_'b) (y:'Applicative'a): 'Applicative'b = Inline.instance (Applicative.Ap, x, y) ()
    let inline empty() :'Alternative'a = Inline.instance Alternative.Empty ()
    let inline (<|>) (x:'Alternative'a) (y:'Alternative'a) :'Alternative'a = Inline.instance (Alternative.Append, x) y
    let inline (<!>)  (f:'a->'b) (x:'Functor'a) :'Functor'b   = map f x
    let inline liftA2 (f:'a->'b->'c) (a:'Applicative'a) (b:'Applicative'b) :'Applicative'c = f <!> a <*> b
    let inline (  *>) (x:'Applicative'a) :'Applicative'b->'Applicative'b = x |> liftA2 (konst id)
    let inline (<*  ) (x:'Applicative'a) :'Applicative'b->'Applicative'a = x |> liftA2  konst
    let inline (<**>) (x:'Applicative'a): 'Applicative'a_'b->'Applicative'b = x |> liftA2 (|>)
    let inline optional (v:'Alternative'a) :'Alternative'Option'a = Some <!> v <|> result None


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
    type NonEmptyList<'t> = {Head: 't; Tail: 't list}

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
    let inline (>>=) (x:'Monad'a) (f:'a->'Monad'b) :'Monad'b = Inline.instance (Monad.Bind, x) f
    let inline (=<<) (f:'a->'Monad'b) (x:'Monad'a) :'Monad'b = Inline.instance (Monad.Bind, x) f
    let inline join (x:'Monad'Monad'a) :'Monad'a =  x >>= id


    // Monoid -----------------------------------------------------------------
    let inline mempty() :'Monoid = Inline.instance Monoid.Mempty ()
    let inline mappend (x:'Monoid) (y:'Monoid) :'Monoid = Inline.instance (Monoid.Mappend, x) y
    let inline mconcat (x:List<'Monoid>) :'Monoid  =
        let foldR f s lst = List.foldBack f lst s
        foldR mappend (mempty()) x


    // Monad plus -------------------------------------------------------------
    let inline sequence (ms:List<'Monad'a>) :'Monad'List'a =
        let k m m' = m >>= fun (x:'Monad'a) -> m' >>= fun xs -> (result :list<'Monad'a> -> 'Monad'List'a) (List.Cons(x,xs))
        List.foldBack k ms ((result :list<'Monad'a> -> 'Monad'List'a) [])

    let inline mapM   (f:'a->'Monad'b) (xs:List<'a>) :'Monad'List'b = sequence (List.map f xs)
    let inline liftM  (f:'a->'b) (m1:'Monad'a) :'Monad'b = m1 >>= (result << f)
    let inline liftM2 (f:'a1->'a2->'r) (m1:'Monad'a1) (m2:'Monad'a2) :'Monad'r = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline ap (x:'Monad'a_'b) (y:'Monad'a): 'Monad'b = liftM2 id x y

    let inline (>=>)  (f:'a->'Monad'b) (g:'b->'Monad'c) (x:'a) :'Monad'c = f x >>= g
    let inline (<=<)  (g:'b->'Monad'c) (f:'a->'Monad'b) (x:'a) :'Monad'c = f x >>= g

    let inline mzero() :'MonadPlus'a = Inline.instance MonadPlus.Mzero ()
    let inline mplus (x:'MonadPlus'a) (y:'MonadPlus'a) :'MonadPlus'a = Inline.instance (MonadPlus.Mplus, x) y
    let inline guard x: 'MonadPlus'unit = if x then result () else mzero()


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
    let inline foldr  (f:'a->'b->'b) (z:'b) (x:'Foldable'a) :'b = Inline.instance (Foldable.Foldr  , x) (f,z)
    let inline foldMap (f:'a->'Monoid) (x:'Foldable'a) :'Monoid = Inline.instance (Foldable.FoldMap, x) f


    // Traversable
    let inline traverse  (f:'a->'Applicative'b) (t:'Traversable'a) : 'Applicative'Traversable'b = Inline.instance (Traversable.Traverse, t) f
    let inline sequenceA (x:'Traversable'Applicative'a) : 'Applicative'Traversable'a = traverse id x


    // Comonads
    let inline extract   (x:'Comonad'a): 'a = Inline.instance (Comonad.Extract, x) ()
    let inline duplicate (x:'Comonad'a): 'Comonad'Comonad'a = Inline.instance (Comonad.Duplicate, x) ()
    let inline extend  (g:'Comonad'a->'b) (s:'Comonad'a): 'Comonad'b = map g (duplicate s)
    let inline (=>>)   (s:'Comonad'a) (g:'Comonad'a->'b): 'Comonad'b = map g (duplicate s)


    // Monad Transformers
    open FsControl.Core.Types

    let inline lift   (x:'Monad'a ) : 'MonadTrans'Monad'a = Inline.instance MonadTrans.Lift x
    let inline liftIO (x:Async<'a>) : 'MonadAsync'a       = Inline.instance MonadAsync.LiftAsync x

    let inline callCC (f:('a->'MonadCont'b)->'MonadCont'a): 'MonadCont'a = Inline.instance  MonadCont.CallCC f
    
    /// <summary>get    :: MonadState  s m => m s</summary>
    let inline get() :'ms = Inline.instance MonadState.Get ()

    /// <summary>put    :: MonadState  s m => s -> m ()</summary>
    let inline put (x:'s) :'m = Inline.instance MonadState.Put x

    /// <summary>ask    :: MonadReader r m => m r</summary>
    let inline ask() :'mr = Inline.instance  MonadReader.Ask ()
    
    /// <summary>local  :: MonadReader r m => (r -> r) -> m a -> m a</summary>
    let inline local (f:'rr) (m:'ma) :'ma = Inline.instance (MonadReader.Local, m) f

    /// <summary>tell   :: MonadWriter w m => w   -> m ()</summary>
    let inline tell (x:'w) :'m = Inline.instance  MonadWriter.Tell x

    /// <summary>listen :: MonadWriter w m => m a -> m (a,w)</summary>
    let inline listen (m:'ma) :'maw = Inline.instance (MonadWriter.Listen, m) ()

    /// <summary>pass   :: MonadWriter w m => m (a, w -> w) -> m a</summary>
    let inline pass (m:'maww) :'ma = Inline.instance (MonadWriter.Pass  , m) ()

    /// <summary>throw :: MonadError e m => e -> m a</summary>
    let inline throw (x:'e) :'ma = Inline.instance  MonadError.ThrowError x

    /// <summary>catch :: MonadError e m => m a -> (e -> m b) -> m b</summary>
    let inline catch (v:'ma) (h:'e->'mb) :'mb = Inline.instance (MonadError.CatchError, v) h


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
