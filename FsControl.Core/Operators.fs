namespace FsControl

open FsControl.Core.TypeMethods

module Operators =

    // Functor ----------------------------------------------------------------

    let inline map    (f:'T->'U) (x:'Functor'T) :'Functor'U = 
        let inline call_3 (a:^a,b:^b,c:^c) =  ((^a or ^b or ^c) : (static member Map: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a ,b , Unchecked.defaultof<'r>) x :'r
        call (Functor.Map.Instance, x) f

    let inline (<!>)  (f:'T->'U) (x:'Functor'T) :'Functor'U = map f x
    let inline (|>>)  (x:'Functor'T) (f:'T->'U) :'Functor'U = map f x

    let inline map_   (action :'T->unit) (source :'Functor'T) =
        let inline call_3 (a:^a,b:^b,c:^c) =  ((^a or ^b or ^c) : (static member Map: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a ,b , Unchecked.defaultof<'r>) x :'r
        call (Functor.Map_.Instance, source) action :unit
   

    // Applicative ------------------------------------------------------------

    let inline result (x:'T): 'Functor'T = 
        let inline call_2 (a:^a,b:^b) = ((^a or ^b) : (static member Return: ^a* ^b -> _) (a,b))
        call_2 (Applicative.Return.Instance, Unchecked.defaultof<'Functor'T>) x

    let inline (<*>) (x:'Applicative'T_'U) (y:'Applicative'T): 'Applicative'U = 
        let inline call_4 (a:^a,b:^b,c:^c,d:^d          ) =                                                          
            ((^a or ^b or ^c or ^d) : (static member Apply: ^a* ^b* ^c* ^d -> _) (a,b,c,d))
        let inline call (a:'a, b:'b, c:'c) = fun (x:'x) -> call_4(a,b,c, Unchecked.defaultof<'r>) x : 'r
        call (Applicative.Apply.Instance, x, y) () : 'Applicative'U

    let inline liftA2 (f:'T->'U->'V) (a:'Applicative'T) (b:'Applicative'U) :'Applicative'V = f <!> a <*> b
    let inline (  *>) (x:'Applicative'a) :'Applicative'b->'Applicative'b = x |> liftA2 (fun   _ -> id)
    let inline (<*  ) (x:'Applicative'a) :'Applicative'b->'Applicative'a = x |> liftA2 (fun k _ -> k )
    let inline (<**>) (x:'Applicative'a): 'Applicative'a_'b->'Applicative'b = x |> liftA2 (|>)
    let inline optional (v:'Alternative'a) :'Alternative'Option'a = Some <!> v <|> result None


    // Monad -----------------------------------------------------------
    
    let inline (>>=) (x:'Monad'T) (f:'T->'Monad'U) :'Monad'U =
        let inline call_3 (a:^a,b:^b,c:^c) = ((^a or ^b or ^c) : (static member Bind: ^a* ^b* ^c -> _) (a,b,c))
        call_3 (Monad.Bind.Instance, x, Unchecked.defaultof<'Monad'U>) f

    let inline (=<<) (f:'T->'Monad'U) (x:'Monad'T) :'Monad'U =
        let inline call_3 (a:^a,b:^b,c:^c) = ((^a or ^b or ^c) : (static member Bind: ^a* ^b* ^c -> _) (a,b,c))
        call_3 (Monad.Bind.Instance, x, Unchecked.defaultof<'Monad'U>) f

    let inline join (x:'Monad'Monad'T) :'Monad'T =
        let inline call_3 (a:^a,b:^b,c:^c) = ((^a or ^b or ^c) : (static member Join: ^a* ^b* ^c -> _) (a,b,c))
        call_3 (Monad.Join.Instance, x, Unchecked.defaultof<'Monad'T>) ()


    // Monoid -----------------------------------------------------------------

    let inline mempty() :'Monoid =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Mempty: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Monoid.Mempty.Instance ()

    let inline mappend (x:'Monoid) (y:'Monoid): 'Monoid =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mappend: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Monoid.Mappend.Instance, x) y

    let inline mconcat (x:list<'Monoid>)      : 'Monoid =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mconcat: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r       
        call (Monoid.Mconcat.Instance, x) ()


    // Alternative/Monadplus/Arrowplus ----------------------------------------

    let inline zero() :'Functor'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Zero: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Functor.Zero.Instance ()

    let inline (<|>) (x:'Functor'T) (y:'Functor'T) :'Functor'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Plus: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r        
        call (Functor.Plus.Instance, x) y


    // M's --------------------------------------------------------------------

    let inline sequence (ms:list<'Monad'a>) =
        let k m m' = m >>= fun (x:'t) -> m' >>= fun xs -> (result :list<'t> -> 'Monad'List'a) (List.Cons(x,xs))
        List.foldBack k ms ((result :list<'t> -> 'Monad'List'a) [])

    let inline mapM (f:'a->'Monad'b) (xs:list<'a>) :'Monad'List'b = sequence (List.map f xs)
    
    let inline foldM (f:'a->'b->'Monad'a) (a:'a) (bx:list<'b>) : 'Monad'a =
        let rec loopM a = function
            | x::xs -> (f a x) >>= fun fax -> loopM fax xs 
            | [] -> result a
        loopM a bx

    let inline filterM (f: 'a -> 'Monad'Bool) (xs: list<'a>) : 'Monad'List'a =
        let rec loopM = function
            | []   -> result []
            | h::t -> 
                f h >>= (fun flg ->
                    loopM t >>= (fun ys ->
                        result (if flg then (h::ys) else ys)))
        loopM xs
   
    let inline liftM  (f:'a->'b) (m1:'Monad'a) :'Monad'b = m1 >>= (result << f)
    let inline liftM2 (f:'a1->'a2->'r) (m1:'Monad'a1) (m2:'Monad'a2) :'Monad'r = m1 >>= fun x1 -> m2 >>= fun x2 -> result (f x1 x2)
    let inline ap (x:'Monad'a_'b) (y:'Monad'a): 'Monad'b = liftM2 id x y

    let inline (>=>)  (f:'a->'Monad'b) (g:'b->'Monad'c) (x:'a) :'Monad'c = f x >>= g
    let inline (<=<)  (g:'b->'Monad'c) (f:'a->'Monad'b) (x:'a) :'Monad'c = f x >>= g

    let inline guard x: 'MonadPlus'unit = if x then result () else zero()

   
    // Arrows -----------------------------------------------------------------

    let inline catId()     =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Id: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Category.Id.Instance ()

    let inline (<<<<)  f g = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Comp: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Category.Comp.Instance, f) g

    let inline (>>>>)  g f = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Comp: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Category.Comp.Instance, f) g

    let inline arr     f   =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Arr: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Arrow.Arr.Instance f

    let inline first   f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member First: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Arrow.First.Instance, f) ()

    let inline second  f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Second: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Arrow.Second.Instance, f) ()

    let inline ( ****) f g = first f >>>> second g

    let inline (&&&&)  f g = arr (fun b -> (b,b)) >>>> f **** g

    let inline (||||)  f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcEither: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call ArrowChoice.AcEither.Instance (f, g)

    let inline (++++)  f g =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member AcMerge: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call ArrowChoice.AcMerge.Instance  (f, g)

    let inline left    f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcLeft: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (ArrowChoice.AcLeft.Instance, f) ()

    let inline right   f   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member AcRight: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (ArrowChoice.AcRight.Instance, f) ()

    let inline arrAp()     =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Apply: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call ArrowApply.Apply.Instance ()


    // Foldable

    let inline foldBack (folder:'T->'State->'State) (foldable:'Foldable'T) (state:'State) :'State =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Foldr: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Foldr.Instance, foldable) (folder, state)

    let inline fold     (folder:'State->'T->'State) (state:'State) (foldable:'Foldable'T) :'State =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Foldl: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Foldl.Instance, foldable) (folder, state)

    let inline foldMap (f:'T->'Monoid) (x:'Foldable'T) :'Monoid =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member FoldMap: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.FoldMap.Instance, x) f

    let inline toList  value :'t list =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToList: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.ToList.Instance, value) ()

    let inline toArray value :'t []   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToArray: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.ToArray.Instance, value) ()

    let inline exists     (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Exists: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Exists.Instance,  source) predicate          :bool

    let inline find       (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Find: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Find.Instance,    source) predicate          :'T

    let inline tryFind    (predicate :'T->bool) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member TryFind: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.TryFind.Instance, source) predicate          :'T option

    let inline pick     (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Pick: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Pick.Instance,   source) chooser            :'U

    let inline tryPick  (chooser:'T->'U option) (source:'Foldable'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member TryPick: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.TryPick.Instance, source) chooser            :'U option
    
    let inline filter (predicate:_->bool) (x:'Foldable'a) :'Foldable'a =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Filter: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Foldable.Filter.Instance, x) predicate


    // Traversable

    let inline traverse (f:'T->'Applicative'U) (t:'Traversable'T) :'Applicative'Traversable'U =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Traverse: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Traversable.Traverse.Instance, t) f

    let inline sequenceA (t:'Traversable'Applicative'T) :'Applicative'Traversable'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SequenceA: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Traversable.SequenceA.Instance, t) ()


    // Comonads

    let inline extract (x:'Comonad'T): 'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Extract: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Comonad.Extract.Instance, x) ()

    let inline extend (g:'Comonad'T->'U) (s:'Comonad'T): 'Comonad'U =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Extend: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Comonad.Extend.Instance, s) g
    let inline (=>>)   (s:'Comonad'T) (g:'Comonad'T->'U): 'Comonad'U = extend g s

    let inline duplicate x = //'Comonad'T -> :'Comonad'Comonad'T 
       let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Duplicate: _*_*_ -> _) a, b, c)
       let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
       call (Comonad.Duplicate.Instance, x) ()



    // Monad Transformers

    open FsControl.Core.Types

    let inline lift      (x:'Monad'T ) :'MonadTrans'Monad'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Lift: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadTrans.Lift.Instance x

    let inline liftAsync (x:Async<'T>) :'MonadAsync'T       =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member LiftAsync: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadAsync.LiftAsync.Instance x

    let inline callCC (f:('T->'MonadCont'U)->'MonadCont'T): 'MonadCont'T =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member CallCC: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadCont.CallCC.Instance f
   
    /// <summary>get    :: MonadState  s m => m s</summary>
    let inline get() :'ms =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Get: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadState.Get.Instance ()

    /// <summary>put    :: MonadState  s m => s -> m ()</summary>
    let inline put (x:'s) :'m =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Put: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadState.Put.Instance x

    /// <summary>ask    :: MonadReader r m => m r</summary>
    let inline ask() :'mr =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Ask: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadReader.Ask.Instance ()
   
    /// <summary>local  :: MonadReader r m => (r -> r) -> m a -> m a</summary>
    let inline local (f:'rr) (m:'ma) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Local: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call (MonadReader.Local.Instance, m) f

    /// <summary>tell   :: MonadWriter w m => w   -> m ()</summary>
    let inline tell (x:'w) :'m =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Tell: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call  MonadWriter.Tell.Instance x

    /// <summary>listen :: MonadWriter w m => m a -> m (a,w)</summary>
    let inline listen (m:'ma) :'maw =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Listen: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call  (MonadWriter.Listen.Instance, m) ()

    /// <summary>pass   :: MonadWriter w m => m (a, w -> w) -> m a</summary>
    let inline pass (m:'maww) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pass: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call (MonadWriter.Pass.Instance, m) ()

    /// <summary>throw :: MonadError e m => e -> m a</summary>
    let inline throw (x:'e) :'ma =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member ThrowError: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call MonadError.ThrowError.Instance x

    /// <summary>catch :: MonadError e m => m a -> (e -> m b) -> m b</summary>
    let inline catch (v:'ma) (h:'e->'mb) :'mb =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member CatchError: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (MonadError.CatchError.Instance, v) h


    // Collection

    let inline skip (n:int) x = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Skip: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Skip.Instance, x) n

    let inline take (n:int) x = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Take: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Take.Instance, x) n

    let inline fromList (value :list<'t>) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromList: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Collection.FromList.Instance value

    let inline fromSeq  (value :seq<'t> ) = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromSeq: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Collection.FromSeq.Instance value

    let inline groupBy    (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member GroupBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.GroupBy.Instance, source) projection

    let inline groupAdjBy (projection:'T->'Key) (source:'Collection'T) : 'Collection'KeyX'Collection'T = 
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member GroupAdjBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.GroupAdjBy.Instance, source) projection

    let inline sortBy (projection:'T->'Key) (source:'Collection'T) : 'Collection'T =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member SortBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.SortBy.Instance, source) projection


    let inline choose (chooser:'T->'U option)   (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Choose: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Choose.Instance, source) chooser         :'Collection'U

    let inline distinct                         (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Distinct: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Distinct.Instance, source) ()              :'Collection'T

    let inline distinctBy (projection:'T->'Key) (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member DistinctBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.DistinctBy.Instance, source) projection      :'Collection'T
    
    let inline head                             (source:'Collection'T)        = extract source

    let inline intersperse      (sep:'T)        (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Intersperse: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Intersperse.Instance, source) sep            :'Collection'T

    let inline iter       (action:'T->unit)     (source:'Collection'T)        = map_ action source

    let inline iteri (action:int->'T->unit)     (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Iteri: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Iteri.Instance,  source) action              :unit

    let inline length (source:'Collection'T)                                  =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Length: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Length.Instance, source) ()                  :int

    let inline mapi    (mapping:int->'T->'U)    (source:'Collection'T)        =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Mapi: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Mapi.Instance,   source) mapping             :'Collection'U
    
    let inline maxBy (projection:'T->'U) (source:'Collection'T)               =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member MaxBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.MaxBy.Instance, source) projection           :'T

    let inline minBy (projection:'T->'U) (source:'Collection'T)               =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member MinBy: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.MinBy.Instance, source) projection           :'T

    let inline rev  (source:'Collection'T)                                    =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Rev: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Rev.Instance, source) ()                   :'Collection'T

    let inline scan (folder:'State'->'T->'State) state (source:'Collection'T) =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Scan: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Scan.Instance, source) folder (state:'State) :'Collection'State

    let inline sort (source:'Collection'T)                                    =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Sort: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.Sort.Instance, source) ()                 :'Collection'T

    let inline toSeq (source:'Collection'T)                                   =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToSeq: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Collection.ToSeq.Instance, source) ()                   :seq<'T>

    let inline zip (source1:'Collection'T1) (source2:'Collection'T2)          =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member Zip: _*_*_*_ -> _) a, b, c, d)
        let inline call (a:'a, b:'b, c:'c) = fun (x:'x) -> call_4 (a, b, c, Unchecked.defaultof<'r>) x :'r
        call (Collection.Zip.Instance, source1, source2) ()           :'Collection'T1'T2




    // Converter

    let inline fromBytesWithOptions (isLtEndian:bool) (startIndex:int) (value:byte[]) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBytes: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.FromBytes.Instance (value, startIndex, isLtEndian)

    let inline fromBytes   (value:byte[]) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBytes: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.FromBytes.Instance (value, 0, true)

    let inline fromBytesBE (value:byte[]) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBytes: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.FromBytes.Instance (value, 0, false)

    let inline toBytes   value :byte[] =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToBytes: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Converter.ToBytes.Instance, value) true

    let inline toBytesBE value :byte[] =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToBytes: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Converter.ToBytes.Instance, value) false

    let inline toStringWithCulture (cultureInfo:System.Globalization.CultureInfo) value:string =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToString: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Converter.ToString.Instance, value) cultureInfo

    let inline toString  value:string  =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToString: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Converter.ToString.Instance, value) System.Globalization.CultureInfo.InvariantCulture  
         
    let inline tryParse (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member TryParse: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.TryParse.Instance value

    let inline parse    (value:string) =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Parse: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.Parse.Instance value

    let inline convert   value:'T      =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Convert: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Converter.Convert.Instance value


    // Numerics

    let inline divRem (D:'T) (d:'T) :'T*'T =
        let inline call_4 (a:^a, b:^b, c:^c, d:^d) = ((^a or ^b or ^c or ^d) : (static member DivRem: _*_*_*_ -> _) a, b, c, d)
        let inline call (a:'a, b:'b, c:'c) = fun (x:'x) -> call_4 (a, b, c, Unchecked.defaultof<'r>) x :'r
        call (Num.DivRem.Instance, D, d) ()

    let inline minValue() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MinValue: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Bounded.MinValue.Instance ()

    let inline maxValue() =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MaxValue: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Bounded.MaxValue.Instance ()


    let inline fromBigInteger  (x:bigint)   :'Num    =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromBigInteger: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Num.FromBigInteger.Instance x

    let inline toBigInteger    (x:'Integral) :bigint =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToBigInteger: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Integral.ToBigInteger.Instance, x) ()

    let inline abs    (x:'Num) :'Num =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Abs: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Num.Abs.Instance, x) ()

    let inline signum (x:'Num) :'Num =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Signum: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Num.Signum.Instance, x) ()

    let inline negate (x:'Num) :'Num =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Negate: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Num.Negate.Instance, x) ()

    let inline (~-)   (x:'Num) :'Num =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member Negate: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Num.Negate.Instance, x) ()

    let inline fromRational (x:Rational) :'Fractional =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member FromRational: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Fractional.FromRational.Instance x

    let inline properFraction x =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ProperFraction: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (RealFrac.ProperFraction.Instance, x) ()

    let inline toRational (x:'Real) :Rational =
        let inline call_3 (a:^a, b:^b, c:^c) = ((^a or ^b or ^c) : (static member ToRational: _*_*_ -> _) a, b, c)
        let inline call (a:'a, b:'b) = fun (x:'x) -> call_3 (a, b, Unchecked.defaultof<'r>) x :'r
        call (Real.ToRational.Instance, x) ()

    let inline pi() :'Floating =
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Pi: _*_ -> _) a, b)
        let inline call (a:'a) = fun (x:'x) -> call_2 (a, Unchecked.defaultof<'r>) x :'r
        call Floating.Pi.Instance ()