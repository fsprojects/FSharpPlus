namespace FSharpPlus

module Compatibility =
    module Haskell =

        // Types
        open FsControl.Core.Types

        let getDual (Dual x) = x
        type Dual<'t> = FsControl.Core.Types.Dual<'t>
        let Dual = Dual.Dual 

        let getAll (All x) = x
        type All  = All
        let All = All.All

        let getAny (Any x) = x
        type Any  = Any
        let Any = Any.Any

        let runKleisli (Kleisli f) = f
        type Kleisli<'t,'u> = FsControl.Core.Types.Kleisli<'t,'u>
        let Kleisli = Kleisli.Kleisli

        // Operatots
        let ($)   x y = x y
        let (.()) x y = x << y
        let const' k _ = k
        let (++) = (@)
        let (==) = (=)
        let (=/) x y = not (x = y)

        type DeReference_op = DeReference_op with
            static member (=>) (DeReference_op, a:'a ref        ) = !a
            static member (=>) (DeReference_op, a:string        ) = a.ToCharArray() |> Array.toList
            static member (=>) (DeReference_op, a:DeReference_op) = DeReference_op

        /// converts a string to list<char> otherwise still works as dereference operator.
        let inline (!) a = DeReference_op => a

        let show x = '\"' :: x ++ !"\""

        type Maybe<'t> = Option<'t>
        let  Just x :Maybe<'t> = Some x
        let  Nothing:Maybe<'t> = None
        let  (|Just|Nothing|) = function Some x -> Just x | _ -> Nothing
        let maybe  n f = function | Nothing -> n | Just x -> f x

        type Either<'a,'b> = Choice<'b,'a>
        let  Right x :Either<'a,'b> = Choice1Of2 x
        let  Left  x :Either<'a,'b> = Choice2Of2 x
        let  (|Right|Left|) = function Choice1Of2 x -> Right x | Choice2Of2 x -> Left x
        let either f g = function Left x -> f x | Right y -> g y

        // IO
        type IO<'a> = Async<'a>
        let runIO (f:IO<'a>) = Async.RunSynchronously f
        let getLine    = async {return System.Console.ReadLine()} :IO<string>
        let putStrLn x = async {printfn "%s" x}                   :IO<unit>
        let print    x = async {printfn "%A" x}                   :IO<unit>

        // Functors

        let inline fmap f x = map f x

        // Applicative functors
            
        let inline pure' x   = result x
        let inline empty()   = mzero ()    
        let inline optional v = Just <!> v <|> pure' Nothing

        // Monoids
        type Ordering = LT|EQ|GT with
            static member        Mempty  () = EQ
            static member        Mappend (x:Ordering, y) = 
                match x, y with
                | LT, _ -> LT
                | EQ, a -> a
                | GT, _ -> GT

        let inline compare' x y =
            match compare x y with
            | a when a > 0 -> GT
            | a when a < 0 -> LT
            | _            -> EQ


        // Foldable
        let inline foldr (f: 'a -> 'b -> 'b) (z:'b) x :'b = foldBack f x z
        let inline foldl (f: 'b -> 'a -> 'b) (z:'b) x :'b = fold     f z x


        // Numerics
        open GenericMath

        type Integer = bigint

        let inline fromInteger  (x:Integer)   :'Num    = fromBigInt x
        let inline toInteger    (x:'Integral) :Integer = toBigInt   x
        let inline fromIntegral (x:'Integral) :'Num = (fromInteger << toInteger) x

        let inline div (a:'Integral) b :'Integral =
            whenIntegral a
            let (a,b) = if b < 0G then (-a,-b) else (a,b)
            (if a < 0G then (a - b + 1G) else a) / b
            
        let inline quot (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a / b
        let inline rem  (a:'Integral) (b:'Integral) :'Integral = whenIntegral a; a % b
        let inline quotRem a b :'Integral * 'Integral = whenIntegral a; divRem a b
        let inline mod'   a b :'Integral = whenIntegral a; ((a % b) + b) % b  
        let inline divMod D d :'Integral * 'Integral =
            let q, r = quotRem D d
            if (r < 0G) then
                if (d > 0G) then (q - 1G, r + d)
                else             (q + 1G, r - d)
            else (q, r)


        let inline ( **) a (b:'Floating) :'Floating = a ** b
        let inline sqrt    (x:'Floating) :'Floating = Microsoft.FSharp.Core.Operators.sqrt x
            
        let inline asinh x :'Floating = log (x + sqrt (1G+x*x))
        let inline acosh x :'Floating = log (x + (x+1G) * sqrt ((x-1G)/(x+1G)))
        let inline atanh x :'Floating = (1G/2G) * log ((1G+x) / (1G-x))
            
        let inline logBase x y  :'Floating =  log y / log x


        /// Monad

        let inline return' x = result x

        let inline sequence ms =
            let k m m' = m >>= fun (x:'a) -> m' >>= fun xs -> (return' :list<'a> -> 'M) (List.Cons(x,xs))
            List.foldBack k ms ((return' :list<'a> -> 'M) [])
            
        let inline mapM f as' = sequence (List.map f as')
        let inline liftM  f m1    = m1 >>= (return' << f)
        let inline liftM2 f m1 m2 = m1 >>= fun x1 -> m2 >>= fun x2 -> return' (f x1 x2)
        let inline ap     x y     = liftM2 id x y
            
        let do' = new Builders.MonadBuilder()


        // Monad Plus
        let inline mplus (x:'a) (y:'a) : 'a = (<|>) x y
        let inline guard x = if x then return' () else mzero()

        let doPlus = new Builders.MonadPlusBuilder()

        let inline mfilter p ma = do' {
            let! a = ma
            if p a then return a else return! mzero()}




        // Arrow
        let inline id'() = catId ()
        let inline (<<<) f g = (<<<<) f g
        let inline (>>>) f g = (>>>>) f g
        let inline ( *** ) f g = arrFirst f >>> arrSecond g
        let inline ( &&& ) f g = arr (fun b -> (b, b)) >>> f *** g
        let inline (|||) f g = (||||) f g
        let inline (+++) f g = (++++) f g
        let inline app() = arrApply ()
        let inline zeroArrow() = mzero ()
        let inline (<+>)   f g = (<|>) f g
            

        // Cont

        let callCC' = Cont.callCC
        let inline when'  p s = if p then s else return' ()
        let inline unless p s = when' (not p) s

        let runCont = Cont.run
        type Cont = Cont
        let Cont = Cont.Cont
            

        // Reader

        let ask'      = Reader.ask
        let local'    = Reader.local

        let runReader = Reader.run
        type Reader = Reader
        let Reader = Reader.Reader
            

        // State

        let get'      = State.get
        let put'      = State.put
        let execState = State.exec

        let runState  = State.run
        type State = State
        let State = State.State

            
        // Monad Transformers
        type MaybeT<'T> = OptionT<'T>
        let MaybeT  x = OptionT x
        let runMaybeT = OptionT.run
        let inline mapMaybeT f x = OptionT.map f x
        let runListT  = ListT.run
        let inline liftIO (x: IO<'a>) = liftAsync x

            
        // ContT
        let runContT  = ContT.run
        type ContT = ContT
        let ContT = ContT.ContT
            
        // ReaderT
        let runReaderT = ReaderT.run
        type ReaderT = ReaderT
        let ReaderT = ReaderT.ReaderT
            
        // StateT
        let runStateT = StateT.run
        type StateT = StateT
        let StateT = StateT.StateT
            
        // MonadError
        let inline throwError x   = throw x
        let inline catchError v h = catch v h
            
        // ErrorT
        let runErrorT = ErrorT.run
        type ErrorT = ErrorT
        let ErrorT = ErrorT.ErrorT