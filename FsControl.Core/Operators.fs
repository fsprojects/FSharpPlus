namespace FsControl

open FsControl

module Operators =

    // Functor ----------------------------------------------------------------

    /// Lift a function into a Functor.
    let inline map    (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

    /// Lift a function into a Functor. Same as map.
    let inline (<!>)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

    /// Lift a function into a Functor. Same as map.
    let inline (<<|)  (f:'T->'U) (x:'``Functor<'T>``) :'``Functor<'U>`` = Map.Invoke f x

    /// Lift a function into a Functor. Same as map but with flipped arguments.
    let inline (|>>)  (x:'``Functor<'T>``) (f:'T->'U) :'``Functor<'U>`` = Map.Invoke f x

    /// Like map but ignoring the results.
    let inline iter   (action :'T->unit) (source :'``Functor<'T>``) :unit = Iterate.Invoke action source
   

    // Applicative ------------------------------------------------------------

    /// Lift a value into a Functor. Same as return in Computation Expressions.
    let inline result (x:'T): '``Functor<'T>`` = Return.Invoke x

    /// Apply a lifted argument to a lifted function.
    let inline (<*>) (x:'``Applicative<'T -> 'U>``) (y:'``Applicative<'T>``): '``Applicative<'U>`` = Apply.Invoke x y : '``Applicative<'U>``

    /// Apply 2 lifted arguments to a lifted function.
    let inline liftA2 (f:'T->'U->'V) (a:'``Applicative<'T>``) (b:'``Applicative<'U>``) : '``Applicative<'V>`` = f <!> a <*> b

    let inline (  *>)   (x:'``Applicative<'T>``) : '``Applicative<'U>``->'``Applicative<'U>`` = x |> liftA2 (fun   _ -> id)
    let inline (<*  )   (x:'``Applicative<'T>``) : '``Applicative<'U>``->'``Applicative<'T>`` = x |> liftA2 (fun k _ -> k )
    let inline (<**>)   (x:'``Applicative<'T>``) : '``Applicative<'T -> 'U>``->'``Applicative<'U>`` = x |> liftA2 (|>)
    let inline optional (v:'``Applicative<'T>``) : '``Applicative<Option'T>`` = Some <!> v <|> result None


    // Monad -----------------------------------------------------------
    
    /// Takes a monadic value and a function from a plain type to a monadic value, and returns a new monadic value.
    let inline (>>=) (x:'``Monad<'T>``) (f:'T->'``Monad<'U>``) :'``Monad<'U>`` = Bind.Invoke x f

    /// Takes a function from a plain type to a monadic value and a monadic value, and returns a new monadic value.
    let inline (=<<) (f:'T->'``Monad<'U>``) (x:'``Monad<'T>``) :'``Monad<'U>`` = Bind.Invoke x f

    let inline (>=>) (f:'T->'``Monad<'U>``) (g:'U->'``Monad<'V>``) (x:'T) : '``Monad<'V>`` = Bind.Invoke (f x) g
    let inline (<=<) (g:'b->'``Monad<'V>``) (f:'T->'``Monad<'U>``) (x:'T) : '``Monad<'V>`` = Bind.Invoke (f x) g

    /// Flattens two layers of monadic information into one.
    let inline join  (x:'``Monad<Monad<'T>>``) : '``Monad<'T>`` = Join.Invoke x


    // Monoid -----------------------------------------------------------------

    let inline getEmpty() :'Monoid = Empty.Invoke()
    let inline (++)   (x:'Monoid) (y:'Monoid): 'Monoid = Append.Invoke x y
    let inline append (x:'Monoid) (y:'Monoid): 'Monoid = Append.Invoke x y
    let inline concat (x:seq<'Monoid>)       : 'Monoid = Concat.Invoke x


    // Alternative/Monadplus/Arrowplus ----------------------------------------

    let inline getMZero() :'``Functor<'T>`` = MZero.Invoke()
    let inline (<|>) (x:'``Functor<'T>``) (y:'``Functor<'T>``) : '``Functor<'T>`` = MPlus.Invoke x y
    let inline guard x: '``MonadPlus<unit>`` = if x then Return.Invoke () else MZero.Invoke()

   
    // Contravariant/Bifunctor/Profunctor -------------------------------------

    let inline contramap (f:'U->'T) (x:'``Contravariant<'T>``) :'``Contravariant<'U>`` = Contramap.Invoke f x
    let inline bimap  (f : 'T->'U) (g : 'V->'W) (source : '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'W>`` = Bimap.Invoke  f g source
    let inline first  (f : 'T->'V) (source : '``Bifunctor<'T,'V>``) : '``Bifunctor<'U,'V>`` = First.Invoke  f source
    let inline second (f : 'V->'W) (source : '``Bifunctor<'T,'V>``) : '``Bifunctor<'T,'W>`` = Second.Invoke f source
    let inline dimap  (f : 'A->'B) (g:'C->'D) (source : '``Profunctor<'B,'C>``) : '``Profunctor<'A,'D>`` = Dimap.Invoke  f g source
    let inline lmap   (f : 'A->'B) (source : ^``Profunctor<'B,'C>``) : ^``Profunctor<'A,'C>`` = LMap.Invoke f source
    let inline rmap   (f : 'C->'D) (source : '``Profunctor<'B,'C>``) : '``Profunctor<'B,'D>`` = RMap.Invoke f source


    // Category ---------------------------------------------------------------

    /// the identity morphism.
    let inline getCatId() = Id.Invoke() : '``Category<'T,'T>``

    /// Right-to-left morphism composition.
    let inline (<<<<) (f : '``Category<'U,'V>``) (g : '``Category<'T,'U>``) : '``Category<'T,'V>`` = Comp.Invoke f g

    /// Left-to-right morphism composition.
    let inline (>>>>) (g : '``Category<'T,'U>``) (f : '``Category<'U,'V>``) : '``Category<'T,'V>`` = Comp.Invoke f g

    
    // Arrow ------------------------------------------------------------------

    /// Lift a function to an arrow.
    let inline arr (f : 'T -> 'U) : '``Arrow<'T,'U>`` = Arr.Invoke f

    /// Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.
    let inline arrFirst  (f : '``Arrow<'T,'U>``) : '``Arrow<('T * 'V),('U * 'V)>`` = ArrFirst.Invoke f

    /// Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.
    let inline arrSecond (f : '``Arrow<'T,'U>``) : '``Arrow<('V * 'T),('V * 'U)>`` = ArrSecond.Invoke f

    /// Split the input between the two argument arrows and combine their output. Note that this is in general not a functor.
    let inline ( ****) (f : '``ArrowChoice<'T1,'U1>``) (g : '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<('T1*'T2),('U1*'U2)>`` = arrFirst f >>>> arrSecond g

    /// Fanout: send the input to both argument arrows and combine their output.
    let inline (&&&&) (f : '``ArrowChoice<'T,'U1>``) (g : '``ArrowChoice<'T,'U2>``) : '``ArrowChoice<'T,('U1*'U2)>`` = arr (fun b -> (b,b)) >>>> f **** g

    /// Fanin: Split the input between the two argument arrows and merge their outputs.
    let inline (||||) (f : '``ArrowChoice<'T,'V>``) (g : '``ArrowChoice<'U,'V>``) : '``ArrowChoice<Choice<'U,'T>,'V>`` = AcEither.Invoke f g

    /// Split the input between both argument arrows, retagging and merging their outputs. Note that this is in general not a functor.
    let inline (++++) (f : '``ArrowChoice<'T1,'U1>``) (g : '``ArrowChoice<'T2,'U2>``) : '``ArrowChoice<Choice<'T2,'T1>,Choice<'U2,'U1>>`` = AcMerge.Invoke f g

    /// Feed marked inputs through the left argument arrow, passing the rest through unchanged to the output.
    let inline left  (f : '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'V,'T>,Choice<'V,'U>>`` =  AcLeft.Invoke f

    /// Feed marked inputs through the right argument arrow, passing the rest through unchanged to the output.
    let inline right (f : '``ArrowChoice<'T,'U>``) : '``ArrowChoice<Choice<'T,'V>,Choice<'U,'V>>`` = AcRight.Invoke f

    /// Apply an arrow produced as the output of some previous computation to an input, producing its output as the output of app. 
    let inline getArrApply() = ArrApply.Invoke() : '``ArrowApply<('ArrowApply<'T,'U> * 'T)>,'U)>``


    // Foldable

    let inline ofList (source :list<'T>) = OfList.Invoke source
    let inline ofSeq  (source :seq<'T> ) = OfSeq.Invoke  source
    let inline foldBack (folder:'T->'State->'State) (foldable:'``Foldable<'T>``) (state:'State) : 'State = FoldBack.Invoke folder state foldable
    let inline fold     (folder:'State->'T->'State) (state:'State) (foldable:'``Foldable<'T>``) : 'State = Fold.Invoke folder state foldable
    let inline foldMap (f:'T->'Monoid) (x:'``Foldable<'T>``) : 'Monoid = FoldMap.Invoke f x
    let inline toList  value :'T list = ToList.Invoke  value
    let inline toArray value :'T []   = ToArray.Invoke value
    let inline exists     (predicate :'T->bool) (source:'``Foldable<'T>``)   = Exists.Invoke  predicate source  :bool
    let inline forall     (predicate :'T->bool) (source:'``Foldable<'T>``)   = ForAll.Invoke  predicate source  :bool
    let inline find       (predicate :'T->bool) (source:'``Foldable<'T>``)   = Find.Invoke    predicate source  :'T
    let inline tryFind    (predicate :'T->bool) (source:'``Foldable<'T>``)   = TryFind.Invoke predicate source  :'T option
    let inline pick     (chooser:'T->'U option) (source:'``Foldable<'T>``)   = Pick.Invoke    chooser   source  :'U
    let inline tryPick  (chooser:'T->'U option) (source:'``Foldable<'T>``)   = TryPick.Invoke chooser   source  :'U option
    let inline filter (predicate:_->bool) (x:'``Foldable<'a>``) :'``Foldable<'a>`` =  Filter.Invoke predicate x


    // Traversable

    /// Map each element of a structure to an action, evaluate these actions from left to right, and collect the results.
    let inline traverse (f:'T->'``Applicative<'U>``) (t:'``Traversable<'T>>``) : '``Applicative<'Traversable<'U>>`` = Traverse.Invoke f t

    /// Evaluate each action in the structure from left to right, and and collect the results.
    let inline sequenceA (t:'``Traversable<'Applicative<'T>>``) :'``Applicative<'Traversable<'T>>`` = SequenceA.Invoke t


    // Indexable

    /// Get an item from the given index.
    let inline item (n:'K) (source:'``Indexed<'T>``) : 'T = Item.Invoke n source

    /// Map with access to the index.
    let inline mapi (mapping:'K->'T->'U) (source:'``FunctorWithIndex<'T>``) : '``FunctorWithIndex<'U>`` = MapIndexed.Invoke mapping source

    /// Map an action with access to an index.
    let inline iteri (action:'K->'T->unit) (source:'``FunctorWithIndex<'T>``) : unit = IterateIndexed.Invoke action source

    /// Left-associative fold of an indexed container with access to the index i.
    let inline foldi (folder:'State->'K->'T->'State) (state:'State) (source:'``FoldableWithIndex<'T>``) : 'State = FoldIndexed.Invoke folder state source
    
    /// Traverse an indexed container. Behaves exactly like a regular traverse except that the traversing function also has access to the key associated with a value.
    let inline traversei (f:'K->'T->'``Applicative<'U>``) (t:'``Traversable<'T>>``) : '``Applicative<'Traversable<'U>>`` = TraverseIndexed.Invoke f t  


    // Comonads

    let inline extract (x:'``Comonad<'T>``): 'T = Extract.Invoke x
    let inline extend (g:'``Comonad<'T>``->'U) (s:'``Comonad<'T>``): '``Comonad<'U>`` = Extend.Invoke g s
    let inline (=>>)  (s:'``Comonad<'T>``) (g:'``Comonad<'T>``->'U): '``Comonad<'U>`` = Extend.Invoke g s

    /// 'Comonad<'T> -> 'Comonad<'Comonad<'T>>
    let inline duplicate x = Duplicate.Invoke x  


    // Monad Transformers

    /// Lift a computation from the inner monad to the constructed monad.
    let inline lift      (x:'``Monad<'T>``) : '``MonadTrans<'Monad<'T>>`` = Lift.Invoke x

    /// A lift specializaed for Async<'T> which is able to bring an Async value from any depth of monad-layers.
    let inline liftAsync (x:Async<'T>) : '``MonadAsync<'T>`` = LiftAsync.Invoke x

    /// (call-with-current-continuation) calls a function with the current continuation as its argument.
    let inline callCC (f:('T->'``MonadCont<'R,'U>``)->'``MonadCont<'R,'T>``) : '``MonadCont<'R,'T>`` = CallCC.Invoke f
   
    /// Return the state from the internals of the monad.
    let inline get< ^``MonadState<'S * 'S>`` when ^``MonadState<'S * 'S>`` : (static member Get : ^``MonadState<'S * 'S>``)> = (^``MonadState<'S * 'S>`` : (static member Get : _) ())

    /// Replace the state inside the monad.
    let inline put (x:'S) : '``MonadState<unit * 'S>`` = Put.Invoke x

    /// Retrieves the monad environment.
    let inline ask< ^``MonadReader<'R,'T>`` when ^``MonadReader<'R,'T>`` : (static member Ask : ^``MonadReader<'R,'T>``)> = (^``MonadReader<'R,'T>`` : (static member Ask : _) ())
   
    /// <summary> Executes a computation in a modified environment. </summary>
    /// <param name="f"> The function to modify the environment.    </param>
    /// <param name="m"> Reader to run in the modified environment. </param>
    let inline local (f:'R1->'R2) (m:'``MonadReader<'R2,'T>``) : '``MonadReader<'R1,'T>`` = Local.Invoke f m

    /// Embeds a simple writer action.
    let inline tell (w:'Monoid) : '``MonadWriter<'Monoid,unit>`` = Tell.Invoke w

    /// <summary> An action that executes the action <paramref name="m"/> and adds its output to the value of the computation. </summary>
    /// <param name="m">The action to be executed.</param>
    let inline listen (m:'``MonadWriter<'Monoid,'T>``) : '``MonadWriter<'Monoid,('T * 'Monoid)>`` = Listen.Invoke m

    /// Action that executes the action m, which returns a value and a function, and returns the value, applying the function to the output.
    let inline pass (m:'``MonadWriter<'Monoid,('T * ('Monoid -> 'Monoid))>``) : '``MonadWriter<'Monoid,'T>`` = Pass.Invoke m

    /// Throws an error value inside the Error monad.
    let inline throw (error:'E) : '``'MonadError<'E,'T>`` = Throw.Invoke error

    /// <summary> Executes a handler when the value contained in the Error monad represents an error.  </summary>
    let inline catch (value:'``'MonadError<'E1,'T>``) (handler:'E1->'``'MonadError<'E2,'T>``) : '``'MonadError<'E2,'T>`` = Catch.Invoke value handler


    // Collection

    let inline nth (n:int) (source:'``Collection<'T>``) : 'T = Nth.Invoke n source

    /// <summary>Returns a collection that skips N elements of the original collection and then yields the
    /// remaining elements of the collection.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// when count exceeds the number of elements in the collection. <c>drop</c>
    /// returns an empty collection instead of throwing an exception.</remarks>
    /// <param name="count">The number of items to skip.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the collection.</exception>
    let inline skip  (count:int) (source:'``Collection<'T>``) : '``Collection<'T>`` = Skip.Invoke count source

    /// <summary>Returns the first N elements of the collection.</summary>
    /// <remarks>Throws <c>InvalidOperationException</c>
    /// if the count exceeds the number of elements in the collection. <c>limit</c>
    /// returns as many items as the collection contains instead of throwing an exception.</remarks>
    ///
    /// <param name="count">The number of items to take.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input collection is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input collection is empty.</exception>
    /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the collection.</exception>
    let inline take  (count:int) (source:'``Collection<'T>``) : '``Collection<'T>`` = Take.Invoke count source

    /// <summary>Returns a collection that drops N elements of the original collection and then yields the
    /// remaining elements of the collection.</summary>
    /// <param name="count">The number of items to drop.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result collection.</returns>
    let inline drop  (count:int) (source:'``Collection<'T>``) : '``Collection<'T>`` = Drop.Invoke count source

    /// <summary>Returns a collection with at most N elements.</summary>
    ///
    /// <param name="count">The maximum number of items to return.</param>
    /// <param name="source">The input collection.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    let inline limit (count:int) (source:'``Collection<'T>``) : '``Collection<'T>`` = Limit.Invoke count source

    let inline groupBy    (projection:'T->'Key) (source:'``Collection<'T>``) : '``Collection<'Key * 'Collection<'T>>`` = GroupBy.Invoke projection source
    let inline groupAdjBy (projection:'T->'Key) (source:'``Collection<'T>``) : '``Collection<'Key * 'Collection<'T>>`` = GroupAdjBy.Invoke projection source


    let inline choose (chooser:'T->'U option)   (source:'``Collection<'T>``) : '``Collection<'U>`` = Choose.Invoke chooser source        

    let inline distinct                         (source:'``Collection<'T>``) : '``Collection<'T>`` = Distinct.Invoke              source 
    let inline distinctBy (projection:'T->'Key) (source:'``Collection<'T>``) : '``Collection<'T>`` = DistinctBy.Invoke projection source 
    
    let inline head                             (source:'``Collection<'T>``)        = Head.Invoke source    :'T
    let inline tryHead                          (source:'``Collection<'T>``)        = TryHead.Invoke source :'T option

    let inline intersperse      (sep:'T)        (source:'``Collection<'T>``)        = Intersperse.Invoke sep source        : '``Collection<'T>``

    let inline length (source:'``Collection<'T>``) :int                             = Length.Invoke source

    let inline maxBy (projection:'T->'U) (source:'``Collection<'T>``)               = MaxBy.Invoke projection  source    : 'T
    let inline minBy (projection:'T->'U) (source:'``Collection<'T>``)               = MinBy.Invoke projection  source    : 'T

    let inline rev  (source:'``Collection<'T>``)                                    = Rev.Invoke source :'``Collection<'T>``
    let inline scan (folder:'State'->'T->'State) state (source:'``Collection<'T>``) = Scan.Invoke folder (state:'State) source : '``Collection<'State>``

    let inline sort                         (source:'``Collection<'T>``) : '``Collection<'T>`` = Sort.Invoke source 
    let inline sortBy (projection:'T->'Key) (source:'``Collection<'T>``) : '``Collection<'T>`` = SortBy.Invoke projection source
    let inline toSeq (source:'``Collection<'T>``) = ToSeq.Invoke source  :seq<'T>

    let inline zip (source1:'``Collection<'T1>``) (source2:'``Collection<'T2>``) : '``Collection<'T1 * 'T2>`` = Zip.Invoke source1 source2    




    // Tuple
    
    /// Gets the value of the first component of a tuple.
    let inline item1 tuple = Item1.Invoke tuple

    /// Gets the value of the second component of a tuple.
    let inline item2 tuple = Item2.Invoke tuple

    /// Gets the value of the third component of a tuple.
    let inline item3 tuple = Item3.Invoke tuple

    /// Gets the value of the fourth component of a tuple.
    let inline item4 tuple = Item4.Invoke tuple

    /// Gets the value of the fifth component of a tuple.
    let inline item5 tuple = Item5.Invoke tuple

    let inline mapItem1 mapping tuple = MapItem1.Invoke mapping tuple
    let inline mapItem2 mapping tuple = MapItem2.Invoke mapping tuple
    let inline mapItem3 mapping tuple = MapItem3.Invoke mapping tuple
    let inline mapItem4 mapping tuple = MapItem4.Invoke mapping tuple
    let        mapItem5 mapping tuple = MapItem5.MapItem5(tuple, mapping)
    
    
    
    // Converter

    /// Convert using the explicit operator.
    let inline explicit    (value:'T) :'U = Explicit.Invoke value

    let inline ofBytesWithOptions (isLtEndian:bool) (startIndex:int) (value:byte[]) = OfBytes.Invoke isLtEndian startIndex value
    let inline ofBytes   (value:byte[]) = OfBytes.Invoke true 0 value
    let inline ofBytesBE (value:byte[]) = OfBytes.Invoke false 0 value

    let inline toBytes   value :byte[] = ToBytes.Invoke true value
    let inline toBytesBE value :byte[] = ToBytes.Invoke false value

    let inline toStringWithCulture (cultureInfo:System.Globalization.CultureInfo) value:string = ToString.Invoke cultureInfo value
    let inline toString  value:string  = ToString.Invoke System.Globalization.CultureInfo.InvariantCulture value  
     
    /// Converts to a value from its string representation.
    let inline parse    (value:string) = Parse.Invoke    value

    /// Converts to a value from its string representation. Returns None if the convertion doesn't succeed.
    let inline tryParse (value:string) = TryParse.Invoke value


    // Numerics

    /// Gets a value that represents the number 0 (zero).
    let inline getZero() = Zero.Invoke()

    /// Gets a value that represents the number 1 (one).
    let inline getOne()  = One.Invoke()

    /// Divides one number by another, returns a tuple with the result and the remainder.
    let inline divRem (D:'T) (d:'T) :'T*'T = DivRem.Invoke D d

    /// Returns the smallest possible value.
    let inline minValue() = MinValue.Invoke()

    /// Returns the largest possible value.
    let inline maxValue() = MaxValue.Invoke()

    /// Converts from BigInteger to the inferred destination type.
    let inline fromBigInt  (x:bigint)    :'Num   = FromBigInt.Invoke x

    /// Converts to BigInteger.
    let inline toBigInt    (x:'Integral) :bigint = ToBigInt.Invoke x

    /// Gets the pi number.
    let inline getPi() :'Floating = Pi.Invoke()

    /// Returns the additive inverse of the number.
    let inline negate  (x:'Num): 'Num = x |> TryNegate.Invoke |> function Choice1Of2 x -> x | Choice2Of2 e -> raise e

    /// Returns the additive inverse of the number.
    /// Works also for unsigned types (Throws an exception if there is no inverse).
    let inline negate'  (x:'Num): 'Num = x |> TryNegate'.Invoke |> function Choice1Of2 x -> x | Choice2Of2 e -> raise e

    /// Returns the additive inverse of the number.
    /// Works also for unsigned types (Returns none if there is no inverse).
    let inline tryNegate'  (x:'Num): 'Num option = TryNegate'.Invoke x |> function Choice1Of2 x -> Some x | Choice2Of2 e -> None

    /// Returns the subtraction between two numbers. Throws an error if the result is negative on unsigned types.
    let inline subtract (x:'Num) (y:'Num): 'Num = Subtract.Invoke x y

    /// Returns the subtraction between two numbers. Returns None if the result is negative on unsigned types.
    let inline trySubtract (x:'Num) (y:'Num): 'Num option = y |> TrySubtract.Invoke x |> function Choice1Of2 x -> Some x | Choice2Of2 e -> None

    /// Returns the division between two numbers. If the numbers are not divisible throws an error.
    let inline div (dividend:'Num) (divisor:'Num): 'Num = Divide.Invoke dividend divisor

    /// Returns the division between two numbers. Returns None if the numbers are not divisible.
    let inline tryDiv (dividend:'Num) (divisor:'Num): 'Num option = divisor |> TryDivide.Invoke dividend |> function Choice1Of2 x -> Some x | Choice2Of2 e -> None

    /// Returns the square root of a number of any type. Throws an exception if there is no square root.
    let inline sqrt x = x |> Sqrt.Invoke

    /// Returns the square root of a number of any type. Returns None if there is no square root.
    let inline trySqrt x = x |> TrySqrt.Invoke |> function Choice1Of2 x -> Some x | Choice2Of2 _ -> None

    /// Returns the square root of an integral number.
    let inline isqrt   (x:'Integral): 'Integral = x |> TrySqrtRem.Invoke |> function Choice1Of2 (x, _) -> x | Choice2Of2 e -> raise e

    /// Returns the square root of an integral number.
    let inline sqrtRem   (x:'Integral): 'Integral*'Integral = x |> TrySqrtRem.Invoke |> function Choice1Of2 x -> x | Choice2Of2 e -> raise e

    /// <summary> Returns a number which represents the sign.
    /// <para/>   Rule: signum x * abs x = x        </summary>
    let inline signum (x:'Num): 'Num = Signum.Invoke x

    /// <summary> Returns a number which represents the sign.
    ///           Works also for unsigned types. 
    /// <para/>   Rule: signum x * abs x = x        </summary>
    let inline signum' (x:'Num): 'Num = Signum'.Invoke x

    /// <summary> Gets the absolute value of a number.
    /// <para/>   Rule: signum x * abs x = x        </summary>
    let inline abs    (x:'Num): 'Num = Abs.Invoke x

    /// <summary> Gets the absolute value of a number.
    ///           Works also for unsigned types. 
    /// <para/>   Rule: signum x * abs x = x        </summary>
    let inline abs'   (x:'Num): 'Num = Abs'.Invoke x
