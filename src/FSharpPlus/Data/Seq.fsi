namespace FSharpPlus.Data
    
    /// Additional operations on Seq
    module Seq =
        
        val inline sequence:
          ms: seq< ^Applicative<'T>> ->  ^Applicative<seq<'T>>
            when (Control.Apply or  ^a or  ^Applicative<'T> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^b * Control.Apply ->  ^b) and
                 (Control.IsLeftZero or  ^Applicative<'T>) :
                   (static member IsLeftZero:
                       ^Applicative<'T> ref * Control.IsLeftZero -> bool) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^Applicative<seq<'T>>) :
                   (static member Map:
                      ( ^b * ('e list -> seq<'e>)) * Control.Map
                        ->  ^Applicative<seq<'T>>) and
                 (Control.Sequence or seq< ^Applicative<'T>> or
                   ^Applicative<seq<'T>>) :
                   (static member Sequence:
                      seq< ^Applicative<'T>> *  ^Applicative<seq<'T>> *
                      Control.Sequence ->  ^Applicative<seq<'T>>)
        
        val inline traverse:
          f: ('T ->  ^Applicative<'U>) -> xs: seq<'T> ->  ^Applicative<seq<'U>>
            when (Control.Apply or  ^a or  ^Applicative<'U> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'U> *  ^b * Control.Apply ->  ^b) and
                 (Control.IsLeftZero or  ^Applicative<'U>) :
                   (static member IsLeftZero:
                       ^Applicative<'U> ref * Control.IsLeftZero -> bool) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^Applicative<seq<'U>>) :
                   (static member Map:
                      ( ^b * ('e list -> seq<'e>)) * Control.Map
                        ->  ^Applicative<seq<'U>>) and
                 (Control.Traverse or seq<'T> or  ^Applicative<seq<'U>>) :
                   (static member Traverse:
                      seq<'T> * ('T ->  ^Applicative<'U>) *
                       ^Applicative<seq<'U>> * Control.Traverse
                        ->  ^Applicative<seq<'U>>)
        
        val inline replicateM:
          count: int -> initial:  ^Applicative<'T> ->  ^e
            when (Control.IsLeftZero or  ^Applicative<'T>) :
                   (static member IsLeftZero:
                       ^Applicative<'T> ref * Control.IsLeftZero -> bool) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or  ^b) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^b * Control.Apply ->  ^b) and
                 (Control.Map or  ^b or  ^a) :
                   (static member Map:
                      ( ^b * ('d list -> 'd -> 'd list)) * Control.Map ->  ^a) and
                 (Control.Return or  ^b) :
                   (static member Return:
                       ^b * Control.Return -> ('c list ->  ^b)) and
                 (Control.Map or  ^b or  ^e) :
                   (static member Map:
                      ( ^b * ('f list -> seq<'f>)) * Control.Map ->  ^e) and
                 (Control.Sequence or seq< ^Applicative<'T>> or  ^e) :
                   (static member Sequence:
                      seq< ^Applicative<'T>> *  ^e * Control.Sequence ->  ^e)
    
    module Internal =
        
        val inline monomorphicBind:
          binder: ('T ->  ^Monad<'T>) -> source:  ^Monad<'T> ->  ^Monad<'T>
            when (Control.Bind or  ^Monad<'T>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<'T>) ->  ^Monad<'T>)
    
    type MonadFxStrictBuilderMod<'monad<'t>> =
        inherit GenericBuilders.MonadFxStrictBuilder<'monad<'t>>
        
        new: unit -> MonadFxStrictBuilderMod<'monad<'t>>
        
        member
          inline Delay: expr: (unit ->  ^Monad<'T>) -> (unit ->  ^Monad<'T>)
                          when (Control.Delay or  ^Monad<'T>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<'T>) *
                                    Control.Delay ->  ^Monad<'T>)
    
    type MonadPlusStrictBuilderMod<'monad<'t>> =
        inherit GenericBuilders.MonadPlusStrictBuilder<'monad<'t>>
        
        new: unit -> MonadPlusStrictBuilderMod<'monad<'t>>
        
        member
          inline Delay: expr: (unit ->  ^Monad<'T>) -> (unit ->  ^Monad<'T>)
                          when (Control.Delay or  ^Monad<'T>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<'T>) *
                                    Control.Delay ->  ^Monad<'T>)
    
    type MonadFxStrictBuilderMod2<'monad<'t>, ^monad<unit>
                                    when (Control.Return or  ^monad<unit>) :
                                           (static member Return:
                                               ^monad<unit> * Control.Return
                                                -> (unit ->  ^monad<unit>)) and
                                         (Control.Bind or  ^monad<unit>) :
                                           (static member (>>=) :
                                               ^monad<unit> *
                                              (unit ->  ^monad<unit>)
                                                ->  ^monad<unit>) and
                                         (Control.Using or  ^monad<unit>) :
                                           (static member Using:
                                              System.IDisposable *
                                              (System.IDisposable
                                                 ->  ^monad<unit>) *
                                              Control.Using ->  ^monad<unit>)> =
        inherit GenericBuilders.StrictBuilder<'monad<'t>>
        
        new: unit -> MonadFxStrictBuilderMod2<'monad<'t>, ^monad<unit>>
        
        member
          inline Combine: a:  ^Monad<unit> * b: (unit ->  ^Monad<'T>)
                            ->  ^Monad<'T>
                            when (Control.Bind or  ^Monad<unit> or  ^Monad<'T>) :
                                   (static member (>>=) :
                                       ^Monad<unit> * (unit ->  ^Monad<'T>)
                                        ->  ^Monad<'T>)
        
        member
          inline Delay: expr: (unit ->  ^Monad<'T>) -> (unit ->  ^Monad<'T>)
                          when (Control.Delay or  ^Monad<'T>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<'T>) *
                                    Control.Delay ->  ^Monad<'T>)
        
        member
          inline For: p: #seq<'T> * rest: ('T ->  ^monad<unit>) ->  ^monad<unit>
        
        member
          inline While: guard: (unit -> bool) * body: (unit ->  ^monad<unit>)
                          ->  ^monad<unit>
        
        member inline Zero: unit ->  ^monad<unit>
    
    module SpecialBuilders =
        
        val innerMonad<'mt> : MonadFxStrictBuilderMod<'mt>
        
        val inline innerMonad2:
          unit -> MonadFxStrictBuilderMod2<'mt, ^a>
            when (Control.Return or  ^a) :
                   (static member Return:  ^a * Control.Return -> (unit ->  ^a)) and
                 (Control.Bind or  ^a) :
                   (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                 (Control.Using or  ^a) :
                   (static member Using:
                      System.IDisposable * (System.IDisposable ->  ^a) *
                      Control.Using ->  ^a)
    
    type IEnumeratorM<'Monad<bool>,'T> =
        inherit System.IDisposable
        
        abstract MoveNext: unit -> 'Monad<bool>
        
        abstract Current: 'T
    
    type IEnumerableM<'Monad<bool>,'T> =
        
        abstract GetEnumerator: unit -> IEnumeratorM<'Monad<bool>,'T>
    
    [<Struct>]
    type SeqT<'monad,'t> =
        | SeqT of IEnumerableM<'monad,'t>
        interface IEnumerableM<'monad,'t>
        
        /// <summary>
        /// Sequences two lists left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( *> ) : x: SeqT< ^Monad<bool>,'T> * y: SeqT< ^Monad<bool>,'U>
                            -> SeqT< ^Monad<bool>,'U>
                            when (Control.Bind or  ^Monad<bool>) :
                                   (static member (>>=) :
                                       ^Monad<bool> * (bool ->  ^Monad<bool>)
                                        ->  ^Monad<bool>) and
                                 (Control.Return or  ^Monad<bool>) :
                                   (static member Return:
                                       ^Monad<bool> * Control.Return
                                        -> (bool ->  ^Monad<bool>)) and
                                 (Control.Delay or  ^Monad<bool>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^Monad<bool>) *
                                      Control.Delay ->  ^Monad<bool>)
        
        /// <summary>
        /// Sequences two lists left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member
          inline ( <* ) : x: SeqT< ^Monad<bool>,'U> * y: SeqT< ^Monad<bool>,'T>
                            -> SeqT< ^Monad<bool>,'U>
                            when (Control.Bind or  ^Monad<bool>) :
                                   (static member (>>=) :
                                       ^Monad<bool> * (bool ->  ^Monad<bool>)
                                        ->  ^Monad<bool>) and
                                 (Control.Return or  ^Monad<bool>) :
                                   (static member Return:
                                       ^Monad<bool> * Control.Return
                                        -> (bool ->  ^Monad<bool>)) and
                                 (Control.Delay or  ^Monad<bool>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^Monad<bool>) *
                                      Control.Delay ->  ^Monad<bool>)
        
        static member
          inline (<!>) : x: SeqT< ^Monad<bool>,'T> * f: ('T -> 'U)
                           -> SeqT< ^Monad<bool>,'U>
                           when (Control.Delay or  ^Monad<bool>) :
                                  (static member Delay:
                                     Control.Delay * (unit ->  ^Monad<bool>) *
                                     Control.Delay ->  ^Monad<bool>) and
                                (Control.Bind or  ^Monad<bool>) :
                                  (static member (>>=) :
                                      ^Monad<bool> * (bool ->  ^Monad<bool>)
                                       ->  ^Monad<bool>) and
                                (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>))
        
        static member
          inline (<*>) : f: SeqT< ^Monad<bool>,('T -> 'U)> *
                         x: SeqT< ^Monad<bool>,'T> -> SeqT< ^Monad<bool>,'U>
                           when (Control.Bind or  ^Monad<bool>) :
                                  (static member (>>=) :
                                      ^Monad<bool> * (bool ->  ^Monad<bool>)
                                       ->  ^Monad<bool>) and
                                (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>))
        
        static member
          inline (<|>) : x: SeqT< ^Monad<bool>,'T> * y: SeqT< ^Monad<bool>,'T>
                           -> SeqT< ^Monad<bool>,'T>
                           when (Control.Delay or  ^Monad<bool>) :
                                  (static member Delay:
                                     Control.Delay * (unit ->  ^Monad<bool>) *
                                     Control.Delay ->  ^Monad<bool>) and
                                (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>)) and
                                (Control.Bind or  ^Monad<bool>) :
                                  (static member (>>=) :
                                      ^Monad<bool> * (bool ->  ^Monad<bool>)
                                       ->  ^Monad<bool>)
        
        static member
          inline (>>=) : x: SeqT< ^Monad<bool>,'T> *
                         f: ('T -> SeqT< ^Monad<bool>,'U>)
                           -> SeqT< ^Monad<bool>,'U>
                           when (Control.Bind or  ^Monad<bool>) :
                                  (static member (>>=) :
                                      ^Monad<bool> * (bool ->  ^Monad<bool>)
                                       ->  ^Monad<bool>) and
                                (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>))
        
        static member
          inline CallCC: f: (('T -> SeqT< ^MonadCont<'R>,'U>) -> SeqT< ^d,'e>)
                           -> SeqT< ^MonadCont<'R>,'T>
                           when (Control.Bind or  ^a or  ^MonadCont<'R>) :
                                  (static member (>>=) :
                                      ^a * (seq<'T> ->  ^MonadCont<'R>)
                                       ->  ^MonadCont<'R>) and
                                (Control.Return or  ^MonadCont<'R>) :
                                  (static member Return:
                                      ^MonadCont<'R> * Control.Return
                                       -> (bool ->  ^MonadCont<'R>)) and
                                (Control.Bind or  ^f or  ^MonadCont<'R>) :
                                  (static member (>>=) :
                                      ^f * (seq<'U> ->  ^MonadCont<'R>)
                                       ->  ^MonadCont<'R>) and
                                (Control.Map or  ^b or  ^a) :
                                  (static member Map:
                                     ( ^b * ('e[] -> seq<'e>)) * Control.Map
                                       ->  ^a) and
                                 ^a:
                                  (static member CallCC:
                                     ((seq<'T> ->  ^f) ->  ^a) ->  ^a) and
                                (Control.Bind or  ^c or  ^b) :
                                  (static member (>>=) :
                                      ^c * (unit ->  ^b) ->  ^b) and
                                (Control.Bind or  ^d or  ^b) :
                                  (static member (>>=) :
                                      ^d * (bool ->  ^b) ->  ^b) and
                                (Control.Using or  ^b) :
                                  (static member Using:
                                     IEnumeratorM< ^d,'e> *
                                     (IEnumeratorM< ^d,'e> ->  ^b) *
                                     Control.Using ->  ^b) and
                                (Control.Return or  ^b) :
                                  (static member Return:
                                      ^b * Control.Return -> ('e[] ->  ^b)) and
                                (Control.Bind or  ^d or  ^c) :
                                  (static member (>>=) :
                                      ^d * (bool ->  ^c) ->  ^c) and
                                (Control.Bind or  ^c) :
                                  (static member (>>=) :
                                      ^c * (unit ->  ^c) ->  ^c) and
                                (Control.Return or  ^c) :
                                  (static member Return:
                                      ^c * Control.Return -> (unit ->  ^c))
        
        static member
          inline Catch: m: SeqT< ^MonadError<'E1>,'T> *
                        h: ('E1 -> SeqT< ^MonadError<'E2>,'T>)
                          -> SeqT< ^MonadError<'E2>,'T>
                          when (Control.Bind or  ^MonadError<'E1> or  ^a) :
                                 (static member (>>=) :
                                     ^MonadError<'E1> * (bool ->  ^a) ->  ^a) and
                               (Control.Bind or  ^MonadError<'E1> or  ^b) :
                                 (static member (>>=) :
                                     ^MonadError<'E1> * (bool ->  ^b) ->  ^b) and
                               (Control.Bind or  ^b or  ^a) :
                                 (static member (>>=) :
                                     ^b * (unit ->  ^a) ->  ^a) and
                               (Control.Using or  ^a) :
                                 (static member Using:
                                    IEnumeratorM< ^MonadError<'E1>,'T> *
                                    (IEnumeratorM< ^MonadError<'E1>,'T> ->  ^a) *
                                    Control.Using ->  ^a) and
                               (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> ('T[] ->  ^a)) and
                               (Control.Map or  ^a or  ^c) :
                                 (static member Map:
                                    ( ^a * ('T[] -> seq<'T>)) * Control.Map
                                      ->  ^c) and
                               (Control.Bind or  ^b) :
                                 (static member (>>=) :
                                     ^b * (unit ->  ^b) ->  ^b) and
                               (Control.Return or  ^b) :
                                 (static member Return:
                                     ^b * Control.Return -> (unit ->  ^b)) and
                               (Control.Catch or  ^c or  ^d) :
                                 (static member Catch:  ^c * ('E1 ->  ^d) ->  ^d) and
                               (Control.Map or  ^e or  ^d) :
                                 (static member Map:
                                    ( ^e * ('T[] -> seq<'T>)) * Control.Map
                                      ->  ^d) and
                               (Control.Bind or  ^d or  ^MonadError<'E2>) :
                                 (static member (>>=) :
                                     ^d * (seq<'T> ->  ^MonadError<'E2>)
                                      ->  ^MonadError<'E2>) and
                               (Control.Bind or  ^f or  ^e) :
                                 (static member (>>=) :
                                     ^f * (unit ->  ^e) ->  ^e) and
                               (Control.Bind or  ^MonadError<'E2> or  ^e) :
                                 (static member (>>=) :
                                     ^MonadError<'E2> * (bool ->  ^e) ->  ^e) and
                               (Control.Using or  ^e) :
                                 (static member Using:
                                    IEnumeratorM< ^MonadError<'E2>,'T> *
                                    (IEnumeratorM< ^MonadError<'E2>,'T> ->  ^e) *
                                    Control.Using ->  ^e) and
                               (Control.Return or  ^e) :
                                 (static member Return:
                                     ^e * Control.Return -> ('T[] ->  ^e)) and
                               (Control.Bind or  ^MonadError<'E2> or  ^f) :
                                 (static member (>>=) :
                                     ^MonadError<'E2> * (bool ->  ^f) ->  ^f) and
                               (Control.Bind or  ^f) :
                                 (static member (>>=) :
                                     ^f * (unit ->  ^f) ->  ^f) and
                               (Control.Return or  ^f) :
                                 (static member Return:
                                     ^f * Control.Return -> (unit ->  ^f)) and
                               (Control.Return or  ^MonadError<'E2>) :
                                 (static member Return:
                                     ^MonadError<'E2> * Control.Return
                                      -> (bool ->  ^MonadError<'E2>))
        
        static member
          inline Delay: body: (unit -> SeqT<'Monad<bool>,'T>)
                          -> SeqT<'Monad<bool>,'T>
        
        static member
          inline Lift: m:  ^Monad<'T> -> SeqT< ^Monad<bool>,'T>
                         when (Control.Bind or  ^Monad<'T> or  ^Monad<bool>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('T ->  ^Monad<bool>)
                                     ->  ^Monad<bool>) and
                              (Control.Map or  ^Monad<'T> or  ^Monad<bool>) :
                                (static member Map:
                                   ( ^Monad<'T> * ('T -> bool)) * Control.Map
                                     ->  ^Monad<bool>) and
                              (Control.Return or  ^Monad<bool>) :
                                (static member Return:
                                    ^Monad<bool> * Control.Return
                                     -> (bool ->  ^Monad<bool>))
        
        static member
          inline Lift2: f: ('T1 -> 'T2 -> 'U) * x1: SeqT< ^Monad<bool>,'T1> *
                        x2: SeqT< ^Monad<bool>,'T2> -> SeqT< ^Monad<bool>,'U>
                          when (Control.Bind or  ^Monad<bool>) :
                                 (static member (>>=) :
                                     ^Monad<bool> * (bool ->  ^Monad<bool>)
                                      ->  ^Monad<bool>) and
                               (Control.Return or  ^Monad<bool>) :
                                 (static member Return:
                                     ^Monad<bool> * Control.Return
                                      -> (bool ->  ^Monad<bool>))
        
        static member
          inline Lift3: f: ('T1 -> 'T2 -> 'T3 -> 'U) *
                        x1: SeqT< ^Monad<bool>,'T1> *
                        x2: SeqT< ^Monad<bool>,'T2> *
                        x3: SeqT< ^Monad<bool>,'T3> -> SeqT< ^Monad<bool>,'U>
                          when (Control.Delay or  ^Monad<bool>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<bool>) *
                                    Control.Delay ->  ^Monad<bool>) and
                               (Control.Bind or  ^Monad<bool>) :
                                 (static member (>>=) :
                                     ^Monad<bool> * (bool ->  ^Monad<bool>)
                                      ->  ^Monad<bool>) and
                               (Control.Return or  ^Monad<bool>) :
                                 (static member Return:
                                     ^Monad<bool> * Control.Return
                                      -> (bool ->  ^Monad<bool>))
        
        static member
          inline LiftAsync: x: Async<'T> -> SeqT< ^MonadAsync,'T>
                              when (Control.Return or  ^MonadAsync) :
                                     (static member Return:
                                         ^MonadAsync * Control.Return
                                          -> (bool ->  ^MonadAsync)) and
                                   (Control.Map or  ^MonadAsync<'T> or
                                     ^MonadAsync) :
                                     (static member Map:
                                        ( ^MonadAsync<'T> * ('T -> bool)) *
                                        Control.Map ->  ^MonadAsync) and
                                   (Control.Bind or  ^MonadAsync<'T> or
                                     ^MonadAsync) :
                                     (static member (>>=) :
                                         ^MonadAsync<'T> * ('T ->  ^MonadAsync)
                                          ->  ^MonadAsync) and
                                   (Control.LiftAsync or  ^MonadAsync<'T>) :
                                     (static member LiftAsync:
                                         ^MonadAsync<'T>
                                          -> (Async<'T> ->  ^MonadAsync<'T>))
        
        static member
          inline Local: m: SeqT< ^MonadReader<'R2>,'T> * f: ('R1 -> 'R2)
                          -> SeqT< ^MonadReader<'R1>,'T>
                          when (Control.Bind or  ^MonadReader<'R2> or  ^a) :
                                 (static member (>>=) :
                                     ^MonadReader<'R2> * (bool ->  ^a) ->  ^a) and
                               (Control.Bind or  ^MonadReader<'R2> or  ^b) :
                                 (static member (>>=) :
                                     ^MonadReader<'R2> * (bool ->  ^b) ->  ^b) and
                               (Control.Bind or  ^b or  ^a) :
                                 (static member (>>=) :
                                     ^b * (unit ->  ^a) ->  ^a) and
                               (Control.Using or  ^a) :
                                 (static member Using:
                                    IEnumeratorM< ^MonadReader<'R2>,'T> *
                                    (IEnumeratorM< ^MonadReader<'R2>,'T> ->  ^a) *
                                    Control.Using ->  ^a) and
                               (Control.Return or  ^a) :
                                 (static member Return:
                                     ^a * Control.Return -> ('T[] ->  ^a)) and
                               (Control.Map or  ^a or  ^c) :
                                 (static member Map:
                                    ( ^a * ('T[] -> seq<'T>)) * Control.Map
                                      ->  ^c) and
                               (Control.Bind or  ^b) :
                                 (static member (>>=) :
                                     ^b * (unit ->  ^b) ->  ^b) and
                               (Control.Return or  ^b) :
                                 (static member Return:
                                     ^b * Control.Return -> (unit ->  ^b)) and
                               (Control.Bind or  ^d or  ^MonadReader<'R1>) :
                                 (static member (>>=) :
                                     ^d * (seq<'T> ->  ^MonadReader<'R1>)
                                      ->  ^MonadReader<'R1>) and
                               (Control.Return or  ^MonadReader<'R1>) :
                                 (static member Return:
                                     ^MonadReader<'R1> * Control.Return
                                      -> (bool ->  ^MonadReader<'R1>)) and
                                ^d:
                                 (static member Local:  ^c * ('R1 -> 'R2) ->  ^d)
        
        static member
          inline Map: x: SeqT< ^Monad<bool>,'T> * f: ('T -> 'U)
                        -> SeqT< ^Monad<bool>,'U>
                        when (Control.Delay or  ^Monad<bool>) :
                               (static member Delay:
                                  Control.Delay * (unit ->  ^Monad<bool>) *
                                  Control.Delay ->  ^Monad<bool>) and
                             (Control.Bind or  ^Monad<bool>) :
                               (static member (>>=) :
                                   ^Monad<bool> * (bool ->  ^Monad<bool>)
                                    ->  ^Monad<bool>) and
                             (Control.Return or  ^Monad<bool>) :
                               (static member Return:
                                   ^Monad<bool> * Control.Return
                                    -> (bool ->  ^Monad<bool>))
        
        static member
          inline Put: x: 'T -> SeqT< ^MonadState<unit>,'S>
                        when (Control.Return or  ^MonadState<unit>) :
                               (static member Return:
                                   ^MonadState<unit> * Control.Return
                                    -> (bool ->  ^MonadState<unit>)) and
                             (Control.Map or  ^a or  ^MonadState<unit>) :
                               (static member Map:
                                  ( ^a * ('S -> bool)) * Control.Map
                                    ->  ^MonadState<unit>) and
                             (Control.Bind or  ^a or  ^MonadState<unit>) :
                               (static member (>>=) :
                                   ^a * ('S ->  ^MonadState<unit>)
                                    ->  ^MonadState<unit>) and
                              ^a: (static member Put: 'T ->  ^a)
        
        static member
          inline Return: x: 'T -> SeqT< ^Monad<bool>,'T>
                           when (Control.Delay or  ^Monad<bool>) :
                                  (static member Delay:
                                     Control.Delay * (unit ->  ^Monad<bool>) *
                                     Control.Delay ->  ^Monad<bool>) and
                                (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>))
        
        static member
          inline Take: source: SeqT< ^Monad<bool>,'T> * count: int *
                       Control.Take -> SeqT< ^Monad<bool>,'T>
                         when (Control.Delay or  ^Monad<bool>) :
                                (static member Delay:
                                   Control.Delay * (unit ->  ^Monad<bool>) *
                                   Control.Delay ->  ^Monad<bool>) and
                              (Control.Return or  ^Monad<bool>) :
                                (static member Return:
                                    ^Monad<bool> * Control.Return
                                     -> (bool ->  ^Monad<bool>)) and
                              (Control.Bind or  ^Monad<bool> or  ^a) :
                                (static member (>>=) :
                                    ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                              (Control.Bind or  ^a or  ^Monad<bool>) :
                                (static member (>>=) :
                                    ^a * (unit ->  ^Monad<bool>)
                                     ->  ^Monad<bool>) and
                              (Control.Map or  ^Monad<bool>) :
                                (static member Map:
                                   ( ^Monad<bool> * (bool -> bool)) *
                                   Control.Map ->  ^Monad<bool>) and
                              (Control.Bind or  ^Monad<bool>) :
                                (static member (>>=) :
                                    ^Monad<bool> * (bool ->  ^Monad<bool>)
                                     ->  ^Monad<bool>) and
                              (Control.Bind or  ^Monad<bool> or  ^c) :
                                (static member (>>=) :
                                    ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                              (Control.Bind or  ^c or  ^Monad<bool>) :
                                (static member (>>=) :
                                    ^c * (unit ->  ^Monad<bool>)
                                     ->  ^Monad<bool>) and
                              (Control.Return or  ^a) :
                                (static member Return:
                                    ^a * Control.Return -> (unit ->  ^a)) and
                              (Control.Bind or  ^a) :
                                (static member (>>=) :
                                    ^a * (unit ->  ^a) ->  ^a) and
                              (Control.Using or  ^a) :
                                (static member Using:
                                   System.IDisposable *
                                   (System.IDisposable ->  ^a) * Control.Using
                                     ->  ^a) and
                              (Control.TryWith or  ^a) :
                                (static member TryWith:
                                   (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                                   Control.TryBlock.True ->  ^a) and
                              (Control.Delay or  ^a) :
                                (static member Delay:
                                   Control.Delay * (unit ->  ^a) * Control.Delay
                                     ->  ^a) and 'b :> exn and
                              (Control.Return or  ^c) :
                                (static member Return:
                                    ^c * Control.Return -> (unit ->  ^c)) and
                              (Control.Bind or  ^c) :
                                (static member (>>=) :
                                    ^c * (unit ->  ^c) ->  ^c) and
                              (Control.Using or  ^c) :
                                (static member Using:
                                   System.IDisposable *
                                   (System.IDisposable ->  ^c) * Control.Using
                                     ->  ^c) and
                              (Control.TryWith or  ^c) :
                                (static member TryWith:
                                   (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                                   Control.TryBlock.True ->  ^c) and
                              (Control.Delay or  ^c) :
                                (static member Delay:
                                   Control.Delay * (unit ->  ^c) * Control.Delay
                                     ->  ^c) and 'd :> exn
        
        static member
          inline Throw: x: 'E -> SeqT< ^MonadError<'E>,'T>
                          when (Control.Return or  ^MonadError<'E>) :
                                 (static member Return:
                                     ^MonadError<'E> * Control.Return
                                      -> (bool ->  ^MonadError<'E>)) and
                               (Control.Map or  ^a or  ^MonadError<'E>) :
                                 (static member Map:
                                    ( ^a * ('T -> bool)) * Control.Map
                                      ->  ^MonadError<'E>) and
                               (Control.Bind or  ^a or  ^MonadError<'E>) :
                                 (static member (>>=) :
                                     ^a * ('T ->  ^MonadError<'E>)
                                      ->  ^MonadError<'E>) and
                               (Control.Throw or  ^a) :
                                 (static member Throw:  ^a * 'E ->  ^a)
        
        static member
          inline TryFinally: computation: SeqT< ^Monad<bool>,'T> *
                             f: (unit -> unit) -> SeqT< ^Monad<bool>,'T>
                               when (Control.Delay or  ^Monad<bool>) :
                                      (static member Delay:
                                         Control.Delay * (unit ->  ^Monad<bool>) *
                                         Control.Delay ->  ^Monad<bool>) and
                                    (Control.Bind or  ^Monad<bool>) :
                                      (static member (>>=) :
                                          ^Monad<bool> * (bool ->  ^Monad<bool>)
                                           ->  ^Monad<bool>) and
                                    (Control.Return or  ^Monad<bool>) :
                                      (static member Return:
                                          ^Monad<bool> * Control.Return
                                           -> (bool ->  ^Monad<bool>))
        
        static member
          inline TryWith: source: SeqT< ^Monad<bool>,'T> *
                          f: (exn -> SeqT< ^Monad<bool>,'T>)
                            -> SeqT< ^Monad<bool>,'T>
                            when (Control.Bind or  ^Monad<bool>) :
                                   (static member (>>=) :
                                       ^Monad<bool> * (bool ->  ^Monad<bool>)
                                        ->  ^Monad<bool>) and
                                 (Control.Return or  ^Monad<bool>) :
                                   (static member Return:
                                       ^Monad<bool> * Control.Return
                                        -> (bool ->  ^Monad<bool>)) and
                                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                                   (static member (>>=) :
                                       ^Monad<bool> * (bool ->  ^Monad<unit>)
                                        ->  ^Monad<unit>) and
                                 (Control.Bind or  ^Monad<unit> or  ^Monad<bool>) :
                                   (static member (>>=) :
                                       ^Monad<unit> * (unit ->  ^Monad<bool>)
                                        ->  ^Monad<bool>) and
                                 (Control.Delay or  ^Monad<bool>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^Monad<bool>) *
                                      Control.Delay ->  ^Monad<bool>) and
                                 (Control.Return or  ^Monad<unit>) :
                                   (static member Return:
                                       ^Monad<unit> * Control.Return
                                        -> (unit ->  ^Monad<unit>)) and
                                 (Control.Bind or  ^Monad<unit>) :
                                   (static member (>>=) :
                                       ^Monad<unit> * (unit ->  ^Monad<unit>)
                                        ->  ^Monad<unit>) and
                                 (Control.Using or  ^Monad<unit>) :
                                   (static member Using:
                                      System.IDisposable *
                                      (System.IDisposable ->  ^Monad<unit>) *
                                      Control.Using ->  ^Monad<unit>) and
                                 (Control.TryWith or  ^Monad<unit>) :
                                   (static member TryWith:
                                      (unit ->  ^Monad<unit>) *
                                      ('a ->  ^Monad<unit>) * Control.TryWith *
                                      Control.TryBlock.True ->  ^Monad<unit>) and
                                 (Control.Delay or  ^Monad<unit>) :
                                   (static member Delay:
                                      Control.Delay * (unit ->  ^Monad<unit>) *
                                      Control.Delay ->  ^Monad<unit>) and
                                 'a :> exn
        
        static member
          inline Using: resource: 'a * f: ('a -> SeqT< ^Monad<bool>,'T>)
                          -> SeqT< ^Monad<bool>,'T>
                          when 'a :> System.IDisposable and
                               (Control.Delay or  ^Monad<bool>) :
                                 (static member Delay:
                                    Control.Delay * (unit ->  ^Monad<bool>) *
                                    Control.Delay ->  ^Monad<bool>) and
                               (Control.Bind or  ^Monad<bool>) :
                                 (static member (>>=) :
                                     ^Monad<bool> * (bool ->  ^Monad<bool>)
                                      ->  ^Monad<bool>) and
                               (Control.Return or  ^Monad<bool>) :
                                 (static member Return:
                                     ^Monad<bool> * Control.Return
                                      -> (bool ->  ^Monad<bool>))
        
        static member
          inline Zip: source1: SeqT< ^Monad<bool>,'T1> *
                      source2: SeqT< ^Monad<bool>,'T2>
                        -> SeqT< ^Monad<bool>,('T1 * 'T2)>
                        when (Control.Delay or  ^Monad<bool>) :
                               (static member Delay:
                                  Control.Delay * (unit ->  ^Monad<bool>) *
                                  Control.Delay ->  ^Monad<bool>) and
                             (Control.Return or  ^Monad<bool>) :
                               (static member Return:
                                   ^Monad<bool> * Control.Return
                                    -> (bool ->  ^Monad<bool>)) and
                             (Control.Bind or  ^Monad<bool> or  ^a) :
                               (static member (>>=) :
                                   ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                             (Control.Bind or  ^a or  ^Monad<bool>) :
                               (static member (>>=) :
                                   ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                             (Control.Map or  ^Monad<bool>) :
                               (static member Map:
                                  ( ^Monad<bool> * (bool -> bool)) * Control.Map
                                    ->  ^Monad<bool>) and
                             (Control.Bind or  ^Monad<bool>) :
                               (static member (>>=) :
                                   ^Monad<bool> * (bool ->  ^Monad<bool>)
                                    ->  ^Monad<bool>) and
                             (Control.Bind or  ^Monad<bool> or  ^c) :
                               (static member (>>=) :
                                   ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                             (Control.Bind or  ^c or  ^Monad<bool>) :
                               (static member (>>=) :
                                   ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                             (Control.Return or  ^a) :
                               (static member Return:
                                   ^a * Control.Return -> (unit ->  ^a)) and
                             (Control.Bind or  ^a) :
                               (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                             (Control.Using or  ^a) :
                               (static member Using:
                                  System.IDisposable *
                                  (System.IDisposable ->  ^a) * Control.Using
                                    ->  ^a) and
                             (Control.TryWith or  ^a) :
                               (static member TryWith:
                                  (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                                  Control.TryBlock.True ->  ^a) and
                             (Control.Delay or  ^a) :
                               (static member Delay:
                                  Control.Delay * (unit ->  ^a) * Control.Delay
                                    ->  ^a) and 'b :> exn and
                             (Control.Return or  ^c) :
                               (static member Return:
                                   ^c * Control.Return -> (unit ->  ^c)) and
                             (Control.Bind or  ^c) :
                               (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                             (Control.Using or  ^c) :
                               (static member Using:
                                  System.IDisposable *
                                  (System.IDisposable ->  ^c) * Control.Using
                                    ->  ^c) and
                             (Control.TryWith or  ^c) :
                               (static member TryWith:
                                  (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                                  Control.TryBlock.True ->  ^c) and
                             (Control.Delay or  ^c) :
                               (static member Delay:
                                  Control.Delay * (unit ->  ^c) * Control.Delay
                                    ->  ^c) and 'd :> exn
        
        static member
          inline get_Ask: unit -> SeqT< ^MonadReader<'R>,'R>
                            when (Control.Return or  ^MonadReader<'R>) :
                                   (static member Return:
                                       ^MonadReader<'R> * Control.Return
                                        -> (bool ->  ^MonadReader<'R>)) and
                                 (Control.Map or  ^a or  ^MonadReader<'R>) :
                                   (static member Map:
                                      ( ^a * ('R -> bool)) * Control.Map
                                        ->  ^MonadReader<'R>) and
                                 (Control.Bind or  ^a or  ^MonadReader<'R>) :
                                   (static member (>>=) :
                                       ^a * ('R ->  ^MonadReader<'R>)
                                        ->  ^MonadReader<'R>) and
                                  ^a: (static member get_Ask: ->  ^a)
        
        static member
          inline get_Empty: unit -> SeqT< ^Monad<bool>,'T>
                              when (Control.Return or  ^Monad<bool>) :
                                     (static member Return:
                                         ^Monad<bool> * Control.Return
                                          -> (bool ->  ^Monad<bool>))
        
        static member
          inline get_Get: unit -> SeqT< ^MonadState<'S>,'S>
                            when (Control.Return or  ^MonadState<'S>) :
                                   (static member Return:
                                       ^MonadState<'S> * Control.Return
                                        -> (bool ->  ^MonadState<'S>)) and
                                 (Control.Map or  ^a or  ^MonadState<'S>) :
                                   (static member Map:
                                      ( ^a * ('S -> bool)) * Control.Map
                                        ->  ^MonadState<'S>) and
                                 (Control.Bind or  ^a or  ^MonadState<'S>) :
                                   (static member (>>=) :
                                       ^a * ('S ->  ^MonadState<'S>)
                                        ->  ^MonadState<'S>) and
                                  ^a: (static member get_Get: ->  ^a)
    
    module SeqT =
        
        val ofIEnumerableM:
          x: IEnumerableM<'Monad<bool>,'T> -> SeqT<'Monad<bool>,'T>
        
        [<RequireQualifiedAccess>]
        type MapState<'Monad<seq<'T>>,'T> =
            | NotStarted of 'Monad<seq<'T>>
            | HaveEnumerator of System.Collections.Generic.IEnumerator<'T>
            | Finished
        
        val inline wrap:
          inp:  ^Monad<seq<'T>> -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<seq<'T>> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<seq<'T>> * (seq<'T> ->  ^Monad<bool>)
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline hoist:
          source: seq<'T> -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<seq<'T>> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<seq<'T>> * (seq<'T> ->  ^Monad<bool>)
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>)) and
                 (Control.Return or  ^Monad<seq<'T>>) :
                   (static member Return:
                       ^Monad<seq<'T>> * Control.Return
                        -> (seq<'T> ->  ^Monad<seq<'T>>))
        
        val inline toArrayM:
          source: SeqT< ^Monad<bool>,'T> ->  ^Monad<'T []>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<'T []>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<'T []>) *
                      Control.Using ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<unit> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Return or  ^Monad<'T []>) :
                   (static member Return:
                       ^Monad<'T []> * Control.Return
                        -> ('T[] ->  ^Monad<'T []>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>))
        
        val inline toListM:
          source: SeqT< ^Monad<bool>,'T> ->  ^Monad<'T list>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<unit> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Using or  ^Monad<'T []>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<'T []>) *
                      Control.Using ->  ^Monad<'T []>) and
                 (Control.Return or  ^Monad<'T []>) :
                   (static member Return:
                       ^Monad<'T []> * Control.Return
                        -> ('T[] ->  ^Monad<'T []>)) and
                 (Control.Map or  ^Monad<'T []> or  ^Monad<'T list>) :
                   (static member Map:
                      ( ^Monad<'T []> * ('T[] -> 'T list)) * Control.Map
                        ->  ^Monad<'T list>)
        
        val inline toSeqM:
          source: SeqT< ^Monad<bool>,'T> ->  ^Monad<'T seq>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<unit> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Using or  ^Monad<'T []>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<'T []>) *
                      Control.Using ->  ^Monad<'T []>) and
                 (Control.Return or  ^Monad<'T []>) :
                   (static member Return:
                       ^Monad<'T []> * Control.Return
                        -> ('T[] ->  ^Monad<'T []>)) and
                 (Control.Map or  ^Monad<'T []> or  ^Monad<'T seq>) :
                   (static member Map:
                      ( ^Monad<'T []> * ('T[] -> seq<'T>)) * Control.Map
                        ->  ^Monad<'T seq>)
        
        val inline run:
          source: SeqT< ^Monad<bool>,'T> ->  ^Monad<'T seq>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Bind or  ^Monad<unit> or  ^Monad<'T []>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<'T []>) ->  ^Monad<'T []>) and
                 (Control.Using or  ^Monad<'T []>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<'T []>) *
                      Control.Using ->  ^Monad<'T []>) and
                 (Control.Return or  ^Monad<'T []>) :
                   (static member Return:
                       ^Monad<'T []> * Control.Return
                        -> ('T[] ->  ^Monad<'T []>)) and
                 (Control.Map or  ^Monad<'T []> or  ^Monad<'T seq>) :
                   (static member Map:
                      ( ^Monad<'T []> * ('T[] -> seq<'T>)) * Control.Map
                        ->  ^Monad<'T seq>) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>))
        
        [<GeneralizableValue>]
        val inline empty<'T, ^Monad<bool>
                           when (Control.Return or  ^Monad<bool>) :
                                  (static member Return:
                                      ^Monad<bool> * Control.Return
                                       -> (bool ->  ^Monad<bool>))> :
          SeqT< ^Monad<bool>,'T>
            when (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline singleton:
          v: 'T -> SeqT< ^Monad<bool>,'T>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline make:
          f: (unit ->  ^Monad<SeqT<'Monad<bool>, 'T>>) -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<SeqT<'Monad<bool>, 'T>> or
                   ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<SeqT<'Monad<bool>, 'T>> *
                      (SeqT< ^Monad<bool>,'T> ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val delay: f: (unit -> SeqT<'Monad<bool>,'T>) -> SeqT<'Monad<bool>,'T>
        
        val inline bindM:
          f: ('T -> SeqT< ^Monad<bool>,'U>) -> inp:  ^Monad<'T>
            -> SeqT< ^Monad<bool>,'U>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<SeqT<'Monad<bool>, 'U>> or
                   ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<SeqT<'Monad<bool>, 'U>> *
                      (SeqT< ^Monad<bool>,'U> ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>)) and
                 (Control.Delay or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<SeqT<'Monad<bool>, 'U>>) *
                      Control.Delay ->  ^Monad<SeqT<'Monad<bool>, 'U>>) and
                 (Control.Bind or  ^Monad<'T> or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<SeqT<'Monad<bool>, 'U>>)
                        ->  ^Monad<SeqT<'Monad<bool>, 'U>>) and
                 (Control.Return or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member Return:
                       ^Monad<SeqT<'Monad<bool>, 'U>> * Control.Return
                        -> (SeqT< ^Monad<bool>,'U>
                              ->  ^Monad<SeqT<'Monad<bool>, 'U>>))
        
        val inline lift:
          source:  ^Monad<'T> -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<'T> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<'T> * ('T ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Map or  ^Monad<'T> or  ^Monad<bool>) :
                   (static member Map:
                      ( ^Monad<'T> * ('T -> bool)) * Control.Map
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        [<RequireQualifiedAccess>]
        type CollectState<'T,'U,'Monad<bool>> =
            | NotStarted of SeqT<'Monad<bool>,'T>
            | HaveInputEnumerator of IEnumeratorM<'Monad<bool>,'T>
            | HaveInnerEnumerator of
              IEnumeratorM<'Monad<bool>,'T> * IEnumeratorM<'Monad<bool>,'U>
            | Finished
        
        val inline collect:
          f: ('T -> SeqT< ^Monad<bool>,'U>) -> inp: SeqT< ^Monad<bool>,'T>
            -> SeqT< ^Monad<bool>,'U>
            when (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline apply:
          f: SeqT< ^Monad<bool>,('T -> 'U)> -> x1: SeqT< ^Monad<bool>,'T>
            -> SeqT< ^Monad<bool>,'U>
            when (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline lift2:
          f: ('T1 -> 'T2 -> 'U) -> x1: SeqT< ^Monad<bool>,'T1>
          -> x2: SeqT< ^Monad<bool>,'T2> -> SeqT< ^Monad<bool>,'U>
            when (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        [<RequireQualifiedAccess>]
        type AppendState<'Monad<bool>,'T> =
            | NotStarted1 of SeqT<'Monad<bool>,'T> * SeqT<'Monad<bool>,'T>
            | HaveEnumerator1 of
              IEnumeratorM<'Monad<bool>,'T> * SeqT<'Monad<bool>,'T>
            | NotStarted2 of SeqT<'Monad<bool>,'T>
            | HaveEnumerator2 of IEnumeratorM<'Monad<bool>,'T>
            | Finished
        
        val inline append:
          inp1: SeqT< ^Monad<bool>,'T> -> inp2: SeqT< ^Monad<bool>,'T>
            -> SeqT< ^Monad<bool>,'T>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>)) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>)
        
        val inline mapM:
          f: ('T ->  ^Monad<'U>) -> source: SeqT< ^Monad<bool>,'T>
            -> SeqT< ^Monad<bool>,'U>
            when (Control.Bind or  ^Monad<'U> or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member (>>=) :
                       ^Monad<'U> * ('U ->  ^Monad<SeqT<'Monad<bool>, 'U>>)
                        ->  ^Monad<SeqT<'Monad<bool>, 'U>>) and
                 (Control.Bind or  ^Monad<SeqT<'Monad<bool>, 'U>> or
                   ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<SeqT<'Monad<bool>, 'U>> *
                      (SeqT< ^Monad<bool>,'U> ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Delay or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<SeqT<'Monad<bool>, 'U>>) *
                      Control.Delay ->  ^Monad<SeqT<'Monad<bool>, 'U>>) and
                 (Control.Return or  ^Monad<SeqT<'Monad<bool>, 'U>>) :
                   (static member Return:
                       ^Monad<SeqT<'Monad<bool>, 'U>> * Control.Return
                        -> (SeqT< ^Monad<bool>,'U>
                              ->  ^Monad<SeqT<'Monad<bool>, 'U>>)) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>)) and
                 (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>)
        
        val inline map:
          f: ('T -> 'U) -> inp: SeqT< ^Monad<bool>,'T> -> SeqT< ^Monad<bool>,'U>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline lift3:
          f: ('T1 -> 'T2 -> 'T3 -> 'U) -> x1: SeqT< ^Monad<bool>,'T1>
          -> x2: SeqT< ^Monad<bool>,'T2> -> x3: SeqT< ^Monad<bool>,'T3>
            -> SeqT< ^Monad<bool>,'U>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline filter:
          f: ('T -> bool) -> inp: SeqT< ^Monad<bool>,'T>
            -> SeqT< ^Monad<bool>,'T>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline iteriM:
          f: (int -> 'T ->  ^Monad<unit>) -> source: SeqT< ^Monad<bool>,'T>
            ->  ^Monad<unit>
            when (Control.Delay or  ^Monad<unit>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<unit>) * Control.Delay
                        ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<unit>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<unit>) *
                      Control.Using ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>)
        
        val inline iterM:
          f: ('T ->  ^Monad<unit>) -> inp: SeqT< ^Monad<bool>,'T>
            ->  ^Monad<unit>
            when (Control.Delay or  ^Monad<unit>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<unit>) * Control.Delay
                        ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<unit>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<unit>) *
                      Control.Using ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>)
        
        val inline iteri:
          f: (int -> 'T -> unit) -> inp: SeqT< ^Monad<bool>,'T> ->  ^Monad<unit>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Delay or  ^Monad<unit>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<unit>) * Control.Delay
                        ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<unit>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<unit>) *
                      Control.Using ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>)
        
        val inline iter:
          f: ('T -> unit) -> source: SeqT< ^Monad<bool>,'T> ->  ^Monad<unit>
            when (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Delay or  ^Monad<unit>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<unit>) * Control.Delay
                        ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<unit>) :
                   (static member Using:
                      IEnumeratorM< ^Monad<bool>,'T> *
                      (IEnumeratorM< ^Monad<bool>,'T> ->  ^Monad<unit>) *
                      Control.Using ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>)
        
        [<RequireQualifiedAccess>]
        type TryWithState<'Monad<bool>,'T> =
            | NotStarted of SeqT<'Monad<bool>,'T>
            | HaveBodyEnumerator of IEnumeratorM<'Monad<bool>,'T>
            | HaveHandlerEnumerator of IEnumeratorM<'Monad<bool>,'T>
            | Finished
        
        /// Implements the 'TryWith' functionality for computation builder
        val inline tryWith:
          inp: SeqT< ^Monad<bool>,'T>
          -> handler: (exn -> SeqT< ^Monad<bool>,'T>) -> SeqT< ^Monad<bool>,'T>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<unit> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool> or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>)) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<unit>) :
                   (static member Return:
                       ^Monad<unit> * Control.Return -> (unit ->  ^Monad<unit>)) and
                 (Control.Bind or  ^Monad<unit>) :
                   (static member (>>=) :
                       ^Monad<unit> * (unit ->  ^Monad<unit>) ->  ^Monad<unit>) and
                 (Control.Using or  ^Monad<unit>) :
                   (static member Using:
                      System.IDisposable * (System.IDisposable ->  ^Monad<unit>) *
                      Control.Using ->  ^Monad<unit>) and
                 (Control.TryWith or  ^Monad<unit>) :
                   (static member TryWith:
                      (unit ->  ^Monad<unit>) * ('a ->  ^Monad<unit>) *
                      Control.TryWith * Control.TryBlock.True ->  ^Monad<unit>) and
                 (Control.Delay or  ^Monad<unit>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<unit>) * Control.Delay
                        ->  ^Monad<unit>) and 'a :> exn
        
        [<RequireQualifiedAccess>]
        type TryFinallyState<'Monad<bool>,'T> =
            | NotStarted of SeqT<'Monad<bool>,'T>
            | HaveBodyEnumerator of IEnumeratorM<'Monad<bool>,'T>
            | Finished
        
        val inline tryFinally:
          inp: SeqT< ^Monad<bool>,'T> -> compensation: (unit -> unit)
            -> SeqT< ^Monad<bool>,'T>
            when (Control.Delay or  ^Monad<bool>) :
                   (static member Delay:
                      Control.Delay * (unit ->  ^Monad<bool>) * Control.Delay
                        ->  ^Monad<bool>) and
                 (Control.Bind or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<bool> * (bool ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
        
        val inline unfold:
          f: ('State ->  ^Monad<('T * 'State) option>) -> s: 'State
            -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<('T * 'State) option> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<('T * 'State) option> *
                      (('T * 'State) option ->  ^Monad<bool>) ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
    
    [<AutoOpen; Class>]
    type SeqTOperations =
        
        static member
          inline SeqT: source:  ^Monad<seq<'T>> -> SeqT< ^Monad<bool>,'T>
                         when (Control.Bind or  ^Monad<seq<'T>> or  ^Monad<bool>) :
                                (static member (>>=) :
                                    ^Monad<seq<'T>> * (seq<'T> ->  ^Monad<bool>)
                                     ->  ^Monad<bool>) and
                              (Control.Return or  ^Monad<bool>) :
                                (static member Return:
                                    ^Monad<bool> * Control.Return
                                     -> (bool ->  ^Monad<bool>))
    
    module SeqTOperations =
        
        val inline seqT:
          source:  ^Monad<seq<'T>> -> SeqT< ^Monad<bool>,'T>
            when (Control.Bind or  ^Monad<seq<'T>> or  ^Monad<bool>) :
                   (static member (>>=) :
                       ^Monad<seq<'T>> * (seq<'T> ->  ^Monad<bool>)
                        ->  ^Monad<bool>) and
                 (Control.Return or  ^Monad<bool>) :
                   (static member Return:
                       ^Monad<bool> * Control.Return -> (bool ->  ^Monad<bool>))
    
    module Extension =
        
        module SeqT =
            
            val inline take:
              count: int -> source: SeqT< ^Monad<bool>,'T>
                -> SeqT< ^Monad<bool>,'T>
                when (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn
            
            val inline map2M:
              f: ('T1 -> 'T2 ->  ^Monad<'U>) -> source1: SeqT< ^Monad<bool>,'T1>
              -> source2: SeqT< ^Monad<bool>,'T2> -> SeqT< ^Monad<bool>,'U>
                when (Control.Bind or  ^Monad<'U> or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<'U> * ('U ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<'U> or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<'U> * ('U -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn
            
            val inline map2:
              f: ('T1 -> 'T2 -> 'U) -> source1: SeqT< ^Monad<bool>,'T1>
              -> source2: SeqT< ^Monad<bool>,'T2> -> SeqT< ^Monad<bool>,'U>
                when (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn
            
            val inline zip:
              source1: SeqT< ^Monad<bool>,'T1>
              -> source2: SeqT< ^Monad<bool>,'T2>
                -> SeqT< ^Monad<bool>,('T1 * 'T2)>
                when (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn
            
            val inline map3:
              f: ('T1 -> 'T2 -> 'T3 -> 'U) -> x1: SeqT< ^Monad<bool>,'T1>
              -> x2: SeqT< ^Monad<bool>,'T2> -> x3: SeqT< ^Monad<bool>,'T3>
                -> SeqT< ^Monad<bool>,'U>
                when (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^e) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^e) ->  ^e) and
                     (Control.Bind or  ^e or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^e * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^g) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^g) ->  ^g) and
                     (Control.Bind or  ^g or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^g * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn and
                     (Control.Return or  ^e) :
                       (static member Return:
                           ^e * Control.Return -> (unit ->  ^e)) and
                     (Control.Bind or  ^e) :
                       (static member (>>=) :  ^e * (unit ->  ^e) ->  ^e) and
                     (Control.Using or  ^e) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^e) *
                          Control.Using ->  ^e) and
                     (Control.TryWith or  ^e) :
                       (static member TryWith:
                          (unit ->  ^e) * ('f ->  ^e) * Control.TryWith *
                          Control.TryBlock.True ->  ^e) and
                     (Control.Delay or  ^e) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^e) * Control.Delay ->  ^e) and
                     'f :> exn and
                     (Control.Return or  ^g) :
                       (static member Return:
                           ^g * Control.Return -> (unit ->  ^g)) and
                     (Control.Bind or  ^g) :
                       (static member (>>=) :  ^g * (unit ->  ^g) ->  ^g) and
                     (Control.Using or  ^g) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^g) *
                          Control.Using ->  ^g) and
                     (Control.TryWith or  ^g) :
                       (static member TryWith:
                          (unit ->  ^g) * ('h ->  ^g) * Control.TryWith *
                          Control.TryBlock.True ->  ^g) and
                     (Control.Delay or  ^g) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^g) * Control.Delay ->  ^g) and
                     'h :> exn
            
            val inline zip3:
              source1: SeqT< ^Monad<bool>,'T1>
              -> source2: SeqT< ^Monad<bool>,'T2>
              -> source3: SeqT< ^Monad<bool>,'T3>
                -> SeqT< ^Monad<bool>,('T1 * 'T2 * 'T3)>
                when (Control.Delay or  ^Monad<bool>) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^Monad<bool>) *
                          Control.Delay ->  ^Monad<bool>) and
                     (Control.Return or  ^Monad<bool>) :
                       (static member Return:
                           ^Monad<bool> * Control.Return
                            -> (bool ->  ^Monad<bool>)) and
                     (Control.Bind or  ^Monad<bool> or  ^a) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^a) ->  ^a) and
                     (Control.Bind or  ^a or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^a * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Map or  ^Monad<bool>) :
                       (static member Map:
                          ( ^Monad<bool> * (bool -> bool)) * Control.Map
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^Monad<bool>)
                            ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^c) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^c) ->  ^c) and
                     (Control.Bind or  ^c or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^c * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^e) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^e) ->  ^e) and
                     (Control.Bind or  ^e or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^e * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Bind or  ^Monad<bool> or  ^g) :
                       (static member (>>=) :
                           ^Monad<bool> * (bool ->  ^g) ->  ^g) and
                     (Control.Bind or  ^g or  ^Monad<bool>) :
                       (static member (>>=) :
                           ^g * (unit ->  ^Monad<bool>) ->  ^Monad<bool>) and
                     (Control.Return or  ^a) :
                       (static member Return:
                           ^a * Control.Return -> (unit ->  ^a)) and
                     (Control.Bind or  ^a) :
                       (static member (>>=) :  ^a * (unit ->  ^a) ->  ^a) and
                     (Control.Using or  ^a) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^a) *
                          Control.Using ->  ^a) and
                     (Control.TryWith or  ^a) :
                       (static member TryWith:
                          (unit ->  ^a) * ('b ->  ^a) * Control.TryWith *
                          Control.TryBlock.True ->  ^a) and
                     (Control.Delay or  ^a) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^a) * Control.Delay ->  ^a) and
                     'b :> exn and
                     (Control.Return or  ^c) :
                       (static member Return:
                           ^c * Control.Return -> (unit ->  ^c)) and
                     (Control.Bind or  ^c) :
                       (static member (>>=) :  ^c * (unit ->  ^c) ->  ^c) and
                     (Control.Using or  ^c) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^c) *
                          Control.Using ->  ^c) and
                     (Control.TryWith or  ^c) :
                       (static member TryWith:
                          (unit ->  ^c) * ('d ->  ^c) * Control.TryWith *
                          Control.TryBlock.True ->  ^c) and
                     (Control.Delay or  ^c) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^c) * Control.Delay ->  ^c) and
                     'd :> exn and
                     (Control.Return or  ^e) :
                       (static member Return:
                           ^e * Control.Return -> (unit ->  ^e)) and
                     (Control.Bind or  ^e) :
                       (static member (>>=) :  ^e * (unit ->  ^e) ->  ^e) and
                     (Control.Using or  ^e) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^e) *
                          Control.Using ->  ^e) and
                     (Control.TryWith or  ^e) :
                       (static member TryWith:
                          (unit ->  ^e) * ('f ->  ^e) * Control.TryWith *
                          Control.TryBlock.True ->  ^e) and
                     (Control.Delay or  ^e) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^e) * Control.Delay ->  ^e) and
                     'f :> exn and
                     (Control.Return or  ^g) :
                       (static member Return:
                           ^g * Control.Return -> (unit ->  ^g)) and
                     (Control.Bind or  ^g) :
                       (static member (>>=) :  ^g * (unit ->  ^g) ->  ^g) and
                     (Control.Using or  ^g) :
                       (static member Using:
                          System.IDisposable * (System.IDisposable ->  ^g) *
                          Control.Using ->  ^g) and
                     (Control.TryWith or  ^g) :
                       (static member TryWith:
                          (unit ->  ^g) * ('h ->  ^g) * Control.TryWith *
                          Control.TryBlock.True ->  ^g) and
                     (Control.Delay or  ^g) :
                       (static member Delay:
                          Control.Delay * (unit ->  ^g) * Control.Delay ->  ^g) and
                     'h :> exn

