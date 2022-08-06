namespace FSharpPlus.Data
    
    /// <summary> Computation type: Computations which can be interrupted and resumed.
    /// <para/>   Binding strategy: Binding a function to a monadic value creates a new continuation which uses the function as the continuation of the monadic computation.
    /// <para/>   Useful for: Complex control structures, error handling, and creating co-routines.</summary>
    [<Struct>]
    type Cont<'r,'t> =
        | Cont of (('t -> 'r) -> 'r)
        
        /// <summary>
        /// Sequences two Conts left-to-right, discarding the value of the first argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member ( *> ) : x: Cont<'R,'T> * y: Cont<'R,'U> -> Cont<'R,'U>
        
        /// <summary>
        /// Sequences two Conts left-to-right, discarding the value of the second argument.
        /// </summary>
        /// <category index="2">Applicative</category>
        static member ( <* ) : x: Cont<'R,'U> * y: Cont<'R,'T> -> Cont<'R,'U>
        
        /// <summary>Lifts a function into a Cont. Same as map.
        /// To be used in Applicative Style expressions, combined with &lt;*&gt;
        /// </summary>
        /// <category index="1">Functor</category>
        static member (<!>) : f: ('T -> 'U) * x: Cont<'R,'T> -> Cont<'R,'U>
        
        static member
          (<*>) : f: Cont<'R,('T -> 'U)> * x: Cont<'R,'T> -> Cont<'R,'U>
        
        static member
          (>>=) : x: Cont<'R,'T> * f: ('T -> Cont<'R,'U>) -> Cont<'R,'U>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          CallCC: f: (('T -> Cont<'R,'U>) -> Cont<'R,'T>) -> Cont<'R,'T>
        
        static member Delay: f: (unit -> Cont<'R,'T>) -> Cont<'R,'T>
        
        static member
          inline Lift: m:  ^Monad<'T> -> ContT< ^Monad<'R>,'T>
                         when (Control.Bind or  ^Monad<'T> or  ^Monad<'R>) :
                                (static member (>>=) :
                                    ^Monad<'T> * ('T ->  ^Monad<'R>)
                                     ->  ^Monad<'R>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift2: f: ('T -> 'U -> 'V) * x: Cont<'R,'T> * y: Cont<'R,'U>
                          -> Cont<'R,'V>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          inline Lift3: f: ('T -> 'U -> 'V -> 'W) * x: Cont<'R,'T> *
                        y: Cont<'R,'U> * z: Cont<'R,'V> -> Cont<'R,'W>
        
        static member inline LiftAsync: x: Async<'T> -> ContT<Async<'R>,'T>
        
        static member
          inline Local: Cont< ^a,'MonadReader<R1,'T>,'U> * f: ('R1 -> 'R2)
                          -> ContT< ^b,'MonadReader<R1,'T>,'U>
                          when  ^a:
                                 (static member Local:  ^b * ('e -> 'd) ->  ^a) and
                               (Control.Bind or  ^c or  ^b) :
                                 (static member (>>=) :  ^c * ('d ->  ^b) ->  ^b) and
                                ^b:
                                 (static member Local:  ^a * ('R1 -> 'R2) ->  ^b) and
                                ^c: (static member get_Ask: ->  ^c)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: x: Cont<'R,'T> * f: ('T -> 'U) -> Cont<'R,'U>
        
        static member
          inline Put: x: 'S ->  ^ContT<'MonadState<'S, 'T>, unit>
                        when  ^ContT<'MonadState<'S, 'T>, unit> :
                               (static member Lift:
                                   ^a ->  ^ContT<'MonadState<'S, 'T>, unit>) and
                              ^a: (static member Put: 'S ->  ^a)
        
        static member Return: n: 'T -> Cont<'R,'T>
        
        static member TryFinally: Cont<'R,'T> * h: (unit -> unit) -> Cont<'R,'T>
        
        static member
          TryWith: Cont<'R,'T> * h: (exn -> Cont<'R,'T>) -> Cont<'R,'T>
        
        static member
          Using: resource: 'a * f: ('a -> Cont<'R,'T>) -> Cont<'R,'T>
                   when 'a :> System.IDisposable
        
        static member
          inline get_Ask: unit ->  ^ContT<'MonadReader<'R,'T>,'R>
                            when  ^ContT<'MonadReader<'R,'T>,'R> :
                                   (static member Lift:
                                       ^a ->  ^ContT<'MonadReader<'R,'T>,'R>) and
                                  ^a: (static member get_Ask: ->  ^a)
        
        static member
          inline get_Get: unit ->  ^ContT<'MonadState<'S, 'T>, 'S>
                            when  ^ContT<'MonadState<'S, 'T>, 'S> :
                                   (static member Lift:
                                       ^a ->  ^ContT<'MonadState<'S, 'T>, 'S>) and
                                  ^a: (static member get_Get: ->  ^a)
    
    /// Basic operations on Cont
    module Cont =
        
        /// The result of running a CPS computation with a given final continuation.
        val run: Cont<'R,'T> -> continuation: ('T -> 'R) -> 'R
        
        /// The result of running a CPS computation with the identity function as the final continuation.
        val eval: Cont<'R,'R> -> 'R
        
        /// (call-with-current-continuation) calls a function with the current continuation as its argument.
        val callCC: f: (('T -> Cont<'R,'U>) -> Cont<'R,'T>) -> Cont<'R,'T>
        
        val map: f: ('T -> 'U) -> Cont<'R,'T> -> Cont<'R,'U>
        
        val bind: f: ('T -> Cont<'R,'U>) -> Cont<'R,'T> -> Cont<'R,'U>
        
        val apply: Cont<'R,('T -> 'U)> -> Cont<'R,'T> -> Cont<'R,'U>
        
        val map2:
          f: ('T -> 'U -> 'V) -> Cont<'R,'T> -> Cont<'R,'U> -> Cont<'R,'V>
        
        val map3:
          f: ('T -> 'U -> 'V -> 'W) -> Cont<'R,'T> -> Cont<'R,'U> -> Cont<'R,'V>
            -> Cont<'R,'W>
    
    /// Monad Transformer for Cont<'R,'T>
    [<Struct>]
    type ContT<'r,'t> = Cont<'r,'t>
    
    /// Basic operations on ContT
    module ContT =
        
        /// The result of running a CPS computation with the identity function as the final continuation.
        val run: ContT<'MR,'T> -> continuation: ('T -> 'MR) -> 'MR
        
        /// The result of running a CPS computation with its inner monad's 'Return' function as the final continuation.
        val inline eval:
          ContT< ^MR,'T> ->  ^MR
            when (Control.Return or  ^MR) :
                   (static member Return:  ^MR * Control.Return -> ('T ->  ^MR))

