namespace FSharpPlus.Data
    
    /// Array with an Applicative functor based on zipping and parallel execution.
    type ParallelArray<'t> =
        | Infinite of 't
        | Bounded of 't array
        
        static member
          inline (+) : x: parray< ^m> * y: parray< ^m> -> parray< ^m>
                         when (Control.Plus or  ^m) :
                                (static member ``+`` :
                                    ^m *  ^m * Control.Plus ->  ^m)
        
        static member
          (<*>) : f: parray<('a -> 'b)> * x: parray<'a> -> parray<'b>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift2: f: ('T -> 'U -> 'V) * x: parray<'T> * y: parray<'U>
                   -> parray<'V>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift3: f: ('T -> 'U -> 'V -> 'W) * x: parray<'T> * y: parray<'U> *
                 z: parray<'V> -> parray<'W>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: x: parray<'a> * f: ('a -> 'b) -> ParallelArray<'b>
        
        static member Return: x: 'a -> ParallelArray<'a>
        
        static member inline get_Zero: unit -> parray<'m>
    
    /// Basic operations on ParallelArray
    module ParallelArray =
        
        val run: _arg1: ParallelArray<'a> -> 'a array
        
        val map: f: ('a -> 'b) -> _arg1: ParallelArray<'a> -> ParallelArray<'b>
        
        val ap:
          f: ParallelArray<('a -> 'b)> -> x: ParallelArray<'a>
            -> ParallelArray<'b>
        
        val map2:
          f: ('a -> 'b -> 'c) -> x: ParallelArray<'a> -> y: ParallelArray<'b>
            -> ParallelArray<'c>
        
        val map3:
          f: ('a -> 'b -> 'c -> 'd) -> x: ParallelArray<'a>
          -> y: ParallelArray<'b> -> z: ParallelArray<'c> -> ParallelArray<'d>
    
    /// A type alias for ParallelArray<'T>
    type parray<'t> = ParallelArray<'t>
    
    module ParallelArrayOperators =
        
        /// Creates a parallel array from a normal array.
        val parray: s: 'a array -> ParallelArray<'a>

