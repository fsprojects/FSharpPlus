namespace FSharpPlus.Data
    
    /// A sequence with an Applicative functor based on zipping.
    [<NoEquality; NoComparison>]
    type ZipList<'s> =
        | ZipList of seq<'s>
        
        static member
          inline (+) : x: ZipList< ^a> * y: ZipList< ^a> -> ZipList< ^a>
                         when (Control.Plus or  ^a) :
                                (static member ``+`` :
                                    ^a *  ^a * Control.Plus ->  ^a)
        
        static member (<*>) : ZipList<('a -> 'b)> * ZipList<'a> -> ZipList<'b>
        
        static member (<|>) : ZipList<'a> * ZipList<'a> -> ZipList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member IsLeftZero: ZipList<'a> -> bool
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift2: f: ('T1 -> 'T2 -> 'a) * x: ZipList<'T1> * y: ZipList<'T2>
                   -> ZipList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Lift3: f: ('T1 -> 'T2 -> 'T3 -> 'a) * x: ZipList<'T1> *
                 y: ZipList<'T2> * z: ZipList<'T3> -> ZipList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: ZipList<'a> * f: ('a -> 'b) -> ZipList<'b>
        
        static member Return: x: 'a -> ZipList<'a>
        
        static member ToSeq: ZipList<'a> -> seq<'a>
        
        static member
          inline Traverse: ZipList<'T> * f: ('T ->  ^Functor<'U>)
                             ->  ^Functor<ZipList<'U>>
                             when (Control.Apply or  ^a or  ^Functor<'U> or  ^b) :
                                    (static member ``<*>`` :
                                        ^a *  ^Functor<'U> *  ^b * Control.Apply
                                         ->  ^b) and
                                  (Control.IsLeftZero or  ^Functor<'U>) :
                                    (static member IsLeftZero:
                                        ^Functor<'U> ref * Control.IsLeftZero
                                         -> bool) and
                                  (Control.Map or  ^b or  ^a) :
                                    (static member Map:
                                       ( ^b * ('d list -> 'd -> 'd list)) *
                                       Control.Map ->  ^a) and
                                  (Control.Return or  ^b) :
                                    (static member Return:
                                        ^b * Control.Return -> ('c list ->  ^b)) and
                                  (Control.Map or  ^b or  ^Functor<List<'U>>) :
                                    (static member Map:
                                       ( ^b * ('e list -> seq<'e>)) *
                                       Control.Map ->  ^Functor<List<'U>>) and
                                  (Control.Traverse or seq<'T> or
                                    ^Functor<List<'U>>) :
                                    (static member Traverse:
                                       seq<'T> * ('T ->  ^Functor<'U>) *
                                        ^Functor<List<'U>> * Control.Traverse
                                         ->  ^Functor<List<'U>>) and
                                  (Control.Map or  ^Functor<List<'U>> or
                                    ^Functor<ZipList<'U>>) :
                                    (static member Map:
                                       ( ^Functor<List<'U>> *
                                        (seq<'f> -> ZipList<'f>)) * Control.Map
                                         ->  ^Functor<ZipList<'U>>)
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Zip: x: ZipList<'a> * y: ZipList<'b> -> ZipList<'a * 'b>
        
        static member get_Empty: unit -> ZipList<'a>
        
        static member
          inline get_Zero: unit -> ZipList< ^a>
                             when (Control.Zero or  ^a) :
                                    (static member Zero:
                                        ^a * Control.Zero ->  ^a)
        
        member Item: n: int -> 's
    
    /// Basic operations on ZipList
    module ZipList =
        
        val run: ZipList<'a> -> seq<'a>
        
        val map: f: ('a -> 'b) -> ZipList<'a> -> ZipList<'b>
        
        val map2:
          f: ('T1 -> 'T2 -> 'U) -> ZipList<'T1> -> ZipList<'T2> -> ZipList<'U>
        
        val map3:
          f: ('T1 -> 'T2 -> 'T3 -> 'U) -> ZipList<'T1> -> ZipList<'T2>
          -> ZipList<'T3> -> ZipList<'U>
        
        val singleton: x: 'a -> ZipList<'a>
        
        /// <summary>Combines the two lists into a list of pairs. The two lists need not have equal lengths:
        /// when one list is exhausted any remaining elements in the other
        /// list are ignored.</summary>
        /// <param name="list1">The first input list.</param>
        /// <param name="list2">The second input list.</param>
        /// <returns>A single list containing pairs of matching elements from the input lists.</returns>
        val zip: list1: ZipList<'T> -> list2: ZipList<'U> -> ZipList<'T * 'U>

