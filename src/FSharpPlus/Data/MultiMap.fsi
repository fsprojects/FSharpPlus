namespace FSharpPlus.Data
    
    /// A Map where keys are not unique.
    type MultiMap<'Key,'Value when 'Key: comparison> =
        private | MMap of Map<'Key,'Value list>
        interface System.Collections.IEnumerable
        interface
            System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<'Key,
                                                                                           'Value>>
        
        static member
          (+) : MultiMap<'a,'b> * MultiMap<'a,'b> -> MultiMap<'a,'b>
                  when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          Map: x: MultiMap<'a,'b> * f: ('b -> 'c) -> MultiMap<'a,'c>
                 when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          OfArray: x: ('a * 'b)[] -> MultiMap<'a,'b> when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          OfList: x: ('a * 'b) list -> MultiMap<'a,'b> when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          OfSeq: x: seq<'a * 'b> -> MultiMap<'a,'b> when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToArray: x: MultiMap<'a,'b> -> ('a * 'b)[] when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToList: x: MultiMap<'a,'b> -> ('a * 'b) list when 'a: comparison
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member
          ToSeq: x: MultiMap<'a,'b> -> seq<'a * 'b> when 'a: comparison
        
        static member get_Zero: unit -> MultiMap<'a,'b> when 'a: comparison
        
        member Item: key: 'Key -> 'Value list with get
    
    module MultiMap =
        
        /// Converts a seq of tuples to a multiMap.
        val ofSeq:
          source: seq<'Key * 'Value> -> MultiMap<'Key,'Value>
            when 'Key: comparison
        
        /// Converts a list of tuples to a multiMap.
        val ofList:
          source: ('Key * 'Value) list -> MultiMap<'Key,'Value>
            when 'Key: comparison
        
        /// Converts an array of tuples to a multiMap.
        val ofArray:
          source: ('Key * 'Value)[] -> MultiMap<'Key,'Value>
            when 'Key: comparison
        
        /// Converts a multiMap to a seq of tuples.
        val toSeq: MultiMap<'a,'b> -> seq<'a * 'b> when 'a: comparison
        
        /// Converts a multiMap to a list of tuples.
        val toList:
          source: MultiMap<'a,'b> -> ('a * 'b) list when 'a: comparison
        
        /// Converts a multiMap to an array of tuples.
        val toArray: source: MultiMap<'a,'b> -> ('a * 'b)[] when 'a: comparison
        
        /// Returns a new multiMap with the new binding added to the given multiMap.
        val add:
          key: 'Key -> value: 'Value -> source: MultiMap<'Key,'Value>
            -> MultiMap<'Key,'Value> when 'Key: comparison
        
        /// Maps values of the original multiMap.
        val mapValues:
          mapping: ('a -> 'b) -> source: MultiMap<'c,'a> -> MultiMap<'c,'b>
            when 'c: comparison

