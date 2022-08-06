namespace FSharpPlus
    
    /// Additional operations on ReadOnlyList<'T>
    module IReadOnlyList =
        
        val ofArray:
          source: 'T array -> System.Collections.Generic.IReadOnlyList<'T>
        
        val toArray:
          source: System.Collections.Generic.IReadOnlyList<'T> -> 'T[]
        
        /// Returns a new IReadOnlyList from a given IReadOnlyList, with replaced binding for index.
        val trySetItem:
          i: int -> value: 'T
          -> source: System.Collections.Generic.IReadOnlyList<'T>
            -> System.Collections.Generic.IReadOnlyList<'T> option
        
        val tryItem:
          i: int -> source: System.Collections.Generic.IReadOnlyList<'a>
            -> 'a option

