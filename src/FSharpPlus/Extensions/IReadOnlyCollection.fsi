namespace FSharpPlus
    
    /// Additional operations on IReadOnlyCollection<'T>
    module IReadOnlyCollection =
        
        val ofArray:
          source: 'T[] -> System.Collections.Generic.IReadOnlyCollection<'T>
        
        val ofList:
          source: 'T list -> System.Collections.Generic.IReadOnlyCollection<'T>
        
        val ofSeq:
          source: seq<'T> -> System.Collections.Generic.IReadOnlyCollection<'T>
        
        val map:
          mapping: ('T -> 'U)
          -> source: System.Collections.Generic.IReadOnlyCollection<'T>
            -> System.Collections.Generic.IReadOnlyCollection<'U>
        
        val iter:
          mapping: ('T -> unit)
          -> source: System.Collections.Generic.IReadOnlyCollection<'T> -> unit

