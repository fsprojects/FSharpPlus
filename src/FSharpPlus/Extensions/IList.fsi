namespace FSharpPlus
    
    /// Additional operations IList<'T>
    module IList =
        
        /// <summary>Converts an IList to an IReadOnlyList (from System.Collections.Generic).</summary>
        /// <param name="source">The System.Collections.Generic.IList</param>
        /// <returns>The list converted to a System.Collections.Generic.IReadOnlyList</returns>
        val toIReadOnlyList:
          source: System.Collections.Generic.IList<'a>
            -> System.Collections.Generic.IReadOnlyList<'a>

