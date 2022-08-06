namespace FSharpPlus
    
    /// Additional operations on Async
    module Async =
        
        /// <summary>Creates an async workflow from another workflow 'x', mapping its result with 'f'.</summary>
        val map: f: ('a -> 'b) -> x: Async<'a> -> Async<'b>
        
        /// <summary>Creates an async workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
        /// <remarks>Workflows are run in sequence.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">First async workflow.</param>
        /// <param name="y">Second async workflow.</param>
        val map2:
          f: ('a -> 'b -> 'c) -> x: Async<'a> -> y: Async<'b> -> Async<'c>
        
        /// <summary>Creates an async workflow from three workflows 'x', 'y' and 'z', mapping its results with 'f'.</summary>
        /// <remarks>Workflows are run in sequence.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">First async workflow.</param>
        /// <param name="y">Second async workflow.</param>
        /// <param name="z">third async workflow.</param>
        val map3:
          f: ('a -> 'b -> 'c -> 'd) -> x: Async<'a> -> y: Async<'b>
          -> z: Async<'c> -> Async<'d>
        
        /// <summary>Creates an async workflow from two workflows 'x' and 'y', tupling its results.</summary>
        val zip: x: Async<'a> -> y: Async<'b> -> Async<'a * 'b>
        
        /// Flatten two nested asyncs into one.
        val join: x: Async<Async<'a>> -> Async<'a>
        
        /// <summary>Creates an async workflow that is the result of applying the resulting function of
        /// an async workflow to the resulting value of another async workflow.</summary>
        /// <param name="f">Async workflow returning a function.</param>
        /// <param name="x">Async workflow returning a value.</param>
        val apply: f: Async<('a -> 'b)> -> x: Async<'a> -> Async<'b>
        
        /// Raises an exception in the async workflow
        val raise: ex: exn -> Async<'T>

