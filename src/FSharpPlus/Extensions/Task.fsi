namespace FSharpPlus
    
    /// Additional operations on Task<'T>
    module Task =
        
        val private (|Canceled|Faulted|Completed|) :
          t: System.Threading.Tasks.Task<'a>
            -> Choice<unit,System.AggregateException,'a>
        
        /// <summary>Creates a task workflow from 'source' another, mapping its result with 'f'.</summary>
        val map:
          f: ('T -> 'U) -> source: System.Threading.Tasks.Task<'T>
            -> System.Threading.Tasks.Task<'U>
        
        /// <summary>Creates a task workflow from two workflows 'x' and 'y', mapping its results with 'f'.</summary>
        /// <remarks>Workflows are run in sequence.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">First task workflow.</param>
        /// <param name="y">Second task workflow.</param>
        val map2:
          f: ('T -> 'U -> 'V) -> x: System.Threading.Tasks.Task<'T>
          -> y: System.Threading.Tasks.Task<'U>
            -> System.Threading.Tasks.Task<'V>
        
        /// <summary>Creates a task workflow from three workflows 'x', 'y' and z, mapping its results with 'f'.</summary>
        /// <remarks>Workflows are run in sequence.</remarks>
        /// <param name="f">The mapping function.</param>
        /// <param name="x">First task workflow.</param>
        /// <param name="y">Second task workflow.</param>
        /// <param name="z">Third task workflow.</param>
        val map3:
          f: ('T -> 'U -> 'V -> 'W) -> x: System.Threading.Tasks.Task<'T>
          -> y: System.Threading.Tasks.Task<'U>
          -> z: System.Threading.Tasks.Task<'V>
            -> System.Threading.Tasks.Task<'W>
        
        /// <summary>Creates a task workflow that is the result of applying the resulting function of a task workflow
        /// to the resulting value of another task workflow</summary>
        /// <param name="f">Task workflow returning a function</param>
        /// <param name="x">Task workflow returning a value</param>
        val apply:
          f: System.Threading.Tasks.Task<('T -> 'U)>
          -> x: System.Threading.Tasks.Task<'T>
            -> System.Threading.Tasks.Task<'U>
        
        /// <summary>Creates a task workflow from two workflows 'x' and 'y', tupling its results.</summary>
        val zip:
          x: System.Threading.Tasks.Task<'T>
          -> y: System.Threading.Tasks.Task<'U>
            -> System.Threading.Tasks.Task<'T * 'U>
        
        /// Flattens two nested tasks into one.
        val join:
          source: System.Threading.Tasks.Task<System.Threading.Tasks.Task<'T>>
            -> System.Threading.Tasks.Task<'T>
        
        /// <summary>Creates a task workflow from 'source' workflow, mapping and flattening its result with 'f'.</summary>
        val bind:
          f: ('T -> System.Threading.Tasks.Task<'U>)
          -> source: System.Threading.Tasks.Task<'T>
            -> System.Threading.Tasks.Task<'U>
        
        /// <summary>Creates a task that ignores the result of the source task.</summary>
        /// <remarks>It can be used to convert non-generic Task to unit Task.</remarks>
        val ignore:
          task: System.Threading.Tasks.Task -> System.Threading.Tasks.Task<unit>
        
        /// Used to de-sugar try .. with .. blocks in Computation Expressions.
        val tryWith:
          body: (unit -> System.Threading.Tasks.Task<'T>)
          -> compensation: (exn -> System.Threading.Tasks.Task<'T>)
            -> System.Threading.Tasks.Task<'T>
        
        /// Used to de-sugar try .. finally .. blocks in Computation Expressions.
        val tryFinally:
          body: (unit -> System.Threading.Tasks.Task<'T>)
          -> compensation: (unit -> unit) -> System.Threading.Tasks.Task<'T>
        
        /// Used to de-sugar use .. blocks in Computation Expressions.
        val using:
          disp: 'a -> body: ('a -> System.Threading.Tasks.Task<'a0>)
            -> System.Threading.Tasks.Task<'a0> when 'a :> System.IDisposable
        
        /// Raises an exception in the Task
        val raise: e: exn -> System.Threading.Tasks.Task<'U>

