namespace FSharpPlus
    
    /// Additional operations on Nullable
    module Nullable =
        
        /// <summary>Monadic Bind; Transforms the value inside a Nullable to a Nullable using a specified binding function.</summary>
        /// <param name="f">The value binding function.</param>
        /// <param name="n">The Nullable value.</param>
        val bind:
          f: ('a -> System.Nullable<'b>) -> n: System.Nullable<'a>
            -> System.Nullable<'b>
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType and
                 'b: (new: unit -> 'b) and 'b: struct and 'b :> System.ValueType
        
        /// <summary>Transforms the value inside a Nullable by using a specified mapping function.</summary>
        /// <param name="f">The value mapping function.</param>
        /// <param name="n">The Nullable value.</param>
        val map:
          f: ('a -> 'b) -> n: System.Nullable<'a> -> System.Nullable<'b>
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType and
                 'b: (new: unit -> 'b) and 'b: struct and 'b :> System.ValueType
        
        /// <summary>Invokes a side-effect function to the value of a Nullable if present and ignores the result.</summary>
        /// <param name="f">The function to apply to the value of a Nullable.</param>
        /// <param name="n">The Nullable value.</param>
        val iter:
          f: ('a -> 'b) -> n: System.Nullable<'a> -> unit
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns whether a Nullable has a value.</summary>
        /// <param name="n">The Nullable value.</param>
        /// <returns>True if the Nullable has a value, false otherwise.</returns>
        val hasValue:
          n: System.Nullable<'a> -> bool
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns whether a Nullable is empty.</summary>
        /// <param name="n">The Nullable value.</param>
        /// <returns>True if the Nullable does not have a value, false otherwise.</returns>
        val isNull:
          n: System.Nullable<'a> -> bool
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns the number of values in the Nullable (0 or 1).</summary>
        /// <param name="n">The Nullable value.</param>
        /// <returns>1 if the Nullable has a value, 0 otherwise.</returns>
        val count:
          n: System.Nullable<'a> -> int
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns the value inside a Nullable if it has one, otherwise returns defValue.</summary>
        /// <param name="defValue">The value to use if the Nullable does not have a value.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>The value inside the Nullable or defValue.</returns>
        val defaultValue:
          defValue: 'a -> n: System.Nullable<'a> -> 'a
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns the value inside a Nullable if it has one, otherwise returns the result of evaluating defThunk.</summary>
        /// <param name="defThunk">The function that returns a default value.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>The value inside the Nullable or the result of defThunk.</returns>
        val defaultWith:
          defThunk: (unit -> 'a) -> n: System.Nullable<'a> -> 'a
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns whether a Nullable contains a value for which the given predicate returns true.</summary>
        /// <param name="pred">Function to test against the inner value.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>True if the Nullable contains a value that matches the predicate, false otherwise.</returns>
        val exists:
          pred: ('a -> bool) -> n: System.Nullable<'a> -> bool
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Filters the value in a Nullable with the given predicate.</summary>
        /// <param name="pred">Function that returns whether the value in the Nullable should remain.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>The original Nullable value if the predicate matched the value, empty Nullable otherwise.</returns>
        val filter:
          pred: ('a -> bool) -> n: System.Nullable<'a> -> System.Nullable<'a>
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Updates state data with an update function and the value from a Nullable if it has one.</summary>
        /// <param name="f">A function to update the state data when given a value from the Nullable.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>The new state if the Nullable contained a value, the original state otherwise.</returns>
        val fold:
          f: ('a -> 'b -> 'a) -> state: 'a -> n: System.Nullable<'b> -> 'a
            when 'b: (new: unit -> 'b) and 'b: struct and 'b :> System.ValueType
        
        /// <summary>Fold, but the update function has reversed arguments.</summary>
        /// <param name="f">A function to update the state data when given a value from the Nullable.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>The new state if the Nullable contained a value, the original state otherwise.</returns>
        val foldBack:
          f: ('a -> 'b -> 'b) -> state: 'b -> n: System.Nullable<'a> -> 'b
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Returns whether a Nullable is empty or its value passes the given predicate. Like exists, but returns true when there is no value.</summary>
        /// <param name="pred">Function to test against the inner value.</param>
        /// <param name="n">The Nullable value.</param>
        /// <returns>True if the Nullable is empty or the value matches the predicate, false otherwise.</returns>
        val forall:
          pred: ('a -> bool) -> n: System.Nullable<'a> -> bool
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Converts a Nullable to an array with 0 or 1 items.</summary>
        /// <param name="n">The Nullable value.</param>
        /// <returns>An array that contains the value in the Nullable if there is one, or an empty array.</returns>
        val toArray:
          n: System.Nullable<'a> -> 'a[]
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType
        
        /// <summary>Converts a Nullable to a list with 0 or 1 items.</summary>
        /// <param name="n">The Nullable value.</param>
        /// <returns>A list that contains the value in the Nullable if there is one, or an empty list.</returns>
        val toList:
          n: System.Nullable<'a> -> 'a list
            when 'a: (new: unit -> 'a) and 'a: struct and 'a :> System.ValueType

