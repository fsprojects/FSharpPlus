namespace FSharpPlus
    
    /// Module containing F#+ Extension Methods on existing types
    module Extensions =
        type IEnumerable<'T> with
            
            member
              GetSlice: (int option * int option
                           -> System.Collections.Generic.IEnumerable<'T>)
        type List<'T> with
            
            member GetSlice: (int option * int option -> List<'T>)
        
        val private (|Canceled|Faulted|Completed|) :
          t: System.Threading.Tasks.Task<'a>
            -> Choice<unit,System.AggregateException,'a>
        type Task<'TResult> with
            
            static member
              WhenAll: tasks: System.Threading.Tasks.Task<'a>[] *
                       ?cancellationToken: System.Threading.CancellationToken
                         -> System.Threading.Tasks.Task<'a[]>
        type Async<'T> with
            
            /// Combine all asyncs in one, chaining them in sequence order.
            static member Sequence: t: seq<Async<'a>> -> Async<seq<'a>>
        type Async<'T> with
            
            /// Combine all asyncs in one, chaining them in sequence order.
            static member Sequence: t: Async<'a> list -> Async<'a list>
        type Async<'T> with
            
            /// Combine all asyncs in one, chaining them in sequence order.
            static member Sequence: t: Async<'a> array -> Async<'a array>
        type Async<'T> with
            
            /// Creates an async Result from a Result where the Ok case is async.
            static member
              Sequence: t: Result<Async<'T>,'Error> -> Async<Result<'T,'Error>>
        type Async<'T> with
            
            /// Creates an async Choice from a Choice where the Choice1Of2 case is async.
            static member
              Sequence: t: Choice<Async<'T>,'Choice2Of2>
                          -> Async<Choice<'T,'Choice2Of2>>
        type Async<'T> with
            
            /// Creates an async Result from a Result where both cases are async.
            static member
              Bisequence: t: Result<Async<'T>,Async<'Error>>
                            -> Async<Result<'T,'Error>>
        type Async<'T> with
            
            /// Creates an async Choice from a Choice where both cases are async.
            static member
              Bisequence: t: Choice<Async<'T>,Async<'Choice2Of2>>
                            -> Async<Choice<'T,'Choice2Of2>>
        type Option<'T> with
            
            /// Returns None if it contains a None element, otherwise a list of all elements
            static member Sequence: t: seq<'T option> -> seq<'T> option

