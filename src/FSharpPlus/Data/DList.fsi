namespace FSharpPlus.Data
    
    type DListData<'T> =
        | Nil
        | Unit of 'T
        | Join of DListData<'T> * DListData<'T>
    
    /// DList is an ordered linear structure implementing the List signature (head, tail, cons), 
    /// end-insertion (add), and O(1) append. Ordering is by insertion history.
    /// DList is an implementation of [John Hughes' append list](http://dl.acm.org/citation.cfm?id=8475).
    [<Sealed>]
    type DList<'T> =
        interface System.Collections.IEnumerable
        interface System.Collections.Generic.IReadOnlyList<'T>
        interface System.Collections.Generic.IReadOnlyCollection<'T>
        interface System.Collections.Generic.IEnumerable<'T>
        
        new: length: int * data: DListData<'T> -> DList<'T>
        
        static member (+) : x: DList<'a> * y: DList<'a> -> DList<'a>
        
        static member (<*>) : f: DList<('a -> 'b)> * x: DList<'a> -> DList<'b>
        
        static member (<|>) : x: DList<'a> * y: DList<'a> -> DList<'a>
        
        static member (>>=) : x: DList<'a> * f: ('b -> DList<'a>) -> DList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Fold: x: 'a * f: ('a -> 'b -> 'a) * z: DList<'b> -> 'a
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Map: x: DList<'a> * f: ('a -> 'b) -> DList<'b>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member OfSeq: x: seq<'a> -> DList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member Return: x: 'a -> DList<'a>
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member ToList: x: DList<'a> -> 'a list
        
        [<System.ComponentModel.EditorBrowsable
          (enum<System.ComponentModel.EditorBrowsableState> (1))>]
        static member ToSeq: x: DList<'a> -> seq<'a>
        
        static member
          append: left: DListData<'a> * right: DListData<'a> -> DListData<'a>
        
        static member
          appendLists: left: DList<'T> * right: DList<'T> -> DList<'T>
        
        static member
          private findi: f: (int -> 'T -> bool) -> l: DList<'T> -> 'T
        
        static member
          fold: f: ('State -> 'T -> 'State) -> state: 'State -> l: DList<'T>
                  -> 'State
        
        static member
          foldBack: f: ('T -> 'State -> 'State) -> l: DList<'T> -> state: 'State
                      -> 'State
        
        static member get_Empty: unit -> DList<'a>
        
        static member get_Zero: unit -> DList<'a>
        
        static member head: data: DListData<'a> -> 'a
        
        static member ofSeq: s: seq<'T> -> DList<'T>
        
        static member
          private tryFindi: f: (int -> 'T -> bool) -> l: DList<'T> -> 'T option
        
        static member tryHead: data: DListData<'a> -> 'a option
        
        /// O(1). Returns a new DList with the element added to the end.
        member Add: x: 'T -> DList<'T>
        
        /// O(1). Returns a new DList with the element added to the front.
        member Cons: hd: 'T -> DList<'T>
        
        override Equals: other: obj -> bool
        
        override GetHashCode: unit -> int
        
        member toSeq: unit -> System.Collections.Generic.IEnumerator<'T>
        
        /// O(log n). Returns the first element.
        member Head: 'T
        
        /// O(1). Returns true if the DList has no elements.
        member IsEmpty: bool
        
        member Item: index: int -> 'T with get
        
        /// O(1). Returns the count of elememts.
        member Length: int
        
        /// O(log n). Returns a new DList of the elements trailing the first element.
        member Tail: DList<'T>
        
        /// O(log n). Returns option first element
        member TryHead: 'T option
        
        /// O(log n). Returns option DList of the elements trailing the first element.
        member TryTail: DList<'T> option
        
        /// O(log n). Returns option first element and tail.
        member TryUncons: ('T * DList<'T>) option
        
        /// O(log n). Returns the first element and tail.
        member Uncons: 'T * DList<'T>
        
        member internal dc: DListData<'T>
    
    module DList =
        
        /// O(1). Returns a new DList of two lists.
        val append: left: DList<'T> -> right: DList<'T> -> DList<'T>
        
        /// O(1). Returns a new DList with the element added to the beginning.
        val cons: hd: 'T -> l: DList<'T> -> DList<'T>
        
        /// O(1). Returns DList of no elements.
        [<GeneralizableValue>]
        val empty<'T> : DList<'T>
        
        /// O(n). Fold walks the DList using constant stack space. Implementation is from Norman Ramsey.
        /// See http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5334068#5334068
        val foldBack:
          f: ('T -> 'State -> 'State) -> l: DList<'T> -> state: 'State -> 'State
        
        val fold:
          f: ('State -> 'T -> 'State) -> state: 'State -> l: DList<'T> -> 'State
        
        /// O(log n). Returns the first element.
        val inline head: l: DList<'T> -> 'T
        
        /// O(log n). Returns option first element.
        val inline tryHead: l: DList<'T> -> 'T option
        
        /// O(1). Returns true if the DList has no elements.
        val inline isEmpty: l: DList<'T> -> bool
        
        /// O(1). Returns the count of elememts.
        val inline length: l: DList<'T> -> int
        
        /// O(1). Returns DList of one elements.
        val singleton: x: 'a -> DList<'a>
        
        /// O(1). Returns a new DList with the element added to the end.
        val inline add: x: 'T -> l: DList<'T> -> DList<'T>
        
        /// O(log n). Returns a new DList of the elements trailing the first element.
        val inline tail: l: DList<'T> -> DList<'T>
        
        /// O(log n). Returns option DList of the elements trailing the first element.
        val inline tryTail: l: DList<'T> -> DList<'T> option
        
        /// O(log n). Returns the first element and tail.
        val inline uncons: l: DList<'T> -> 'T * DList<'T>
        
        /// O(log n). Returns option first element and tail.
        val inline tryUncons: l: DList<'T> -> ('T * DList<'T>) option
        
        /// O(n). Returns a DList of the seq.
        val ofSeq: s: seq<'T> -> DList<'T>
        
        /// O(n). Returns a list of the DList elements.
        val inline toList: l: DList<'a> -> 'a list
        
        /// O(n). Returns a seq of the DList elements.
        val inline toSeq: l: DList<'T> -> seq<'T>
        
        val inline map: f: ('a -> 'b) -> x: DList<'a> -> DList<'b>
        
        val concat: x: DList<DList<'a>> -> DList<'a>
        
        val inline ap: f: DList<('a -> 'b)> -> x: DList<'a> -> DList<'b>
        
        val inline bind: m: DList<'a> -> k: ('b -> DList<'a>) -> DList<'a>

