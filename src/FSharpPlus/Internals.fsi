namespace FSharpPlus.Internals
    
    /// <namespacedoc>
    /// <summary>
    /// Internal to the library - please ignore
    /// </summary>
    /// </namespacedoc>
    [<Class>]
    type Default6
    
    [<Class>]
    type Default5 =
        inherit Default6
    
    [<Class>]
    type Default4 =
        inherit Default5
    
    [<Class>]
    type Default3 =
        inherit Default4
    
    [<Class>]
    type Default2 =
        inherit Default3
    
    [<Class>]
    type Default1 =
        inherit Default2
    
    module internal Prelude =
        
        val inline flip: f: ('a -> 'b -> 'c) -> x: 'b -> y: 'a -> 'c
        
        val inline const': k: 'a -> 'b -> 'a
        
        val inline tupleToOption: bool * 'a -> 'a option
        
        val inline opaqueId: x: 'a -> 'a
        
        val inline retype: x: 'T -> 'U
        
        val inline tuple1: x: 't -> System.Tuple<'t>
    
    module internal Implicit =
        
        val inline Invoke:
          x:  ^t ->  ^R
            when ( ^R or  ^t) : (static member op_Implicit:  ^t ->  ^R)
    
    module Errors =
        
        val exnDivByZero: exn
        
        val exnNoDivision: System.Exception
        
        val exnSqrtOfNegative: System.Exception
        
        val exnNoSqrt: System.Exception
        
        val exnNoSubtraction: System.Exception
        
        val exnUnreachable: System.InvalidOperationException
    
    module Decimal =
        
        val inline trySqrt: x: decimal -> Result<decimal,System.Exception>
    
    module Rational =
        
        val inline numerator:
          x:  ^F -> 'R when  ^F: (member get_Numerator:  ^F -> 'R)
        
        val inline denominator:
          x:  ^F -> 'R when  ^F: (member get_Denominator:  ^F -> 'R)
    
    module BigInteger =
        
        val trySqrtRem:
          x: System.Numerics.BigInteger
            -> Result<(System.Numerics.BigInteger * System.Numerics.BigInteger),
                      System.Exception>
    
    module Constraints =
        
        /// Constrain 't to be a nested tuple of <'t1,'t2,'t3,'t4,'t5,'t6,'t7,'tr>
        val inline whenNestedTuple:
          t:  ^t -> 't1 * 't2 * 't3 * 't4 * 't5 * 't6 * 't7 * 'tr
            when  ^t: (member get_Item1:  ^t -> 't1) and
                  ^t: (member get_Item2:  ^t -> 't2) and
                  ^t: (member get_Item3:  ^t -> 't3) and
                  ^t: (member get_Item4:  ^t -> 't4) and
                  ^t: (member get_Item5:  ^t -> 't5) and
                  ^t: (member get_Item6:  ^t -> 't6) and
                  ^t: (member get_Item7:  ^t -> 't7) and
                  ^t: (member get_Rest:  ^t -> 'tr)
    
    type Id<'t> =
        
        new: v: 't -> Id<'t>
        
        member getValue: 't
    
    module Id =
        
        val run: x: Id<'a> -> 'a
        
        val map: f: ('a -> 'b) -> x: Id<'a> -> Id<'b>
        
        val create: x: 'a -> Id<'a>
    
    type Id0 =
        
        new: v: string -> Id0
        
        member getValue: string
    
    type Either<'t,'u> =
        | Left of 't
        | Right of 'u
    
    [<Struct>]
    type DmStruct
    
    [<Sealed>]
    type Set2<'T when 'T: comparison> =
        
        new: unit -> Set2<'T>
    
    [<Class>]
    type BitConverter =
        
        /// Converts a byte into an array of bytes with length one.
        static member GetBytes: value: bool -> byte[]
        
        /// Converts a double into an array of bytes with length
        /// eight.
        static member GetBytes: value: float * isLittleEndian: bool -> byte[]
        
        /// Converts a float into an array of bytes with length
        /// four.
        static member GetBytes: value: float32 * isLittleEndian: bool -> byte[]
        
        /// Converts an unsigned long into an array of bytes with
        /// length eight.
        static member GetBytes: value: uint64 * isLittleEndian: bool -> byte[]
        
        /// Converts an uint into an array of bytes with
        /// length four.
        static member GetBytes: value: uint32 * isLittleEndian: bool -> byte[]
        
        /// Converts an ushort into an array of bytes with
        /// length two.
        static member GetBytes: value: uint16 * isLittleEndian: bool -> byte[]
        
        /// Converts a long into an array of bytes with length
        /// eight.
        static member GetBytes: value: int64 * isLittleEndian: bool -> byte[]
        
        /// Converts an int into an array of bytes with length
        /// four.
        static member GetBytes: value: int * isLittleEndian: bool -> byte[]
        
        /// Converts a short into an array of bytes with length
        /// two.
        static member GetBytes: value: int16 * isLittleEndian: bool -> byte[]
        
        /// Converts a char into an array of bytes with length two.
        static member GetBytes: value: char * isLittleEndian: bool -> byte[]
        
        static member private GetHexValue: i: int -> char
        
        /// Converts an array of bytes into a char.
        static member
          ToChar: value: byte[] * startIndex: int * isLittleEndian: bool -> char
        
        /// Converts an array of bytes into a double.
        static member
          ToDouble: value: byte[] * startIndex: int * isLittleEndian: bool
                      -> float
        
        /// Converts an array of bytes into a short.
        static member
          ToInt16: value: byte[] * startIndex: int * isLittleEndian: bool
                     -> int16
        
        /// Converts an array of bytes into an int.
        static member
          ToInt32: value: byte[] * startIndex: int * isLittleEndian: bool -> int
        
        /// Converts an array of bytes into a long.
        static member
          ToInt64: value: byte[] * startIndex: int * isLittleEndian: bool
                     -> int64
        
        /// Converts an array of bytes into a float.
        static member
          ToSingle: value: byte[] * startIndex: int * isLittleEndian: bool
                      -> float32
        
        /// Converts an array of bytes into a String.
        static member ToString: value: byte[] -> string
        
        /// Converts an array of bytes into a String.
        static member ToString: value: byte[] * startIndex: int -> string
        
        /// Converts an array of bytes into a String.
        static member
          ToString: value: byte[] * startIndex: int * length: int -> string
        
        /// Converts an array of bytes into an ushort.
        ///
        static member
          ToUInt16: value: byte[] * startIndex: int * isLittleEndian: bool
                      -> uint16
        
        /// Converts an array of bytes into an uint.
        ///
        static member
          ToUInt32: value: byte[] * startIndex: int * isLittleEndian: bool
                      -> uint32
        
        /// Converts an array of bytes into an unsigned long.
        ///
        static member
          ToUInt64: value: byte[] * startIndex: int * isLittleEndian: bool
                      -> uint64
    
    module FindSliceIndex =
        
        val seqImpl: slice: seq<'a> -> source: seq<'a> -> int
        
        val sequenceEqual: a: seq<'a> -> b: seq<'a> -> bool
        
        val listImpl: slice: 'a list -> source: 'a list -> int
        
        val arrayImpl: slice: 'a[] -> source: 'a[] -> int

