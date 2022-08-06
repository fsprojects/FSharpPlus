namespace FSharpPlus.Control
    
    [<Class>]
    type Explicit =
        inherit Internals.Default1
        
        static member
          inline Explicit: char * Explicit -> ( ^a -> char)
                             when  ^a: (static member op_Explicit:  ^a -> char)
        
        static member
          inline Explicit: decimal * Explicit -> ( ^a -> decimal)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> decimal)
        
        static member
          inline Explicit: float32 * Explicit -> ( ^a -> float32)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> float32)
        
        static member
          inline Explicit: float * Explicit -> ( ^a -> float)
                             when  ^a: (static member op_Explicit:  ^a -> float)
        
        static member
          inline Explicit: unativeint * Explicit -> ( ^a -> unativeint)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> uint32)
        
        static member
          inline Explicit: nativeint * Explicit -> ( ^a -> nativeint)
                             when  ^a: (static member op_Explicit:  ^a -> int)
        
        static member
          inline Explicit: uint64 * Explicit -> ( ^a -> uint64)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> uint64)
        
        static member
          inline Explicit: int64 * Explicit -> ( ^a -> int64)
                             when  ^a: (static member op_Explicit:  ^a -> int64)
        
        static member
          inline Explicit: uint32 * Explicit -> ( ^a -> uint32)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> uint32)
        
        static member
          inline Explicit: int32 * Explicit -> ( ^a -> int)
                             when  ^a: (static member op_Explicit:  ^a -> int)
        
        static member
          inline Explicit: uint16 * Explicit -> ( ^a -> uint16)
                             when  ^a:
                                    (static member op_Explicit:  ^a -> uint16)
        
        static member
          inline Explicit: int16 * Explicit -> ( ^a -> int16)
                             when  ^a: (static member op_Explicit:  ^a -> int16)
        
        static member
          inline Explicit: sbyte * Explicit -> ( ^a -> sbyte)
                             when  ^a: (static member op_Explicit:  ^a -> sbyte)
        
        static member
          inline Explicit: byte * Explicit -> ( ^a -> byte)
                             when  ^a: (static member op_Explicit:  ^a -> byte)
        
        static member
          inline Explicit:  ^t * Internals.Default1 -> unit
                             when  ^t: null and  ^t: struct
        
        static member
          inline Explicit:  ^R * Internals.Default1 -> ( ^t ->  ^R)
                             when ( ^R or  ^t) :
                                    (static member op_Explicit:  ^t ->  ^R)
        
        static member
          inline Invoke: value:  ^a ->  ^T
                           when (Explicit or  ^T or  ^a) :
                                  (static member Explicit:
                                      ^T * Explicit -> ( ^a ->  ^T))
    
    [<Class>]
    type OfBytes =
        
        static member
          inline Invoke: isLtEndian: bool -> startIndex: int -> value: byte[]
                           ->  ^a
                           when (OfBytes or  ^a) :
                                  (static member OfBytes:
                                      ^a * OfBytes
                                       -> (byte[] * int * bool ->  ^a))
        
        static member
          OfBytes: uint64 * OfBytes -> (byte[] * int * bool -> uint64)
        
        static member
          OfBytes: uint32 * OfBytes -> (byte[] * int * bool -> uint32)
        
        static member
          OfBytes: uint16 * OfBytes -> (byte[] * int * bool -> uint16)
        
        static member OfBytes: string * OfBytes -> (byte[] * int * 'b -> string)
        
        static member
          OfBytes: float32 * OfBytes -> (byte[] * int * bool -> float32)
        
        static member OfBytes: int64 * OfBytes -> (byte[] * int * bool -> int64)
        
        static member OfBytes: int * OfBytes -> (byte[] * int * bool -> int)
        
        static member OfBytes: int16 * OfBytes -> (byte[] * int * bool -> int16)
        
        static member OfBytes: float * OfBytes -> (byte[] * int * bool -> float)
        
        static member OfBytes: char * OfBytes -> (byte[] * int * bool -> char)
        
        static member OfBytes: bool * OfBytes -> (byte[] * int * 'a -> bool)
    
    [<Class>]
    type ToBytes =
        
        static member
          inline Invoke: isLittleEndian: bool -> value:  ^a -> byte[]
                           when (ToBytes or  ^a) :
                                  (static member ToBytes:
                                      ^a * bool * ToBytes -> byte[])
        
        static member ToBytes: x: uint64 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: uint32 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: uint16 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: string * 'b * ToBytes -> byte[]
        
        static member ToBytes: x: float32 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: int64 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: int * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: int16 * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: float * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: char * e: bool * ToBytes -> byte[]
        
        static member ToBytes: x: bool * 'a * ToBytes -> byte[]
    
    [<Class>]
    type TryParse =
        inherit Internals.Default1
        
        static member
          inline Invoke: value: string ->  ^a option
                           when (TryParse or  ^a) :
                                  (static member TryParse:
                                      ^a * TryParse -> (string ->  ^a option))
        
        static member
          inline TryParse:  ^R * Internals.Default1 -> (string ->  ^R option)
                             when  ^R:
                                    (static member TryParse:
                                       string ->  ^R option)
        
        static member
          inline TryParse:  ^t * Internals.Default1 -> ('a -> 'a)
                             when  ^t: null and  ^t: struct
        
        static member
          inline TryParse:  ^R * Internals.Default2 -> ('a ->  ^R option)
                             when  ^R:
                                    (static member TryParse:
                                       'a * byref< ^R> -> bool)
        
        static member
          TryParse: System.DateTimeOffset * TryParse
                      -> (string -> System.DateTimeOffset option)
        
        static member
          TryParse: System.DateTime * TryParse
                      -> (string -> System.DateTime option)
        
        static member
          TryParse: System.Text.StringBuilder * TryParse
                      -> (string -> System.Text.StringBuilder option)
        
        static member TryParse: string * TryParse -> (string -> string option)
        
        static member TryParse: int64 * TryParse -> (string -> int64 option)
        
        static member TryParse: int * TryParse -> (string -> int option)
        
        static member TryParse: int16 * TryParse -> (string -> int16 option)
        
        static member TryParse: uint64 * TryParse -> (string -> uint64 option)
        
        static member TryParse: uint32 * TryParse -> (string -> uint32 option)
        
        static member TryParse: uint16 * TryParse -> (string -> uint16 option)
        
        static member TryParse: float * TryParse -> (string -> float option)
        
        static member TryParse: float32 * TryParse -> (string -> float32 option)
        
        static member TryParse: decimal * TryParse -> (string -> decimal option)
    
    [<Class>]
    type Parse =
        inherit Internals.Default1
        
        static member
          inline Invoke: value: string ->  ^a
                           when (Parse or  ^a) :
                                  (static member Parse:
                                      ^a * Parse -> (string ->  ^a))
        
        static member
          Parse: System.Text.StringBuilder * Parse
                   -> (string -> System.Text.StringBuilder)
        
        static member Parse: string * Parse -> (string -> string)
        
        static member Parse: char * Parse -> (string -> char)
        
        static member Parse: bool * Parse -> (string -> bool)
        
        static member
          inline Parse: 'T * Parse -> (string -> 'enum)
                          when 'T: enum<'a> and 'enum: (new: unit -> 'enum) and
                               'enum: struct and 'enum :> System.ValueType
        
        static member
          inline Parse:  ^R * Parse -> (string ->  ^R)
                          when  ^R:
                                 (static member Parse:
                                    string * System.Globalization.CultureInfo
                                      ->  ^R)
        
        static member
          inline Parse:  ^R * Internals.Default1 -> (string ->  ^R)
                          when  ^R: (static member Parse: string ->  ^R)

