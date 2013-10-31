namespace FsControl.Core.Abstractions

open System

module Converter =

    type FromBytes = FromBytes with
        static member instance (FromBytes, _:bool   ) = fun (x, i) -> BitConverter.ToBoolean(x, i)
        static member instance (FromBytes, _:char   ) = fun (x, i) -> BitConverter.ToChar   (x, i)
        static member instance (FromBytes, _:float  ) = fun (x, i) -> BitConverter.ToDouble (x, i)
        static member instance (FromBytes, _: int16 ) = fun (x, i) -> BitConverter.ToInt16  (x, i)
        static member instance (FromBytes, _: int   ) = fun (x, i) -> BitConverter.ToInt32  (x, i)
        static member instance (FromBytes, _:int64  ) = fun (x, i) -> BitConverter.ToInt64  (x, i)
        static member instance (FromBytes, _:float32) = fun (x, i) -> BitConverter.ToSingle (x, i)
        static member instance (FromBytes, _:string ) = fun (x, i) -> BitConverter.ToString (x, i)
        static member instance (FromBytes, _:uint16 ) = fun (x, i) -> BitConverter.ToUInt16 (x, i)
        static member instance (FromBytes, _:uint32 ) = fun (x, i) -> BitConverter.ToUInt32 (x, i)
        static member instance (FromBytes, _:uint64 ) = fun (x, i) -> BitConverter.ToUInt64 (x, i)
        
    let inline internal fromBytesWithOffset (startIndex:int) (value:byte[]) = Inline.instance FromBytes (value, startIndex)
    let inline internal fromBytes                            (value:byte[]) = Inline.instance FromBytes (value, 0         )


    type ToBytes = ToBytes with
        static member instance (ToBytes, x:bool   , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:char   , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:float  , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x: int16 , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x: int   , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:int64  , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:float32, _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:string , _) = fun () -> Array.map byte (x.ToCharArray())
        static member instance (ToBytes, x:uint16 , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:uint32 , _) = fun () -> BitConverter.GetBytes(x)
        static member instance (ToBytes, x:uint64 , _) = fun () -> BitConverter.GetBytes(x)

    let inline internal toBytes value :byte[] = Inline.instance (ToBytes, value) ()


    type Parse = Parse with
        static member instance (Parse, _:bool   ) = Boolean.Parse
        static member instance (Parse, _:char   ) = Char   .Parse
        static member instance (Parse, _:float  ) = Double .Parse
        static member instance (Parse, _: int16 ) = Int16  .Parse
        static member instance (Parse, _: int   ) = Int32  .Parse
        static member instance (Parse, _:int64  ) = Int64  .Parse
        static member instance (Parse, _:float32) = Single .Parse
        static member instance (Parse, _:string ) = id :string->_            
        static member instance (Parse, _:uint16 ) = UInt16 .Parse
        static member instance (Parse, _:uint32 ) = UInt32 .Parse
        static member instance (Parse, _:uint64 ) = UInt64 .Parse

    let inline internal parse (value:string) = Inline.instance Parse value