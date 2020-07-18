module ExtensionsTests

open Testing
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data

type StringCodec<'t> = StringCodec of ( (string -> Result<'t,string>) * ('t -> string) ) with
    static member Invmap (StringCodec (d, e), f: 'T -> 'U, g: 'U -> 'T) = StringCodec (d >> Result.map f, e << g) : StringCodec<'U>

open System

// Begin Validation test

type ValidationErrorType =
    | InvalidLatitude
    | InvalidLongitude
    | EmptyString
    | InvalidStringLength of expected: int * actual: int
    | NegativeNumber
    | InvalidGuidString

type ValidationError = ValidationError of (string * ValidationErrorType NonEmptyList) with
    static member (+) (ValidationError (s1,v1), ValidationError (s2,v2)) = ValidationError (s1+s2, v1+v2)

type NonEmptyString =
    | NonEmptyString of string

    static member Read (NonEmptyString v) = v

    static member Parse (v: string) =
        if String.IsNullOrWhiteSpace(v) then
            Failure ( nel { EmptyString })
        else
            Success(NonEmptyString v)

type Identifier =
    private
    | Identifier of Guid

    static member Read (Identifier (identifier)) = identifier

    static member Parse (v: string) =
        match Guid.TryParse(v) with
        | true, v -> Identifier v |> Success
        | false, _ -> Failure ( nel { InvalidGuidString } )

type Latitude =
    | Latitude of float

    static member Read (Latitude v) = v

    static member Parse (v: float) =
        if (v >= -90.0) && (v <= 90.0) then
            Latitude v |> Success
        else
            Failure ( nel { InvalidLatitude } )

type Longitude =
    private
    | Longitude of float

    static member Read (Longitude v) = v

    static member Parse (v: float) =
        if (v >= -180.0) && (v <= 180.) then
            Longitude v |> Success
        else
            Failure ( nel { InvalidLongitude } )


type Id<'t> = Id of 't with
    static member (<!>) (f, Id x) = Id ( f x)
    static member (<*>) (Id f, Id x) = Id (f x)

let a = curry (fun (x: float, y: float) -> x + y) <!> Id 1. <*> Id 8.   // <*> Id 10

type Location =
    private
    | Location of Latitude * Longitude

    static member Read (Location (Latitude (lat), Longitude (lng))) = lat, lng

    static member Parse lat lng =
        (curry Location <!> Latitude.Parse lat) </Validation.apply/> Longitude.Parse lng

type ThreeLetterString =
    private
    | ThreeLetterString of string

    static member Read (ThreeLetterString (s)) = s

    static member Parse s =
        if String.IsNullOrWhiteSpace s then
            Failure ( nel { EmptyString  } )
        elif s.Length <> 3 then
            Failure ( nel { InvalidStringLength(3, s.Length)  } )
        else
            Success(ThreeLetterString(s))

type Currency =
    private
    | Currency of decimal * ThreeLetterString

    static member Read (Currency (v, t)) = v, t

    static member Parse (value: decimal) (currencyType: string) =
        if value <= 0m then
            Failure ( nel { NegativeNumber } )
        else
            ThreeLetterString.Parse currencyType
            |> Validation.map (fun currencyType -> Currency(value, currencyType))

let private mapValidationError propertyName v =
    match v with
    | Success s -> Success s
    | Failure errors -> ValidationError(propertyName, errors) |> Failure

type LocationAdded =
    { Id: Identifier
      Name: NonEmptyString
      Location: Location
      Price: Currency
      IsDraft: bool
      Remark: string option
      Created: DateTimeOffset
      Creator: NonEmptyString }

    static member Parse id name lat lng price currency isDraft remark created creator =
        let createFn =
            fun id name location price isDraft remark created creator ->
                { Id = id
                  Name = name
                  Location = location
                  Price = price
                  IsDraft = isDraft
                  Remark = remark
                  Created = created
                  Creator = creator }

        createFn
        <!> (Identifier.Parse >> mapValidationError "id") id
        <*> (NonEmptyString.Parse >> mapValidationError "name") name
        <*> (fun lat lng ->
                Location.Parse lat lng
                |> mapValidationError "location")
                lat
                lng
        <*> (fun p c ->
                Currency.Parse p c
                |> mapValidationError "currency")
                price
                currency
        <*> (Validation.Success >> mapValidationError "draft") isDraft
        <*> (Validation.Success >> mapValidationError "remark") remark
        <*> (Validation.Success >> mapValidationError "created") created
        <*> (NonEmptyString.Parse
             >> mapValidationError "creator") creator
             
// End Validation test


module StringCodec =
    let decode (StringCodec (d,_)) x = d x
    let encode (StringCodec (_,e)) x = e x


let ExtensionsTest = 

    let dlistA = DList.ofSeq [1;2;3]
    let dlistB = DList.ofSeq [1;2;3]
    let dlistC = DList.ofSeq [1;2]
    let dlistD = DList.ofSeq [1 :> obj;2:> obj;3:> obj]

    testList "Extension Tests" [

      testCase "Applying Option.Zip and Option.Unzip returns the original value" 
        (fun () -> let (fst, lst) = Option.unzip (Some (1, 2))
                   equal (Some (1, 2)) (Option.zip fst lst))

      testCase "List.Cons of an item and empty list is equivalent to List.Singleton" 
        (fun () -> let m1 = List.cons 1 List.empty
                   let m2 = List.singleton 1
                   equal m1 m2)

      testCase "Map.union gives same map when joined with an empty map (identity)" 
        (fun () -> let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
                   let r1 = Map.union m1 Map.empty |> Map.toList
                   equalSeq [1, "2"; 2,"4"; 4,"8"] r1)

      testCase "Map.union returns same results independent of the order (associative)" 
        (fun () -> let m1 = [1, "1"; 2,"2"; 3,"3"] |> Map.ofList
                   let m2 = [3, "3"; 4,"4"; 5,"6"] |> Map.ofList
                   let r1 = m1 |> Map.union m2
                   let r2 = m2 |> Map.union m1
                   equalMap r1 r2)

      testCase "Map.union provides same end result as Map.unionWith picking the first source value for dupes" 
        (fun () -> let m1 = [1, "2"; 2,"4"; 4,"8"] |> Map.ofList
                   let m2 = [1, "4"; 2,"8"; 4,"16"] |> Map.ofList
                   let r1 = m1 |> Map.union m2
                   let r2 = m1 |> Map.unionWith konst m2
                   equalMap r1 r2)


      testCase "Bind" 
        (fun () ->  let x = [1;2] >>= fun x -> [string x ; string (x + 1000) ]
                    let y = { Head = 1; Tail = [2] } >>= fun x -> { Head = string x ; Tail = [string (x + 1000)] }
                    let z = ("a", 1) >>= fun x -> (string x, x + 10)
                    equal ["1"; "1001"; "2"; "1002"] x
                    equal { Head = "1"; Tail = ["1001"; "2"; "1002"] } y
                    equal ("a1", 11) z)

      testCase "Comonad" 
        (fun () ->  let x = [1;2;3;4;5]
                    let y = { Head = 1 ; Tail = [2;3;4;5] }
                    equal (List.head x) 1
                    equal (y.Head) 1
                    equal (duplicate x) [[1; 2; 3; 4; 5]; [2; 3; 4; 5]; [3; 4; 5]; [4; 5]; [5]]
                    equal (duplicate y) { Head = { Head = 1; Tail = [2; 3; 4; 5] }; Tail = [{ Head = 2; Tail = [3; 4; 5] }; { Head = 3; Tail = [4; 5] }; { Head = 4; Tail = [5] }; { Head = 5; Tail = [] }] }
                    equal (extend List.head x) x
                    equal (extend (fun x -> x.Head) y) y)

      testCase "Invariant"
        (fun () ->  let tryParse x =
                        match System.Double.TryParse (x: string) with
                        | (true, x) -> Some x
                        | (false, _) -> None
        
                    let floatCodec = StringCodec ( (tryParse >> Option.toResultWith "Parse error"), string<float>)
                    let floatParsed  = StringCodec.decode floatCodec "1.8"
                    let floatEncoded = StringCodec.encode floatCodec 1.5
                    equal floatParsed (Result<float, string>.Ok 1.8)
                    equal floatEncoded "1.5" 
        
                    let intCodec = invmap int<float> float<int> floatCodec
                    let oneParsed  = StringCodec.decode intCodec "1"
                    let tenEncoded = StringCodec.encode intCodec 10
                    equal oneParsed (Result<int, string>.Ok 1)
                    equal tenEncoded "10" )

      testCase "Tuple"
        (fun () ->
                   equal (mapItem2 string (1,2,3)) (1,"2",3)
                   equal (item3 (1,2,3)) 3
                   )

      testCase "eq on DList 1" (fun () -> equal true  (dlistA = dlistB))
      testCase "eq on DList 2" (fun () -> equal false (dlistA = dlistC))
      testCase "eq on DList 3" (fun () -> equal true  ((dlistA :> obj) = (dlistB :> obj)))
      testCase "eq on DList 4" (fun () -> equal false ((dlistA :> obj) = (dlistC :> obj)))
      testCase "eq on DList 5" (fun () -> equal true  ((dlistA :> obj) = (dlistD :> obj))) // this behavior differs from (non-fable) F# but same way it would be with normal lists.

]
