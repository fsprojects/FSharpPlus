namespace FSharpPlus.Tests

#nowarn "40" "49"

open System
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Tests.Helpers
open NUnit.Framework

module Free =
    
    // primitive functor types
    let aFreeOfListInt = Roll [Roll [Roll [Pure 2]]]
    let mFreeOfListString = map string aFreeOfListInt
    let aFreeOfListFloat = aFreeOfListInt >>= (fun x -> let a = Roll [ Pure (string x) ] in a) >>= (fun x -> Roll [ Pure (float (x+".5")) ])
    
    // user defined functor types
    let aFreeOfIdentityInt = Roll (Identity (Pure 1))
    let aFreeOfIdentityString = map string aFreeOfIdentityInt
    let aFreeOfIdentityFloat = aFreeOfIdentityInt >>= (fun x -> Roll (Identity (Pure (float x))))

    // Structural Equality
    areStEqual aFreeOfListInt (Roll [Roll [Roll [Pure 2]]])


module Sample1 =

    // Free monad-interpreter in F# from http://www.fssnip.net/7SX/title/Freemonad-interpreter
    // (based on: http://programmers.stackexchange.com/a/242803/145941)

    type DSL<'next> =
        | Get of key: string *       (string -> 'next)
        | Set of key: string * value: string *  'next
    with
        static member Map (x: DSL<'a>, f: 'a -> 'b) =
            match x with
            | Get (k,    c) -> Get (k,      c >> f)
            | Set (k, v, c) -> Set (k, v, f c     )

    type FreeDSL<'a> = Free<DSL<'a>,'a>

    let ex1  = Set ("alma", "bela", (Get ("alma", id)))
    let exF1 = Roll (Set ("alma", "bela", (Roll (Get ("alma", (fun s -> Pure s))))))

    let get key       = Free.liftF (Get (key, id))
    let set key value = Free.liftF (Set (key, value, ()))

    let exF2 = set "foo" "bar" >>= fun _ -> get "foo"

    let exF3 = monad {
        let! value  = get "foo"
        do! set "bar" value
        get "bar" |> ignore
    }


    let rec interpreter: ('a -> unit) -> FreeDSL<'a> -> unit =
        fun              receiver        free        ->
            match Free.run free with

            | Roll (Get(key,        nextF)) -> printfn "Get %s" key
                                               nextF (sprintf "'get.%s'" key) |> interpreter receiver
            | Roll (Set(key, value, next )) -> printfn "Set %s = %s" key value
                                               next                           |> interpreter receiver
            | Pure v                        -> printfn "return(%A)" v
                                               receiver v


    interpreter (printfn "Received: %A") exF1
    interpreter (printfn "Received: %A") exF2
    interpreter (printfn "Received: %A") exF3


module Sample2 =

    // Free monad-interpreter in F# from https://blog.ploeh.dk/2017/07/17/a-pure-command-line-wizard/

    type CommandLineInstruction<'t> =
        | ReadLine  of (string -> 't)
        | WriteLine of  string  * 't
    with static member Map (x, f) =
            match x with
            | ReadLine   g     -> ReadLine  (f << g)
            | WriteLine (s, g) -> WriteLine (s, f g)

    let readLine    = Free.liftF (ReadLine id)
    let writeLine s = Free.liftF (WriteLine (s, ()))


    let rec interpretCommandLine = Free.run >> function
        | Pure x -> x
        | Roll (ReadLine      next)  -> Console.ReadLine () |> next |> interpretCommandLine
        | Roll (WriteLine (s, next)) ->
            Console.WriteLine s
            next |> interpretCommandLine

    let rec readQuantity = monad {
        do! writeLine "Please enter number of diners:"
        let! l = readLine
        match tryParse l with
        | Some dinerCount -> return dinerCount
        | None ->
            do! writeLine "Not an integer."
            return! readQuantity }

    let rec readDate = monad {
        do! writeLine "Please enter your desired date:"
        let! l = readLine
        match DateTimeOffset.TryParse l with
        | true, dt -> return dt
        | _ ->
            do! writeLine "Not a date."
            return! readDate }

    let readName = monad {
        do! writeLine "Please enter your name:"
        return! readLine }
 
    let readEmail = monad {
        do! writeLine "Please enter your email address:"
        return! readLine }


    type Reservation = {
        Date : DateTimeOffset
        Name : string
        Email : string
        Quantity : int }
        with static member Create (Quantity, Date, Name, Email) = { Date = Date; Name = Name; Email = Email; Quantity = Quantity }

    let readReservationRequest =
        curryN Reservation.Create
        <!> readQuantity
        <*> readDate
        <*> readName
        <*> readEmail



    let mainFunc () =
        readReservationRequest
        >>= (writeLine << (sprintf "%A"))
        |> interpretCommandLine
        0


module Sample3 =

    // Combining free monads in Haskell from https://blog.ploeh.dk/2017/07/24/combining-free-monads-in-haskell/

    type Slot = { Date : DateTimeOffset; SeatsLeft : int }

    type Reservation = {
        Date : DateTimeOffset
        Name : string
        Email : string
        Quantity : int }
        with static member Create count date name email = { Date = date; Name = name; Email = email; Quantity = count }

    type CommandLineInstruction<'t> =
        | ReadLine of (string -> 't)
        | WriteLine of string * 't
        with static member Map (x, f) =
                match x with
                | ReadLine g  -> ReadLine (f << g)
                | WriteLine (s, g) -> WriteLine (s, f g)

    type ReservationsApiInstruction<'a> =
        | GetSlots of (DateTimeOffset * (Slot list -> 'a))
        | PostReservation of Reservation * 'a
        with static member Map (x, f) = x |> function
                | GetSlots (x, next) -> GetSlots (x, next >> f)
                | PostReservation (x, next) -> PostReservation (x, next |> f)

    type Program<'t> = Free<Coproduct<CommandLineInstruction<'t>, ReservationsApiInstruction<'t>>,'t>


    let readLine = (Free.liftF << InL) (ReadLine id) : Program<_>
    let writeLine s = (Free.liftF << InL) (WriteLine (s, ())) : Program<_>

    let rec readQuantity = monad {
        do! writeLine "Please enter number of diners:"
        let! l = readLine
        match Int32.TryParse l with
        | true, dinerCount -> return dinerCount
        | _ ->
            do! writeLine "Not an integer."
            return! readQuantity }

    let rec readDate = monad {
        do! writeLine "Please enter your desired date:"
        let! l = readLine
        match DateTimeOffset.TryParse l with
        | true, dt -> return dt
        | _ ->
            do! writeLine "Not a date."
            return! readDate }

    let readName = monad {
        do! writeLine "Please enter your name:"
        return! readLine }
 
    let readEmail = monad {
        do! writeLine "Please enter your email address:"
        return! readLine }

    let inline getSlots d = (Free.liftF << InR) (GetSlots (d, id)) : Program<_>
 
    let inline postReservation r = (Free.liftF << InR) (PostReservation (r, ())) : Program<_>

    let tryReserve : Program<_> = monad {
        let! count = readQuantity
        let! date  = readDate
        let! availableSeats = getSlots date >>= (List.sumBy (fun slot -> slot.SeatsLeft) >> result)
        if availableSeats < count
        then 
            do! sprintf "Only %i remaining seats." availableSeats |> writeLine
        else
            let! name  =  readName
            let! email =  readEmail
            do! postReservation { Date = date; Name = name; Email = email; Quantity = count }
        }

    /// Mock HttpClient
    module ReservationHttpClient =
        let getSlots (d: DateTimeOffset)     = async { return [{ Date = d; SeatsLeft = 10 }]}
        let postReservation (r: Reservation) = async { return printfn "Posting reservation %A" r}

    let interpretCommandLine = function
        | ReadLine next          -> async { return Console.ReadLine ()} >>= next
        | WriteLine (line, next) -> async { return Console.WriteLine line } >>= fun _ -> next

    let interpretReservationsApi = function
        | GetSlots (zt, next)       -> ReservationHttpClient.getSlots zt >>= next
        | PostReservation (r, next) -> ReservationHttpClient.postReservation r >>= fun _ -> next

    let rec interpret (program: Program<_>) = 
        let go = function
            | InL cmd -> interpretCommandLine cmd
            | InR res -> interpretReservationsApi res
        Free.iterM go program

    let interpretedProgram = interpret tryReserve


module TestCoproduct =

    let a11 = if true  then InL (Some 1) else InR ([20])
    let a12 = if false then InL (Some 1) else InR ([20])

    let a14 = map ((+)10) a11
    let a15 = map ((+)10) a12

    let a16 = map string a11
    let a17 = map string a12

    let a31 = if true  then InL (Some 1) else InR (ZipList [20])
    let a32 = if false then InL (Some 1) else InR (ZipList [20])

    let a34 = map ((+)10) a31
    let a35 = map ((+)10) a32

    let a36 = map string a31
    let a37 = map string a32

    let a41 = InL [3] : Coproduct<_,_ list>
    let a42 = map ((+)10 >> string) a41

    open Sample3

    let readReservationRequest =
        Reservation.Create
        <!> readQuantity
        <*> readDate
        <*> readName
        <*> readEmail

module Fold =
    type FooId = FooId of string

    type Foo = { Id: FooId; Name: string }

    // An example of an operation in the domain implemented as a Free monad that reads a foo
    module GetFoo =
        type Error = NotFound

        type Instruction<'next> =
            | Read of (FooId * (Foo option -> 'next))
            static member Map(instruction, f: 'a -> 'b) =
                match instruction with
                | Read (id, next) -> Read(id, next >> f)

        type Program<'a> = Free<Instruction<'a>, 'a>

        let read fooId = Read(fooId, id) |> Free.liftF

        type Request = { Id: FooId }
        type Response = Result<Foo, Error>

        let handle request: Program<Response> =
            monad {
                let! foo = read request.Id
                return foo |> Option.toResultWith NotFound
            }

    [<Test>]
    let ``should interpret program with fold`` () =
        let request: GetFoo.Request = { Id = FooId "1" }
        let response = 
            request
            |> GetFoo.handle 
            |> Free.fold 
                (function
                | GetFoo.Read (fooId, next) -> { Id = fooId; Name = "test" } |> Some |> next |> result) 
            |> Identity.run

        areStEqual (Ok { Id = FooId "1"; Name = "test" }) response

module Lift3 =

    type Instruction<'next> =
        | Read of int * (string -> 'next)
        static member Map(i, f) =
            match i with
            | Read (x, next) -> Read(x, next >> f)

    let read x = Read(x, id) |> Free.liftF

    type ApplicativeBuilder<'a>() =
        inherit MonadFxStrictBuilder<'a>()

        member inline _.BindReturn(x, f) = map f x

    let applicative<'a> = ApplicativeBuilder<'a>()

    [<Test>]
    let ``should be able to use applicative CE which requires Lift3`` () =
        let program = 
            applicative {
                let! a = read 1
                and! b = read 2
                and! c = read 3
                return a, b, c
            }

        let result = 
            program 
            |> Free.fold 
                (function 
                | Read (i, next) -> i |> string |> next |> result) 
            |> Identity.run

        areStEqual result ("1", "2", "3")
