(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"

(**
Monad
=====

Defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of an F# programmer, however, it is best to think of a monad as an abstract datatype of actions. F#+ generic computation expressions provide a convenient syntax for writing monadic expressions.

___



Minimal complete definition
---------------------------


 * ``return x``/``result x``
 * ``(>>=) x f``
*)
(**
    static member Return (x: 'T) : 'Monad<'T>
    static member (>>=) (x: Monad<'T>, f: 'T->Monad<'U>) : Monad<'U>
*)
(**

Note: ``return`` can't be used outside computation expressions, use ``result`` instead.

Other operations
----------------

 * ``join``
*)
(**
    static member Join (x:'Monad<'Monad<'T>>) :'Monad<'T>
*)
(**



Rules
-----
*)
(**
    return a >>= k = k a
    m >>= return = m
    m >>= (fun x -> k x >>= h) = (m >>= k) >>= h
*)
(**


Related Abstractions
--------------------

 - [Functor](abstraction-functor.html): Monads are automatically functors.
 
 - [Applicative](abstraction-applicative.html) : Monads are automatically applicatives.


Concrete implementations
------------------------

From F#
 
 -  ``seq<'T>``
 -  ``list<'T>``
 -  ``array<'T>``
 -  ``option<'T>`` 
 -  ``voption<'T>`` 
 -  ``Lazy<'T>``
 -  ``Async<'T>``
 -  ``Result<'T,'U>`` 
 -  ``Choice<'T,'U>``
 -  ``'Monoid * 'T``
 -  ``struct ('Monoid * 'T)``
 -  ``Task<'T>``
 -  ``ValueTask<'T>``
 -  ``'R->'T``
 -  ``ResizeArray<'T>``

 
From F#+

 -  [``Identity<'T>``](type-identity.html)
 -  [``Cont<'R,'T>``](type-cont.html)
 -  [``ContT<'R,'T>``](type-contt.html)
 -  [``Reader<'R,'T>``](type-reader.html)
 -  [``ReaderT<'R,'Monad<'T>>``](type-readert.html)
 -  [``Writer<'Monoid,'T>``](type-writer.html)
 -  [``WriterT<'Monad<'T * 'Monoid>>``](type-writert.html)
 -  [``State<'S,'T * 'S>``](type-state.html)
 -  [``StateT<'S,'Monad<'T * 'S>>``](type-statet.html)
 -  [``OptionT<'Monad<option<'T>>``](type-optiont.html)
 -  [``ValueOptionT<'Monad<voption<'T>>``](type-valueoptiont.html)
 -  [``SeqT<'Monad<seq<'T>>``](type-seqt.html)
 -  [``ListT<'Monad<list<'T>>``](type-listt.html)
 -  [``ResultT<'Monad<Result<'T,'TError>>``](type-resultt.html)
 -  [``ChoiceT<'Monad<Choice<'T,'TError>>``](type-choicet.html)
 -  [``Free<'Functor<'T>,'T>``](type-free.html)
 -  [``NonEmptyList<'T>``](type-nonempty.html)
 -  [``DList<'T>``](type-dlist.html)
 
 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation


Examples
--------

*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)

open FSharpPlus
open FSharpPlus.Data


// Monads allow us to use our generic computation expressions

// This will return the list [11;21;12;22] which is both lists combined in different ways with the (+) operation
let lst11n21n12n22 =
    monad {
        let! x1 = [1;   2]
        let! x2 = [10; 20]
        return ((+) x1 x2) }

// This is the same example but with a non-empty list
let neLst11n21n12n22 = 
    monad {
        let! x1 = { NonEmptyList.Head =  1; Tail =  [2] }
        let! x2 = { NonEmptyList.Head = 10; Tail = [20] }
        return ((+) x1 x2)}

// And now an example with options
let some14 =
    monad {
        let! x1 = Some 4
        let! x2 = tryParse "10"
        return ((+) x1 x2) }



// MONAD TRANSFORMERS
// ==================
//
// Monads do not compose directly, we need to use Monad Transformers

(**
```f#
let fn : ResultT<Reader<int,Result<_,string>>> = 
    monad {
       let! x1 = lift ask
       let! x2 = 
           if x1 > 0 then result 1
           else ResultT (result (Error "Negative value"))
       return x1 + x2
    }

let x = (fn |> ResultT.run |> Reader.run) 10
// Result<int,string> = Ok 11
let y = (fn |> ResultT.run |> Reader.run) -1
// Result<int,string> = Error "Negative value"
```
*)


// The following example comes from Haskell
// async is used instead of IO

open System

// First let's define some functions we'll use later
let getLine    = async { return Console.ReadLine () }
let putStrLn x = async { printfn "%s" x }
let isValid s =
    String.length s >= 8
        && String.exists Char.IsLetter s
        && String.exists Char.IsNumber s
        && String.exists Char.IsPunctuation s

let decodeError = function
    | -1 -> "Password not valid"
    | _  -> "Unknown"


// Now the following functions compose the Error monad with the Async one.

let getValidPassword : ResultT<_> =
    monad {
        let! s = liftAsync getLine
        if isValid s then return s
        else return! throw -1}
    </catch/>
        (fun s -> throw ("The error was: " + decodeError s))

let askPassword = monad {
    do! lift <| putStrLn "Insert your new password:"
    let! value = getValidPassword
    //do! lift <| putStrLn "Storing in database..."
    return value}

//try -> Async.RunSynchronously (ResultT.run askPassword)


// After getting used to monadic CEs it's natural
// to feel the need to combine monads
// (from https://stackoverflow.com/a/37900264 )

module CombineWriterWithResult =
    
    let divide5By = function
        | 0.0 -> Error "Divide by zero"
        | x   -> Ok (5.0 / x)

    let eitherConv logSuccessF logFailF f v =
        ResultT <|
            match f v with
            | Ok a -> Writer(Ok a, ["Success: " + logSuccessF a])
            | Error b -> Writer(Error b, ["ERROR: "   + logFailF b])

    let ew = monad {
        let! x = eitherConv (sprintf "%f") (sprintf "%s") divide5By 6.0
        let! y = eitherConv (sprintf "%f") (sprintf "%s") divide5By 3.0
        let! z = eitherConv (sprintf "%f") (sprintf "%s") divide5By 0.0
        return (x, y, z) }

    let (_, log) = ew |> ResultT.run |> Writer.run


// You can also stack monad transformers.

// A monad transformer and a monad is itself a monad, so you can pass that into another monad transformer.
// For example, below we are stacking them like:
// type Example = ReaderT<DateTime, ResultT<Writer<string list, Result<string * string * string, string>>>>)

// Catch and throw is generic over all monad transformers in F#+ so catch works in this example
// because there is a Result in the stack. We use it here to consolidate Result's 'TError.

module CombineReaderWithWriterWithResult =

    let divide5By : float -> Result<float, string> = function
        | 0.0 -> Error "Divide by zero"
        | x   -> Ok (5.0 / x)

    let otherDivide5By : float -> Result<float, unit>  = function
        | 0.0 -> Error ()
        | x   -> Ok (5.0 / x)

    let eitherConv f v =
        ReaderT <| fun (now : System.DateTime) ->
        ResultT <|
            match f v with
            | Ok a    -> Writer(Ok a,    [sprintf "Success at %s: %A" (now.ToString "o") a])
            | Error b -> Writer(Error b, [sprintf "ERROR at %s: %A"   (now.ToString "o") b])

    let divide = monad {
        let! w = eitherConv divide5By       6.0
        let! x = eitherConv divide5By       3.0
        let! y = eitherConv divide5By       0.0
        let! z = eitherConv otherDivide5By  0.0 </catch/> (throw << (fun _ -> "Unknown error"))

        return (w, x, y, z) }

    let run expr = ReaderT.run expr >> ResultT.run >> Writer.run

    let (_, log) = run divide DateTime.UtcNow


// Many popular F# libraries are in fact an instantiation of a specific monad combination.
// The following example demonstrate how to code a mini-Suave lib in a few lines

module Suave =
    // setup something that reminds us of what Suave can work with
    // this is an overly simplified model of Suave in order to show how OptionT can be used 
    // in conjunction with generic Kleisli composition (fish) operator
    type WebPart<'a> = 'a -> OptionT<Async<'a option>>
    let inline succeed x = async.Return (Some x)

    module WebPart =
        /// Comment from <a href="https://github.com/SuaveIO/suave/blob/v2.4.3/src/Suave/WebPart.fsi#L39-L42">WebPart.fsi</a>
        /// Entry-point for composing the applicative routes of the http application,
        /// by iterating the options, applying the context, arg, to the predicate
        /// from the list of options, until there's a match/a Some(x) which can be
        /// run.
        let choose (options: WebPart<'a> list) = fun x -> choice (List.map ((|>) x) options)

    module Http =
        type HttpResponse = { status: int; content: string }
        type HttpRequest  = { url: Uri; ``method``: string }
        type HttpContext  = { request: HttpRequest; response: HttpResponse }

    module Successful =
        open Http
        let private withStatusCode statusCode s =
            OptionT << fun ctx -> { ctx with response = { ctx.response with status = statusCode; content = s }} |> succeed
        let OK s = withStatusCode 200 s
        let BAD_REQUEST s = withStatusCode 400 s

    module Filters =
        open Http
        let ``method`` (m: string) =
            OptionT << fun (x: HttpContext) -> async.Return (if (m = x.request.``method``) then Some x else None)
        let GET  (x : HttpContext) = ``method`` "GET" x
        let POST (x : HttpContext) = ``method`` "POST" x
  
        let path s =
            OptionT << fun (x: HttpContext) -> async.Return (if (s = x.request.url.AbsolutePath) then Some x else None)

    // Stub implementations: here you can plug Fleece or another similar Json library
    let toJson o : string  = failwith "Not implemented"
    let ofJson (s: string) = failwith "Not implemented"

    module Request =
        let tryGet _s (_r: Http.HttpRequest) = Ok "FORM VALUE"

    let authenticated (f: Http.HttpContext -> int -> OptionT<Async<'a option>>) =
        // we assume that authenticated executes f only if auth, otherwise returns 401
        // we fake it as:
        fun (ctx: Http.HttpContext) -> f ctx -1

    // Usage:
    open Successful
    open Filters
    type Note = { id: int; text: string }
    type NoteList = { notes: Note list; offset: int; chunk: int; total: int }
    type IDb =
        abstract member getUserNotes: int -> Async<NoteList>
        abstract member addUserNote: int -> string -> Async<Note>
    type OverviewViewModel = { myNotes: Note list }
    let app (db: IDb) =
        let overview =
            GET >=> (authenticated <| fun ctx userId ->
                monad {
                  let! res = lift (db.getUserNotes userId)
                  let ovm = toJson { myNotes = res.notes }
                  return! OK ovm ctx
                })
        let register =
            POST >=> (authenticated <| fun ctx userId ->
                monad {
                  match ctx.request |> Request.tryGet "text" with 
                  | Ok text ->
                      let! newNote = lift (db.addUserNote userId text)
                      let rvm = toJson newNote
                      return! OK rvm ctx
                  | Error msg -> 
                      return! BAD_REQUEST msg ctx
                })
        WebPart.choose [ path "/" >=> (OK "/")
                         path "/note" >=> register
                         path "/notes" >=> overview ]