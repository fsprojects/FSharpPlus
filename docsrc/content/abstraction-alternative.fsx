(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Alternative
===========
Applicative Functors which also have a monoid structure.
___
Minimal complete definition
---------------------------
 * ``return x``/``result x`` 
 * ``(<*>) f x``
 * ``empty``
 * ``append x y``/``(<|>) x y``
*)
(**
    static member Return (x:'T) : 'Alternative<'T>
    static member (<*>) (f:'T->'U, x:Alternative<'T>) : Alternative<'U>
    static member get_Empty () :'Alternative
    static member (<|>) (x:'Alternative<'T>, y:'Alternative<'T>) :'Alternative<'T>
*)
(**
Note: ``return`` can't be used outside computation expressions, use ``result`` instead.
Other operations
----------------
 * ``mfilter``
*)
(**
    static member MFilter (x:seq<'Alternative>) :'Alternative
*)
(**
 * ``choice``
*)
(**
Rules
-----
*)
(**
    empty <|> x = x
    x <|> empty = x
    (x <|> y) <|> z = x <|> (y <|> z)
    f <!> (x <|> y) = (f <!> x) <|> (f <!> y)
    (f <|> g) <*> x = (f <*> x) <|> (g <*> x)
    empty <*> f = empty
*)
(**
Related Abstractions
--------------------
 - [Monoid](abstraction-monoid.html): An Alternative is a Monoid that is also an Applicative Functor
 - [Applicative](abstraction-applicative.html): An Alternative is a Monoid that is also an Applicative Functor
 - MonadPlus: Alternatives that are also Monads
Concrete implementations
------------------------
From .Net/F#
 
 -  ``list<'T>``
 -  ``option<'T>``
 -  ``array<'T>``
 -  ``seq<'T>``
 
From F#+
 
 -  [``ReaderT<'R, 'MonadPlus<'T>>``](type-readert.html)
 -  [``WriterT<'MonadPlus<'T * 'Monoid>>``](type-writert.html)
 -  [``StateT<'S,'MonadPlus<'T * 'S>>``](type-statet.html)
 -  [``Compose<'AlternativeF<'AlternativeG<'T>>>``](type-compose.html)
 -  [``DList<'T>``](type-dlist.html)
 -  [``ZipList<'S>``](type-ziplist.html)
 
 [Suggest another](https://github.com/fsprojects/FSharpPlus/issues/new) concrete implementation

Examples
--------
*)



#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"

open FSharpPlus

// this gives [2; 3; 4; 5]
let x = [2;3] <|> [] <|> [4;5]

// but I could have written
let y = [2;3] <|> empty <|> [4;5]

// choice sample usage
let alternatives = [None; Some "Result is OK"; None ; Some "Result is still OK"]
let firstGood = choice alternatives //Some "Result is OK"

// it did something like
let fstGood = None <|> Some "Result is OK" <|>  None <|> Some "Result is still OK"

// mfilter usage
let fstMatch = mfilter ((=) 5) [1;2;3;4]    // [] -> no element found, it uses the empty value


// MonadPlus

let getLine    = async { return System.Console.ReadLine() }
let putStrLn x = async { printfn "%s" x}

let nameAndAddress = traverse (fun x -> putStrLn x >>= fun _ -> getLine) ["name";"address"]

let a:list<int> = empty
let res123      = empty <|> [1;2;3]

let inline mfilter p ma = monad.plus {
  let! a = ma
  if p a then return a else return! empty}

let mfilterRes2 = mfilter ((=)2) (Some 2)

// sample code from http://en.wikibooks.org/wiki/Haskell/MonadPlus
let pythags = monad {
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  do! guard (x*x + y*y = z*z)
  return (x, y, z)}

// same operation but using the monad.plus computation expression
let pythags' = monad.plus {
  let! z = [1..50]
  let! x = [1..z]
  let! y = [x..z]
  if (x*x + y*y = z*z) then return (x, y, z) else ()}

let allCombinations = sequence [['a'; 'b'; 'c']; ['1'; '2']]


// An Alternative is automatically a Monoid and a Functor

type Maybe<'t> =
    | Just of 't
    | Nothing 
    with
        static member Return (x:'a)     = Just x
        static member (<*>) (f, x) = 
            match (f, x) with 
            | Just f, Just x -> Just (f x) 
            | _              -> Nothing
        static member inline get_Empty () = Nothing
        static member inline (<|>) (x, y) = match x with Nothing -> y | xs -> xs

let r5 = Nothing ++ Just 5 ++ Just 6 ++ zero
let r6 = map string (Just 6)


// But not always the Monoidal behaviour is the same

let r3 = Some 2 ++ Some 1   // addition         => Some 3
let r2 = Some 2 <|> Some 1  // first success    => Some 2