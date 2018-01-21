(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Haskell Compatibility
=====================

 - This module helps to run Haskell fragments or to port code from/to Haskell.

 - By opening this module many F# and F#+ operators get shadowed.


Here's a table listing some common operators/functions:


```
+-----------------------+--------------------+-----------------------+--------------------+
| Operation             | F-Sharp (Plus)     | Haskell-Compatibility | Haskell            |
+=======================+====================+=======================+====================+
| List.append           | @                  |                       | ++                 |
+-----------------------+--------------------+-----------------------+--------------------+
| Function composition  | f << g             | f . (g)               | f . g              |
+-----------------------+--------------------+-----------------------+--------------------+
| Equality              | =                  | ==                    | ==                 |
+-----------------------+--------------------+-----------------------+--------------------+
| Avoid Parentheses     | <|                 | $                     | $                  |
+-----------------------+--------------------+-----------------------+--------------------+
| Inequality            | <>                 | =/                    | /=                 |
+-----------------------+--------------------+-----------------------+--------------------+
| Flip                  | />                 |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Apply function        | </ or |>           | not available         | not available      | 
| to a value            |                    | in Haskell            | in Haskell         |
+-----------------------+--------------------+-----------------------+--------------------+
|                       |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Functors                                                                                |
+-----------------------+--------------------+-----------------------+--------------------+
| Map                   | <!> or <<|         | <!> or fmap           | <$> or fmap        |
+-----------------------+--------------------+-----------------------+--------------------+
| Map with              |                    |                       |                    |
| interchanged          | |>>                |                       |                    |
| arguments             |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
|                       |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Monoids                                                                                 |
+-----------------------+--------------------+-----------------------+--------------------+
| Zero element          | zero               | mempty                | mempty             |
+-----------------------+--------------------+-----------------------+--------------------+
| Append                | ++ or plus         | mappend               | mappend            |
+-----------------------+--------------------+-----------------------+--------------------+
|                       |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Applicative functors                                                                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Apply (combine)       | <*>                | <*>                   | <*>                |
+-----------------------+--------------------+-----------------------+--------------------+
| Sequence actions      |                    |                       |                    |
| and discard values    | *>                 | *>                    | *>                 |
| of first argument     |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Sequence actions      |                    |                       |                    |
| and discard values    | <*                 | <*                    | <*                 |
| of second argument    |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| A variant of <*>      |                    |                       |                    |
| with the arguments    | <**>               | <**>                  | <**>               |
| reversed.             |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Lift a value.         | result             | pure'                 | pure               |
+-----------------------+--------------------+-----------------------+--------------------+
|                       |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Alternatives                                                                            |
+-----------------------+--------------------+-----------------------+--------------------+
| Binary operation      | <|>                | <|>                   | <|>                |
+-----------------------+--------------------+-----------------------+--------------------+
|                       |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Monads                                                                                  |
+-----------------------+--------------------+-----------------------+--------------------+
| Bind sequentially and |                    |                       |                    |
| compose two actions,  |                    |                       |                    |
| passing any value     | >>=                | >>=                   | >>=                |
| produced by the       |                    |                       |                    |
| first as an argument  |                    |                       |                    |
| to the second.        |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
| Bind with             |                    |                       |                    |
| interchanged          | =<<                | =<<                   | =<<                |
| arguments.            |                    |                       |                    |
+-----------------------+--------------------+-----------------------+--------------------+
```


Examples
========

You may run this script step-by-step.



*)

#r @"../../src/FSharpPlus/bin/Release/net45/FSharpPlus.dll"
open FSharpPlus
open FSharpPlus.Compatibility.Haskell
open FSharpPlus.Data

#nowarn "62"





// Operators

let res9 = ((+) 5) $ 4
let res10 = filter ((=/) 9) ([9] ++ [10])
let halfThenSqrt = sqrt . (flip (/) 2.0) 


// IO

let main = print (halfThenSqrt 32.0)
runIO main                                                  // prints 4.0
let get3strings  = sequenceA [getLine;getLine;getLine]      // try runIO get3strings


// Test IO
let action = do' {
    do! putStrLn  "What is your first name?"
    let! fn = getLine
    do! putStrLn  ("Thanks, " + fn) 
    do! putStrLn  ("What is your last name?")
    let! ln = getLine
    let  fullname = fn + " " + ln
    do! putStrLn  ("Your full name is: " + fullname)
    return fullname }
// try -> runIO action ;;



// List

let lstOf20Elements = map ((+) 100) (take 20 (cycle [1;2;3]))


// Applicatives

let just3 :Maybe<_> = return' 3
let just4           = Just 4
let just7 = Just (+) <*> just3 <*> just4
let lst1625 =  (+) <!> [11;20] <*> [5]


// Monoids

let res9823 = mconcat (fmap Dual [mempty();"3";"2";"8";"9"])    // Dual "9823"
let resLtDualGt= mappend  (LT, Dual GT) (mempty())              // (LT, Dual GT)


// Monad

// F#                           // Haskell
let result = 
    do' {                       // do {
        let! x1 = [ 1;  2]      //   x1 <- [ 1;  2]
        let! x2 = [10; 20]      //   x2 <- [10; 20]
        return ((+) x1 x2) }    //   return ((+) x1 x2) }

// desugared version
let lst11n21n12n22 = [1; 2]  >>= (fun x1 -> [10; 20] >>= (fun x2 ->  return' ((+) x1 x2)))

let just2 = mfilter ((==) 2) (Just 2)

let someListOf42 = replicateM 5 (Some 42)


// Arrow

let res3n6n9 = (arr (fun y -> [y; y * 2 ; y * 3])) 3
let resSome2n4n6:Maybe<_> = runKleisli (arr (fun y -> [y; y * 2 ; y * 3])) 2
let res500n19 = ((*) 100) *** ((+) 9)  $ (5, 10)
let res500n14 = ((*) 100) &&& ((+) 9)  $ 5
let resLeft7  = ((+) 2)   +++ ((*) 10) $ Left  5


// Monad transformers

// from http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
// askString :: (String -> ContT () IO String) -> ContT () IO String
let askString next = do' {
  do! (liftIO $ putStrLn "Please enter a string") 
  let! s = liftIO $ getLine
  return! next s}

// reportResult :: String -> IO ()
let reportResult s = do' {
  return! putStrLn ("You entered: " + s) }
  
let mainAction = runContT (callCC askString) reportResult       //try -> runIO mainAction