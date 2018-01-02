#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module Samples.Haskell
#endif
#nowarn "3186"
#nowarn "62"
open FSharpPlus
open FSharpPlus.Compatibility.Haskell
open FSharpPlus.Data



// Operators

let res9 = ((+) 5) $ 4
let res10 = filter ((=/) 9) ([9] ++ [10])
let halfThenSqrt = sqrt . (flip (/) 2.0) 


// IO

let main = print (halfThenSqrt 32.0)
runIO main                                                  // prints 4.0
let get3strings  = sequenceA [getLine;getLine;getLine]      // try runIO get3strings


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