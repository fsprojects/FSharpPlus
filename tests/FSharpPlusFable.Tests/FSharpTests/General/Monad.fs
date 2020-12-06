module General.Monad
open Testing
open General.Util
open FSharpPlus
open FSharpPlus.Data
#nowarn "686"

let monad = testList "Monad" [
    #if !FABLE_COMPILER
    testCase "joinDefaultCustom" (fun () -> 
        let x = join [[1];[2]]
        equal [1;2] x
        let y: WrappedListE<_> = join (WrappedListE [WrappedListE [1];WrappedListE [2]])
        equal (WrappedListE [1;2]) y
        SideEffects.reset ()
        let z = join (WrappedListF [WrappedListF [1];WrappedListF [2]])
        equal (WrappedListF [1;2]) z
        equal ["Join"] (SideEffects.get ()))
    #endif

    #if !FABLE_COMPILER
    testCase "workFlow" (fun () ->       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        Assert.IsInstanceOf<WrappedListD<int>> (testVal))
    #endif

    #if !FABLE_COMPILER
    testCase "DelayForCont" (fun () -> 
        // If Delay is not properly implemented this will stack-overflow
        // See http://stackoverflow.com/questions/11188779/stackoverflow-in-continuation-monad
        let map f xs =
            let rec loop xs =
                monad {
                    match xs with
                    | [] -> return []
                    | x :: xs ->
                        let! xs = loop xs
                        return f x :: xs }
            Cont.run (loop xs) id
        let _ = [1..100000] |> map ((+) 1)
        ()
    )
    #endif

]
