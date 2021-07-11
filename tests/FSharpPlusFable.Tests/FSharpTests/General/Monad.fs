module General.Monad
open Testing
open General.Util
open FSharpPlus
open FSharpPlus.Data
#nowarn "686"

let monad = testList "Monad" [
    #if !FABLE_COMPILER || FABLE_COMPILER_3
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

    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "workFlow" (fun () ->       
        let testVal = 
            monad {
                let! x1 = WrappedListD [1;2]
                let! x2 = WrappedListD [10;20]
                return ((+) x1 x2) }
        #if !FABLE_COMPILER
        Assert.IsInstanceOf<WrappedListD<int>> (testVal)
        #endif
        () )
    #endif
    
    // Exception: TypeError: Cannot read property '0' of undefined 
    #if !FABLE_COMPILER
    testCase "return Const First using invoke on instance" (fun () ->
        let cf : Const<First<int>,int> = Control.Return.InvokeOnInstance 1
        equal None (cf |> Const.run |> First.run)
    )
    #endif
    
    #if !FABLE_COMPILER || FABLE_COMPILER_3
    testCase "return Const First using explicit method" (fun () ->
        let cf : Const<First<int>,int> = Const.Return<_,_> 1
        equal None (cf |> Const.run |> First.run)
    )
    testCase "return Const" (fun () ->
        let c : Const<int,int> = Control.Return.InvokeOnInstance 1 in equal 0 (Const.run c)
    )
    
    testCase "specialized maybe monad" (fun () ->
        let option<'t> = monad<Option<'t>>
        let v = option {
            let! x = Some 10
            let! y = Some 15
            return x + y
        }
        
        equal (Some 25) v
    )    
    
    #endif

    //  Exception: RangeError: Maximum call stack size exceeded
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
