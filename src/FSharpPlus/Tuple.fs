namespace FsControl

open System
open System.Text
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Internals


[<Extension;Sealed>]
type Item1 =
    static member inline       Item1 (t :'a          ) = ((^a) : (member Item1: _ ) t)
    [<Extension>]static member Item1 ((a, _, _, _, _)) = a
    [<Extension>]static member Item1 ((a, _, _, _)   ) = a
    [<Extension>]static member Item1 ((a, _, _)      ) = a
    [<Extension>]static member Item1 ((a, _)         ) = a

    static member inline Invoke value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member Item1: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item1> , value)

[<Extension;Sealed>]
type Item2 =
    static member inline       Item2 (t :'a          ) = ((^a) : (member Item2: _ ) t)
    [<Extension>]static member Item2 ((_, b, _, _, _)) = b
    [<Extension>]static member Item2 ((_, b, _, _)   ) = b
    [<Extension>]static member Item2 ((_, b, _)      ) = b
    [<Extension>]static member Item2 ((_, b)         ) = b

    static member inline Invoke value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member Item2: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item2> , value)

[<Extension;Sealed>]
type Item3 =
    static member inline       Item3 (t :'a          ) = ((^a) : (member Item3: _ ) t)
    [<Extension>]static member Item3 ((_, _, c, _, _)) = c
    [<Extension>]static member Item3 ((_, _, c, _)   ) = c
    [<Extension>]static member Item3 ((_, _, c)      ) = c

    static member inline Invoke value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member Item3: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item3> , value)

[<Extension;Sealed>]
type Item4 =
    static member inline       Item4 (t :'a          ) = ((^a) : (member Item4: _ ) t)
    [<Extension>]static member Item4 ((_, _, _, d, _)) = d
    [<Extension>]static member Item4 ((_, _, _, d)   ) = d

    static member inline Invoke value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member Item4: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item4> , value)

[<Extension;Sealed>]
type Item5 =
    static member inline       Item5 (t :'a          ) = ((^a) : (member Item5: _ ) t)
    [<Extension>]static member Item5 ((_, _, _, _, e)) = e

    static member inline Invoke value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member Item5: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item5> , value)


type MapItem1 =   
    static member MapItem1 ((a, b, c, d, e)         , fn) = (fn a, b, c, d, e)
    static member MapItem1 ((a, b, c, d)            , fn) = (fn a, b, c, d)      
    static member MapItem1 ((a, b, c)               , fn) = (fn a, b, c)         
    static member MapItem1 ((a, b)                  , fn) = (fn a, b)

    static member inline Invoke f value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member MapItem1: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem1> , value)

[<Extension;Sealed>]
type MapItem2 =
    static member MapItem2 ((a, b, c, d, e)         , fn) = (a, fn b, c, d, e)
    static member MapItem2 ((a, b, c, d)            , fn) = (a, fn b, c, d)      
    static member MapItem2 ((a, b, c)               , fn) = (a, fn b, c)         
    static member MapItem2 ((a, b)                  , fn) = (a, fn b)            

    static member inline Invoke f value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member MapItem2: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem2> , value)

type MapItem3 =
    static member MapItem3 ((a, b, c, d, e)         , fn) = (a, b, fn c, d, e)
    static member MapItem3 ((a, b, c, d)            , fn) = (a, b, fn c, d)      
    static member MapItem3 ((a, b, c)               , fn) = (a, b, fn c)

    static member inline Invoke f value = 
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member MapItem3: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem3> , value)

type MapItem4 =  
    static member MapItem4 ((a, b, c, d, e), fn) = (a, b, c, fn d, e)
    static member MapItem4 ((a, b, c, d)   , fn) = (a, b, c, fn d)

    static member inline Invoke f value =
        let inline call_2 (_:^a, b:^b) = ((^a or ^b) : (static member MapItem4: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem4> , value)

type MapItem5 =
    static member MapItem5 ((a, b, c, d, e), fn) = (a, b, c, d, fn e)