namespace FsControl

open System
open System.Text
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsControl.Core.Internals
open FsControl.Core.Types


[<Extension;Sealed>]
type Item1 =
    [<Extension>]static member inline Item1 (t :'a                   ) = ((^a) : (member Item1: _ ) t)
    [<Extension>]static member        Item1 ((a, b, c, d, e, f, g, h)) = a
    [<Extension>]static member        Item1 ((a, b, c, d, e, f, g)   ) = a
    [<Extension>]static member        Item1 ((a, b, c, d, e, f)      ) = a
    [<Extension>]static member        Item1 ((a, b, c, d, e)         ) = a
    [<Extension>]static member        Item1 ((a, b, c, d)            ) = a
    [<Extension>]static member        Item1 ((a, b, c)               ) = a
    [<Extension>]static member        Item1 ((a, b)                  ) = a

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item1: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item1> , value)

[<Extension;Sealed>]
type Item2 =
    [<Extension>]static member inline Item2 (t :'a                   ) = ((^a) : (member Item2: _ ) t)
    [<Extension>]static member        Item2 ((a, b, c, d, e, f, g, h)) = b
    [<Extension>]static member        Item2 ((a, b, c, d, e, f, g)   ) = b
    [<Extension>]static member        Item2 ((a, b, c, d, e, f)      ) = b
    [<Extension>]static member        Item2 ((a, b, c, d, e)         ) = b
    [<Extension>]static member        Item2 ((a, b, c, d)            ) = b
    [<Extension>]static member        Item2 ((a, b, c)               ) = b
    [<Extension>]static member        Item2 ((a, b)                  ) = b

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item2: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item2> , value)

[<Extension;Sealed>]
type Item3 =
    [<Extension>]static member inline Item3 (t :'a                   ) = ((^a) : (member Item3: _ ) t)
    [<Extension>]static member        Item3 ((a, b, c, d, e, f, g, h)) = c
    [<Extension>]static member        Item3 ((a, b, c, d, e, f, g)   ) = c
    [<Extension>]static member        Item3 ((a, b, c, d, e, f)      ) = c
    [<Extension>]static member        Item3 ((a, b, c, d, e)         ) = c
    [<Extension>]static member        Item3 ((a, b, c, d)            ) = c
    [<Extension>]static member        Item3 ((a, b, c)               ) = c

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item3: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item3> , value)

[<Extension;Sealed>]
type Item4 =
    [<Extension>]static member inline Item4 (t :'a                   ) = ((^a) : (member Item4: _ ) t)
    [<Extension>]static member        Item4 ((a, b, c, d, e, f, g, h)) = d
    [<Extension>]static member        Item4 ((a, b, c, d, e, f, g)   ) = d
    [<Extension>]static member        Item4 ((a, b, c, d, e, f)      ) = d
    [<Extension>]static member        Item4 ((a, b, c, d, e)         ) = d
    [<Extension>]static member        Item4 ((a, b, c, d)            ) = d

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item4: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item4> , value)

[<Extension;Sealed>]
type Item5 =
    [<Extension>]static member inline Item5 (t :'a                   ) = ((^a) : (member Item5: _ ) t)
    [<Extension>]static member        Item5 ((a, b, c, d, e, f, g, h)) = e
    [<Extension>]static member        Item5 ((a, b, c, d, e, f, g)   ) = e
    [<Extension>]static member        Item5 ((a, b, c, d, e, f)      ) = e
    [<Extension>]static member        Item5 ((a, b, c, d, e)         ) = e

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item5: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item5> , value)

[<Extension;Sealed>]
type Item6 =
    [<Extension>]static member inline Item6 (t :'a                   ) = ((^a) : (member Item6: _ ) t)
    [<Extension>]static member        Item6 ((a, b, c, d, e, f, g, h)) = f
    [<Extension>]static member        Item6 ((a, b, c, d, e, f, g)   ) = f
    [<Extension>]static member        Item6 ((a, b, c, d, e, f)      ) = f

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item6: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item6> , value)

[<Extension;Sealed>]
type Item7 =
    [<Extension>]static member inline Item7 (t :'a                   ) = ((^a) : (member Item7: _ ) t)
    [<Extension>]static member        Item7 ((a, b, c, d, e, f, g, h)) = g
    [<Extension>]static member        Item7 ((a, b, c, d, e, f, g)   ) = g

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item7: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item7> , value)

[<Extension;Sealed>]
type Item8 =
    [<Extension>]static member inline Item8 (t :'a                   ) = ((^a) : (member Item8: _ ) t)
    [<Extension>]static member        Item8 ((a, b, c, d, e, f, g, h)) = h

    static member inline Invoke value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member Item8: _ -> _) b)
        let inline call (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<Item8> , value)

[<Extension;Sealed>]
type MapItem1 =
    [<Extension>]static member MapItem1 ((a, b, c, d, e, f, g, h), fn) = (fn a, b, c, d, e, f, g, h)
    [<Extension>]static member MapItem1 ((a, b, c, d, e, f, g)   , fn) = (fn a, b, c, d, e, f, g)
    [<Extension>]static member MapItem1 ((a, b, c, d, e, f)      , fn) = (fn a, b, c, d, e, f)   
    [<Extension>]static member MapItem1 ((a, b, c, d, e)         , fn) = (fn a, b, c, d, e)      
    [<Extension>]static member MapItem1 ((a, b, c, d)            , fn) = (fn a, b, c, d)         
    [<Extension>]static member MapItem1 ((a, b, c)               , fn) = (fn a, b, c)            
    [<Extension>]static member MapItem1 ((a, b)                  , fn) = (fn a, b)

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem1: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem1> , value)

[<Extension;Sealed>]
type MapItem2 =
    [<Extension>]static member MapItem2 ((a, b, c, d, e, f, g, h), fn) = (a, fn b, c, d, e, f, g, h)
    [<Extension>]static member MapItem2 ((a, b, c, d, e, f, g)   , fn) = (a, fn b, c, d, e, f, g)
    [<Extension>]static member MapItem2 ((a, b, c, d, e, f)      , fn) = (a, fn b, c, d, e, f)   
    [<Extension>]static member MapItem2 ((a, b, c, d, e)         , fn) = (a, fn b, c, d, e)      
    [<Extension>]static member MapItem2 ((a, b, c, d)            , fn) = (a, fn b, c, d)         
    [<Extension>]static member MapItem2 ((a, b, c)               , fn) = (a, fn b, c)            
    [<Extension>]static member MapItem2 ((a, b)                  , fn) = (a, fn b)               

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem2: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem2> , value)

[<Extension;Sealed>]
type MapItem3 =
    [<Extension>]static member MapItem3 ((a, b, c, d, e, f, g, h), fn) = (a, b, fn c, d, e, f, g, h)
    [<Extension>]static member MapItem3 ((a, b, c, d, e, f, g)   , fn) = (a, b, fn c, d, e, f, g)
    [<Extension>]static member MapItem3 ((a, b, c, d, e, f)      , fn) = (a, b, fn c, d, e, f)   
    [<Extension>]static member MapItem3 ((a, b, c, d, e)         , fn) = (a, b, fn c, d, e)      
    [<Extension>]static member MapItem3 ((a, b, c, d)            , fn) = (a, b, fn c, d)         
    [<Extension>]static member MapItem3 ((a, b, c)               , fn) = (a, b, fn c)   

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem3: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem3> , value)

[<Extension;Sealed>]
type MapItem4 =
    [<Extension>]static member MapItem4 ((a, b, c, d, e, f, g, h), fn) = (a, b, c, fn d, e, f, g, h)
    [<Extension>]static member MapItem4 ((a, b, c, d, e, f, g)   , fn) = (a, b, c, fn d, e, f, g)
    [<Extension>]static member MapItem4 ((a, b, c, d, e, f)      , fn) = (a, b, c, fn d, e, f)   
    [<Extension>]static member MapItem4 ((a, b, c, d, e)         , fn) = (a, b, c, fn d, e)      
    [<Extension>]static member MapItem4 ((a, b, c, d)            , fn) = (a, b, c, fn d)

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem4: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem4> , value)

[<Extension;Sealed>]
type MapItem5 =
    [<Extension>]static member MapItem5 ((a, b, c, d, e, f, g, h), fn) = (a, b, c, d, fn e, f, g, h)
    [<Extension>]static member MapItem5 ((a, b, c, d, e, f, g)   , fn) = (a, b, c, d, fn e, f, g)
    [<Extension>]static member MapItem5 ((a, b, c, d, e, f)      , fn) = (a, b, c, d, fn e, f)   
    [<Extension>]static member MapItem5 ((a, b, c, d, e)         , fn) = (a, b, c, d, fn e)

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem5: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem5> , value)

[<Extension;Sealed>]
type MapItem6 =
    [<Extension>]static member MapItem6 ((a, b, c, d, e, f, g, h), fn) = (a, b, c, d, e, fn f, g, h)
    [<Extension>]static member MapItem6 ((a, b, c, d, e, f, g)   , fn) = (a, b, c, d, e, fn f, g)
    [<Extension>]static member MapItem6 ((a, b, c, d, e, f)      , fn) = (a, b, c, d, e, fn f)

    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem6: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem6> , value)

[<Extension;Sealed>]
type MapItem7 =
    [<Extension>]static member MapItem7 ((a, b, c, d, e, f, g, h), fn) = (a, b, c, d, e, f, fn g, h)
    [<Extension>]static member MapItem7 ((a, b, c, d, e, f, g)   , fn) = (a, b, c, d, e, f, fn g)
    
    static member inline Invoke f value = 
        let inline call_2 (a:^a, b:^b) = ((^a or ^b) : (static member MapItem7: _ * _ -> _) b, f)
        let inline call   (a:'a, b:'b) = call_2 (a, b)
        call (Unchecked.defaultof<MapItem7> , value)

[<Extension;Sealed>]
type MapItem8 =
    [<Extension>]static member MapItem8 ((a, b, c, d, e, f, g, h), fn) = (a, b, c, d, e, f, g, fn h)