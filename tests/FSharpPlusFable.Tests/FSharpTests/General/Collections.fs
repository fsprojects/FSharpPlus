module General.Collections
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic
open FSharpPlus.Data
#nowarn "686"

open System
open System.Collections.ObjectModel
open System.Collections

module Collections =

    open System.Collections.Concurrent

    #if !FABLE_COMPILER
    let testCollections =
        let bigSeq = seq {1..10000000}
        let bigLst = [ 1..10000000 ]
        let bigArr = [|1..10000000|]
        let bigMut = ResizeArray(seq {1..10000000})

        let _ = head bigSeq
        #if !FABLE_COMPILER
        let _ = head bigLst
        #endif
        let _ = head bigArr

        #if !FABLE_COMPILER
        let _ = skip 1000 bigSeq
        #endif
        let _ = skip 1000 bigLst
        let _ = skip 1000 bigArr
        let _ = skip 1000 bigMut
        let _ = "hello world" |> skip 6 |> toList
        let _ = ofList ['h';'e';'l';'l';'o';' '] + "world"
        let _ = item 2 "hello"
        ()
    #endif

    #if !FABLE_COMPILER
    let testSeqConversions =
        #if !FABLE_COMPILER
        let sk: Generic.Stack<_>          = ofSeq { 1 .. 3 }
        #endif
        let sg: string                    = ofSeq {'1'..'3'}  // but it will come back as seq<char>
        let sb: Text.StringBuilder        = ofSeq {'1'..'3'}  // but it will come back as seq<char>
        let sq1:_ seq                     = ofSeq { 1 .. 3 }
        let sq2:_ seq                     = ofSeq (seq [(1, "One"); (2, "Two")])
        let sq3:_ seq                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        let sq4:_ seq                     = ofSeq (seq [(1, "One", '1', 1M); (2, "Two", '2', 2M)])
        let ls1:_ list                    = ofSeq {'1'..'3'}
        let ls2:_ list                    = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        #if !FABLE_COMPILER
        let st1:_ Set                     = ofSeq {'1'..'3'}
        let st2:_ Set                     = ofSeq (seq [(1, "One", '1'); (2, "Two", '2')])
        let ss: Generic.SortedSet<_>      = ofSeq (seq [3..6])
        let ra: Generic.List<_>           = ofSeq (seq [1..3])
        let sl: Generic.SortedList<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _sl:Generic.SortedList<_,_>   = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _mp:Map<_,_>                  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let d : Generic.IDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let _d: Generic.IDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let r : IReadOnlyDictionary<_,_>  = ofSeq (seq [("One", 1)])             // but it will come back as ...
        let _r: IReadOnlyDictionary<_,_>  = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let rc: IReadOnlyCollection<_>    = ofSeq (seq [2..7])
        let ut: Hashtable                 = ofSeq (seq [1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofSeq (seq ["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofSeq (seq [4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofSeq {'1'..'3'}                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofSeq (seq [(1, "One"); (2, "Two")]) // but it will come back as ...
        let _cd:ConcurrentDictionary<_,_> = ofSeq (seq [KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofSeq {'1'..'3'}
        #endif
        // now go back
        #if !FABLE_COMPILER
        let _sk'  = toSeq sk
        #endif
        let _sg'  = toSeq sg
        let _sb'  = toSeq sb
        let _sq1' = toSeq sq1
        let _sq2' = toSeq sq2
        let _sq3' = toSeq sq3
        let _sq4' = toSeq sq4
        let _ls1' = toSeq ls1
        let _ls2' = toSeq ls2
        #if !FABLE_COMPILER
        let _st1' = toSeq st1
        let _st2' = toSeq st2
        let _ss'  = toSeq ss 
        let _ra'  = toSeq ra 
        let _sl'  = toSeq sl 
        let _dc'  = toSeq dc 
        let _mp'  = toSeq mp 
        let _d'   = toSeq d  
        let _r'   = toSeq r
        let _rc'  = toSeq rc
        let _ut'  = toSeq ut 
        let _al'  = toSeq al 
        let _us'  = toSeq us 
        let _cc'  = toSeq cc 
        let _cd'  = toSeq cd 
        let _cb'  = toSeq cb
        #endif

        // there are some 'one-way' collections that can only be converted toSeq

        #if !FABLE_COMPILER
        let columns = 
            let d = new Data.DataTable () 
            [|new Data.DataColumn "id";new Data.DataColumn "column1";new Data.DataColumn "column2"|] |> d.Columns.AddRange
            d.Columns
        let _col1 = columns |> find (fun x -> x.ColumnName = "column1")
        let _cols = columns |> toList |> map  (fun x -> x.ColumnName)
        #endif
        // Defaults
        
        #if !FABLE_COMPILER
        let _12: WrappedListI<_> = seq [1;2] |> ofSeq
        #endif

        ()
    #endif

    #if !FABLE_COMPILER
    let testListConversions =

        // From sequence
        #if !FABLE_COMPILER
        let sk: Generic.Stack<_>          = ofList [ 1 .. 3 ]
        #endif
        let sg: string                    = ofList ['1'..'3']  // but it will come back as seq<char>
        let sb: Text.StringBuilder        = ofList ['1'..'3']  // but it will come back as seq<char>
        let sq1:_ seq                     = ofList [ 1 .. 3 ]
        let sq2:_ seq                     = ofList ([(1, "One"); (2, "Two")])
        let sq3:_ seq                     = ofList ([(1, "One", '1'); (2, "Two", '2')])
        let sq4:_ seq                     = ofList ([(1, "One", '1', 1M); (2, "Two", '2', 2M)])
        let ls1:_ list                    = ofList ['1'..'3']
        let ls2:_ list                    = ofList ([(1, "One", '1'); (2, "Two", '2')])
        #if !FABLE_COMPILER
        let st1:_ Set                     = ofList ['1'..'3']
        let st2:_ Set                     = ofList ([(1, "One", '1'); (2, "Two", '2')])
        let ss: Generic.SortedSet<_>      = ofList ([3..6])
        let ra: Generic.List<_>           = ofList ([1..3])
        let sl: Generic.SortedList<_,_>   = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _sl:Generic.SortedList<_,_>   = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let dc :Generic.Dictionary<_,_>   = ofList ([(1, "One"); (2, "Two")]) // but it will come back as KeyValuePair
        let mp :Map<_,_>                  = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _mp:Map<_,_>                  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        #endif
        let d : Generic.IDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let _d: Generic.IDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        #if !FABLE_COMPILER
        let r : IReadOnlyDictionary<_,_>  = ofList ([("One", 1)])             // but it will come back as ...
        let _r: IReadOnlyDictionary<_,_>  = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        #endif
        let rc: IReadOnlyCollection<_>    = ofList ([2..5])
        #if !FABLE_COMPILER
        let ut: Hashtable                 = ofList ([1,'1';2, '2';3,'3'])     // but it will come back as seq<obj>
        let al: ArrayList                 = ofList (["1";"2";"3"])            // but it will come back as seq<obj>
        let us: SortedList                = ofList ([4,'2';3,'4'])            // but it will come back as seq<obj>
        let cc: BlockingCollection<_>     = ofList ['1'..'3']                     // but it will come back as seq<obj>
        let cd: ConcurrentDictionary<_,_> = ofList ([(1, "One"); (2, "Two")]) // but it will come back as ...
        let _cd:ConcurrentDictionary<_,_> = ofList ([KeyValuePair(1, "One"); KeyValuePair(2, "Two")])
        let cb: ConcurrentBag<_>          = ofList ['1'..'3']
        #endif

        // now go back
        #if !FABLE_COMPILER
        let _sk'  = toList sk
        #endif
        let _sg'  = toList sg
        let _sb'  = toList sb
        let _sq1' = toList sq1
        let _sq2' = toList sq2
        let _sq3' = toList sq3
        let _sq4' = toList sq4
        let _ls1' = toList ls1
        let _ls2' = toList ls2
        #if !FABLE_COMPILER
        let _st1' = toList st1
        let _st2' = toList st2
        let _ss'  = toList ss
        let _ra'  = toList ra
        let _sl'  = toList sl
        let _dc'  = toList dc
        let _mp'  = toList mp
        #endif
        let _d'   = toList d
        #if !FABLE_COMPILER
        let _r'   = toList r
        #endif
        let _rc'  = toList rc
        #if !FABLE_COMPILER
        let _ut'  = toList ut
        let _al'  = toList al
        let _us'  = toList us
        let _cc'  = toList cc
        let _cd'  = toList cd
        let _cb'  = toList cb
        #endif

        ()
    #endif

    #if !FABLE_COMPILER
    let testSorts =
        let _r1 = [4..1] |> sort
        let _r2 = [4..1] |> sortBy string
        let _r3 = seq [4..1] |> sort
        let _r4 = seq [4..1] |> sortBy string
        let _r5 = ResizeArray [4..1] |> sort
        let _r6 = ResizeArray [4..1] |> sortBy string

        ()
    #endif

    #if !FABLE_COMPILER
    let testGeneralizableValues () =
        let a: list<_> = empty
        let _ =  0 ::a
        let _ = '0'::a

        let b: WrappedSeqA<_> = empty
        let _ = WrappedSeqA [ 0 ] <|> b
        let _ = WrappedSeqA ['0'] <|> b
         
        ()
    #endif

let collections = testList "Collections" [

    #if !FABLE_COMPILER
    testCase "readOnlyNth" (fun () ->
        let readOnlyCollection = ReadOnlyCollection [|1..10|]
        let iReadOnlyList = readOnlyCollection :> IReadOnlyList<_>
        equal 2 (nth 1 [1..10])
        equal 2 (nth 1 readOnlyCollection)
        equal 2 (nth 1 iReadOnlyList))

    testCase "readOnlyNthIndex" (fun () ->
        let l = ListOnlyIndex [1..10]
        equal 2 (nth 1 l)
        let rl = ReadOnlyListOnlyIndex [1..10]
        equal 2 (nth 1 rl))
    #endif

    #if !FABLE_COMPILER
    testCase "choose" (fun () -> 
        let d = choose Some ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some d)

        let d' = choose Some ((ofSeq :seq<_*_> -> IDictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some d')
        
        let rd = choose Some ((ofSeq :seq<_*_> -> IReadOnlyDictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IReadOnlyDictionary<string,int>>> (Some rd)
        
        let m = choose Some ((ofSeq :seq<_*_> -> Map<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Map<string,int>>> (Some m))
    #endif
    ]
