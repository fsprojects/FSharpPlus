module General.Functor
open Testing
open General.Util
open FSharpPlus
open System.Collections.Generic

open FSharpPlus.Data
#nowarn "686"

open System.Threading.Tasks


let functor = testList "Functor" [
    #if !FABLE_COMPILER
    testCase "mapDefaultCustom" (fun () -> 

        SideEffects.reset ()
        // NonEmptyList<_> has Map but at the same time is a seq<_>
        let testVal1 = map ((+) 1) (nelist {10; 20; 30})
        Assert.IsInstanceOf<Option<NonEmptyList<int>>> (Some testVal1)

        let testVal2 = map ((+) 1) ((ofSeq :seq<_*_> -> Dictionary<_,_>) (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<Dictionary<string,int>>> (Some testVal2)

        let testVal3 = map ((+) 1) (dict (seq ["a", 1; "b", 2]))
        Assert.IsInstanceOf<Option<IDictionary<string,int>>> (Some testVal3)

        let testVal4 = map ((+) 1) (NonEmptySet.Create(10, 20, 30))
        Assert.IsInstanceOf<Option<NonEmptySet<int>>> (Some testVal4)

        let testVal5 = map ((+) 1) (NonEmptyMap.Create(("a", 1), ("b", 2)))
        Assert.IsInstanceOf<Option<NonEmptyMap<string,int>>> (Some testVal5)

        let testVal6 = map ((+) 1) (TestNonEmptyCollection.Create 1)
        Assert.IsInstanceOf<Option<NonEmptySeq<int>>> (Some testVal6)

        // WrappedSeqD is Applicative. Applicatives are Functors => map should work
        equal [] (SideEffects.get ())
        let testVal4 = map ((+) 1) (WrappedSeqD [1..3])
        Assert.IsInstanceOf<Option<WrappedSeqD<int>>> (Some testVal4)
        equal ["Using WrappedSeqD's Return"; "Using WrappedSeqD's Apply"] (SideEffects.get ())
        SideEffects.reset ()
        
        // WrappedListE is a Monad. Monads are Functors => map should work
        let testVal5 = map ((+) 1) (WrappedListE [1..3])
        Assert.IsInstanceOf<Option<WrappedListE<int>>> (Some testVal5)

        // Same with WrappedListD but WrappedListD is also IEnumerable<_>
        equal [] (SideEffects.get ())
        let testVal6 = map ((+) 1) (WrappedListD [1..3])
        Assert.IsInstanceOf<Option<WrappedListD<int>>> (Some testVal6)
        equal ["Using WrappedListD's Bind"; "Using WrappedListD's Return"; "Using WrappedListD's Return"; "Using WrappedListD's Return"] (SideEffects.get ())
        SideEffects.reset ()

        let testVal7 = TestNonEmptyCollection.Create 42
        head testVal7 |> ignore
        equal ["Using TestNonEmptyCollection's Head"] (SideEffects.get ())
        let testVal8 = testVal7 >>= fun i -> result (string i)
        Assert.IsInstanceOf<Option<NonEmptySeq<string>>> (Some testVal8)
        
        let testVal9 = map ((+) 1) (IReadOnlyCollection.ofList [1..3])
        Assert.IsInstanceOf<Option<IReadOnlyCollection<int>>> (Some testVal9)

        let testVal10 = map ((+) 1) (async { return 1})
        Assert.IsInstanceOf<Option<Async<int>>> (Some testVal10)
        equal 2 (testVal10 |> Async.RunSynchronously))
    #endif

    #if !FABLE_COMPILER
    testCase "unzip" (fun () -> 
        let testVal = unzip {Head = (1, 'a'); Tail = [(2, 'b');(3, 'b')]}
        Assert.IsInstanceOf<Option<NonEmptyList<int> * NonEmptyList<char>>> (Some testVal)

        let testVal2 = unzip (NonEmptyMap.Create((1,(true, 'a')), (2, (false, 'b'))))
        Assert.IsInstanceOf<Option<NonEmptyMap<int, bool> * NonEmptyMap<int, char>>> (Some testVal2))
    #endif

    #if !FABLE_COMPILER
    testCase "zipTest" (fun () ->

        SideEffects.reset ()
        let _a = zip (seq [1;2;3]) (seq [1. .. 3. ])
        equal [] (SideEffects.get ())

        let _b = zip (WrappedListD [1;2;3]) (WrappedListD [1. .. 3. ])
        equal ["Using WrappedListD's zip"] (SideEffects.get ())

        let _c = zip (dict [1,'1' ; 2,'2' ; 4,'4']) (dict [1,'1' ; 2,'2' ; 3,'3'])
        let _d = zip [ 1;2;3 ] [ 1. .. 3. ]
        let _e = zip [|1;2;3|] [|1. .. 3.|]
        let _g = zip ((seq [1;2;3]).GetEnumerator ()) ((seq [1. .. 3. ]).GetEnumerator ())
        let _h = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let _i = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let _j = zip (async {return 1}) (async {return '2'})
        let _h = zip (Task.FromResult 1) (Task.FromResult '2')
        let _i = zip List.singleton<int> Array.singleton<int>
        let _k = zip (TestNonEmptyCollection.Create 1) (result 2)

        let _fa a = zip a (seq [1. .. 3. ])
        let _fb a = zip a (WrappedListD [1. .. 3. ])
        let _fc a = zip a (dict [1,'1' ; 2,'2' ; 3,'3'])
        let _fd a = zip a [ 1. .. 3. ]
        let _fe a = zip a [|1. .. 3.|]
        let _fg a = zip a ((seq [1. .. 3. ]).GetEnumerator ())
        let _fh a = zip a (Map.ofSeq [1,'1' ; 2,'2' ; 3,'3'])
        let _fi a = zip a (ofSeq [1,'1' ; 2,'2' ; 3,'3'] : Dictionary<_,_>)
        let _fj a = zip a (async {return '2'})
        let _fh a = zip a (Task.FromResult '2')
        let _fi a = zip a Array.singleton<int>
        let _fj a = zip a (TestNonEmptyCollection.Create 2)

        let _ga b = zip (seq [1;2;3]) b
        let _gb b = zip (WrappedListD [1;2;3]) b
        let _gc b = zip (dict [1,'1' ; 2,'2' ; 4,'4']) b
        let _gd b = zip  [ 1;2;3 ] b
        let _ge b = zip  [|1;2;3|] b
        let _gg b = zip ((seq [1;2;3]).GetEnumerator ()) b
        let _gh b = zip (Map.ofSeq [1,'1' ; 2,'2' ; 4,'4']) b
        let _gi b = zip (ofSeq [1,'1' ; 2,'2' ; 4,'4'] : Dictionary<_,_>) b
        let _gj b = zip (async {return 1}) b
        let _gh b = zip (Task.FromResult 1) b
        let _gh b = zip List.singleton<int> b
        let _gj b = zip (TestNonEmptyCollection.Create 1) b

        let _ha : _ -> _ -> _ seq            = zip
        let _hb : _ -> _ -> _ WrappedListD   = zip
        let _hc : _ -> _ -> IDictionary<_,_> = zip
        let _hd : _ -> _ -> _ list           = zip
        let _he : _ -> _ -> _ []             = zip
        let _hg : _ -> _ -> _ IEnumerator    = zip
        let _hh : _ -> _ -> Map<_,_>         = zip
        let _hi : _ -> _ -> Dictionary<_,_>  = zip
        let _hj : _ -> _ -> Async<_>         = zip
        let _hh : _ -> _ -> Task<_>          = zip
        let _hi : _ -> _ -> (int -> _ )      = zip
        let _hj : _ -> _ -> NonEmptySeq<_>   = zip

        ())
    #endif

    #if !FABLE_COMPILER
    testCase "genericZipShortest" (fun () ->
        let a = zip [|1; 2; 3|]  [|"a"; "b"|]
        equalSeq [|1,"a"; 2,"b"|] a
        
        let l = zip [1; 2]  ["a"; "b"; "c"]
        equalSeq [1,"a"; 2,"b"] l
        
        let e = zip (ResizeArray [1; 2]) (ResizeArray ["a"; "b"; "c"])
        equalSeq (ResizeArray [1,"a"; 2,"b"]) e
        
        let nel = zip (NonEmptyList.ofList [1; 2]) (NonEmptyList.ofList ["a"; "b"; "c"])
        equalSeq (NonEmptyList.ofList [1,"a"; 2,"b"]) nel)
    #endif
    ]