namespace FSharpPlus.Tests

open FSharpPlus
open FSharpPlus.Data
open NUnit.Framework

#if TEST_TRACE
open FSharpPlus.Internals
#endif

module Collections =
    
    [<Test>]    
    let chunkBy () =
        #if TEST_TRACE
        Traces.reset()
        #endif
        let source = [1; 2; 3; 5; 7; 9]
        let expected = [(1, [1]); (0, [2]); (1, [3; 5; 7; 9])]
        let actual = chunkBy (flip (%) 2) source
        CollectionAssert.AreEqual(expected, actual)
        #if TEST_TRACE
        CollectionAssert.AreEqual (["ChunkBy, list<'T>"], Traces.get())
        #endif
