namespace FSharpPlus
    
    /// Additional operations on Tuple (,)
    module Tuple2 =
        
        val mapItem1: f: ('a -> 'b) -> x: 'a * y: 'c -> 'b * 'c
        
        val mapItem2: f: ('a -> 'b) -> x: 'c * y: 'a -> 'c * 'b
    
    /// Additional operations on Tuple (,,)
    module Tuple3 =
        
        val mapItem1: f: ('a -> 'b) -> x: 'a * y: 'c * z: 'd -> 'b * 'c * 'd
        
        val mapItem2: f: ('a -> 'b) -> x: 'c * y: 'a * z: 'd -> 'c * 'b * 'd
        
        val mapItem3: f: ('a -> 'b) -> x: 'c * y: 'd * z: 'a -> 'c * 'd * 'b

