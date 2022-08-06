namespace FSharpPlus.Control
    
    [<Class>]
    type Item1 =
        
        static member
          inline Invoke: value:  ^t -> 'a
                           when  ^t: (member get_Item1:  ^t -> 'a)
    
    [<Class>]
    type Item2 =
        
        static member
          inline Invoke: value:  ^t -> 'a
                           when  ^t: (member get_Item2:  ^t -> 'a)
    
    [<Class>]
    type Item3 =
        
        static member
          inline Invoke: value:  ^t -> 'a
                           when  ^t: (member get_Item3:  ^t -> 'a)
    
    [<Class>]
    type Item4 =
        
        static member
          inline Invoke: value:  ^t -> 'a
                           when  ^t: (member get_Item4:  ^t -> 'a)
    
    [<Class>]
    type Item5 =
        
        static member
          inline Invoke: value:  ^t -> 'a
                           when  ^t: (member get_Item5:  ^t -> 'a)
    
    [<Class>]
    type MapItem1 =
        
        static member
          inline Invoke: f: 'a -> value:  ^b -> 'c
                           when (MapItem1 or  ^b) :
                                  (static member MapItem1:  ^b * 'a -> 'c)
        
        static member
          MapItem1: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * fn: ('a -> 'h)
                      -> 'h * 'b * 'c * 'd * 'e * 'f * 'g
        
        static member
          MapItem1: ('a * 'b * 'c * 'd * 'e * 'f) * fn: ('a -> 'g)
                      -> 'g * 'b * 'c * 'd * 'e * 'f
        
        static member
          MapItem1: ('a * 'b * 'c * 'd * 'e) * fn: ('a -> 'f)
                      -> 'f * 'b * 'c * 'd * 'e
        
        static member
          MapItem1: ('a * 'b * 'c * 'd) * fn: ('a -> 'e) -> 'e * 'b * 'c * 'd
        
        static member MapItem1: ('a * 'b * 'c) * fn: ('a -> 'd) -> 'd * 'b * 'c
        
        static member MapItem1: ('a * 'b) * fn: ('a -> 'c) -> 'c * 'b
        
        static member
          MapItem1: x: System.Tuple<'a> * fn: ('a -> 'b) -> System.Tuple<'b>
        
        static member
          inline MapItem1: t:  ^t * fn: ('t1 -> 'a)
                             -> System.Tuple<'a,'t2,'t3,'t4,'t5,'t6,'t7,'tr>
                             when  ^t: (member get_Rest:  ^t -> 'tr) and
                                   ^t: (member get_Item7:  ^t -> 't7) and
                                   ^t: (member get_Item6:  ^t -> 't6) and
                                   ^t: (member get_Item5:  ^t -> 't5) and
                                   ^t: (member get_Item4:  ^t -> 't4) and
                                   ^t: (member get_Item3:  ^t -> 't3) and
                                   ^t: (member get_Item2:  ^t -> 't2) and
                                   ^t: (member get_Item1:  ^t -> 't1)
    
    [<Class>]
    type MapItem2 =
        
        static member
          inline Invoke: f: 'a -> value:  ^b -> 'c
                           when (MapItem2 or  ^b) :
                                  (static member MapItem2:  ^b * 'a -> 'c)
        
        static member
          MapItem2: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * fn: ('b -> 'h)
                      -> 'a * 'h * 'c * 'd * 'e * 'f * 'g
        
        static member
          MapItem2: ('a * 'b * 'c * 'd * 'e * 'f) * fn: ('b -> 'g)
                      -> 'a * 'g * 'c * 'd * 'e * 'f
        
        static member
          MapItem2: ('a * 'b * 'c * 'd * 'e) * fn: ('b -> 'f)
                      -> 'a * 'f * 'c * 'd * 'e
        
        static member
          MapItem2: ('a * 'b * 'c * 'd) * fn: ('b -> 'e) -> 'a * 'e * 'c * 'd
        
        static member MapItem2: ('a * 'b * 'c) * fn: ('b -> 'd) -> 'a * 'd * 'c
        
        static member MapItem2: ('a * 'b) * fn: ('b -> 'c) -> 'a * 'c
        
        static member
          MapItem2: x: Internals.Id<'a> * fn: ('a -> 'b) -> Internals.Id<'b>
        
        static member
          inline MapItem2: t:  ^t * fn: ('t2 -> 'a)
                             -> System.Tuple<'t1,'a,'t3,'t4,'t5,'t6,'t7,'tr>
                             when  ^t: (member get_Rest:  ^t -> 'tr) and
                                   ^t: (member get_Item7:  ^t -> 't7) and
                                   ^t: (member get_Item6:  ^t -> 't6) and
                                   ^t: (member get_Item5:  ^t -> 't5) and
                                   ^t: (member get_Item4:  ^t -> 't4) and
                                   ^t: (member get_Item3:  ^t -> 't3) and
                                   ^t: (member get_Item2:  ^t -> 't2) and
                                   ^t: (member get_Item1:  ^t -> 't1)
    
    [<Class>]
    type MapItem3 =
        
        static member
          inline Invoke: f: 'a -> value:  ^b -> 'c
                           when (MapItem3 or  ^b) :
                                  (static member MapItem3:  ^b * 'a -> 'c)
        
        static member
          MapItem3: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * fn: ('c -> 'h)
                      -> 'a * 'b * 'h * 'd * 'e * 'f * 'g
        
        static member
          MapItem3: ('a * 'b * 'c * 'd * 'e * 'f) * fn: ('c -> 'g)
                      -> 'a * 'b * 'g * 'd * 'e * 'f
        
        static member
          MapItem3: ('a * 'b * 'c * 'd * 'e) * fn: ('c -> 'f)
                      -> 'a * 'b * 'f * 'd * 'e
        
        static member
          MapItem3: ('a * 'b * 'c * 'd) * fn: ('c -> 'e) -> 'a * 'b * 'e * 'd
        
        static member MapItem3: ('a * 'b * 'c) * fn: ('c -> 'd) -> 'a * 'b * 'd
        
        static member
          MapItem3: x: Internals.Id<'a> * fn: ('a -> 'b) -> Internals.Id<'b>
        
        static member
          inline MapItem3: t:  ^t * fn: ('t3 -> 'a)
                             -> System.Tuple<'t1,'t2,'a,'t4,'t5,'t6,'t7,'tr>
                             when  ^t: (member get_Rest:  ^t -> 'tr) and
                                   ^t: (member get_Item7:  ^t -> 't7) and
                                   ^t: (member get_Item6:  ^t -> 't6) and
                                   ^t: (member get_Item5:  ^t -> 't5) and
                                   ^t: (member get_Item4:  ^t -> 't4) and
                                   ^t: (member get_Item3:  ^t -> 't3) and
                                   ^t: (member get_Item2:  ^t -> 't2) and
                                   ^t: (member get_Item1:  ^t -> 't1)
    
    [<Class>]
    type MapItem4 =
        
        static member
          inline Invoke: f: 'a -> value:  ^b -> 'c
                           when (MapItem4 or  ^b) :
                                  (static member MapItem4:  ^b * 'a -> 'c)
        
        static member
          MapItem4: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * fn: ('d -> 'h)
                      -> 'a * 'b * 'c * 'h * 'e * 'f * 'g
        
        static member
          MapItem4: ('a * 'b * 'c * 'd * 'e * 'f) * fn: ('d -> 'g)
                      -> 'a * 'b * 'c * 'g * 'e * 'f
        
        static member
          MapItem4: ('a * 'b * 'c * 'd * 'e) * fn: ('d -> 'f)
                      -> 'a * 'b * 'c * 'f * 'e
        
        static member
          MapItem4: ('a * 'b * 'c * 'd) * fn: ('d -> 'e) -> 'a * 'b * 'c * 'e
        
        static member
          MapItem4: x: Internals.Id<'a> * fn: ('a -> 'b) -> Internals.Id<'b>
        
        static member
          inline MapItem4: t:  ^t * fn: ('t4 -> 'a)
                             -> System.Tuple<'t1,'t2,'t3,'a,'t5,'t6,'t7,'tr>
                             when  ^t: (member get_Rest:  ^t -> 'tr) and
                                   ^t: (member get_Item7:  ^t -> 't7) and
                                   ^t: (member get_Item6:  ^t -> 't6) and
                                   ^t: (member get_Item5:  ^t -> 't5) and
                                   ^t: (member get_Item4:  ^t -> 't4) and
                                   ^t: (member get_Item3:  ^t -> 't3) and
                                   ^t: (member get_Item2:  ^t -> 't2) and
                                   ^t: (member get_Item1:  ^t -> 't1)
    
    [<Class>]
    type MapItem5 =
        
        static member
          inline Invoke: f: 'a -> value:  ^b -> 'c
                           when (MapItem5 or  ^b) :
                                  (static member MapItem5:  ^b * 'a -> 'c)
        
        static member
          MapItem5: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * fn: ('e -> 'h)
                      -> 'a * 'b * 'c * 'd * 'h * 'f * 'g
        
        static member
          MapItem5: ('a * 'b * 'c * 'd * 'e * 'f) * fn: ('e -> 'g)
                      -> 'a * 'b * 'c * 'd * 'g * 'f
        
        static member
          MapItem5: ('a * 'b * 'c * 'd * 'e) * fn: ('e -> 'f)
                      -> 'a * 'b * 'c * 'd * 'f
        
        static member
          MapItem5: x: Internals.Id<'a> * fn: ('a -> 'b) -> Internals.Id<'b>
        
        static member
          inline MapItem5: t:  ^t * fn: ('t5 -> 'a)
                             -> System.Tuple<'t1,'t2,'t3,'t4,'a,'t6,'t7,'tr>
                             when  ^t: (member get_Rest:  ^t -> 'tr) and
                                   ^t: (member get_Item7:  ^t -> 't7) and
                                   ^t: (member get_Item6:  ^t -> 't6) and
                                   ^t: (member get_Item5:  ^t -> 't5) and
                                   ^t: (member get_Item4:  ^t -> 't4) and
                                   ^t: (member get_Item3:  ^t -> 't3) and
                                   ^t: (member get_Item2:  ^t -> 't2) and
                                   ^t: (member get_Item1:  ^t -> 't1)
    
    [<Class>]
    type Curry =
        
        static member
          Curry: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * Curry
                   -> (('h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o) -> 'h -> 'i
                       -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o)
        
        static member
          Curry: ('a * 'b * 'c * 'd * 'e * 'f) * Curry
                   -> (('g * 'h * 'i * 'j * 'k * 'l -> 'm) -> 'g -> 'h -> 'i
                       -> 'j -> 'k -> 'l -> 'm)
        
        static member
          Curry: ('a * 'b * 'c * 'd * 'e) * Curry
                   -> (('f * 'g * 'h * 'i * 'j -> 'k) -> 'f -> 'g -> 'h -> 'i
                       -> 'j -> 'k)
        
        static member
          Curry: ('a * 'b * 'c * 'd) * Curry
                   -> (('e * 'f * 'g * 'h -> 'i) -> 'e -> 'f -> 'g -> 'h -> 'i)
        
        static member
          Curry: ('a * 'b * 'c) * Curry
                   -> (('d * 'e * 'f -> 'g) -> 'd -> 'e -> 'f -> 'g)
        
        static member
          Curry: ('a * 'b) * Curry -> (('c * 'd -> 'e) -> 'c -> 'd -> 'e)
        
        static member
          Curry: System.Tuple<'t1> * Curry
                   -> ((System.Tuple<'a> -> 'b) -> 'a -> 'b)
        
        static member
          inline Curry: t:  ^t * Curry
                          -> (('c -> 'a) -> 't1 -> 't2 -> 't3 -> 't4 -> 't5
                              -> 't6 -> 't7 -> 'b)
                          when  ^t: (member get_Item1:  ^t -> 't1) and
                                ^t: (member get_Item2:  ^t -> 't2) and
                                ^t: (member get_Item3:  ^t -> 't3) and
                                ^t: (member get_Item4:  ^t -> 't4) and
                                ^t: (member get_Item5:  ^t -> 't5) and
                                ^t: (member get_Item6:  ^t -> 't6) and
                                ^t: (member get_Item7:  ^t -> 't7) and
                                ^t: (member get_Rest:  ^t ->  ^tr) and
                               (Curry or  ^tr) :
                                 (static member Curry:
                                     ^tr * Curry -> (( ^tr -> 'a) -> 'b))
        
        static member
          inline Invoke: f: ( ^t -> 'r) -> 'args
                           when (Curry or  ^t) :
                                  (static member Curry:
                                      ^t * Curry -> (( ^t -> 'r) -> 'args))
    
    [<Class>]
    type Uncurry =
        
        static member
          inline Invoke: f: 'a -> t:  ^b -> 'r
                           when (Uncurry or  ^b) :
                                  (static member Uncurry:
                                      ^b * Uncurry -> ('a -> 'r))
        
        static member
          Uncurry: ('a * 'b * 'c * 'd * 'e * 'f * 'g) * Uncurry
                     -> (('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'h)
        
        static member
          Uncurry: ('a * 'b * 'c * 'd * 'e * 'f) * Uncurry
                     -> (('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g)
        
        static member
          Uncurry: ('a * 'b * 'c * 'd * 'e) * Uncurry
                     -> (('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f)
        
        static member
          Uncurry: ('a * 'b * 'c * 'd) * Uncurry
                     -> (('a -> 'b -> 'c -> 'd -> 'e) -> 'e)
        
        static member
          Uncurry: ('a * 'b * 'c) * Uncurry -> (('a -> 'b -> 'c -> 'd) -> 'd)
        
        static member Uncurry: ('a * 'b) * Uncurry -> (('a -> 'b -> 'c) -> 'c)
        
        static member
          Uncurry: x: System.Tuple<'t1> * Uncurry -> (('t1 -> 'a) -> 'a)
        
        static member
          inline Uncurry: t:  ^t * Uncurry
                            -> (('t1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7
                                   -> 'a) -> 'b)
                            when  ^t: (member get_Rest:  ^t ->  ^tr) and
                                  ^t: (member get_Item7:  ^t -> 't7) and
                                  ^t: (member get_Item6:  ^t -> 't6) and
                                  ^t: (member get_Item5:  ^t -> 't5) and
                                  ^t: (member get_Item4:  ^t -> 't4) and
                                  ^t: (member get_Item3:  ^t -> 't3) and
                                  ^t: (member get_Item2:  ^t -> 't2) and
                                  ^t: (member get_Item1:  ^t -> 't1) and
                                 (Uncurry or  ^tr) :
                                   (static member Uncurry:
                                       ^tr * Uncurry -> ('a -> 'b))

