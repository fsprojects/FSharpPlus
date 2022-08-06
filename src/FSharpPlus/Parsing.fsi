namespace FSharpPlus
    
    module Parsing =
        
        val inline private getGroups:
          pf: PrintfFormat<'a,'b,'c,'d,'e> -> s: string -> (string * string)[]
        
        val inline private conv:
          destType: System.Type -> b: int -> s: string -> obj
        
        val inline private parse:
          s: string * f: string ->  ^r
            when (Control.Parse or  ^r) :
                   (static member Parse:  ^r * Control.Parse -> (string ->  ^r))
        
        val inline private tryParse:
          s: string * f: string ->  ^r option
            when (Control.TryParse or  ^r) :
                   (static member TryParse:
                       ^r * Control.TryParse -> (string ->  ^r option))
        
        [<Class>]
        type ParseArray =
            
            static member
              inline Invoke: g: (string * string)[] ->  ^a
                               when (ParseArray or  ^a) :
                                      (static member ParseArray:
                                          ^a * ParseArray
                                           -> ((string * string)[] ->  ^a))
            
            static member
              inline ParseArray: ('t1 * 't2' * 't3 * 't4 * 't5 * 't6 * 't7) *
                                 ParseArray
                                   -> ((string * string)[]
                                         ->  ^a *  ^b *  ^c *  ^d *  ^e *  ^f *
                                             ^g)
                                   when (Control.Parse or  ^a) :
                                          (static member Parse:
                                              ^a * Control.Parse
                                               -> (string ->  ^a)) and
                                        (Control.Parse or  ^b) :
                                          (static member Parse:
                                              ^b * Control.Parse
                                               -> (string ->  ^b)) and
                                        (Control.Parse or  ^c) :
                                          (static member Parse:
                                              ^c * Control.Parse
                                               -> (string ->  ^c)) and
                                        (Control.Parse or  ^d) :
                                          (static member Parse:
                                              ^d * Control.Parse
                                               -> (string ->  ^d)) and
                                        (Control.Parse or  ^e) :
                                          (static member Parse:
                                              ^e * Control.Parse
                                               -> (string ->  ^e)) and
                                        (Control.Parse or  ^f) :
                                          (static member Parse:
                                              ^f * Control.Parse
                                               -> (string ->  ^f)) and
                                        (Control.Parse or  ^g) :
                                          (static member Parse:
                                              ^g * Control.Parse
                                               -> (string ->  ^g))
            
            static member
              inline ParseArray: ('t1 * 't2' * 't3 * 't4 * 't5 * 't6) *
                                 ParseArray
                                   -> ((string * string)[]
                                         ->  ^h *  ^i *  ^j *  ^k *  ^l *  ^m)
                                   when (Control.Parse or  ^h) :
                                          (static member Parse:
                                              ^h * Control.Parse
                                               -> (string ->  ^h)) and
                                        (Control.Parse or  ^i) :
                                          (static member Parse:
                                              ^i * Control.Parse
                                               -> (string ->  ^i)) and
                                        (Control.Parse or  ^j) :
                                          (static member Parse:
                                              ^j * Control.Parse
                                               -> (string ->  ^j)) and
                                        (Control.Parse or  ^k) :
                                          (static member Parse:
                                              ^k * Control.Parse
                                               -> (string ->  ^k)) and
                                        (Control.Parse or  ^l) :
                                          (static member Parse:
                                              ^l * Control.Parse
                                               -> (string ->  ^l)) and
                                        (Control.Parse or  ^m) :
                                          (static member Parse:
                                              ^m * Control.Parse
                                               -> (string ->  ^m))
            
            static member
              inline ParseArray: ('t1 * 't2' * 't3 * 't4 * 't5) * ParseArray
                                   -> ((string * string)[]
                                         ->  ^n *  ^o *  ^p *  ^q *  ^r)
                                   when (Control.Parse or  ^n) :
                                          (static member Parse:
                                              ^n * Control.Parse
                                               -> (string ->  ^n)) and
                                        (Control.Parse or  ^o) :
                                          (static member Parse:
                                              ^o * Control.Parse
                                               -> (string ->  ^o)) and
                                        (Control.Parse or  ^p) :
                                          (static member Parse:
                                              ^p * Control.Parse
                                               -> (string ->  ^p)) and
                                        (Control.Parse or  ^q) :
                                          (static member Parse:
                                              ^q * Control.Parse
                                               -> (string ->  ^q)) and
                                        (Control.Parse or  ^r) :
                                          (static member Parse:
                                              ^r * Control.Parse
                                               -> (string ->  ^r))
            
            static member
              inline ParseArray: ('t1 * 't2' * 't3 * 't4) * ParseArray
                                   -> ((string * string)[]
                                         ->  ^s *  ^t *  ^a1 *  ^a2)
                                   when (Control.Parse or  ^s) :
                                          (static member Parse:
                                              ^s * Control.Parse
                                               -> (string ->  ^s)) and
                                        (Control.Parse or  ^t) :
                                          (static member Parse:
                                              ^t * Control.Parse
                                               -> (string ->  ^t)) and
                                        (Control.Parse or  ^a1) :
                                          (static member Parse:
                                              ^a1 * Control.Parse
                                               -> (string ->  ^a1)) and
                                        (Control.Parse or  ^a2) :
                                          (static member Parse:
                                              ^a2 * Control.Parse
                                               -> (string ->  ^a2))
            
            static member
              inline ParseArray: ('t1 * 't2' * 't3) * ParseArray
                                   -> ((string * string)[] ->  ^a3 *  ^a4 *  ^a5)
                                   when (Control.Parse or  ^a3) :
                                          (static member Parse:
                                              ^a3 * Control.Parse
                                               -> (string ->  ^a3)) and
                                        (Control.Parse or  ^a4) :
                                          (static member Parse:
                                              ^a4 * Control.Parse
                                               -> (string ->  ^a4)) and
                                        (Control.Parse or  ^a5) :
                                          (static member Parse:
                                              ^a5 * Control.Parse
                                               -> (string ->  ^a5))
            
            static member
              inline ParseArray: ('t1 * 't2) * ParseArray
                                   -> ((string * string)[] ->  ^a6 *  ^a7)
                                   when (Control.Parse or  ^a6) :
                                          (static member Parse:
                                              ^a6 * Control.Parse
                                               -> (string ->  ^a6)) and
                                        (Control.Parse or  ^a7) :
                                          (static member Parse:
                                              ^a7 * Control.Parse
                                               -> (string ->  ^a7))
            
            static member
              inline ParseArray: Internals.Id<'t1> * ParseArray
                                   -> ((string * string)[] -> Internals.Id< ^a8>)
                                   when (Control.Parse or  ^a8) :
                                          (static member Parse:
                                              ^a8 * Control.Parse
                                               -> (string ->  ^a8))
            
            static member
              inline ParseArray: System.Tuple< ^t1> * ParseArray
                                   -> ((string * string)[] -> System.Tuple< ^t1>)
                                   when (Control.Parse or  ^t1) :
                                          (static member Parse:
                                              ^t1 * Control.Parse
                                               -> (string ->  ^t1))
            
            static member
              inline ParseArray: unit * ParseArray
                                   -> ((string * string)[] -> unit)
            
            static member
              inline ParseArray: t:  ^t * ParseArray
                                   -> ((string * string)[] ->  ^t)
                                   when  ^t: (member get_Item1:  ^t ->  ^t1) and
                                         ^t: (member get_Item2:  ^t ->  ^t2) and
                                         ^t: (member get_Item3:  ^t ->  ^t3) and
                                         ^t: (member get_Item4:  ^t ->  ^t4) and
                                         ^t: (member get_Item5:  ^t ->  ^t5) and
                                         ^t: (member get_Item6:  ^t ->  ^t6) and
                                         ^t: (member get_Item7:  ^t ->  ^t7) and
                                         ^t: (member get_Rest:  ^t ->  ^tr) and
                                        (Control.Parse or  ^t1) :
                                          (static member Parse:
                                              ^t1 * Control.Parse
                                               -> (string ->  ^t1)) and
                                        (Control.Parse or  ^t2) :
                                          (static member Parse:
                                              ^t2 * Control.Parse
                                               -> (string ->  ^t2)) and
                                        (Control.Parse or  ^t3) :
                                          (static member Parse:
                                              ^t3 * Control.Parse
                                               -> (string ->  ^t3)) and
                                        (Control.Parse or  ^t4) :
                                          (static member Parse:
                                              ^t4 * Control.Parse
                                               -> (string ->  ^t4)) and
                                        (Control.Parse or  ^t5) :
                                          (static member Parse:
                                              ^t5 * Control.Parse
                                               -> (string ->  ^t5)) and
                                        (Control.Parse or  ^t6) :
                                          (static member Parse:
                                              ^t6 * Control.Parse
                                               -> (string ->  ^t6)) and
                                        (Control.Parse or  ^t7) :
                                          (static member Parse:
                                              ^t7 * Control.Parse
                                               -> (string ->  ^t7)) and
                                        (ParseArray or  ^tr) :
                                          (static member ParseArray:
                                              ^tr * ParseArray
                                               -> ((string * string)[] ->  ^tr))
            
            static member
              inline ParseArray:  ^t * obj -> ((string * string)[] ->  ^t)
                                   when (Control.Parse or  ^t) :
                                          (static member Parse:
                                              ^t * Control.Parse
                                               -> (string ->  ^t))
        
        val inline private tryParseElemAt:
          i: int -> g: (string * string)[] ->  ^a option
            when (Control.TryParse or  ^a) :
                   (static member TryParse:
                       ^a * Control.TryParse -> (string ->  ^a option))
        
        [<Class>]
        type TryParseArray =
            
            static member
              inline Invoke: g: (string * string)[] ->  ^a option
                               when (TryParseArray or  ^a) :
                                      (static member TryParseArray:
                                          ^a * TryParseArray
                                           -> ((string * string)[] ->  ^a option))
            
            static member
              inline TryParseArray: ('t1 * 't2' * 't3 * 't4 * 't5 * 't6 * 't7) *
                                    TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^a *  ^b *  ^c *  ^d *  ^e *
                                                 ^f *  ^g) option)
                                      when (Control.TryParse or  ^a) :
                                             (static member TryParse:
                                                 ^a * Control.TryParse
                                                  -> (string ->  ^a option)) and
                                           (Control.TryParse or  ^b) :
                                             (static member TryParse:
                                                 ^b * Control.TryParse
                                                  -> (string ->  ^b option)) and
                                           (Control.TryParse or  ^c) :
                                             (static member TryParse:
                                                 ^c * Control.TryParse
                                                  -> (string ->  ^c option)) and
                                           (Control.TryParse or  ^d) :
                                             (static member TryParse:
                                                 ^d * Control.TryParse
                                                  -> (string ->  ^d option)) and
                                           (Control.TryParse or  ^e) :
                                             (static member TryParse:
                                                 ^e * Control.TryParse
                                                  -> (string ->  ^e option)) and
                                           (Control.TryParse or  ^f) :
                                             (static member TryParse:
                                                 ^f * Control.TryParse
                                                  -> (string ->  ^f option)) and
                                           (Control.TryParse or  ^g) :
                                             (static member TryParse:
                                                 ^g * Control.TryParse
                                                  -> (string ->  ^g option))
            
            static member
              inline TryParseArray: ('t1 * 't2' * 't3 * 't4 * 't5 * 't6) *
                                    TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^h *  ^i *  ^j *  ^k *  ^l *
                                                 ^m) option)
                                      when (Control.TryParse or  ^h) :
                                             (static member TryParse:
                                                 ^h * Control.TryParse
                                                  -> (string ->  ^h option)) and
                                           (Control.TryParse or  ^i) :
                                             (static member TryParse:
                                                 ^i * Control.TryParse
                                                  -> (string ->  ^i option)) and
                                           (Control.TryParse or  ^j) :
                                             (static member TryParse:
                                                 ^j * Control.TryParse
                                                  -> (string ->  ^j option)) and
                                           (Control.TryParse or  ^k) :
                                             (static member TryParse:
                                                 ^k * Control.TryParse
                                                  -> (string ->  ^k option)) and
                                           (Control.TryParse or  ^l) :
                                             (static member TryParse:
                                                 ^l * Control.TryParse
                                                  -> (string ->  ^l option)) and
                                           (Control.TryParse or  ^m) :
                                             (static member TryParse:
                                                 ^m * Control.TryParse
                                                  -> (string ->  ^m option))
            
            static member
              inline TryParseArray: ('t1 * 't2' * 't3 * 't4 * 't5) *
                                    TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^n *  ^o *  ^p *  ^q *  ^r) option)
                                      when (Control.TryParse or  ^n) :
                                             (static member TryParse:
                                                 ^n * Control.TryParse
                                                  -> (string ->  ^n option)) and
                                           (Control.TryParse or  ^o) :
                                             (static member TryParse:
                                                 ^o * Control.TryParse
                                                  -> (string ->  ^o option)) and
                                           (Control.TryParse or  ^p) :
                                             (static member TryParse:
                                                 ^p * Control.TryParse
                                                  -> (string ->  ^p option)) and
                                           (Control.TryParse or  ^q) :
                                             (static member TryParse:
                                                 ^q * Control.TryParse
                                                  -> (string ->  ^q option)) and
                                           (Control.TryParse or  ^r) :
                                             (static member TryParse:
                                                 ^r * Control.TryParse
                                                  -> (string ->  ^r option))
            
            static member
              inline TryParseArray: ('t1 * 't2' * 't3 * 't4) * TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^s *  ^t *  ^a1 *  ^a2) option)
                                      when (Control.TryParse or  ^s) :
                                             (static member TryParse:
                                                 ^s * Control.TryParse
                                                  -> (string ->  ^s option)) and
                                           (Control.TryParse or  ^t) :
                                             (static member TryParse:
                                                 ^t * Control.TryParse
                                                  -> (string ->  ^t option)) and
                                           (Control.TryParse or  ^a1) :
                                             (static member TryParse:
                                                 ^a1 * Control.TryParse
                                                  -> (string ->  ^a1 option)) and
                                           (Control.TryParse or  ^a2) :
                                             (static member TryParse:
                                                 ^a2 * Control.TryParse
                                                  -> (string ->  ^a2 option))
            
            static member
              inline TryParseArray: ('t1 * 't2' * 't3) * TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^a3 *  ^a4 *  ^a5) option)
                                      when (Control.TryParse or  ^a3) :
                                             (static member TryParse:
                                                 ^a3 * Control.TryParse
                                                  -> (string ->  ^a3 option)) and
                                           (Control.TryParse or  ^a4) :
                                             (static member TryParse:
                                                 ^a4 * Control.TryParse
                                                  -> (string ->  ^a4 option)) and
                                           (Control.TryParse or  ^a5) :
                                             (static member TryParse:
                                                 ^a5 * Control.TryParse
                                                  -> (string ->  ^a5 option))
            
            static member
              inline TryParseArray: ('t1 * 't2) * TryParseArray
                                      -> ((string * string)[]
                                            -> ( ^a6 *  ^a7) option)
                                      when (Control.TryParse or  ^a6) :
                                             (static member TryParse:
                                                 ^a6 * Control.TryParse
                                                  -> (string ->  ^a6 option)) and
                                           (Control.TryParse or  ^a7) :
                                             (static member TryParse:
                                                 ^a7 * Control.TryParse
                                                  -> (string ->  ^a7 option))
            
            static member
              inline TryParseArray: Internals.Id<'t1> * TryParseArray
                                      -> ((string * string)[]
                                            -> Internals.Id< ^a8> option)
                                      when (Control.TryParse or  ^a8) :
                                             (static member TryParse:
                                                 ^a8 * Control.TryParse
                                                  -> (string ->  ^a8 option))
            
            static member
              inline TryParseArray: System.Tuple< ^t1> * TryParseArray
                                      -> ((string * string)[]
                                            -> System.Tuple< ^t1> option)
                                      when (Control.TryParse or  ^t1) :
                                             (static member TryParse:
                                                 ^t1 * Control.TryParse
                                                  -> (string ->  ^t1 option))
            
            static member
              inline TryParseArray: unit * TryParseArray
                                      -> ((string * string)[] -> unit)
            
            static member
              inline TryParseArray: t:  ^t * TryParseArray
                                      -> ((string * string)[] ->  ^t option)
                                      when  ^t: (member get_Item1:  ^t ->  ^t1) and
                                            ^t: (member get_Item2:  ^t ->  ^t2) and
                                            ^t: (member get_Item3:  ^t ->  ^t3) and
                                            ^t: (member get_Item4:  ^t ->  ^t4) and
                                            ^t: (member get_Item5:  ^t ->  ^t5) and
                                            ^t: (member get_Item6:  ^t ->  ^t6) and
                                            ^t: (member get_Item7:  ^t ->  ^t7) and
                                            ^t: (member get_Rest:  ^t ->  ^tr) and
                                           (Control.TryParse or  ^t1) :
                                             (static member TryParse:
                                                 ^t1 * Control.TryParse
                                                  -> (string ->  ^t1 option)) and
                                           (Control.TryParse or  ^t2) :
                                             (static member TryParse:
                                                 ^t2 * Control.TryParse
                                                  -> (string ->  ^t2 option)) and
                                           (Control.TryParse or  ^t3) :
                                             (static member TryParse:
                                                 ^t3 * Control.TryParse
                                                  -> (string ->  ^t3 option)) and
                                           (Control.TryParse or  ^t4) :
                                             (static member TryParse:
                                                 ^t4 * Control.TryParse
                                                  -> (string ->  ^t4 option)) and
                                           (Control.TryParse or  ^t5) :
                                             (static member TryParse:
                                                 ^t5 * Control.TryParse
                                                  -> (string ->  ^t5 option)) and
                                           (Control.TryParse or  ^t6) :
                                             (static member TryParse:
                                                 ^t6 * Control.TryParse
                                                  -> (string ->  ^t6 option)) and
                                           (Control.TryParse or  ^t7) :
                                             (static member TryParse:
                                                 ^t7 * Control.TryParse
                                                  -> (string ->  ^t7 option)) and
                                           (TryParseArray or  ^tr) :
                                             (static member TryParseArray:
                                                 ^tr * TryParseArray
                                                  -> ((string * string)[]
                                                        ->  ^tr option))
            
            static member
              inline TryParseArray:  ^t * obj
                                      -> ((string * string)[] ->  ^t option)
                                      when (Control.TryParse or  ^t) :
                                             (static member TryParse:
                                                 ^t * Control.TryParse
                                                  -> (string ->  ^t option))
        
        /// Gets a tuple with the result of parsing each element of a string array.
        val inline parseArray:
          source: string[] ->  ^(T1 * T2 * ... * Tn)
            when (ParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member ParseArray:
                       ^(T1 * T2 * ... * Tn) * ParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn)))
        
        /// Gets a tuple with the result of parsing each element of a formatted text.
        val inline sscanf:
          pf: PrintfFormat<'a,'b,'c,'d, ^(T1 * T2 * ... * Tn)> -> s: string
            ->  ^(T1 * T2 * ... * Tn)
            when (ParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member ParseArray:
                       ^(T1 * T2 * ... * Tn) * ParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn)))
        
        /// Gets a tuple with the result of parsing each element of a formatted text from the Console.
        val inline scanfn:
          pf: PrintfFormat<'a,'b,'c,'d, ^(T1 * T2 * ... * Tn)>
            ->  ^(T1 * T2 * ... * Tn)
            when (ParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member ParseArray:
                       ^(T1 * T2 * ... * Tn) * ParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn)))
        
        /// Gets a tuple with the result of parsing each element of a string array. Returns None in case of failure.
        val inline tryParseArray:
          source: string[] ->  ^(T1 * T2 * ... * Tn) option
            when (TryParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member TryParseArray:
                       ^(T1 * T2 * ... * Tn) * TryParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn) option))
        
        /// Gets a tuple with the result of parsing each element of a formatted text. Returns None in case of failure.
        val inline trySscanf:
          pf: PrintfFormat<'a,'b,'c,'d, ^(T1 * T2 * ... * Tn)> -> s: string
            ->  ^(T1 * T2 * ... * Tn) option
            when (TryParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member TryParseArray:
                       ^(T1 * T2 * ... * Tn) * TryParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn) option))
        
        /// Gets a tuple with the result of parsing each element of a formatted text from the Console. Returns None in case of failure.
        val inline tryScanfn:
          pf: PrintfFormat<'a,'b,'c,'d, ^(T1 * T2 * ... * Tn)>
            ->  ^(T1 * T2 * ... * Tn) option
            when (TryParseArray or  ^(T1 * T2 * ... * Tn)) :
                   (static member TryParseArray:
                       ^(T1 * T2 * ... * Tn) * TryParseArray
                        -> ((string * string)[] ->  ^(T1 * T2 * ... * Tn) option))

