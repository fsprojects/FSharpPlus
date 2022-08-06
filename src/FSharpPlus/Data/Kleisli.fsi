namespace FSharpPlus.Data
    
    /// Kleisli arrows of a monad. Represents a function 'T -> 'Monad<'U>
    [<NoEquality; NoComparison; Struct>]
    type Kleisli<'t,'monad<'u>> =
        | Kleisli of ('t -> 'monad<'u>)
        
        static member
          inline (+++) : Kleisli<'T, ^u> * Kleisli<'v, ^w>
                           -> Kleisli<Choice<'v,'T>, ^z>
                           when (Control.Bind or  ^u or  ^z) :
                                  (static member (>>=) :
                                      ^u * ('a ->  ^z) ->  ^z) and
                                (Control.Return or  ^z) :
                                  (static member Return:
                                      ^z * Control.Return
                                       -> (Choice<'b,'a> ->  ^z)) and
                                (Control.Bind or  ^w or  ^z) :
                                  (static member (>>=) :
                                      ^w * ('b ->  ^z) ->  ^z)
        
        static member
          inline (<<<) : Kleisli<'a, ^b> * Kleisli<'d, ^c> -> Kleisli<'d, ^b>
                           when (Control.Bind or  ^c or  ^b) :
                                  (static member (>>=) :
                                      ^c * ('a ->  ^b) ->  ^b)
        
        static member
          inline (|||) : Kleisli<'a,'b> * Kleisli<'c,'b>
                           -> Kleisli<Choice<'c,'a>,'b>
        
        static member
          inline Arr: f: ('a -> 'b) -> Kleisli<'a, ^c>
                        when (Control.Return or  ^c) :
                               (static member Return:
                                   ^c * Control.Return -> ('b ->  ^c))
        
        static member
          Contramap: Kleisli<'B,'Monad<'C>> * k: ('A -> 'B)
                       -> Kleisli<'A,'Monad<'C>>
        
        static member
          inline Dimap: Kleisli<'B, ^Monad<'C>> * ab: ('A -> 'B) *
                        cd: ('C -> 'D) -> Kleisli<'A, ^Monad<'D>>
                          when (Control.Map or  ^Monad<'C> or  ^Monad<'D>) :
                                 (static member Map:
                                    ( ^Monad<'C> * ('C -> 'D)) * Control.Map
                                      ->  ^Monad<'D>)
        
        static member
          inline Empty: _output: Kleisli<'T,'Monad<'U>> * _mthd: Control.Empty
                          -> Kleisli<'a, ^b>
                          when (Control.Empty or  ^b) :
                                 (static member Empty:
                                     ^b * Control.Empty ->  ^b)
        
        static member
          inline First: Kleisli<'a, ^b> -> Kleisli<('a * 'e), ^c>
                          when (Control.Bind or  ^b or  ^c) :
                                 (static member (>>=) :  ^b * ('d ->  ^c) ->  ^c) and
                               (Control.Return or  ^c) :
                                 (static member Return:
                                     ^c * Control.Return -> ('d * 'e ->  ^c))
        
        static member
          inline Left: Kleisli<'a,'b> ->  ^c
                         when (Control.AcMerge or  ^c) :
                                (static member ``+++`` :
                                   Kleisli<'a,'b> *  ^d *  ^c * Control.AcMerge
                                     ->  ^c) and
                              (Control.Arr or  ^d) :
                                (static member Arr:
                                   ('e -> 'e) *  ^d * Control.Arr ->  ^d)
        
        static member
          inline Map: Kleisli<'B, ^Monad<'C>> * cd: ('C -> 'D)
                        -> Kleisli<'B, ^Monad<'D>>
                        when (Control.Map or  ^Monad<'C> or  ^Monad<'D>) :
                               (static member Map:
                                  ( ^Monad<'C> * ('C -> 'D)) * Control.Map
                                    ->  ^Monad<'D>)
        
        static member
          inline Right: Kleisli<'a,'b> ->  ^c
                          when (Control.AcMerge or  ^c) :
                                 (static member ``+++`` :
                                     ^d * Kleisli<'a,'b> *  ^c * Control.AcMerge
                                      ->  ^c) and
                               (Control.Arr or  ^d) :
                                 (static member Arr:
                                    ('e -> 'e) *  ^d * Control.Arr ->  ^d)
        
        static member
          inline Second: Kleisli<'a, ^b> -> Kleisli<('d * 'a), ^c>
                           when (Control.Bind or  ^b or  ^c) :
                                  (static member (>>=) :
                                      ^b * ('e ->  ^c) ->  ^c) and
                                (Control.Return or  ^c) :
                                  (static member Return:
                                      ^c * Control.Return -> ('d * 'e ->  ^c))
        
        static member
          inline ``<|>`` : Kleisli<'a, ^b> * Kleisli<'a, ^b> *
                           _mthd: Control.Append -> Kleisli<'a, ^b>
                             when (Control.Append or  ^b) :
                                    (static member ``<|>`` :
                                        ^b *  ^b * Control.Append ->  ^b)
        
        static member get_App: unit -> Kleisli<(Kleisli<'a,'b> * 'a),'b>
        
        static member
          inline get_Id: unit -> Kleisli<'a, ^b>
                           when (Control.Return or  ^b) :
                                  (static member Return:
                                      ^b * Control.Return -> ('a ->  ^b))
    
    /// Basic operations on Kleisli
    module Kleisli =
        
        val run: Kleisli<'a,'b> -> ('a -> 'b)

