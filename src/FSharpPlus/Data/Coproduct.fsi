namespace FSharpPlus.Data
    
    [<AbstractClass>]
    type CoproductBase<'functorL<'t>,'functorR<'t>> =
        
        new: left: 'functorL<'t> * right: 'functorR<'t> * isLeft: bool
               -> CoproductBase<'functorL<'t>,'functorR<'t>>
        
        static member
          inline Map: x: CoproductBase< ^FunctorL<'T>, ^FunctorR<'T>> *
                      f: ('T -> 'U) -> Coproduct< ^FunctorL<'U>, ^FunctorR<'U>>
                        when (Control.Map or  ^FunctorL<'T> or  ^FunctorL<'U>) :
                               (static member Map:
                                  ( ^FunctorL<'T> * ('T -> 'U)) * Control.Map
                                    ->  ^FunctorL<'U>) and
                             (Control.Map or  ^FunctorR<'T> or  ^FunctorR<'U>) :
                               (static member Map:
                                  ( ^FunctorR<'T> * ('T -> 'U)) * Control.Map
                                    ->  ^FunctorR<'U>)
        
        override Equals: o: obj -> bool
        
        override GetHashCode: unit -> int
        
        member getContents: unit -> 'functorL<'t> * 'functorR<'t> * bool
    
    type Coproduct<[<EqualityConditionalOn>] 'functorL<'t>,'functorR<'t>> =
        inherit CoproductBase<[<EqualityConditionalOn>] 'functorL<'t>,
                              'functorR<'t>>
        
        new: left: 'functorL<'t> * right: 'functorR<'t> * isLeft: bool
               -> Coproduct<'functorL<'t>,'functorR<'t>>
        
        static member
          inline Map: a: Coproduct< ^FunctorL<'T>, ^FunctorR<'T>> *
                      f: ('T -> 'U) -> Coproduct<'FunctorL<'U>,'FunctorR<'U>>
                        when  ^FunctorL<'T> :
                               (static member Map:
                                   ^FunctorL<'T> * ('T -> 'U) -> 'FunctorL<'U>) and
                              ^FunctorR<'T> :
                               (static member Map:
                                   ^FunctorR<'T> * ('T -> 'U) -> 'FunctorR<'U>)
    
    module CoproductPrimitives =
        
        val InL: x: 'functorL<'t> -> Coproduct<'functorL<'t>,'functorR<'t>>
        
        val InR: x: 'functorR<'t> -> Coproduct<'functorL<'t>,'functorR<'t>>
        
        val (|InL|InR|) :
          x: Coproduct<'functorL<'t>,'functorR<'t>>
            -> Choice<'functorL<'t>,'functorR<'t>>

