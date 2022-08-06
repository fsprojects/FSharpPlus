namespace FSharpPlus.Internals
    
    module internal MonadOps =
        
        val inline (>>=) :
          x:  ^a -> f: ('c ->  ^b) ->  ^b
            when (Control.Bind or  ^a or  ^b) :
                   (static member (>>=) :  ^a * ('c ->  ^b) ->  ^b)
        
        val inline result:
          x: 'a ->  ^b
            when (Control.Return or  ^b) :
                   (static member Return:  ^b * Control.Return -> ('a ->  ^b))
        
        val inline (<*>) :
          f:  ^a -> x:  ^b ->  ^c
            when (Control.Apply or  ^a or  ^b or  ^c) :
                   (static member ``<*>`` :
                       ^a *  ^b *  ^c * Control.Apply ->  ^c)
        
        val inline (<|>) :
          x:  ^a -> y:  ^a ->  ^a
            when (Control.Append or  ^a) :
                   (static member ``<|>`` :  ^a *  ^a * Control.Append ->  ^a)
        
        val inline (>=>) :
          f: ('a ->  ^Monad<'b>) -> g: ('b ->  ^Monad<'c>) -> x: 'a
            ->  ^Monad<'c>
            when (Control.Bind or  ^Monad<'b> or  ^Monad<'c>) :
                   (static member (>>=) :
                       ^Monad<'b> * ('b ->  ^Monad<'c>) ->  ^Monad<'c>)

