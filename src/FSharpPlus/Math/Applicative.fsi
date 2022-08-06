namespace FSharpPlus.Math
    
    /// <summary>Math Operators ready to use over Applicative Functors.</summary>
    module Applicative =
        
        val inline (~-.) :
          x:  ^Functor<'T> ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^a)) * Control.Map
                        ->  ^Functor<'T>) and
                  ^a: (static member (~-) :  ^a ->  ^a)
        
        val inline (.+) :
          x:  ^Functor<'T> -> y:  ^T ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>) and
                 ( ^a or  ^T) : (static member (+) :  ^a *  ^T ->  ^b)
        
        val inline (+.) :
          x:  ^T -> y:  ^Functor<'T> ->  ^Functor<'T>
            when ( ^T or  ^a) : (static member (+) :  ^T *  ^a ->  ^b) and
                 (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>)
        
        val inline (.+.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<'T>
            when (Control.Apply or  ^a or  ^Applicative<'T>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<'T> *
                      Control.Apply ->  ^Applicative<'T>) and
                 (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ( ^b ->  ^c ->  ^d)) * Control.Map
                        ->  ^a) and
                 ( ^b or  ^c) : (static member (+) :  ^b *  ^c ->  ^d)
        
        val inline (.-) :
          x:  ^Functor<'T> -> y:  ^T ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>) and
                 ( ^a or  ^T) : (static member (-) :  ^a *  ^T ->  ^b)
        
        val inline (-.) :
          x:  ^T -> y:  ^Functor<'T> ->  ^Functor<'T>
            when ( ^T or  ^a) : (static member (-) :  ^T *  ^a ->  ^b) and
                 (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>)
        
        val inline (.-.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<'T>
            when (Control.Apply or  ^a or  ^Applicative<'T>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<'T> *
                      Control.Apply ->  ^Applicative<'T>) and
                 (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ( ^b ->  ^c ->  ^d)) * Control.Map
                        ->  ^a) and
                 ( ^b or  ^c) : (static member (-) :  ^b *  ^c ->  ^d)
        
        val inline ( .* ) :
          x:  ^Functor<'T> -> y:  ^T ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>) and
                 ( ^a or  ^T) : (static member ( * ) :  ^a *  ^T ->  ^b)
        
        val inline ( *. ) :
          x:  ^T -> y:  ^Functor<'T> ->  ^Functor<'T>
            when ( ^T or  ^a) : (static member ( * ) :  ^T *  ^a ->  ^b) and
                 (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>)
        
        val inline (.*.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<'T>
            when (Control.Apply or  ^a or  ^Applicative<'T>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<'T> *
                      Control.Apply ->  ^Applicative<'T>) and
                 (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ( ^b ->  ^c ->  ^d)) * Control.Map
                        ->  ^a) and
                 ( ^b or  ^c) : (static member ( * ) :  ^b *  ^c ->  ^d)
        
        val inline (.%) :
          x:  ^Functor<'T> -> y:  ^T ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>) and
                 ( ^a or  ^T) : (static member (%) :  ^a *  ^T ->  ^b)
        
        val inline (%.) :
          x:  ^T -> y:  ^Functor<'T> ->  ^Functor<'T>
            when ( ^T or  ^a) : (static member (%) :  ^T *  ^a ->  ^b) and
                 (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>)
        
        val inline (.%.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<'T>
            when (Control.Apply or  ^a or  ^Applicative<'T>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<'T> *
                      Control.Apply ->  ^Applicative<'T>) and
                 (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ( ^b ->  ^c ->  ^d)) * Control.Map
                        ->  ^a) and
                 ( ^b or  ^c) : (static member (%) :  ^b *  ^c ->  ^d)
        
        val inline (./) :
          x:  ^Functor<'T> -> y:  ^T ->  ^Functor<'T>
            when (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>) and
                 ( ^a or  ^T) : (static member (/) :  ^a *  ^T ->  ^b)
        
        val inline (/.) :
          x:  ^T -> y:  ^Functor<'T> ->  ^Functor<'T>
            when ( ^T or  ^a) : (static member (/) :  ^T *  ^a ->  ^b) and
                 (Control.Map or  ^Functor<'T>) :
                   (static member Map:
                      ( ^Functor<'T> * ( ^a ->  ^b)) * Control.Map
                        ->  ^Functor<'T>)
        
        val inline (./.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<'T>
            when (Control.Apply or  ^a or  ^Applicative<'T>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<'T> *
                      Control.Apply ->  ^Applicative<'T>) and
                 (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ( ^b ->  ^c ->  ^d)) * Control.Map
                        ->  ^a) and
                 ( ^b or  ^c) : (static member (/) :  ^b *  ^c ->  ^d)
        
        val inline (.=) :
          x:  ^Functor<'T> -> y: 'T ->  ^Functor<bool>
            when (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>) and 'T: equality
        
        val inline (=.) :
          x: 'T -> y:  ^Functor<'T> ->  ^Functor<bool>
            when 'T: equality and
                 (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.=.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<bool>
            when (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ('b -> 'b -> bool)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or
                   ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and 'b: equality
        
        val inline (.>) :
          x:  ^Functor<'T> -> y: 'T ->  ^Functor<bool>
            when (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>) and 'T: comparison
        
        val inline (>.) :
          x: 'T -> y:  ^Functor<'T> ->  ^Functor<bool>
            when 'T: comparison and
                 (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.>.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<bool>
            when (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ('b -> 'b -> bool)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or
                   ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and 'b: comparison
        
        val inline (.<) :
          x:  ^Functor<'T> -> y: 'T ->  ^Functor<bool>
            when (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>) and 'T: comparison
        
        val inline (<.) :
          x: 'T -> y:  ^Functor<'T> ->  ^Functor<bool>
            when 'T: comparison and
                 (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.<.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<bool>
            when (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ('b -> 'b -> bool)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or
                   ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and 'b: comparison
        
        val inline (.||) :
          x:  ^Functor<bool> -> y: bool ->  ^Functor<bool>
            when (Control.Map or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<bool> * (bool -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (||.) :
          x: bool -> y:  ^Functor<bool> ->  ^Functor<bool>
            when (Control.Map or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<bool> * (bool -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.||.) :
          x:  ^Applicative<bool> -> y:  ^Applicative<bool>
            ->  ^Applicative<bool>
            when (Control.Apply or  ^a or  ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<bool> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and
                 (Control.Map or  ^Applicative<bool> or  ^a) :
                   (static member Map:
                      ( ^Applicative<bool> * (bool -> bool -> bool)) *
                      Control.Map ->  ^a)
        
        val inline (.&&) :
          x:  ^Functor<bool> -> y: bool ->  ^Functor<bool>
            when (Control.Map or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<bool> * (bool -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (&&.) :
          x: bool -> y:  ^Functor<bool> ->  ^Functor<bool>
            when (Control.Map or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<bool> * (bool -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.&&.) :
          x:  ^Applicative<bool> -> y:  ^Applicative<bool>
            ->  ^Applicative<bool>
            when (Control.Apply or  ^a or  ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<bool> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and
                 (Control.Map or  ^Applicative<bool> or  ^a) :
                   (static member Map:
                      ( ^Applicative<bool> * (bool -> bool -> bool)) *
                      Control.Map ->  ^a)
        
        val inline (.<=) :
          x:  ^Functor<'T> -> y: 'T ->  ^Functor<bool>
            when (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>) and 'T: comparison
        
        val inline (<=.) :
          x: 'T -> y:  ^Functor<'T> ->  ^Functor<bool>
            when 'T: comparison and
                 (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.<=.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<bool>
            when (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ('b -> 'b -> bool)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or
                   ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and 'b: comparison
        
        val inline (.>=) :
          x:  ^Functor<'T> -> y: 'T ->  ^Functor<bool>
            when (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>) and 'T: comparison
        
        val inline (>=.) :
          x: 'T -> y:  ^Functor<'T> ->  ^Functor<bool>
            when 'T: comparison and
                 (Control.Map or  ^Functor<'T> or  ^Functor<bool>) :
                   (static member Map:
                      ( ^Functor<'T> * ('T -> bool)) * Control.Map
                        ->  ^Functor<bool>)
        
        val inline (.>=.) :
          x:  ^Applicative<'T> -> y:  ^Applicative<'T> ->  ^Applicative<bool>
            when (Control.Map or  ^Applicative<'T> or  ^a) :
                   (static member Map:
                      ( ^Applicative<'T> * ('b -> 'b -> bool)) * Control.Map
                        ->  ^a) and
                 (Control.Apply or  ^a or  ^Applicative<'T> or
                   ^Applicative<bool>) :
                   (static member ``<*>`` :
                       ^a *  ^Applicative<'T> *  ^Applicative<bool> *
                      Control.Apply ->  ^Applicative<bool>) and 'b: comparison

