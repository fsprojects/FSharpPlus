namespace FSharpPlus.Math
    
    /// <summary>
    /// Generic numbers, functions and operators.
    /// By opening this module some common operators become restricted, like (+) to 'T->'T->'T
    /// </summary>
    module Generic =
        
        val inline fromIntegral:
          x:  ^Integral ->  ^Num
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                 (Control.FromBigInt or  ^Num) :
                   (static member FromBigInt:
                       ^Num * Control.FromBigInt -> (bigint ->  ^Num))
        
        module NumericLiteralG =
            
            val inline FromZero:
              unit ->  ^a
                when (Control.Zero or  ^a) :
                       (static member Zero:  ^a * Control.Zero ->  ^a)
            
            val inline FromOne:
              unit ->  ^a
                when (Control.One or  ^a) :
                       (static member One:  ^a * Control.One ->  ^a)
            
            val inline FromInt32:
              i: int ->  ^a
                when (Control.FromInt32 or  ^a) :
                       (static member FromInt32:
                           ^a * Control.FromInt32 -> (int32 ->  ^a))
            
            val inline FromInt64:
              i: int64 ->  ^a
                when (Control.FromInt64 or  ^a) :
                       (static member FromInt64:
                           ^a * Control.FromInt64 -> (int64 ->  ^a))
            
            val inline FromString:
              i: string ->  ^a
                when (Control.FromBigInt or  ^a) :
                       (static member FromBigInt:
                           ^a * Control.FromBigInt -> (bigint ->  ^a))
        
        val inline (+) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when (Control.Plus or  ^Num) :
                   (static member ``+`` :  ^Num *  ^Num * Control.Plus ->  ^Num)
        
        val inline (-) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when  ^Num: (static member (-) :  ^Num *  ^Num ->  ^Num)
        
        val inline (*) :
          a:  ^Num -> b:  ^Num ->  ^Num
            when  ^Num: (static member ( * ) :  ^Num *  ^Num ->  ^Num)
        
        val inline (/) :
          a:  ^Fractional -> b:  ^Fractional ->  ^Fractional
            when  ^Fractional:
                   (static member (/) :
                       ^Fractional *  ^Fractional ->  ^Fractional)
        
        val inline internal whenIntegral:
          a:  ^a -> unit
            when (Control.ToBigInt or  ^a) :
                   (static member ToBigInt:  ^a -> bigint)
        
        /// Integer division. Same as (/) for Integral types.
        val inline div:
          a:  ^Integral -> b:  ^Integral ->  ^Integral
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                  ^Integral:
                   (static member (/) :  ^Integral *  ^Integral ->  ^Integral)
        
        /// Euclidean integer division, following the mathematical convention where the mod is always positive.
        val inline divE:
          a:  ^Integral -> b:  ^Integral ->  ^Integral
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                  ^Integral: (static member (~-) :  ^Integral ->  ^Integral) and
                  ^Integral:
                   (static member (/) :  ^Integral *  ^Integral ->  ^Integral) and
                  ^Integral: comparison and
                 (Control.Zero or  ^Integral) :
                   (static member Zero:  ^Integral * Control.Zero ->  ^Integral) and
                 (Control.Plus or  ^Integral) :
                   (static member ``+`` :
                       ^Integral *  ^Integral * Control.Plus ->  ^Integral) and
                  ^Integral:
                   (static member (-) :  ^Integral *  ^Integral ->  ^Integral) and
                 (Control.One or  ^Integral) :
                   (static member One:  ^Integral * Control.One ->  ^Integral)
        
        /// Remainder of Integer division. Same as (%).
        val inline rem:
          a:  ^Integral -> b:  ^Integral ->  ^Integral
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                  ^Integral:
                   (static member (%) :  ^Integral *  ^Integral ->  ^Integral)
        
        /// Euclidean remainder of integer division, following the mathematical convention where the mod is always positive.
        val inline remE:
          a:  ^Integral -> b:  ^Integral ->  ^Integral
            when (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                  ^Integral:
                   (static member (%) :  ^Integral *  ^Integral ->  ^Integral) and
                 (Control.Plus or  ^Integral) :
                   (static member ``+`` :
                       ^Integral *  ^Integral * Control.Plus ->  ^Integral)
        
        /// Euclidean division-remainder, following the mathematical convention where the mod is always positive.
        val inline divRemE:
          D:  ^a -> d:  ^a ->  ^a *  ^a
            when  ^a: (static member (-) :  ^a *  ^a ->  ^a) and
                 (Control.Plus or  ^a) :
                   (static member ``+`` :  ^a *  ^a * Control.Plus ->  ^a) and
                 (Control.DivRem or  ^a) :
                   (static member DivRem:
                       ^a *  ^a * Control.DivRem ->  ^a *  ^a) and
                 (Control.Zero or  ^a) :
                   (static member Zero:  ^a * Control.Zero ->  ^a) and
                 (Control.One or  ^a) :
                   (static member One:  ^a * Control.One ->  ^a) and
                  ^a: comparison
        
        /// Greatest Common Divisor.
        val inline gcd:
          x:  ^Integral -> y:  ^Integral ->  ^Integral
            when (Control.Zero or  ^Integral) :
                   (static member Zero:  ^Integral * Control.Zero ->  ^Integral) and
                 (Control.ToBigInt or  ^Integral) :
                   (static member ToBigInt:  ^Integral -> bigint) and
                  ^Integral:
                   (static member (%) :  ^Integral *  ^Integral ->  ^Integral) and
                  ^Integral: equality and
                 (Control.Abs or  ^Integral) :
                   (static member Abs:  ^Integral * Control.Abs ->  ^Integral)

