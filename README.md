FsControl
=========

This is basically an overload library.

The goal is to create overloads of standard F# types, organized into 'Type-Methods' to be used from other projects/libraries.

It's also possible to create libraries (like [FSharpPlus](https://github.com/gmpl/FSharpPlus)) that define their own custom types and add overloads for those types, making them instances of the Type Methods defined in FsControl.

There are basically two Types involved in these overloads:

1) The type that will implement the abstraction. This will be a “real” type, for example <code>List</code> or <code>Tree</code>. We may refer to this type as the the type or as the instance, since it represents an instance of the abstraction.

2) The type that represent the abstraction: This will be a "dummy" type implemented as an empty Type or a single case DU with a single method which will be overloaded. Examples of these types are Functor, Monad, Monoid, etc. From now on and in order to differentiate from (1) we will call this Type the Type Method.

As you can see this 'Type Methods' abstraction is similar to Haskell's Type Classes but with a single method.


How to use FsControl
--------------------

You may find hard to understand how to use FsControl, the documentation is not complete at the moment because the syntax is not 100% final, so the best is to have a look at the samples in the source code, if you just want to use the generic functions use Functions.fsx or [FSharpPlus](https://github.com/gmpl/FSharpPlus).


FsControl contains overloads, the purpose of these overloads is to associate Types with Type Methods, here we can have three different scenarios:

 1) Add a new Type-Method and instances for existing Type.

This is the most complex scenario, to define a new Type Method is not straightforward, there will be some guidelines but at the moment the best is to have a look at the source code.

 2) Add a new Type and make it an instance of an existing Type-Method.
 
At the moment there are 2 ways:

The easy way is to look at the signature of the method you want to implement, see below the section "How can I make my classes FsControl-ready?"

The old way is to use the same kind of signature used within the Type-Method definition:

In the type definition we will add a static member called instance, which will follow this convention:

    static member [inline] instance (_:[module.]TypeMethodName, arg1:Type, [more polymorphic args], _[:ReturnType]) = 
        fun [non-polymorphic args in curried form, if none then a unit () arg] -> 
            Implementation

To find the exact signature you need to look at the source code of the Type-Method.

Here's an example:

In the source code for <code>Map</code> (in Functor.fs) the <code>option</code> instance is defined like this:

    static member instance (_:Map, x:option<_>, _) = fun f -> Option.map f x

So you can create a Type <code>Tree</code> and add an instance for the existing Type Method <code>Map</code> this way:

    // Define a type Tree
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Leaf of 'a
        static member map f (t:Tree<'a>)  =
            match t with
            | Leaf x -> Leaf (f x)
            | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1, Tree.map f t2)

    // add an ìnstance for Map (Functor)
        static member instance (_:Functor.Map, x:Tree<_>, _) = fun f -> Tree.map f x



 3) Add an instance for an existing Type of an existing Type-Method:

We can’t do this. This is only possible if we have control over the source code of either the Type or the Type-Method.
The association must be done either in the Type (2) or in the Type Method (1) due to both a technical limitation and a conceptual reason:
 - Extensions methods are not taken into account in overload resolution.
 - It may lead to a bad design practice, something similar happens in Haskell with Type Classes (see [orphan instances](http://www.haskell.org/haskellwiki/Orphan_instance)).


How can I make my classes FsControl-ready?
------------------------------------------

Note: this feature is not 100% tested at the moment. If you find problems you can still define it as described in 2).

An easy way to make classes in your project callable from FsControl without referencing FsControl DLLs at all is to use standard signatures for your methods. Here's a list of the standard signatures available at the moment:

 Functors:
 
     static member (<!>) (f:'T->'U) (x:MyFunctor<'T>) = {your map impl.} : MyFunctor<'U>
     
 Applicatives:
 
     static member Return (x:'T) = {your Return impl.} : MyApplicative<'T>
     static member (<*>) (f:MyApplicative<'T->'U>, x:MyApplicative<'U>) = {your Apply impl.} : MyApplicative<'U>
     
 Monads:
 
     static member Return // as defined in Applicatives
     static member Bind (x:MyMonad<'T>) = fun (f:'T->MyMonad<'U>) -> {your Bind impl.} : MyMonad<'U>
   
Monoids:

	static member Mempty() = {your Mempty impl.} : MyMonoid
	static member Mappend (x:MyMonoid, y:MyMonoid) = {your Mappend impl.} : MyMonoid
	static member Mconcat (x:list<MyMonoid>) = {your Mconcat impl.} : MyMonoid // optional: can be automatically derived from Mappend






