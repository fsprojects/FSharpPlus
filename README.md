FsControl
=========

This is basically an overload library.

The goal is to create overloads of standard F# types, organized into 'Type-Methods' to be used from other projects/libraries.

It's also possible to create libraries like [FSharpPlus](https://github.com/gmpl/FSharpPlus) that define their own custom types and add overloads for those types, making them instances of the Type-Methods defined in FsControl.

There are basically two types involved in these overloads:

1) The type that will implement the abstraction. This will be a “real” type, for example <code>List</code> or <code>Tree</code>. We may refer to this type as the the type or as the instance, since it represents an instance of the abstraction. At the same time we can classify these types in primitive types and custom types. By primitive types we refer to existing types in the .NET framework.

2) The type that represent the abstraction: This will be a "dummy" type implemented as an empty type or a single case DU with a single method which will be overloaded. Examples of these types are <code>Functor.Map</code>, <code>Monad.Bind</code>, <code>Monoid.Mappend</code>, etc. From now on and in order to differentiate from (1) we will call this type the Type-Method.

As you can see this 'Type-Methods' abstraction is similar to Haskell's Type Classes but with a single method.

FsControl contains overloads mainly for primitive types, but the generic functions will resolve to any member of a type (a custom type) having a matching signature.

So this means that FsControl allows you to define for instance the <code>(>>=)</code> operator (bind for monads), working with primitive types and your custom types at the same time.


How to use FsControl
--------------------

You may find hard to understand how to use FsControl, the documentation is not complete at the moment because the syntax is not 100% final, so the best is to have a look at the samples in the source code, if you just want to use the generic functions for primitive types open <code>FsControl.Operators</code> module.


The purpose of the overloads is to associate types with Type-Methods, here we can have three different scenarios:

 1) Add a new Type-Method and instances for existing types.

This is the most complex scenario, to define a new Type Method is not straightforward, there will be some guidelines but at the moment the best is to have a look at the source code.

 2) Add a new type and make it an instance of an existing Type-Method.
 
At the moment there are 2 ways:

 a) The easy way is to look at the signature of the method you want to implement

The old way is to use the same kind of signature used within the Type-Method definition:

In the type definition we will add a static member, which will follow this convention:

    static member [inline] [TypeMethodName] (arg1:Type, [more args], _[:ReturnType], _:[TypeMethodName]) =
            Implementation

To find the exact signature you need to look at the source code of the Type-Method.

Here's an example:

In the source code for <code>Map</code> (in Functor.fs) the <code>option</code> instance is defined like this:

    [<Extension>]static member Map (x:option<_>      , f, [<Optional>]impl:Map) = Option.map  f x

So you can create a type <code>Tree</code> and add an instance for the existing Type Method <code>Map</code> this way:

    // Define a type Tree
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Leaf of 'a
        static member 

    // add an ìnstance for Map (Functor)
        static member Map (x:Tree<_>, f, _) = 
		    let rec loop f (t:Tree<'a>)  =
                match t with
                | Leaf x -> Leaf (f x)
                | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
		    loop f x

 b) Some methods accept also a 'clean signature' without the last parameters.
 
See below the section "How can I make my classes FsControl-ready?"

 3) Add an instance for an existing type of an existing Type-Method:

We can’t do this. This is only possible if we have control over the source code of either the Type or the Type-Method.
The association must be done either in the Type (2) or in the Type Method (1) due to both a technical limitation and a conceptual reason:
 - Extensions methods are not taken into account in overload resolution.
 - It may lead to a bad design practice, something similar happens in Haskell with Type Classes (see [orphan instances](http://www.haskell.org/haskellwiki/Orphan_instance)).


How can I make my classes FsControl-ready?
------------------------------------------

Note: this feature is not 100% tested at the moment. If you find problems (typically insane compile times) you can still define it as described in 2).

An easy way to make classes in your project callable from FsControl without referencing FsControl DLLs at all is to use standard signatures for your methods. Here's a list of the standard signatures available at the moment:

 Functors:
 
     static member Map (x:MyFunctor<'T>, f:'T->'U) = {your map impl.} : MyFunctor<'U>
     
 Applicatives:
 
     static member Return (x:'T) = {your Return impl.} : MyApplicative<'T>
     static member (<*>) (f:MyApplicative<'T->'U>, x:MyApplicative<'U>) = {your Apply impl.} : MyApplicative<'U>
     
 Monads:
 
     static member Return // as defined in Applicatives
     static member Bind (x:MyMonad<'T>, f:'T->MyMonad<'U>) -> {your Bind impl.} : MyMonad<'U>
   
Monoids:

	static member Mempty() = {your Mempty impl.} : MyMonoid
	static member Mappend (x:MyMonoid, y:MyMonoid) = {your Mappend impl.} : MyMonoid
	static member Mconcat (x:list<MyMonoid>) = {your Mconcat impl.} : MyMonoid // optional: can be automatically derived from Mappend

Foldables:

    static member FoldBack (source:MyFoldable<'T>, folder:'T->'State->'State, state:'State) = {your FoldBack impl.} : 'State
	static member ToSeq (source:MyFoldable<'T>) = {your ToSeq impl.} : seq<'T>
	static memner FromSeq (source: seq<'T>) = {your FromSeq impl.} : MyFoldable<'T>