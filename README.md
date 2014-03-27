FsControl
=========

This is basically an overload library.

The goal is to create overloads of standard F# types, organized into 'type methods' to be used from other projects/libraries.

It is also possible to create libraries (like [FSharpPlus](https://github.com/gmpl/FSharpPlus)) that define their own custom types and add overloads for those types, making them instances of the Type Methods defined in FsControl.

There are basically two Types involved in these overloads:

1) The type that will implement the abstraction. This will be a “real” type, for example List or Tree. We may refer to this type as the the type or as the instance, since it represents an instance of the abstraction.

2) The type that represent the abstraction: This will be a "dummy" type implemented as an empty Type or a single case DU with a single method which will be overloaded. Examples of these types are Functor, Monad, Monoid, etc. From now on and in order to differentiate from the previous type we will call this type the Type Method.

As you can see Type Methods are similar to Haskell's Type Classes but with a single method.


How to use FsControl
--------------------

You may find hard to understand how to use FsControl, the documentation is not complete at the moment because the systax is not 100% final, so the best is to have a look at the samples, if you just want to use the generic functions use Functions.fsx or [FSharpPlus](https://github.com/gmpl/FSharpPlus).


The purpose of these overloads is to associate Types with Type Methods. 

This association can lead us to 3 different scenarios:

1) Add a new Type Method and instances for existing type.
This is the most complex scenario, to define a new Type Method is not straightforward, they will be some guidelines but at the moment the best is to have a look at the source code.

2) Add a new type and make it an instance of an existing Type Method.
In the type definition we will add a static member called instance, which will respect the following convention:

    static member [inline] instance (_AbstractionName:MethodName, arg1:Type, [additional polymorphic args] ,  _[:ReturnType]) = fun (arg2[:arg2Type]) [additional non-polymorphic args] -> Implementation

To find the exact signature you need to look at the source code of the Type Method.

Here's an example:

In the source code Map for option is defined like this:

    static member instance (_:Map, x:option<_>    , _) = fun f -> Option.map  f x

So you can create a Tree and add an instance for Map this way:

    // Define a type Tree
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Leaf of 'a
        static member map f (t:Tree<'a>  )  =
            match t with
            | Leaf x -> Leaf (f x)
            | Tree(x,t1,t2) -> Tree(f x, Tree.map f t1, Tree.map f t2)

    // add ìnstance for Functor
        static member instance (_Functor:Map, x:Tree<_>, _) = fun f -> Tree.map f x



3) Set an existing type as instance of an existing Type Method:
We can’t do this. This is only possible if we have control over the source code of the Type or the Type Method.
The association must be done either in the Type (2) or in the Type Method (1) for both a technical reason (Extensions methods are not taken into account in overload resolution) and a conceptual reason: it may lead to a bad design practice, something similar happens in Haskell with Type Classes (see orphan instances).







