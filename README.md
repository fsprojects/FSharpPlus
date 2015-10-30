FsControl
=========

A base library with a collection of generic methods overloaded for .NET and F# core types but extensible to other types at the same time.

### Getting Started


 - Download binaries from [Nuget](https://www.nuget.org/packages/FsControl/), use the latest version (2.x)

 - Open an F# script file or the F# interactive and reference the library

        #r @"C:\Your path to the binaries\FsControl.Core.dll";;

 Ignore warnings about F# metadata.

 - Now you can create generic functions, here's an example with <code>map</code> ([fmap](https://wiki.haskell.org/Functor) for Haskellers, [Select](http://www.dotnetperls.com/select) for C-sharpers):

        let inline map f x = FsControl.Core.TypeMethods.Map.Invoke f x;;

 Static constraints will be inferred automatically.
    
 - Test it with .NET / F# primitive types:

        map string [|2;3;4;5|];;
        // val it : string [] = [|"2"; "3"; "4"; "5"|]
    
        map ((+) 9) (Some 3);;
        // val it : int option = Some 12

 - You can also create your own type with a method <code>Map</code>:

        type Tree<'t> =
            | Tree of 't * Tree<'t> * Tree<'t>
            | Leaf of 't
            static member Map (x:Tree<'a>, f) = 
                let rec loop f = function
                    | Leaf x -> Leaf (f x)
                    | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
                loop f x
            
 By adding the static member <code>Map</code> we say that we're making <code>Tree</code> an instance of <code>Map</code>.

 - Try to map over your new type:

        let myTree = Tree(6, Tree(2, Leaf 1, Leaf 3), Leaf 9);;
        map ((*) 10) myTree;;
        // val it : Tree<int> = Tree (60,Tree (20,Leaf 10,Leaf 30),Leaf 90)
    
Generic functions may be seen as an exotic thing in F# that only saves a few key strokes (<code>map</code> instead of <code>List.map</code> or <code>Array.map</code>) still they allow you to reach a higher abstraction level, using ad-hoc polymorphism.

But more interesting is the use of operators. You can't prefix them with the module they belong to, I mean you can but then it's no longer an operator, as an example many F# libraries define the bind operator <code>(>>=)</code> but it's not generic so if you use two different types which are both monads you will need to prefix it e.g. <code>State.(>>=)</code> and <code>Reader.(>>=)</code> which defeats the purpose of having an operator.

Here you can easily define a generic bind operator:

    let inline (>>=) x f = Bind.Invoke x f

Or if you do [Railway Oriented Programming](https://www.google.ch/#q=railway+oriented+programming) you can finally have your generic Kleisli composition (fish) operator:

    let inline (>=>) f g x = Bind.Invoke (f x) g

Also when working with combinators, the generic applicative functor (space invaders) operator is very handy:

    let inline (<*>) x y = Apply.Invoke x y
    
Of course they are already defined in the FsControl.Operators module and they work with primitive and user defined types.
    

### Next steps:
 - Have a look at the [sample files](https://github.com/gmpl/FsControl/blob/master/FsControl.Core/Samples/) adjust the path of the binaries and run the .fsx scripts.
 - Before creating your own library of generic functions be aware that [FsControl.Operators](https://github.com/gmpl/FsControl/blob/master/FsControl.Core/Operators.fs) is a lightweight module with some operators and functions used mainly to test the project. Also take the time to visit [F#+](https://github.com/gmpl/FSharpPlus) which is a library which re-export all those functions but apart from that provides more derived operators, builders and other interesting stuff.
 - In the rare case that you are not interested in the generic stuff but want to re-use specific implementations many methods in FsControl are defined as extension methods and some have a C# friendly signature.
 - Keep reading the doc.

How does it works
-----------------

There are basically two types involved in these overloads:

 - The type that will implement the abstraction. This will be a “real” type, for example <code>List</code> or <code>Tree</code>. We may refer to this type as the the type or as the instance, since it represents an instance of the abstraction. At the same time we can classify these types in primitive types and custom types. By primitive types we refer to existing types in the .NET framework.

 - The type that represent the abstraction: Examples of these types are <code>Map</code>, <code>Bind</code>, <code>Mappend</code>, etc. This will be a "dummy" type implemented as a static type with a single method  (usually with the same name as the type) which will be overloaded and an entry point method called 'Invoke'. From now on and in order to differentiate from the type-instance we will call this type the type-method.

For Haskellers this 'Type-Methods' abstraction is similar to Haskell's Type-Classes but with a single method.

For OOP-ers it may compare to interfaces or abstract classes but with a single method, early binding (compile-time) and without dependencies on the assembly where the interface is defined.

FsControl contains overloads mainly for primitive types, but the generic functions will resolve to any member of a type (a user-defined type) having a matching signature. This makes possible to use some libraries that don't depend on FsControl, as long as the signature is the right one it will work.



How to use FsControl
--------------------

You may find hard to understand how to use FsControl, the best is to have a look at the source code, if you just want to use the generic functions for primitive types open <code>FsControl.Operators</code> module.


The purpose of the overloads is to associate types with Type-Methods, here we can have three different scenarios:

 1) Add a new Type-Method and Type-Instances for existing types.

This is the most complex scenario, to define a new Type-Method is not straightforward, there will be some guidelines but at the moment the best is to have a look at the source code.

 2) Add a new type and make it an instance of an existing Type-Method.
 
There are 2 ways:

 a) You can have a look at the signature of the method you want to implement in the source code, which will follow this convention:

    static member [inline] [TypeMethodName] (arg1:Type, [more args], output[:ReturnType], impl:[TypeMethodName]) =
            Implementation

To find the exact signature you need to look at the source code of the Type-Method you are interested.

Here's an example:

In the source code for <code>Map</code> (in Functor.fs) the <code>option</code> instance is defined like this:

    [<Extension>]static member Map (x:option<_>      , f, [<Optional>]impl:Map) = Option.map  f x

So you can create a type <code>Tree</code> and add an instance for the existing Type Method <code>Map</code> this way:

    // Define a type Tree
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Leaf of 'a

    // add an instance for Map (Functor)
        static member Map (x:Tree<_>, f, _) = 
            let rec loop f (t:Tree<'a>)  =
                match t with
                | Leaf x -> Leaf (f x)
                | Tree (x, t1, t2) -> Tree (f x, loop f t1, loop f t2)
            loop f x

 b) Some methods accept also a 'clean signature' without the unused parameters <code>output</code> and <code>impl</code>. You can find a list of these methods below, in the section "How can I make my classes FsControl-ready?". This way it doesn't require to reference FsControl binaries.

 3) Add an instance for an existing Type of an existing Type-Method:

We can’t do this. This is only possible if we have control over the source code of either the Type-Instance or the Type-Method.
The association must be done either in the Type-Instance (2) or in the Type-Method (1) due to both a technical limitation <code>(*)</code> and a conceptual reason <code>(**)</code>.

 - <code>(*)</code> Extensions methods are not taken into account in overload resolution.
 - <code>(**)</code> It may lead to a bad design practice, something similar happens in Haskell with Type Classes (see [orphan instances](http://www.haskell.org/haskellwiki/Orphan_instance)).


How can I make my classes FsControl-ready?
------------------------------------------

An easy way to make classes in your project callable from FsControl without referencing FsControl DLLs at all is to use standard signatures for your methods. Here's a list of the standard signatures available at the moment, this list is not exhaustive:

 Functors:
 
     static member Map (x:MyFunctor<'T>, f:'T->'U) = {your map impl.} : MyFunctor<'U>
     
 Applicatives:
 
     static member Return (x:'T) = {your Return impl.} : MyApplicative<'T>
     static member (<*>) (f:MyApplicative<'T->'U>, x:MyApplicative<'T>) = {your Apply impl.} : MyApplicative<'U>
     
 Monads:
 
     static member Return // as defined in Applicatives
     static member Bind (x:MyMonad<'T>, f:'T->MyMonad<'U>) -> {your Bind impl.} : MyMonad<'U>
   
Monoids:

	static member Mempty() = {your Mempty impl.} : MyMonoid
	static member Mappend (x:MyMonoid, y:MyMonoid) = {your Mappend impl.} : MyMonoid
	static member Mconcat (x:list<MyMonoid>) = {your Mconcat impl.} : MyMonoid // optional: it can be automatically derived from Mappend

Foldables:

    static member FoldBack (source:MyFoldable<'T>, folder:'T->'State->'State, state:'State) = {your FoldBack impl.} : 'State
	static member ToSeq (source:MyFoldable<'T>) = {your ToSeq impl.} : seq<'T>
	static member FromSeq (source: seq<'T>) = {your FromSeq impl.} : MyFoldable<'T>
	

If you find problems (typically insane compile times) you can still define it as described in 2).
	
FAQ
---

Q: Is there a performance penalty in using this library?

A: Normally not, because all these constraints are resolved at compile time and code is inlined so on the contrary there might be eventually some speed up at run-time. On the other hand, the more overloads the more pressure on the compiler, this project will take several minutes to compile but this doesn't mean that by linking the FsControl dll to an application the compile time of the application will slow down. It will slow down depending on how much generic code (inlined) uses.

Q: What about the generic abstractions? Do they mean that by having a generic solution the code will be less efficient?

A: In many cases yes, but in FsControl the most generic code is in 'default methods' and normally the overloads define specialized methods which are optimized for specific instances.

Q: Where can I find more information about the abstractions provided?

A: There are many posts on Haskell everywhere, of those some are very formal and other are more intuitive. Apart from that in the last years there were some F# intuitive explanations in different blogs about the same abstractions implemented in F# in a non-generic way.

Q: Is this a Haskell emulator?

A: No, there some abstractions specifics to F#, however it's true that this library (as many others F# libs) is heavily inspired in concepts coming from Haskell but as F# is another language with another type system, strict evaluation and some different conventions there are many differences in names, types and implementations. Also there are some specific F# abstractions. Anyway by knowing those differences you may be able to translate Haskell code to F#. There is a sample file which mimics basic haskell functions and types.

Q: How can I contribute to this library?

A: You can review the code, find better implementations of specific instances, add missing instances for primitive types, add/propose new type-methods, add sample files an so on. Finding issues, making suggestions, giving feedback, discussion in general is also welcome.
