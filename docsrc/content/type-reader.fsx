(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"../../src/FSharpPlus/bin/Release/netstandard2.0/FSharpPlus.dll"
(**
Reader<'R,'T>
=============

The Reader monad is good for computations which read values from a shared environment.

Related Types
------------

 - [State](type-state.html): Similar, but it allows you to modify the environment.

Examples
--------
*)

(**
```f#
#r @"nuget: FSharpPlus"
```
*)

(**
One usage of the Reader monad is an alternative to dependency injection or currying
in order to pass around dependencies. The below code comes from [F# Online - Josef Starýchfojtů - FSharpPlus - Advanced FP concepts in F#](https://www.youtube.com/watch?v=pxJCHJgG8ws). You can find the presenter on github as [@starychfojtu](https://github.com/starychfojtu).

Why would you want to do this style?

- When you want to pass around a single environment instead of using dependency injection.

Why wouldn't you want to use this style?

- The downside of this style is that it supposes that your environment is relatively immutable. If you have different lifetimes for different implementation classes dependency injection frameworks can be easier to use.

Note:

- The # in ``(env : #IUserRepository)`` is a [flexible type annotation](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/flexible-types).
*)
open System
open FSharpPlus
open FSharpPlus.Data

type IUserRepository =
    abstract GetUser : email : string -> string

type IShoppingListRepository =
    abstract AddToCart : shoppingList : string list -> string

let getUser email =
    Reader(fun (env : #IUserRepository) -> env.GetUser email)

let addToShoppingList shoppingListItems =
    Reader(fun (env : #IShoppingListRepository) -> env.AddToCart shoppingListItems)

let addShoppingListM email = monad {
    let! user = getUser email
    // 
    let shoppingListItems = ["Apple"; "Pear";]
    return! addToShoppingList shoppingListItems
}

type MockDataEnv() = // This is how an environment could be constructed
    interface IUserRepository with
        member this.GetUser email =
                "Sandeep"
    interface IShoppingListRepository with
            member this.AddToCart shoppingListItems =
                sprintf "Added the following items %A to the cart" shoppingListItems

Reader.run (addShoppingListM "sandeep@test.com")  (MockDataEnv())

(**
Sample from [The Reader monad on Haskell Wiki](https://wiki.haskell.org/All_About_Monads#The_Reader_monad)
*)
open System
open FSharpPlus
open FSharpPlus.Data

/// This the abstract syntax representation of a template
type Template =
    /// Text
    | T of string
    /// Variable
    | V of Template
    /// Quote
    | Q of Template
    /// Include
    | I of Template*(Definition list)
    /// Compound
    | C of Template list
and Definition = | D of Template*Template

/// Our environment consists of an association list of named templates and
/// an association list of named variable values.
type Environment = {templates: Map<string,Template>
                    variables: Map<string,string>}

/// lookup a variable from the environment
let lookupVar (name:string) (env:Environment) : string option = tryItem name env.variables

/// lookup a template from the environment
let lookupTemplate (name:string) (env:Environment) : Template option = tryItem name env.templates

/// add a list of resolved definitions to the environment
let addDefs (defs:(string*string) list) env = { env with variables = plus (Map.ofList defs) env.variables}

/// resolve a template into a string
let rec resolve : Template -> Reader<Environment,string>  = function 
                       | T s -> result s
                       | V t -> monad {
                                   let! varName = resolve t
                                   let! env = ask
                                   let varValue = lookupVar varName env
                                   return option id "" varValue }
                        | Q t -> monad {
                                   let! tmplName = resolve t
                                   let! env = ask
                                   let body = lookupTemplate tmplName env
                                   return option string "" body }
                        | I (t,ds) -> monad {
                                    let! tmplName = resolve t
                                    let! env = ask
                                    let body = lookupTemplate tmplName env
                                    match body with
                                    | Some t' ->
                                                let! defs = List.traverse resolveDef ds
                                                return! local (addDefs defs) (resolve t')
                                    | None -> return ""
                                    }
                        | C ts   -> monad {
                                      let! resolved = List.traverse resolve ts
                                      return String.Concat<string> resolved
                                    }
and
   /// resolve a Definition and produce a (name,value) pair
   resolveDef: Definition -> Reader<Environment,string*string> = 
                                      function 
                                      | D (t,d) -> monad {
                                        let! name = resolve t
                                        let! value = resolve d
                                        return (name,value) }

