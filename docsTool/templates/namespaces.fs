module Namespaces

open System
open Fable.React
open Fable.React.Props
open FSharp.MetadataFormat


type ByCategory = {
    Name : string
    Index : string
    Types : Type array
    Modules : Module array
}

let generateNamespaceDocs (asm : AssemblyGroup) (props) =
    let parts =
        asm.Namespaces
        |> Seq.mapi(fun nsi ns ->
            let allByCategories =
                ns.Types
                |> Seq.map(fun t -> t.Category)
                |> Seq.append (ns.Modules |> Seq.map(fun m -> m.Category))
                |> Seq.distinct
                |> Seq.sortBy(fun s ->
                    if String.IsNullOrEmpty(s) then "ZZZ"
                    else s)
                |> Seq.mapi(fun ci c ->
                {
                    Name = if String.IsNullOrEmpty(c) then "Other namespace members" else c
                    Index = sprintf "%d_%d" nsi ci
                    Types = ns.Types |> Seq.filter(fun t -> t.Category = c) |> Seq.toArray
                    Modules = ns.Modules |> Seq.filter(fun m -> m.Category = c) |> Seq.toArray
                })
                |> Seq.filter(fun c -> c.Types.Length + c.Modules.Length > 0)
                |> Seq.toArray
            [
                yield h2 [] [
                    Helpers.createAnchor ns.Name ns.Name
                ]
                if allByCategories.Length > 1 then
                    yield ul [] [
                        for c in allByCategories do
                            yield
                                li [] [
                                    a [Href (sprintf "#section%s" c.Index)] [
                                        str c.Name
                                    ]
                                ]
                    ]


                for c in allByCategories do
                    if allByCategories.Length > 1 then
                        yield h3 [] [
                            a  [Class "anchor"; Name (sprintf "section%s" c.Index); Href (sprintf "#section%s" c.Index)] [
                                str c.Name
                            ]
                        ]
                    yield! PartNested.partNested c.Types c.Modules
            ]
        )
        |> Seq.collect id
    div [ Class "container-fluid py-3" ] [
        div [ Class "row" ] [
            div [ Class "col-12" ] [
                yield h1 [] [
                    Helpers.createAnchor asm.Name asm.Name
                ]
                yield! parts
            ]
        ]
    ]
