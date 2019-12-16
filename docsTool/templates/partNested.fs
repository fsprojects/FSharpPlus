module PartNested

open System
open Fable.React
open Fable.React.Props
open FSharp.MetadataFormat
open Helpers

let partNested (types : Type array) (modules : Module array) =
    [
        if types.Length > 0 then
            yield table [ Class "table" ] [
                thead [] [
                    tr [] [
                        th [Class "fit"] [

                        ]
                        th [] [
                            str "Type"
                        ]
                        th [] [
                            str "Description"
                        ]
                    ]
                ]
                tbody [] [
                    for t in types do
                        yield tr [] [
                            td [] [
                                Helpers.createAnchorIcon t.Name
                            ]
                            td [Class "type-name"] [
                                a [Href (sprintf "%s.html" t.UrlName)] [
                                    str t.Name
                                ]
                            ]
                            td [Class "xmldoc"] [
                                yield! renderObsoleteMessage t
                                yield RawText t.Comment.Blurb
                            ]
                        ]
                ]
            ]
        if modules.Length > 0 then
            yield table [ Class "table" ] [
                thead [] [
                    tr [] [
                        th [Class "fit"] [

                        ]
                        th [] [
                            str "Module"
                        ]
                        th [] [
                            str "Description"
                        ]
                    ]
                ]
                tbody [] [
                    for t in modules do
                        yield tr [] [
                            td [] [
                                Helpers.createAnchorIcon t.Name
                            ]
                            td [Class "Modules-name"] [
                                a [Href (sprintf "%s.html" t.UrlName)] [
                                    str t.Name
                                ]
                            ]
                            td [Class "xmldoc"] [
                                yield! renderObsoleteMessage t
                                yield RawText t.Comment.Blurb
                            ]
                        ]
                ]
            ]
    ]
