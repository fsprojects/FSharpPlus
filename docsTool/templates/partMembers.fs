module PartMembers

open System
open Fable.React
open Fable.React.Props
open FSharp.MetadataFormat
open System.Collections.Generic
open Helpers

type ModuleByCategory = {
    Index : int
    GroupKey : string
    Members : list<Member>
    Name : string
}


let signature (m : Member) = seq {
    if m.Details.Signature  |> String.IsNullOrEmpty |> not then
        yield
            code [ Class "function-or-value"] [
                str m.Details.Signature
            ]
}

let repoSourceLink (m: Member) = seq {
    if m.Details.FormatSourceLocation |> String.IsNullOrEmpty |> not then
        yield a [
            Href m.Details.FormatSourceLocation
            Class "float-right"
            HTMLAttr.Custom("aria-label", "View source on GitHub")
        ] [
            yield i [
                Class "fab fa-github text-dark"
            ] []
        ]
}

let replaceh2withh5 (content : string) =
    content.Replace("<h2>", "<h2 class=\"h5\">")


let normalize (content : string) =
    content
    |> replaceh2withh5



let commentBlock (c: Comment) =
    let (|EmptyDefaultBlock|NonEmptyDefaultBlock|Section|) (KeyValue(section, content)) =
        match section, content with
        | "<default>", c when String.IsNullOrEmpty c -> EmptyDefaultBlock
        | "<default>", c -> NonEmptyDefaultBlock c
        | section, content -> Section (section, content)

    let renderSection (s : KeyValuePair<string,string>): Fable.React.ReactElement list =
        match s with
        | EmptyDefaultBlock -> []
        | NonEmptyDefaultBlock content -> [ div [ Class "comment-block" ] [ RawText (normalize content)  ] ]
        | Section(name, content) -> [ h5 [] [ str name ] // h2 is obnoxiously large for this context, go with the smaller h5
                                      RawText (normalize content) ]
    c.Sections
    |> List.collect renderSection

let compiledName (m: Member) = seq {
    if m.Details.FormatCompiledName |> String.IsNullOrEmpty |> not then
        yield p [] [
            strong [] [ str "CompiledName:" ]
            code [] [ str m.Details.FormatCompiledName ]
        ]
}

let partMembers (header : string) (tableHeader : string) (members : #seq<Member>) = [
    if members |> Seq.length > 0 then
        yield h3 [] [
            str header
        ]

        yield table [
            Class "table"
        ] [
            thead [] [

                tr [] [
                    th [Class "fit"] [

                    ]
                    th [] [
                        str tableHeader
                    ]

                    th [] [
                        str "Signature"
                    ]

                    th [] [
                        str "Description"
                    ]
                ]
            ]
            tbody [] [
                for it in members do
                    let id = Guid.NewGuid().ToString()
                    yield tr [] [
                        td [] [
                                Helpers.createAnchorIcon (it.Details.FormatUsage(40))
                            ]
                        td [
                            Class "member-name"
                        ] [
                            code [
                                Class "function-or-value"
                                HTMLAttr.Custom("data-guid", id)
                            ] [
                                str (it.Details.FormatUsage(40))
                            ]
                        ]
                        td [
                            Class "member-name"
                        ] [
                            yield! signature it
                        ]

                        td [
                            Class "xmldoc"
                        ] [
                            yield! renderObsoleteMessage it
                            yield! repoSourceLink it
                            yield! commentBlock it.Comment
                            yield! compiledName it
                        ]
                    ]
            ]
        ]
]
