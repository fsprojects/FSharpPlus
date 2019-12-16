module Types

open System
open Fable.React
open Fable.React.Props
open FSharp.MetadataFormat
open PartMembers
open Helpers

let generateTypeDocs (model : TypeInfo) (props) =
    let members = model.Type.AllMembers
    let comment = model.Type.Comment
    let ``type`` = model.Type
    let byCategory =
        members
        |> List.groupBy (fun m -> m.Category)
        |> List.sortBy (fun (k,v) -> if String.IsNullOrEmpty(k) then "ZZZ" else k )
        |> List.mapi (fun i (k,v) -> {
            Index = i
            GroupKey = k
            Members = v |> List.sortBy (fun m -> if m.Kind = MemberKind.StaticParameter then "" else m.Name)
            Name = if String.IsNullOrEmpty(k) then "Other type members" else k
        })
    [
        yield h1 [] [
            str model.Type.Name
        ]

        yield p [] [
            yield! renderObsoleteMessage model.Type
            yield! renderNamespace model.Namespace
            if model.HasParentModule then
                yield br []
                yield span [] [
                    str "Parent Module: "

                    a [
                        Href (sprintf "%s.html" model.ParentModule.Value.UrlName)
                    ] [
                        str model.ParentModule.Value.Name
                    ]
                ]


            if ``type``.Attributes |> Seq.isEmpty |> not then
                yield br []
                yield span [] [
                    yield str "Attributes: "

                    yield br []

                    for attr in ``type``.Attributes do
                        yield str (attr.Format())
                        yield br []
                ]
        ]

        yield div [
            Class "xmldoc"
        ] [
            for sec in comment.Sections do
                if byCategory |> Seq.exists (fun m -> m.GroupKey = sec.Key) |> not then
                    if sec.Key <> "<default>" then
                        yield h2 [] [
                            str sec.Key
                        ]
                    yield RawText sec.Value
        ]

        if byCategory |> Seq.length > 1 then
            yield h2 [] [
                str "Table of contents"
            ]

            yield ul [] [
                for g in byCategory do
                    yield li [] [
                        a [
                            Href (sprintf "#section%d" g.Index)
                        ] [
                            str g.Name
                        ]
                    ]
            ]

        for g in byCategory do
            if byCategory |> Seq.length > 1 then
                yield h2 [] [
                    str g.Name

                    a [
                        Name (sprintf "section%d" g.Index)
                    ] [
                        str "&#160;"
                    ]
                ]

                match comment.Sections |> Seq.tryFind (fun kvp -> kvp.Key = g.GroupKey) with
                | Some info ->
                    yield div [
                        Class "xmldoc"
                    ] [
                        str info.Value
                    ]
                | None -> yield nothing

            yield! partMembers "Union Cases" "Union Case" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.UnionCase))
            yield! partMembers "Record Fields" "Record Field" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.RecordField))
            yield! partMembers "Static parameters" "Static parameters" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.StaticParameter))
            yield! partMembers "Contructors" "Constructor" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.Constructor))
            yield! partMembers "Instance members" "Instance member" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.InstanceMember))
            yield! partMembers "Static members" "Static member" (g.Members |> Seq.filter(fun m -> m.Kind = MemberKind.StaticMember))
    ]
