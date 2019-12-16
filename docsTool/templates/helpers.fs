module Helpers
open System
open Fable.React
open Fable.React.Props
open FSharp.MetadataFormat


let createAnchorIcon name =
    let normalized = name
    let href = sprintf "#%s" normalized
    a [Href href; Id normalized] [
        str "#"
    ]

let createAnchor fullName name =
    let fullNameNormalize = fullName
    a [
        Name fullNameNormalize
        Href (sprintf "#%s" fullNameNormalize)
        Class "anchor"
    ] [
        str name
    ]

let renderNamespace (ns: Namespace) = [
    h3 [] [ str "Namespace" ]
    str ns.Name
]

let inline isObsolete< ^t when ^t : (member IsObsolete: bool)> t =
    (^t : (member IsObsolete: bool) (t))

let inline obsoleteMessage< ^t when ^t : (member ObsoleteMessage: string)> t =
    (^t : (member ObsoleteMessage:string) (t))

let inline renderObsoleteMessage item =
    if isObsolete item
    then
        let text = match obsoleteMessage item with | "" | null -> "This member is obsolete" | s -> s
        [
            div [Class "alert alert-warning"] [
                strong [] [ str "OBSOLETE: "]
                str text
            ]
        ]
    else
        []
