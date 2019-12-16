module Master

open System
open Fable.React
open Fable.React.Props
open DocsTool

type MasterTemplateConfig = {
    SiteBaseUrl : Uri
    GitHubRepoUrl : Uri
    ProjectName : string
    ReleaseVersion : string
    ReleaseDate : DateTimeOffset
    RepositoryRoot: IO.DirectoryInfo
}

type FAIcon =
| Solid of name: string
| Brand of name: string

let footerLink uri image linkText =
    let faClass, img =
        match image with
        | Solid name -> "fas", name
        | Brand name -> "fab", name
    a [Href uri; Class "text-white"] [
        i [Class (sprintf "%s fa-%s fa-fw mr-2" faClass img)] []
        str linkText
    ]

let repoFileLink repoUrl filePathFromRepoRoot =
    let link = repoUrl |> Uri.simpleCombine (sprintf "blob/master/%s" filePathFromRepoRoot)
    footerLink link

let linkColumn headerTitle items =
    div [Class "col-12 col-md-4 mb-4 mb-md-0"] [
        div [Class "text-light"] [
            h2 [Class "h5"] [ str headerTitle ]
            ul [Class "list-group list-group-flush"]
                (items |> List.choose (function | [] -> None
                                                | items -> Some(li [Class "list-group-item list-group-item-dark ml-0 pl-0"] items)))
        ]
    ]

let renderFooter (cfg : MasterTemplateConfig) (pageSource : string option) =
    let hasFile relPath =
        match cfg.RepositoryRoot.GetFiles(relPath) with
        | [||] -> false
        | [|file|] -> true
        | files -> false

    let repoFileLink relPath image title =
        if hasFile relPath
        then [ repoFileLink cfg.GitHubRepoUrl relPath image title ]
        else []

    footer [Class "footer font-small m-0 py-4 bg-dark"] [
        div [Class "container"] [
            div [Class "row"] [
                linkColumn "Project Resources" [
                    repoFileLink "README.md" (Solid "book-reader") "README"
                    repoFileLink "RELEASE_NOTES.md" (Solid "sticky-note") "Release Notes / Changelog"
                    repoFileLink "LICENSE.md" (Solid "id-card") "License"
                    repoFileLink "CONTRIBUTING.md" (Solid "directions") "Contributing"
                    repoFileLink "CODE_OF_CONDUCT.md" (Solid "users") "Code of Conduct"
                ]
                linkColumn "Other Links" [
                    [footerLink "https://docs.microsoft.com/en-us/dotnet/fsharp/" (Brand "microsoft") "F# Documentation"]
                    [footerLink "https://fsharp.org/guides/slack/" (Brand "slack") "F# Slack"]
                    [a [Href "http://foundation.fsharp.org/"; Class "text-white"] [
                        img [Class "fsharp-footer-logo mr-2"; Src "https://fsharp.org/img/logo/fsharp.svg"; Alt "FSharp Logo"]
                        str "F# Software Foundation"
                    ]]
                ]
                linkColumn "Metadata" [
                    [str "Generated for version "
                     a [Class "text-white"; Href (cfg.GitHubRepoUrl |> Uri.simpleCombine (sprintf "releases/tag/%s" cfg.ReleaseVersion))] [str cfg.ReleaseVersion]
                     str (sprintf " on %s" (cfg.ReleaseDate.ToString("yyyy/MM/dd")))]
                    match pageSource with
                    | Some p ->
                        let page = cfg.GitHubRepoUrl |> Uri.simpleCombine "edit/master" |> Uri |> Uri.simpleCombine p
                        [ str "Found an issue? "
                          a [Class "text-white"; Href (page |> string)] [ str "Edit this page." ] ]
                    | None ->
                        ()
                ]
            ]
            div [Class "row"] [
                div [Class "col text-center"] [
                    small [Class "text-light"] [
                        i [Class "fas fa-copyright mr-1"] []
                        str (sprintf "%s MyCoolNewLib, All rights reserved" (DateTimeOffset.UtcNow.ToString("yyyy")))
                    ]
                ]
            ]
        ]
    ]

let masterTemplate (cfg : MasterTemplateConfig) navBar titletext bodyText pageSource =
    html [Lang "en"] [
        head [] [
            title [] [ str (sprintf "%s docs / %s" cfg.ProjectName titletext) ]
            link [
                Href "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
                Rel "stylesheet"
                Integrity "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
                CrossOrigin "anonymous"
            ]
            link [
                Href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
                Rel "stylesheet"
                Integrity "sha384-KA6wR/X5RY4zFAHpv/CnoG2UW1uogYfdnP67Uv7eULvTveboZJg0qUpmJZb5VqzN"
                CrossOrigin "anonymous"
            ]
            link [
                Href (cfg.SiteBaseUrl |> Uri.simpleCombine (sprintf "/content/style.css?version=%i" cfg.ReleaseDate.Ticks) )
                Type "text/css"
                Rel "stylesheet"
            ]

        ]
        body [] [
            yield navBar
            yield div [Class "wrapper d-flex flex-column justify-content-between min-vh-100"] [
                main [Class "container main mb-4"] bodyText
                renderFooter cfg pageSource
            ]
            yield script [
                Src "https://code.jquery.com/jquery-3.4.1.slim.min.js"
                Integrity "sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n"
                CrossOrigin "anonymous"
                ] []
            yield script [
                Src "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
                Integrity "sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo"
                CrossOrigin "anonymous"
                ] []
            yield script [
                Src "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"
                Integrity "sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6"
                CrossOrigin "anonymous"
                ] []
            yield script [Src (cfg.SiteBaseUrl |> Uri.simpleCombine (sprintf "/content/tips.js?version=%i" cfg.ReleaseDate.Ticks)) ] []
            yield script [Src (cfg.SiteBaseUrl |> Uri.simpleCombine (sprintf "/content/hotload.js?version=%i" cfg.ReleaseDate.Ticks)) ] []
            yield script [Src (cfg.SiteBaseUrl |> Uri.simpleCombine (sprintf "/content/submenu.js?version=%i" cfg.ReleaseDate.Ticks)) ] []
        ]
    ]
