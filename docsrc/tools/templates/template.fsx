#if INTERACTIVE
#load "../../../.paket/load/netstandard2.0/docs/Fable.React.fsx"
#else
module Template
#endif
open Fable.React
open Fable.React.Props
type PropertyMeta={Author: string ; Github:string; NuGet:string; Name:string ;Description:string; Title:string; Root:string->string; Body:string;}
let template (m:PropertyMeta) =
    fragment [] [
        RawText "<!DOCTYPE html>"
        RawText "\n"
        html [ Lang "en" ]
            [ head [ ]
                [ meta [ CharSet "utf-8" ]
                  title [ ]
                    [ str m.Title ]
                  meta [ Name "viewport"
                         HTMLAttr.Content "width=device-width, initial-scale=1.0" ]
                  meta [ Name "description"
                         HTMLAttr.Content m.Description ]
                  meta [ Name "author"
                         HTMLAttr.Content m.Author ]
                  script [ Src "https://code.jquery.com/jquery-1.8.0.js" ]
                    [ ]
                  script [ Src "https://code.jquery.com/ui/1.8.23/jquery-ui.js" ]
                    [ ]
                  script [ Src "https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js" ]
                    [ ]
                  link [ Href "https://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/css/bootstrap-combined.min.css"
                         Rel "stylesheet" ]
                  link [ Type "text/css"
                         Rel "stylesheet"
                         Href <| m.Root "/content/style.css" ]
                  script [ Type "text/javascript"
                           Src <| m.Root "/content/tips.js" ]
                    [ ] ]
              body [ ]
                [ div [ Class "container" ]
                    [ div [ Class "masthead" ]
                        [ ul [ Class "nav nav-pills pull-right" ]
                            [ li [ ]
                                [ a [ Href "http://fsharp.org" ]
                                    [ str "fsharp.org" ] ]
                              li [ ]
                                [ a [ Href m.Github ]
                                    [ str "github page" ] ] ]
                          h3 [ Class "muted" ]
                            [ a [ Href <| m.Root "/index.html" ]
                                [ str m.Name ] ] ]
                      hr [ ]
                      div [ Class "row" ]
                        [ div [ Class "span9"
                                Id "main" ]
                            [ RawText m.Body ]
                          div [ Class "span3" ]
                            [ img [ Src <| m.Root "/img/logo-color.svg"
                                    Alt "F# Project"
                                    Style [ Width "150px"
                                            Margin "10px" ] ]
                              ul [ Class "nav nav-list"
                                   Id "menu"
                                   Style [ MarginTop "20px" ] ]
                                [ li [ Class "nav-header" ]
                                    [ str m.Name ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/index.html" ]
                                        [ str "Home page" ] ]
                                  li [ Class "divider" ]
                                    [ ]
                                  li [ ]
                                    [ a [ Href m.NuGet ]
                                        [ str "Get Library via NuGet" ] ]
                                  li [ ]
                                    [ a [ Href m.Github ]
                                        [ str "Source Code on GitHub" ] ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/license.html" ]
                                        [ str "License" ] ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/release-notes.html" ]
                                        [ str "Release Notes" ] ]
                                  li [ Class "nav-header" ]
                                    [ str "Getting started" ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/tutorial.html" ]
                                        [ str "Tutorial" ] ]
                                  li [ Class "nav-header" ]
                                    [ str "Documentation" ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/reference/index.html" ]
                                        [ str "API Reference" ] ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/abstractions.html" ]
                                        [ str "Abstractions" ] ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/computation-expressions.html" ]
                                        [ str "Computation Expressions" ] ]
                                  li [ ]
                                    [ a [ Href <| m.Root "/numerics.html" ]
                                        [ str "Numerics" ] ] ] ] ] ]
                  a [ Href m.Github ]
                    [ img [ Style [ Position PositionOptions.Absolute
                                    Top "0"
                                    Right "0"
                                    Border "0" ]
                            Src "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
                            Alt "Fork me on GitHub" ] ] ] ] ]