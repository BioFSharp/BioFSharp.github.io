#r "../_lib/Fornax.Core.dll"
#load "layout.fsx"

open Html
open Globals.HTMLComponents

let generate' (ctx : SiteContents) (_: string) =
    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo> ()
    let desc =
        siteInfo
        |> Option.map (fun si -> si.description)
        |> Option.defaultValue ""

    Layout.layout ctx "Home" [
        section [Class "hero is-medium"] [
            div [Class "hero-body"] [
                div [Class "container has-text-centered"] [
                    figure [Class "image"] [
                        img [Src "images/logo-large.svg"]
                    ]
                    p [Class "title"] [!!"The bioinformatics toolbox for F# and .NET"]
                ]
            ]
        ]
    ]

let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
  generate' ctx page
  |> Layout.render ctx