#r "../_lib/Fornax.Core.dll"
#load "../globals.fsx"
#if !FORNAX
#load "../loaders/pageloader.fsx"
#load "../loaders/globalloader.fsx"
#endif


open Html
open Globals.HTMLComponents

let injectWebsocketCode (webpage:string) =
    let websocketScript =
        """
        <script type="text/javascript">
          var wsUri = "ws://localhost:8080/websocket";
      function init()
      {
        websocket = new WebSocket(wsUri);
        websocket.onclose = function(evt) { onClose(evt) };
      }
      function onClose(evt)
      {
        console.log('closing');
        websocket.close();
        document.location.reload();
      }
      window.addEventListener("load", init, false);
      </script>
        """
    let head = "<head>"
    let index = webpage.IndexOf head
    webpage.Insert ( (index + head.Length + 1),websocketScript)

let layout (ctx : SiteContents) active bodyCnt =
    let pages = ctx.TryGetValues<Pageloader.Page> () |> Option.defaultValue Seq.empty
    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo> ()
    let ttl =
        siteInfo
        |> Option.map (fun si -> si.title)
        |> Option.defaultValue ""

    let menuEntries =
        pages
        |> Seq.map (fun p ->
            let cls = if p.title = active then "navbar-item is-active smooth-hover" else "navbar-item"
            a [Class cls; Href p.link] [!! p.title ]
        )
        |> Seq.toList

    html [Class "has-navbar-fixed-top"] [
        head [] [
            meta [CharSet "utf-8"]
            meta [Name "viewport"; Content "width=device-width, initial-scale=1"]
            link [Rel "icon"; Type "image/png"; Sizes "32x32"; Href "/images/favicon.png"]

            title [] [!! ttl]


            link [Rel "stylesheet"; Href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"]

            link [Rel "stylesheet"; Type "text/css"; Href ("/style/style.css")]
            link [Rel "stylesheet"; Type "text/css"; Href "https://cdn.jsdelivr.net/npm/@creativebulma/bulma-collapsible@1.0.4/dist/css/bulma-collapsible.min.css"]
            
            script [ Defer true; Src "https://kit.fontawesome.com/0d3e0ea7a6.js"; CrossOrigin "anonymous"][]
            script [ Src "/js/navbar.js"][]
        ]
        body [] [
            nav [Class "navbar is-fixed-top has-bg-darkblue"] [
                div [Class "navbar-brand"] [
                    a [Class "navbar-item"; Href "/"] [
                        img [Src ("/images/logo.png"); Alt "Logo"; Width "32"; Height "32"]
                    ]
                    a [
                        Class "navbar-burger"; 
                        Custom ("data-target", "navMenu"); 
                        Custom ("aria-label", "menu"); 
                        HtmlProperties.Role "button"
                        Custom ("aria-expanded", "false")
                    ] [
                        span [HtmlProperties.Custom ("aria-hidden","true")] []
                        span [HtmlProperties.Custom ("aria-hidden","true")] []
                        span [HtmlProperties.Custom ("aria-hidden","true")] []
                    ]
                ]
                div [Id "navMenu"; Class "navbar-menu has-bg-darkblue"] [
                    div [Class "navbar-start is-justify-content-center is-flex-grow-1"] menuEntries
                    div [Class "navbar-end"] [
                        a [Class "navbar-item"; Href "https://twitter.com/biofsharp"] [icon "fab fa-twitter"]
                        a [Class "navbar-item"; Href "https://github.com/biofsharp"] [icon "fab fa-github"]
                    ]
                ]
            ]
            yield! bodyCnt
        ]
        footer [Class "footer has-bg-orange-lighter-20"] [ 
            div [Class "container"] [
                div [Class "columns"] [
                    div [Class "column is-4 m-4"] [
                        block [ h3 [Class "subtitle is-white"] [!!"BioFSharp - The bioinformatics toolbox for F# and .NET"]]
                        block [ 
                            p [] [
                                !!"BioFSharp is developed and maintained by members of"
                                a [Href "https://www.nfdi.de/"] [!!"CSBiology"]
                                !!"and open source contributors"
                            ]
                        ]
                        block [ 
                            p [] [
                                !!"This website is developed and maintained by members of the "
                                a [] [!!"CSBiology"]
                                !!"and"
                                a [] [!!"BioFSharp"]
                                !!"organizations"
                            ]
                        ]
                    ]
                    div [Class "column is-4 m-4"] [
                        block [ h3 [Class "subtitle is-white"] [!!"Navigation"]]
                        ul [] [
                            block [li [] [a [Href "/"] [!!"Home"]]]
                            block [li [] [a [Href "/"] [!!"Projects"]]]
                            block [li [] [a [Href "/"] [!!"About"]]]
                        ]
                    ]
                    div [Class "column is-4 m-4"] [
                        block [ h3 [Class "subtitle is-white"] [!!"Social"]]
                        block [ whiteIcon "fab fa-twitter"; a [Href "https://twitter.com/biofsharp"] [!!"BioFSharp on Twitter"]]
                        block [ whiteIcon "fab fa-github"; a [Href "https://github.com/biofsharp"] [!!"BioFSharp open source projects on GitHub"]]
                    ]
                ]
            ]
        ]
            
    ]

let render (ctx : SiteContents) cnt =
    cnt
    |> HtmlElement.ToString
    #if WATCH
    |> injectWebsocketCode 
    #endif
