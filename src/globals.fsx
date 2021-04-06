#r "_lib/Fornax.Core.dll"
#r "_lib/Markdig.dll"

open System.IO
open Markdig

module Colors =

    let validCSBColorNames = 
        [
            "darkblue"           
            "darkblue-darker-10" 
            "darkblue-darker-20" 
            "darkblue-darker-30" 
            "darkblue-darker-40" 
            "darkblue-darker-50" 
            "darkblue-darker-60" 
            "darkblue-darker-70" 
            "darkblue-darker-80" 
            "darkblue-darker-90" 
            "darkblue-lighter-10"
            "darkblue-lighter-20"
            "darkblue-lighter-30"
            "darkblue-lighter-40"
            "darkblue-lighter-50"
            "darkblue-lighter-60"
            "darkblue-lighter-70"
            "darkblue-lighter-80"
            "darkblue-lighter-90"
            "orange"             
            "orange-darker-10"   
            "orange-darker-20"   
            "orange-darker-30"   
            "orange-darker-40"   
            "orange-darker-50"   
            "orange-darker-60"   
            "orange-darker-70"   
            "orange-darker-80"   
            "orange-darker-90"   
            "orange-lighter-10"  
            "orange-lighter-20"  
            "orange-lighter-30"  
            "orange-lighter-40"  
            "orange-lighter-50"  
            "orange-lighter-60"  
            "orange-lighter-70"  
            "orange-lighter-80"  
            "orange-lighter-90"  
            "grey"               
            "grey-darker-10"     
            "grey-darker-20"     
            "grey-darker-30"     
            "grey-darker-40"     
            "grey-darker-50"     
            "grey-darker-60"     
            "grey-darker-70"     
            "grey-darker-80"     
            "grey-darker-90"     
            "grey-lighter-10"    
            "grey-lighter-20"    
            "grey-lighter-30"    
            "grey-lighter-40"    
            "grey-lighter-50"    
            "grey-lighter-60"    
            "grey-lighter-70"    
            "grey-lighter-80"    
            "grey-lighter-90"    
            "yellow"             
            "yellow-darker-10"   
            "yellow-darker-20"   
            "yellow-darker-30"   
            "yellow-darker-40"   
            "yellow-darker-50"   
            "yellow-darker-60"   
            "yellow-darker-70"   
            "yellow-darker-80"   
            "yellow-darker-90"   
            "yellow-lighter-10"  
            "yellow-lighter-20"  
            "yellow-lighter-30"  
            "yellow-lighter-40"  
            "yellow-lighter-50"  
            "yellow-lighter-60"  
            "yellow-lighter-70"  
            "yellow-lighter-80"  
            "yellow-lighter-90"  
            "blue"               
            "blue-darker-10"     
            "blue-darker-20"     
            "blue-darker-30"     
            "blue-darker-40"     
            "blue-darker-50"     
            "blue-darker-60"     
            "blue-darker-70"     
            "blue-darker-80"     
            "blue-darker-90"     
            "blue-lighter-10"    
            "blue-lighter-20"    
            "blue-lighter-30"    
            "blue-lighter-40"    
            "blue-lighter-50"    
            "blue-lighter-60"    
            "blue-lighter-70"    
            "blue-lighter-80"    
            "blue-lighter-90"    
            "green"              
            "green-darker-10"    
            "green-darker-20"    
            "green-darker-30"    
            "green-darker-40"    
            "green-darker-50"    
            "green-darker-60"    
            "green-darker-70"    
            "green-darker-80"    
            "green-darker-90"    
            "green-lighter-10"   
            "green-lighter-20"   
            "green-lighter-30"   
            "green-lighter-40"   
            "green-lighter-50"   
            "green-lighter-60"   
            "green-lighter-70"   
            "green-lighter-80"   
            "green-lighter-90"   
        ] 
        |> Set.ofList

    let isValidCSBColorName n = validCSBColorNames.Contains(n)
    
    let hasBgColor cName = 
        if isValidCSBColorName cName then
            sprintf "has-bg-%s" cName
        else failwithf "%s is not a valid color from the NFDI color palette." cName

    let hasBorderColor cName = 
        if isValidCSBColorName cName then
            sprintf "has-border-color-%s" cName
        else failwithf "%s is not a valid color from the NFDI color palette." cName

    let isColor cName = 
        if isValidCSBColorName cName then
            sprintf "is-%s" cName
        else failwithf "%s is not a valid color from the NFDI color palette." cName

module HTMLComponents =

    open Html

    let whiteIcon iconClass = 
        span [Class"icon is-white"] [
            i [Class iconClass][]
        ]

    let icon iconClass = 
        span [Class"icon"] [
            i [Class iconClass][]
        ]

    let iconTextRight iconClass text = 
        span [Class "icon-text"][
            icon iconClass
            span [] [!!text]
        ]

    let iconTextLeft iconClass text = 
        span [Class "icon-text"][
            span [] [!!text]
            icon iconClass
        ]

    let block children = 
        div [Class "block"] children

module MarkdownProcessing =

    let markdownPipeline =
        MarkdownPipelineBuilder()
            .UsePipeTables()
            .UseGridTables()
            .UseGenericAttributes()
            .UseEmphasisExtras()
            .UseListExtras()
            .UseCitations()
            .UseCustomContainers()
            .UseFigures()
            .Build()

    let isFrontMatterSeparator (input : string) =
        input.StartsWith "---"

    let trimString (str : string) =
        str.Trim().TrimEnd('"').TrimStart('"')

    ///`fileContent` - content of page to parse. Usually whole content of `.md` file
    ///returns content of config that should be used for the page
    let getFrontMatter (fileContent : string) =
        let fileContent = 
            fileContent.Split '\n'
            |> Array.skip 1 //First line must be ---

        let indexOfSeperator = fileContent |> Array.findIndex isFrontMatterSeparator

        let splitKey (line: string) = 
            let seperatorIndex = line.IndexOf(':')
            if seperatorIndex > 0 then
                let key = line.[.. seperatorIndex - 1].Trim().ToLower()
                let value = line.[seperatorIndex + 1 ..].Trim() 
                Some(key, trimString value)
            else 
                None

        fileContent
        |> Array.splitAt indexOfSeperator
        |> fst
        |> Seq.choose splitKey
        |> Map.ofSeq        

        
    ///`fileContent` - content of page to parse. Usually whole content of `.md` file
    ///returns HTML version of content of the page
    let getMarkdownContent (fileContent : string) =
        let fileContent = fileContent.Split '\n'
        let fileContent = fileContent |> Array.skip 1 //First line must be ---
        let indexOfSeperator = fileContent |> Array.findIndex isFrontMatterSeparator
        let content = 
            fileContent 
            |> Array.splitAt indexOfSeperator
            |> snd
            |> Array.skip 1 
            |> String.concat "\n"

        Markdown.ToHtml(content, markdownPipeline)

module Predicates =
        
    let learnMoreHeroPredicate (projectRoot: string, page: string) =
        let fileName = Path.GetFileNameWithoutExtension page
        let ext = Path.GetExtension page
        page.Contains("learn-more")
        && fileName = "hero"
        && ext = ".md"

    let isLearnMoreHero f = learnMoreHeroPredicate("",f)

    let markdownPredicate (projectRoot: string, page: string) =
        let ext = Path.GetExtension page
        let fileName = Path.GetFileNameWithoutExtension page
        fileName.ToUpperInvariant() <> "README"
        && ext = ".md"

    let isMarkdownFile f = markdownPredicate ("",f)

    let newsPredicate (projectRoot: string, page: string) = 
        let ext = Path.GetExtension page
        page.Contains("content")
        && page.Contains("news")
        && ext = ".md"
        && (not (page.Contains "_public"))

    let isNewsFile f = newsPredicate("",f)

    let staticPredicate (projectRoot: string, page: string) =
        let ext = Path.GetExtension page
        if page.Contains "_public" ||
           page.Contains "_bin" ||
           page.Contains "content" ||
           page.Contains "_lib" ||
           page.Contains "_data" ||
           page.Contains "_settings" ||
           page.Contains "_config.yml" ||
           page.Contains ".sass-cache" ||
           page.Contains ".git" ||
           page.Contains ".ionide" ||
           page.Contains "configuration" ||
           page.Contains "bulma-0.9.1" ||
           page.Contains "style_src" ||
           ext = ".fsx" ||
           ext = ".scss"
        then
            false
        else
            true