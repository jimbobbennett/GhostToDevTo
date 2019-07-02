module MarkdownRenderer

    open System.Text
    open MobileDoc
    open Utils
    open Newtonsoft.Json

    type CardMarkdown = 
        {
            cardName : string
            markdown : string
        }

    type CardImage = 
        {
            src : string
            caption : string
        }

    type CardCode = 
        {
            language : string
            code : string
        }

    let normalizeNewLines (s : string) =
        s.Replace("\\n", "\n")

    let processMarkdown card =
        let cardMarkdown = JsonConvert.DeserializeObject<CardMarkdown>(card.payload)
        normalizeNewLines cardMarkdown.markdown

    let processImage card =
        let cardImage = JsonConvert.DeserializeObject<CardImage>(card.payload)
        sprintf "![%s](%s)" cardImage.caption cardImage.src

    let processCode card =
        let cardCode = JsonConvert.DeserializeObject<CardCode>(card.payload)
        sprintf "```%s\n%s\n```" cardCode.language cardCode.code

    let renderCardPayload card = 
        match card.name with
        | "image" -> processImage card
        | "code" -> processCode card
        | "hr" -> "<hr/>"
        | "card-markdown" -> processMarkdown card
        | "markdown" -> processMarkdown card
        | _ -> failwith (sprintf "Card type %s not supported" card.name)

    let renderTextMarker (tm : TextMarker) =
        // handle open and closing tags
        tm.value

    let renderMarker marker : string =
        match marker with
        | Atom m -> ""
        | Text m -> renderTextMarker m

    let renderMarkers markers =
        markers 
            |> Seq.map(renderMarker)
            |> String.concat ""

    let renderMarkupSection (markup:MarkupSection) =
        let section = match markup.tagName with
                      | H1 -> "\n# "
                      | H2 -> "\n## "
                      | H3 -> "\n### "
                      | H4 -> "\n#### "
                      | H5 -> "\n##### "
                      | H6 -> "\n###### "
                      | Aside -> ""
                      | Blockquote -> "> "
                      | P -> "\n"

        section + (renderMarkers markup.markers) + "\n"

    let renderListMarkupSection (l : ListSection) =
        let delimiter = match l.tagName with
                        | OL -> "1. "
                        | UL -> "* "

        l.listMarkers
            |> Seq.map (fun x -> delimiter + renderMarkers x.markers)
            |> String.concat "\n"

    let renderSection section (mobileDoc : MobileDoc) =
        match section with
        | Card c -> 
            let card = mobileDoc.cards.[c.cardIndex]
            renderCardPayload card
        | List l -> 
            renderListMarkupSection l
        | Image i -> sprintf "![](%s)" i.src
        | Markup m -> 
            renderMarkupSection m

    let renderToMarkdown (mobileDoc : MobileDoc) =
        let sb = StringBuilder()

        mobileDoc.sections
            |> Seq.map (fun s -> renderSection s mobileDoc)
            |> Seq.iter (fun s -> sb.AppendLine(s) |> ignore)

        sb.ToString()