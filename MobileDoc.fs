module MobileDoc

    open Utils

    type Version =
        {
            version : string
        }

    type Atom =
        {
            atom : string
        }

    type Card = 
        {
            name : string
            payload : string
        }

    type MarkupAttribute =
        {
            key : string
            value : string
        }

    type MarkupTag =
        | HypertextLink
        | Bold
        | Code
        | Emphasis
        | Italic
        | StrikeThrough
        | Strong
        | Subscript
        | Superscript
        | Underline

    let parseMarkupTag s =
        match s with
        | "a" -> HypertextLink
        | "b" -> Bold
        | "code" -> Code
        | "em" -> Emphasis
        | "i" -> Italic
        | "s" -> StrikeThrough
        | "strong" -> Strong
        | "sub" -> Subscript
        | "sup" -> Superscript
        | "u" -> Underline
        | _ -> failwith (sprintf "Markup tag %s not support" s)

    type Markup = 
        {
            tag : MarkupTag
            optionalMarkup : MarkupAttribute
        }

    type MarkupSectionTagName = 
        | Aside
        | Blockquote
        | H1
        | H2
        | H3
        | H4
        | H5
        | H6
        | P

    let parseMarkupSectionTagName s =
        match s with
        | "aside" -> Aside
        | "blockquote" -> Blockquote
        | "h1" -> H1
        | "h2" -> H2
        | "h3" -> H3
        | "h4" -> H4
        | "h5" -> H5
        | "h6" -> H6
        | "p" -> P
        | _ -> failwith (sprintf "Section tag name %s not recognized" s)

    type ListSectionTagName = 
        | UL
        | OL

    let parseListSectionTagName s =
        match s with
        | "ul" -> UL
        | "ol" -> OL
        | _ -> failwith (sprintf "Section tag name %s not recognized" s)

    type AtomMarker = 
        {
            openMarkupsIndexes : int[]
            numberOfClosedMarkups : int
            atomIndex : int
        }

    type TextMarker = 
        {
            openMarkupsIndexes : int[]
            numberOfClosedMarkups : int
            value : string
        }

    type Marker =
        | Atom of AtomMarker
        | Text of TextMarker

    type MarkupSection = 
        {
            tagName : MarkupSectionTagName
            markers : Marker[]
        }

    type ListMarkers = 
        {
           markers : Marker[]
        }

    type ListSection = 
        {
            tagName : ListSectionTagName
            listMarkers : ListMarkers[]
        }

    type CardSection = 
        {
            cardIndex : int
        }

    type ImageSection = 
        {
            src : string
        }
 
    type Section = 
        | Markup of MarkupSection
        | List of ListSection
        | Card of CardSection
        | Image of ImageSection

    type MobileDoc =
        {
            version : Version
            atoms : Atom[]
            cards : Card[]
            markups : Markup[]
            sections : Section[]
        }

    type KeyValue =
        {
            key : string
            value : string
        }

    let readVersion (body : string) = 
        getKeyAndRemaining body

    let rec findClose (body : string) position openCount =
        match body.[position] with
        | '[' -> findClose body (position + 1) (openCount + 1)
        | ']' -> if (openCount > 1) then
                    findClose body (position + 1) (openCount - 1)
                 else
                    position
        | _ -> findClose body (position + 1) openCount

    let readBrackets (body : string) =
        let startIndex = body |> Seq.findIndex (fun c -> c = '[')
        let bracketsStuff = body.Substring(startIndex)
        let endIndex = findClose bracketsStuff 0 0

        let content = bracketsStuff.Substring(1, endIndex-1)
        let remain = bracketsStuff.Substring(endIndex + 1)
        (content, remain)

    let readCards content =
        let parseCardText (cardText : string) = 
            let splitterIndex = cardText |> Seq.findIndex (fun c -> c = ',')
            let name = cardText.Substring(0, splitterIndex)
                            .Trim()
                            .TrimStart('\"')
                            .TrimEnd('\"')

            let payload = cardText.Substring(splitterIndex + 1)
                            .Trim()
            { 
                name = name
                payload = payload 
            }

        let rec readAllCards (c : string) s =
            if c.Length = 0 then
                s
            else
                let cardText, rest = readBrackets c
                let card = parseCardText cardText
                let newValues = (Seq.singleton card) |> Seq.append s
                readAllCards rest newValues

        readAllCards content Seq.empty<Card> 

    let rec readAllMarkers (content : string) s =
        if content.Trim() = "" then
            s
        else
            let items, rest = readBrackets (content.Trim())
            if items = "" then
                s
            else
                let markerType, remainingMarkerValues = getKeyAndRemaining items
                match markerType with
                | "0" -> 
                    let omi, last2 = getKeyAndRemaining remainingMarkerValues
                    let nocm, value = getKeyAndRemaining last2
                    let m = Text { openMarkupsIndexes = getIntArray omi; numberOfClosedMarkups = int nocm; value = value.TrimStart('\"').TrimEnd('\"') }
                    let newValues = (Seq.singleton m) |> Seq.append s
                    readAllMarkers rest newValues
                | "1" ->
                    let omi, last2 = getKeyAndRemaining remainingMarkerValues
                    let nocm, ai = getKeyAndRemaining last2
                    let m = Atom { openMarkupsIndexes = getIntArray omi; numberOfClosedMarkups = int nocm; atomIndex = int ai }
                    let newValues = (Seq.singleton m) |> Seq.append s
                    readAllMarkers rest newValues
                | _ -> failwith (sprintf "Marker type %s no supported" markerType)
        

    let readMarkers (content : string) : Marker[]=
        let core = 
            if content.StartsWith("[[") then
                content.Substring(1, content.Length-2)
             else
                content
        readAllMarkers core Seq.empty<Marker>
            |> Seq.toArray

    let readListMarkers (content : string) : ListMarkers[]=
        
        let rec readAllGroups c s =
            if c = "" then 
                s
            else            
                let items, rest = readBrackets c
                let markers = readMarkers items
                let listMarker = {markers = markers}
                let newValues = (Seq.singleton listMarker) |> Seq.append s

                readAllGroups rest newValues

        let core = 
            if content.StartsWith("[[") then
                content.Substring(1, content.Length-2)
             else
                content

        let groups = readAllGroups core Seq.empty<ListMarkers>
        
        groups |> Seq.toArray


    let readSections content = 
        let parseSectionText (sectionText : string)= 
            let sectionType, remaining = getKeyAndValue sectionText

            match sectionType with
            | "1" -> 
                    let tagPart, rest = getKeyAndValue remaining
                    Markup { tagName = (parseMarkupSectionTagName tagPart); markers = (readMarkers rest) }
            | "2" -> 
                    Image { src = remaining.TrimStart('\"').TrimEnd('\"') }
            | "3" -> 
                    let tagPart, rest = getKeyAndValue remaining
                    List { tagName = (parseListSectionTagName tagPart); listMarkers = (readListMarkers rest) }
            | "10" -> 
                    Card { cardIndex =  int remaining }
            | _ -> failwith (sprintf "Section type %s not recognized" sectionType)

        let rec readAllSections (c : string) s =
            if c.Length = 0 then
                s
            else
                let sectionText, rest = readBrackets c
                let section = parseSectionText sectionText
                let newValues = (Seq.singleton section) |> Seq.append s
                readAllSections (rest.TrimStart(',').Trim()) newValues

        readAllSections content Seq.empty<Section> 

    let readMarkups content =
        let parseMarkupText (markupText : string) = 
            let splitterIndex = markupText |> Seq.findIndex (fun c -> c = ',')
            let name = markupText.Substring(0, splitterIndex)
                            .Trim()
                            .TrimStart('\"')
                            .TrimEnd('\"')
            let tag = parseMarkupTag name

            let payload = markupText.Substring(splitterIndex + 1)
                            .Trim()
            let key, value = getKeyAndValue payload                    
            { tag = tag; optionalMarkup = {key = key; value = value} }

        let rec readAllMarkups (m : string) s =
            if m.Length = 0 then
                s
            else
                let markupText, rest = readBrackets m
                let markup = parseMarkupText markupText
                let newValues = (Seq.singleton markup) |> Seq.append s
                readAllMarkups rest newValues

        readAllMarkups content Seq.empty<Markup> |> ignore
        Array.empty<Markup>

    let readValue key body =
        let value, after = match key with
                           | "version" -> readVersion body
                           | "atoms" -> readBrackets body
                           | "cards" -> readBrackets body
                           | "markups" -> readBrackets body
                           | "sections" -> readBrackets body
                           | _ -> ("", "")  
        value, after.TrimStart(',')                        

    let rec readNextKey (body : string) keyValues =
        let splitterIndex = body |> Seq.tryFindIndex (fun c -> c = ':')

        match splitterIndex with
        | None -> keyValues
        | Some x ->
            let key = body.Substring(0, x)
                          .Trim()
                          .TrimStart('\"')
                          .TrimEnd('\"')
                       
            let afterKey = body.Substring(x + 1)
                                    .Trim()
            
            let value, afterValue = readValue key afterKey

            let keyValue = 
                {
                    key = key
                    value = value
                }

            let newValues = (Seq.singleton keyValue) |> Seq.append keyValues
            readNextKey afterValue newValues

    let parseMobileDoc (mobileDoc : string) = 
        let body = mobileDoc.TrimStart('{').TrimEnd('}')

        let values = readNextKey body Seq.empty<KeyValue>

        let mobileDoc = 
            {
                version = { version = ""}
                atoms = Array.empty<Atom>
                cards = 
                        let cards = values |> Seq.find (fun kv -> kv.key = "cards")
                        readCards cards.value |> Seq.toArray
                markups = 
                        let markups = values |> Seq.find (fun kv -> kv.key = "markups")
                        readMarkups markups.value |> Seq.toArray
                sections = 
                        let sections = values |> Seq.find (fun kv -> kv.key = "sections")
                        readSections sections.value |> Seq.toArray
            }        
        
        mobileDoc