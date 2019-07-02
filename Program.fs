open System
open System.IO
open Newtonsoft.Json
open Ghost
open MobileDoc
open MarkdownRenderer

let getTags db (post : Post) =
    let tagIds = db.data.posts_tags
                    |> Seq.filter (fun t -> t.post_id = post.id)
                    |> Seq.map (fun t -> t.tag_id)

    db.data.tags
        |> Seq.where (fun t -> tagIds |> Seq.contains t.id)
        |> Seq.map (fun t -> t.name)
        |> Seq.toArray

let getPublishedDate post =
    let publishedDate = 
        let d = match post.published_at with
                | null -> post.created_at
                | _ -> post.published_at
        d.Split(" ")
            |> Seq.head
               
    let couldParse, _ = DateTime.TryParse(publishedDate)

    match couldParse with
    | true -> publishedDate
    | false -> let postTicks = float publishedDate
               DateTime(1970,1,1).AddMilliseconds(postTicks).ToString("yyyy-MM-dd")

let getFrontMatter post (tags : string[]) =
        let layout = match post.page with | 0 -> "post" |_ -> "default"
        let published = post.status = "published" && post.visibility = "public"
        let title = post.title.Replace(":", " -")
        let date = getPublishedDate post

        sprintf "---\nlayout: %s\ntitle: %s\npermalink: /%s\ndate: %s\npublished: %s\ntags: %s\n---" 
                    layout title post.slug date (published.ToString().ToLower()) (String.Join(", ", tags))

let getContent post frontMatter =
    let mobileDoc = parseMobileDoc post.mobiledoc
    let markdown = renderToMarkdown mobileDoc
    frontMatter + Environment.NewLine + Environment.NewLine + markdown

let write db post (directory : DirectoryInfo) fileName =
    let tags = getTags db post
    let frontMatter = getFrontMatter post tags
    let postContent = getContent post frontMatter
    let fullPath = Path.Combine(directory.FullName, fileName)
    File.WriteAllText(fullPath, postContent)

let writePost db post  (directory : DirectoryInfo) =
    let realDate = getPublishedDate post
    let fileName = realDate + "-" + post.slug + ".md"
    write db post directory fileName

let processDb db rootDir =
    let newPath = Path.Combine(rootDir, db.meta.exported_on.ToString())
    let dbDirectory = DirectoryInfo(newPath)
    if not dbDirectory.Exists then dbDirectory.Create()

    let postsPath = Path.Combine(dbDirectory.FullName, "_posts");
    let postDirectory = DirectoryInfo(postsPath);
    if not postDirectory.Exists then postDirectory.Create()

    db.data.posts
        |> Seq.filter (fun p -> p.page = 0)
        |> Seq.iter (fun p -> writePost db p postDirectory)

    db.data.posts
        |> Seq.filter (fun p -> p.page = 1)
        |> Seq.iter (fun p -> write db p dbDirectory (p.slug + ".md"))

let processFile file = 
    let content = File.ReadAllText(file)
    let export = JsonConvert.DeserializeObject<GhostExport>(content)
    let rootDir = Path.GetDirectoryName(file)
    export.db
        |> Seq.iter (fun d -> processDb d rootDir)

[<EntryPoint>]
let main argv =
    
    match argv.Length with
    | 1 -> processFile argv.[0]
           0
    | _ -> printf "Missing file argument"
           1

