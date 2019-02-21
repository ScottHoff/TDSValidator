namespace ValidatorLib

open System
open FSharp.Data
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Text
open Microsoft.FSharp.Core.Printf

type ProjectReport = {
    name: string
    path: string
    sitecoreItemsCount: int
    fileSystemItemsCount: int
    duplicatesCount: int
    dupesList: string list
    missingFromFSCount: int
    missingFromFilesystem: string list
    missingFromProjectCount: int
    missingFromProject: string list
}

module Validator =
    let printWithHeader sb (header:string, items:string list) =
        match items.Length with
        | y when y > 1 -> 
            bprintf sb "%s %s\r\n" (header + ":") (Seq.head items)
            let spaces = String.replicate (header.Length + 1) " "
            Seq.tail items |> Seq.iter (bprintf sb "%s %s\r\n" spaces)
        | 1 -> items |> Seq.head |> bprintf sb "%s %s\r\n" header
        | _ -> ()
        | 0 -> ()

    let printProject p =
        let sb = new StringBuilder()
        bprintf sb "Project: %s\r\n" p.name
        bprintf sb "Path:    %s\r\n" p.path
        bprintf sb "Total scproj items:     %i\r\n" p.sitecoreItemsCount
        bprintf sb "FS Items:               %i\r\n" p.fileSystemItemsCount
        bprintf sb "SC items not on FS:     %i\r\n" p.missingFromFSCount
        bprintf sb "FS Items not in scproj: %i\r\n" p.missingFromProjectCount
        bprintf sb "Duplicate scproj items: %i\r\n" p.duplicatesCount
        printWithHeader sb ("Duplicated in scproj", p.dupesList)
        printWithHeader sb ("Missing in files", p.missingFromFilesystem)
        printWithHeader sb ("Not in scproj", p.missingFromProject)
        String.replicate 30 "-" |> bprintf sb "%s\r\n"
        sb.ToString()

    let rec allFiles dir =
        Seq.append 
            (dir |> Directory.GetFiles)
            (dir |> Directory.GetDirectories |> Seq.collect allFiles )

    let tdsProjectTypeGuid = "{CAA73BB0-EF22-4D79-A57E-DF67B3BA9C80}"

    let printTuple (x,y) =
        printfn "%s %i" x y

    let isTdsProject s = 
        let result = Regex.Match(s, """^Project\("(.+?)"\)""")
        if isNull result then false
        else 
            let projectType = result.Groups.[1].Value
            projectType = tdsProjectTypeGuid

    type TdsProject = XmlProvider<"""TDSProject1.scproj""", Global=true>

    let getTdsProjFileName (p:string) =
        let t = p.Replace("\"", "")
        let parts = t.Split [|','|]
        let path = parts.[1].Split [| '\\' |] 
        let cleanPath = seq { for i in path -> (i.Trim()) } |> Seq.toArray
        Path.Combine cleanPath

    let getSitecoreItems (p:TdsProject.Project) =
        let itemGroups = p.ItemGroups |> Seq.filter (fun x -> x.SitecoreItems.Length > 0) |> Seq.toList
        let itemGroup = Seq.head itemGroups
        let items = itemGroup.SitecoreItems
        let rootItem = (Seq.head items).Include
        let scItems = [|
            for item in items do
                yield item.Include
        |]
        scItems
    
    let checkDupes (scItems) =
        let uniq = scItems |> Set
        let counts = scItems |> Seq.countBy id
        let dupes = counts |> Seq.filter (fun i -> snd i > 1)
        dupes

    let prefixDollar (x:string) =
        match x with
        | x when x.IndexOf("%24") > -1 -> x.Replace("%24", "%2524")
        | _ -> x

    let getFsItems absoluteFilePath =
        let rootDir = Path.Combine [| (Path.GetDirectoryName absoluteFilePath) ; "sitecore" |] 
        let result = allFiles rootDir |> Seq.filter (fun x -> x.EndsWith(".item")) 
        result

    let checkProject (file,path) =
        let projFileAndPath = Path.Combine [| path; file  |]
        let fsItemsRaw = getFsItems projFileAndPath |> Set
        let fsItems = fsItemsRaw |> Seq.map (fun x -> x.[x.IndexOf("sitecore")..x.Length-1]) |> Seq.map prefixDollar |> Set
        let sample = TdsProject.Load projFileAndPath
        let sitecoreItemsRaw = getSitecoreItems sample
        let scItems = 
            if Path.DirectorySeparatorChar = '/' then 
                sitecoreItemsRaw |> Seq.map (fun x -> x.Replace('\\','/'))
            else
                sitecoreItemsRaw |> Seq.ofArray
        let uniqScItems = scItems |> Set
        let dupes = checkDupes scItems
        let missing = uniqScItems |> Seq.filter (fsItems.Contains >> not) 
        let missingProjItems = fsItems |> Seq.filter (uniqScItems.Contains >> not)
        {
            name = Path.GetFileName file
            path = file
            sitecoreItemsCount = uniqScItems.Count
            dupesList = dupes |> Seq.map (fun (x,_) -> x) |> Seq.toList
            duplicatesCount = Seq.length dupes
            fileSystemItemsCount = fsItems.Count
            missingFromFSCount = Seq.length missing
            missingFromFilesystem = Seq.toList missing
            missingFromProjectCount = Seq.length missingProjItems
            missingFromProject = Seq.toList missingProjItems
        }

    let checkSolution fileName = 
        let solutionPath = Path.GetDirectoryName fileName
        let projects = File.ReadAllLines fileName |> Seq.filter isTdsProject
        let projectFiles = seq { for project in projects ->  getTdsProjFileName project } 
        let reports = seq { for project in projectFiles -> checkProject (project, solutionPath) } 
        reports