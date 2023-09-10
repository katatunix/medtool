open System.IO
open MetadataExtractor
open MetadataExtractor.Formats.Exif
open MetadataExtractor.Formats.QuickTime

let sortDirs (dirs: Directory seq) =
    dirs
    |> Seq.sortBy (fun dir ->
        match dir with
        | :? ExifSubIfdDirectory -> 0
        | :? ExifDirectoryBase -> 1
        | :? QuickTimeMetadataHeaderDirectory -> 2
        | :? QuickTimeMovieHeaderDirectory -> 3
        | _ -> 4
    )

let getCreationDateTimeFromDir (dir: Directory) =
    let tagTypes =
        match dir with
        | :? ExifSubIfdDirectory ->
            [ ExifSubIfdDirectory.TagDateTimeDigitized
              ExifSubIfdDirectory.TagDateTimeOriginal
              ExifSubIfdDirectory.TagDateTime ]
        | :? ExifDirectoryBase ->
            [ ExifDirectoryBase.TagDateTimeDigitized
              ExifDirectoryBase.TagDateTimeOriginal
              ExifDirectoryBase.TagDateTime ]
        | :? QuickTimeMetadataHeaderDirectory ->
            [ QuickTimeMetadataHeaderDirectory.TagCreationDate ]
        | :? QuickTimeMovieHeaderDirectory ->
            [ QuickTimeMovieHeaderDirectory.TagCreated ]
        | _ ->
            []
    tagTypes
    |> Seq.map (fun tagType ->
        try dir.GetDateTime tagType |> Some
        with _ -> None
    )
    |> Seq.tryPick id

let getCreationDateTimeFromFile (filePath: string) =
    filePath
    |> ImageMetadataReader.ReadMetadata
    |> sortDirs
    |> Seq.map getCreationDateTimeFromDir
    |> Seq.tryPick id
    |> Option.defaultWith (fun _ -> File.GetCreationTime filePath)

let genNewFilePath (filePath: string) (newBaseName: string) =
    let dir = Path.GetDirectoryName filePath
    let currentBaseName = Path.GetFileNameWithoutExtension filePath
    let ext = Path.GetExtension(filePath).ToLower()
    let rec loop i =
        let candidateBaseName = if i = 1 then newBaseName else $"{newBaseName}[{i}]"
        let candidatePath = Path.Combine (dir, candidateBaseName + ext)
        if candidateBaseName = currentBaseName then
            candidatePath
        elif File.Exists candidatePath then
            loop (i + 1)
        else
            candidatePath
    loop 1

let processFile changesFileName file =
    let dateTime = getCreationDateTimeFromFile file
    File.SetCreationTime (file, dateTime)
    File.SetLastWriteTime (file, dateTime)
    if changesFileName then
        let newBaseName = dateTime.ToString "yyyy-MM-dd HHmmss"
        let newFilePath = genNewFilePath file newBaseName
        File.Move (file, newFilePath)
    dateTime

let visitFolder (folder: string) (pattern: string) fileAction =
    let errors = System.Collections.Generic.List<_> ()
    Directory.GetFiles (folder, pattern, SearchOption.AllDirectories)
    |> Array.iteri (fun i file ->
        printf "[%d] %s" (i + 1) file
        try
            let result = fileAction file
            printfn " ✅ %A" result
        with ex ->
            printfn " ❌ %s" ex.Message
            errors.Add (file, ex.Message)
    )
    printfn "Done!"
    if errors.Count > 0 then
        printfn "%d file(s) failed:" errors.Count
        for file, msg in errors do
            printfn "❌ %s: %s" file msg

let processFolder folder pattern changesFileNames =
    visitFolder folder pattern (processFile changesFileNames)

let listFolder (folder: string) (pattern: string) =
    visitFolder folder pattern getCreationDateTimeFromFile

let readMetadataFromFile (file: string) =
    let dirs = ImageMetadataReader.ReadMetadata file
    for dir in dirs do
        printfn $"🟢 {dir.Name}:"
        for tag in dir.Tags do
            printfn $"\t{tag.Name}: {tag.Description}"

let usageRead () =
    printfn "Usage: medtool read <file>"

let usageList () =
    printfn "Usage: medtool list <folder>"

let usageProcess () =
    printfn "Usage: medtool <folder> [pattern] [changesFileNames]"

let usage = usageRead >> usageList >> usageProcess

let (| Bool |) (str: string) = str.ToLower() = "true"

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | [ "read"; file ] ->
        readMetadataFromFile file
        0
    | "read" :: _ ->
        usageRead ()
        1

    | [ "list"; folder ] ->
        listFolder folder "*"
        0
    | [ "list"; folder; pattern ] ->
        listFolder folder pattern
        0
    | "list" :: _ ->
        usageList ()
        1

    | [ "process"; folder ] ->
        processFolder folder "*" true
        0
    | [ "process"; folder; pattern ] ->
        processFolder folder pattern true
        0
    | [ "process"; folder; pattern; Bool changesFileNames ] ->
        processFolder folder pattern changesFileNames
        0
    | "process" :: _ ->
        usageProcess ()
        1

    | _ ->
        usage ()
        1
