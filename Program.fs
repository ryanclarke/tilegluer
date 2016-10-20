open FSharp.Data
open System.IO
open System.Drawing

type CoordOrder =
    | Zxy
    | Zyx

// CONFIGURATION
let tileSize = 256
let baseUrl = ""
let outputDir = "images"
let imageExtension = "jpg"
let coordOrder = CoordOrder.Zyx

type Coord = {
    X: int
    Y: int
    Z: int
}

type Tile = {
    X: int
    Y: int
    Url: string
    Path: string
}

let coords depth originX originY width height =
    let range origin length =
        let count = float(length) / float(tileSize) |> floor |> int
        [origin..(origin + count)]

    range originX width
    |> List.collect (fun x ->
        range originY height
        |> List.map (fun y ->
            {X = x; Y = y; Z = depth}))

let tiles originX originY coords =
    let sortCoord coord =
        match coordOrder with
        | Zxy -> (coord.Z, coord.X, coord.Y)
        | Zyx -> (coord.Z, coord.Y, coord.X)

    coords
    |> List.map (fun coord ->
        let (first, second, third) = sortCoord coord
        let location = fun root ->
                sprintf "%s/%d/%d/%d.%s" root first second third imageExtension
        {
            X = (coord.X - originX) * tileSize;
            Y = (coord.Y - originY) * tileSize;
            Url = location baseUrl;
            Path = location outputDir;
        }
    )

let fetch tile =
    match File.Exists(tile.Path) with
    | true ->
        Some tile
    | false ->
        match Http.Request(tile.Url).Body with
        | Binary bytes ->
            use s = new MemoryStream(bytes)
            let b = new Bitmap(s)
            tile.Path |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
            b.Save(tile.Path)
            Some tile
        | _ ->
            printfn "ERROR: %s" tile.Url
            None

let combineAll finalPath tiles =
    let sizer getDimension = tileSize * (tiles |> List.distinctBy getDimension |> List.length)
    let width = sizer (fun t -> t.X)
    let height = sizer (fun t -> t.Y)

    use bitmap = new Bitmap(width, height)
    use graphics = Graphics.FromImage(bitmap)
    graphics.Clear(SystemColors.AppWorkspace)
    tiles
    |> List.map (fun tile ->
        use image = Image.FromFile(tile.Path)
        graphics.DrawImage(image, tile.X, tile.Y)
    ) |> ignore
    bitmap.Save(finalPath)
    bitmap.Save("images/image.jpg")
    printfn "%s" finalPath

let doIt depth originX originY width height =
    let theCoords = coords depth originX originY width height
    let theTiles = tiles originX originY theCoords
    let theAvailableTiles = theTiles |> List.map fetch |> List.choose id
    theAvailableTiles |> combineAll (sprintf "images/image_%dx%d_z%dy%dx%d.jpg" width height depth originY originX)

[<EntryPoint>]
let main argv = 
    match baseUrl with
    | "" ->
        printfn "%s" "ERROR: baseUrl is required."
        2
    | _ ->
        match argv |> Array.map System.Int32.Parse with
        | [|depth; originY; originX; width; height|] ->
            doIt depth originX originY width height
            0
        | _ ->
            printfn "%s" "ERROR: required args -> DEPTH ORIGINX ORIGINY WIDTH HEIGHT"
            1
