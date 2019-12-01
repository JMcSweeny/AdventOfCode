module Coordinates

type Boundry = { startX: int; endX: int; startY: int; endY: int }

let boundryToCoords (boundry: Boundry) =
    let xValues = seq { boundry.startX..boundry.endX }
    let yValues = seq { boundry.startY..boundry.endY }

    yValues |> Seq.collect (fun y -> xValues |> Seq.map (fun x -> (x, y))) |> Set.ofSeq