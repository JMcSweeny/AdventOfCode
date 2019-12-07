module Coordinates

type Boundry = { startX: int; endX: int; startY: int; endY: int }

let boundryToCoords (boundry: Boundry) =
    let xValues = seq { boundry.startX..boundry.endX }
    let yValues = seq { boundry.startY..boundry.endY }

    yValues |> Seq.collect (fun y -> xValues |> Seq.map (fun x -> (x, y)))

let getManhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)