namespace Solutions2019

module Day3 =
    open Common
    open Coordinates

    let move direction (x, y) =
        match direction with
            | 'R' -> (x + 1, y)
            | 'L' -> (x - 1, y)
            | 'D' -> (x, y + 1)
            | 'U' -> (x, y - 1)
            | _   -> failwithf "Invalid direction %c" direction

    let instructionToCoords (x: int, y: int) (instruction: string) =
        let direction = instruction.[0]
        let steps = instruction.[1..] |> int
        let numSteps = seq {1..steps}
        let getNextCoord coords lastCoord = 
            let nextCoord = move direction lastCoord
            (List.append coords [nextCoord], nextCoord)
        Seq.fold (fun (coords, lastCoord) _ -> getNextCoord coords lastCoord) (List.empty, (x, y)) numSteps

    let mapWire wire =
        let computeInstruction existingCoords lastCoord instruction =
            let (coords, lastCoord) = instructionToCoords lastCoord instruction
            List.append existingCoords coords, lastCoord
            
        Seq.fold (fun (coords, lastCoord) instruction -> computeInstruction coords lastCoord instruction) (List.empty, (0,0)) wire
        |> fst
            
    let parseInput fileName =
        let lines = 
            fileName
            |> readLinesAs (fun s -> s.Split(',') |> Array.toList)
        (Seq.item 0 lines, Seq.item 1 lines)

    [<Solution(2019, 3, 1)>]
    let part1 fileName = 
        let (wire1, wire2) =
            fileName
            |> parseInput

        let wire1Coords = mapWire wire1
        let wire2Coords = mapWire wire2

        let intersections = Set.intersect (wire1Coords |> set) (wire2Coords |> set)

        intersections
        |> Seq.map (fun coord -> getManhattanDistance coord (0,0))
        |> Seq.min

    [<Solution(2019, 3, 2)>]
    let part2 fileName = 
        let (wire1, wire2) =
            fileName
            |> parseInput

        let wire1Coords = mapWire wire1
        let wire2Coords = mapWire wire2

        let intersections = Set.intersect (wire1Coords |> set) (wire2Coords |> set)

        intersections
        |> Seq.map (fun i -> 
            let index1 = List.findIndex (fun coord -> coord = i) wire1Coords + 1
            let index2 = List.findIndex (fun coord -> coord = i) wire2Coords + 1
            index1 + index2
            )
        |> Seq.min
        
        

