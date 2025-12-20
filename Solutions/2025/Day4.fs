namespace Solutions2025

module Day4 =
    open Common

    let getAdjacentPostions (x, y) =
        [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1); (x, y + 1); (x - 1, y + 1); (x - 1, y)]

    let findAccessible (grid: Map<int * int, char>) =
        let isAccessible (position: int * int) (object: char) =
            match object with
            | '.' -> false
            | '@' -> 
                let adjacentRolls = 
                    getAdjacentPostions position 
                    |> List.choose (fun p -> Map.tryFind p grid) 
                    |> List.filter (fun c -> c = '@')
                adjacentRolls.Length < 4
            | _ -> raise (System.Exception $"Unknown Object: {object}")

        grid |> Map.filter isAccessible

    let removeRolls (grid: Map<int * int, char>) =
        let rec remove (currentGrid: Map<int * int, char>) (totalRemoved: int) =
            let accessible = findAccessible currentGrid
            match accessible.Count with
            | 0 -> totalRemoved
            | _ -> 
                let replacedRolls = accessible |> Map.map (fun _ _-> '.')
                let newGrid = (currentGrid, replacedRolls) ||> Map.fold (fun acc k v -> Map.add k v acc)
                remove newGrid totalRemoved + replacedRolls.Count
        remove grid 0
        
    [<Solution(2025, 4, 1)>]
    let part1 fileName =
        fileName
        |> readGridAs (fun x y c -> (x, y), c)
        |> Map.ofSeq
        |> findAccessible
        |> Seq.length
        
    [<Solution(2025, 4, 2)>]
    let part2 fileName =
        fileName
        |> readGridAs (fun x y c -> (x, y), c)
        |> Map.ofSeq
        |> removeRolls