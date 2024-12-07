namespace Soltions2024

module Day6 =
    open Common
    
    type Position = (int * int) * char

    let foldInitialState (map: Map<int * int, char>, guard: Position) (position: Position) =
        match position with
        | (x, y), c when c = '^' || c = '>' || c = 'v' || c = '<' -> Map.add (x, y) '.' map, position
        | (x, y), c -> Map.add (x, y) c map, guard

    let getNextGuard (((x, y), guardDirection): Position) =
        match guardDirection with
        | '^' -> (x, y - 1), guardDirection
        | '>' -> (x + 1, y), guardDirection
        | 'v' -> (x, y + 1), guardDirection
        | '<' -> (x - 1, y), guardDirection
        |_ -> (x, y), guardDirection

    let rotateGuardPostition (guardPosition: char) =
        match guardPosition with
        | '^' -> '>'
        | '>' -> 'v'
        | 'v' -> '<'
        | '<' -> '^'
        | _ -> guardPosition
 
    let walk (map: Map<int * int, char>, initialGuard: Position) =
        let rec innerWalk (traversed: list<Position>) (guard: Position) =
            let coords, guardDirection = guard
            match Map.tryFind coords map with
            | None -> traversed
            | Some nextC when nextC = '#' -> innerWalk traversed (traversed |> List.last |> fst, rotateGuardPostition guardDirection)
            | Some nextC when traversed |> Seq.exists (fun t -> t = guard) -> []
            | _ -> innerWalk (traversed @ [guard]) (getNextGuard guard)
        innerWalk [initialGuard] (getNextGuard initialGuard)

    let walkAllMaps (map: Map<int * int, char>, initialGuard: Position) =
        let traversedPositions = walk (map, initialGuard) |> List.map fst

        map
        |> Map.toList
        |> List.filter (fun (k, _) -> List.exists (fun coord -> coord = k) traversedPositions)
        |> List.map (fun (k, _) -> Map.add k '#' map)
        |> List.map (fun m -> walk (m, initialGuard))

    [<Solution(2024, 6, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id
        |> Seq.fold foldInitialState (Map.empty, ((0,0), '*'))
        |> walk
        |> List.distinctBy fst
        |> List.length

    [<Solution(2024, 6, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id
        |> Seq.fold foldInitialState (Map.empty, ((0,0), '*'))
        |> walkAllMaps
        |> Seq.filter (fun l -> l.Length = 0)
        |> Seq.length
