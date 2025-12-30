namespace Solutions2025

module Day7 =
    open Common

    type State = {
        PathCounts: Map<int, int64>
        TotalSplitsHit: int
    }

    let moveBeams (state: State) (line: string): State =
        match line |> contains "S" with
        | true -> { state with PathCounts = Map.empty |> Map.add (line |> indexOf 'S') 1 }
        | false -> 
            state.PathCounts
            |> Map.fold (fun acc x count -> 
                match line.[x] with
                | '^' ->
                    let nextPaths = 
                        acc.PathCounts
                        |> Map.change (x - 1) (fun c -> Some(defaultArg c 0 + count)) 
                        |> Map.change (x + 1) (fun c -> Some(defaultArg c 0 + count))

                    { PathCounts = nextPaths; TotalSplitsHit = acc.TotalSplitsHit + 1 }
                | _ ->
                    let nextPaths =
                        acc.PathCounts
                        |> Map.change x (fun c -> Some(defaultArg c 0 + count))
                    { acc with PathCounts = nextPaths }
            ) { PathCounts = Map.empty; TotalSplitsHit = state.TotalSplitsHit }

    [<Solution(2025, 7, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold moveBeams { PathCounts = Map.empty; TotalSplitsHit = 0 }
        |> fun state -> state.TotalSplitsHit

    [<Solution(2025, 7, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold moveBeams { PathCounts = Map.empty; TotalSplitsHit = 0 }
        |> fun state -> state.PathCounts |> Map.values |> Seq.sum