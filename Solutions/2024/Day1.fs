namespace Solutions2024

module Day1 =
    open Common

    let parseLine = split "   " >> (fun parts -> int parts.[0], int parts.[1])

    let populateCount x = 
        match x with
        | Some count -> Some (count + 1)
        | None -> Some 1

    let foldOccurrences occurrences num =
        occurrences
        |> Map.change num populateCount

    let buildOccurrencesMap rightList = 
        rightList
        |> List.fold foldOccurrences Map.empty

    let findSimilarityScore occurrencesMap num = 
        occurrencesMap
        |> Map.tryFind num
        |> (fun v -> 
            match v with
            | Some count -> num * count
            | None -> 0
        )

    [<Solution(2024, 1, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.toList
        |> List.unzip
        |> (fun (a, b) -> List.zip (List.sort a) (List.sort b))
        |> List.map (fun (x, y) -> abs (x - y))
        |> List.sum

    [<Solution(2024, 1, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.toList
        |> List.unzip
        |> (fun (a, b) -> a, (buildOccurrencesMap b))
        |> (fun (nums, occurrencesMap) -> nums |> List.map (findSimilarityScore occurrencesMap))
        |> List.sum


