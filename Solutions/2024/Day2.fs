namespace Solutions2024

module Day2 =
    open Common

    let parseLine = split " " >> Seq.map int

    let differenceOfPairs = Seq.pairwise >> Seq.map (fun (x, y) -> x - y)

    let isIncreasing diff = diff = -1 || diff = -2 || diff = -3
    let isDecreasing diff = diff = 1 || diff = 2 || diff = 3

    let isSafe levels = (Seq.forall isDecreasing levels) || (Seq.forall isIncreasing levels)

    let generateAllPossiblePairs levels =
        levels
        |> Seq.mapi (fun index _ -> 
            levels
            |> Seq.mapi (fun i level ->
                match i with
                | i when i <> index -> Some level
                | _ -> None
            )
            |> Seq.choose id
            |> differenceOfPairs
        )

    [<Solution(2024, 2, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.map differenceOfPairs
        |> Seq.filter isSafe
        |> Seq.length

    [<Solution(2024, 2, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.map (fun levels -> Seq.append [differenceOfPairs levels] (generateAllPossiblePairs levels))
        |> Seq.filter (Seq.exists isSafe)
        |> Seq.length



