namespace Solutions2020

module Day3 = 

    open Common

    let filterY slope =
        Seq.mapi (fun i x -> x, i) >>
        Seq.choose (fun (x, i) ->
            match (i + 1) % slope with
            | 0 -> Some(x)
            | _ -> None
        )

    let getSlopeEncounters (x, y) =
        let getEncounters (currX: int, encounters: List<char>) (line: string) =
            let nextX = (currX + x) % line.Length
            let nextEncounters = encounters |> List.append [line.[nextX]]
            (nextX, nextEncounters)
        Seq.skip 1 >> filterY y >> Seq.fold getEncounters (0, List.empty) >> snd >> Seq.filter ((=) '#') >> Seq.length

    [<Solution(2020, 3, 1)>]
    let part1 fileName =
        fileName 
        |> readLines
        |> getSlopeEncounters (3, 1)

    [<Solution(2020, 3, 2)>]
    let part2 fileName =
        let grid = fileName |> readLines

        [(1,1);(3,1);(5,1);(7,1);(1,2)]
        |> Seq.map (fun slope -> getSlopeEncounters slope grid)
        |> Seq.map bigint
        |> Seq.fold (*) bigint.One




