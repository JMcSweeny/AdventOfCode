namespace Solutions2025

module Day1 =
    open Common

    let step (line: string) =
        let direction = line.[0]
        let steps = line.[1..] |> int

        match direction with
        | 'L' -> -1 * steps
        | 'R' -> 1 * steps
        | _ -> raise (System.Exception $"Unknown Direction: {direction}")

    let boundriesCrossed round currentPoint delta =
        let startCycle = float currentPoint / 100.0 |> round |> int
        let endCycle = float delta / 100.0 |> round |> int
        endCycle - startCycle

    let rotate (currentPoint: int, points: list<int>, passedZero: int) (step: int) =
        let delta = currentPoint + step
        let nextPoint = (delta % 100 + 100) % 100
        let timesPassedZero = 
            match step with
            | s when s > 0 -> boundriesCrossed floor currentPoint delta
            | s when s < 0 -> boundriesCrossed ceil currentPoint delta |> abs
            | _ -> 0
        nextPoint, points @ [nextPoint], timesPassedZero + passedZero

    [<Solution(2025, 1, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs step
        |> Seq.fold rotate (50, List.empty, 0)
        |> fun (_,points,_) -> points
        |> List.filter (fun x -> x = 0)
        |> List.length

    [<Solution(2025, 1, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs step
        |> Seq.fold rotate (50, List.empty, 0)
        |> fun (_,_,passedZero) -> passedZero