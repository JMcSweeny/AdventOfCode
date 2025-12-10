namespace Solutions2022

module Day10 =
    open Common

    let parseLine ((register: int), (cycles: list<int>)) (line: string) = 
        match line |> split " " with
        | parts when parts.[0] = "noop" -> (register, register :: cycles)
        | parts -> (register + (int <| parts.[1]), register :: register :: cycles)

    let getSumOfSignals (cycles: list<int>) =
        [| 20; 60; 100; 140; 180; 220 |]
        |> Seq.map (fun i -> i * cycles.[i])
        |> Seq.fold (+) 0


    let drawRow (cyles: list<int>) =
        cyles
        |> Seq.fold (fun (cycle, pixels) register -> 
            let pixel = 
                match register with
                | r when abs (r - cycle) < 2 -> "#"
                | _ -> "."

            (cycle + 1, pixels + pixel)
        ) (0, "")
        |> snd

    [<Solution(2022, 10, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine (1, List.empty)
        |> snd
        |> List.rev
        |> getSumOfSignals

    [<Solution(2022, 10, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine (1, List.empty)
        |> snd
        |> List.rev
        |> List.chunkBySize 40
        |> Seq.map drawRow
        |> Seq.iter (printfn "%s")