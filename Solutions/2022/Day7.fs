namespace Solutions2022

module Day7 =
    open Common
    open System

    let changeDirectory path line =
        let newDir = (line |> split " ").[2]
        match newDir with
        | "/" -> ["/"]
        | ".." -> path |> List.tail
        | _ -> newDir :: path

    let addFile (path: list<string>) (sizes: Map<string, int>) (line: string) =
        let size = (line |> split " ").[0] |> int

        [1..path |> Seq.length]
        |> Seq.map (fun i -> path |> Seq.skip (i - 1) |> String.concat "--")
        |> Seq.fold (fun m k -> 
            m |> Map.change k (fun v ->
                match v with
                | Some c -> Some (c + size)
                | None -> Some size
            )
        ) sizes

    let parseLine (path: list<string>, sizes: Map<string, int>) (line: string) =
        let command = (line |> Seq.take 4 |> String.Concat)
        match command with
        | "$ cd" -> (changeDirectory path line, sizes)
        | "$ ls" -> (path, sizes)
        | "dir " -> (path, sizes)
        | _ -> (path, addFile path sizes line)

            
    let findSmallestToDelete (sizes: Map<string, int>) =
        let rootSize = sizes |> Map.find "/"
        let unusedSpace = 70000000 - rootSize
        let spaceToDelete = 30000000 - unusedSpace

        sizes
        |> Map.filter (fun _ v -> v >= spaceToDelete)
        |> Map.toSeq
        |> Seq.minBy snd
        |> snd

    [<Solution(2022, 7, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine ([], Map.empty)
        |> snd
        |> Map.filter (fun _ v -> v <= 100000)
        |> Map.toSeq
        |> Seq.sumBy snd

    [<Solution(2022, 7, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine ([], Map.empty)
        |> snd
        |> findSmallestToDelete