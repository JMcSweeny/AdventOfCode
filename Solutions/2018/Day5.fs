namespace Solutions2018

module Day5 = 
    open Common

    let parseInput = readLines >> Seq.head

    let reaction a b = abs (a - b) = 32

    let triggerReactions polymer unit =
        match polymer with
            | p :: ps when reaction p unit -> ps
            | ps -> unit :: ps

    let reactPolymer polymer = Seq.fold triggerReactions List.empty polymer

    [<Solution(2018, 5, 1)>]
    let part1 fileName = 
        fileName
        |> parseInput
        |> Seq.map int
        |> reactPolymer
        |> Seq.length

    let filterUnit polymer unit = Seq.filter (fun u -> unit <> u && unit <> (u - 32)) polymer

    [<Solution(2018, 5, 2)>]
    let part2 fileName = 
        let fullPolymer = 
            fileName
            |> parseInput
            |> Seq.map int
        let allUnits = seq { 65..90 }

        allUnits
        |> Seq.map (filterUnit fullPolymer)
        |> Seq.map reactPolymer
        |> Seq.map Seq.length
        |> Seq.min
        


