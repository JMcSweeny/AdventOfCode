namespace Soltions2022

module Day5 =
    open Common
    open System

    let parseInstruction (line: string) =
        let parts = line |> split " "
        (int parts.[1], int parts.[3], int parts.[5])

    let parseStack (stacks: seq<string>) (line: string) =
        let trimCrate = trim >> replace "[" "" >> replace "]" ""
        let crates = line |> Seq.chunkBySize 4 |> Seq.map String |> Seq.map trimCrate
        
        match stacks |> Seq.length with
        | 0 -> crates
        | _ -> Seq.zip crates stacks |> Seq.map (fun (crate, stack) -> stack + crate)

    let parseInput (stacks: seq<string>, instructions: seq<int * int * int>) (line: string) =
        match line |> trim with
        | "" -> (stacks, instructions)
        | l when Seq.head l = '[' -> (parseStack stacks line, instructions) 
        | l when Seq.head l = 'm' -> (stacks, Seq.append instructions [parseInstruction line])
        | _ -> (stacks, instructions)

    let moveCrates (getCrates: seq<char> -> string) (stacks: Map<int, string>) ((count: int), (fromStack: int), (toStack: int)) =
        let stackFrom = Map.find fromStack stacks
        let stackTo = Map.find toStack stacks
        let addedCrates = Seq.take count stackFrom |> getCrates
       
        stacks
        |> Map.add toStack (addedCrates + stackTo)
        |> Map.add fromStack (Seq.skip count stackFrom |> String.Concat)

    let moveCratesReverse = moveCrates (Seq.rev >> String.Concat)
    let moveCratesForward = moveCrates String.Concat

    let getTopCrates (stacks: Map<int, string>) =
        stacks
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map Seq.head
        |> String.Concat

    [<Solution(2022, 5, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold parseInput (Seq.empty, Seq.empty)
        |> (fun (stacks, instructions) -> Seq.fold moveCratesReverse (stacks |> Seq.mapi (fun i s -> (i + 1, s)) |> Map.ofSeq) instructions)
        |> getTopCrates

    [<Solution(2022, 5, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold parseInput (Seq.empty, Seq.empty)
        |> (fun (stacks, instructions) -> Seq.fold moveCratesForward (stacks |> Seq.mapi (fun i s -> (i + 1, s)) |> Map.ofSeq) instructions)
        |> getTopCrates