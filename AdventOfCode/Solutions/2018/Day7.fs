namespace Solutions2018

module Day7 = 
    open Common

    let parseLine (line: string) = 
        let parts = line.Split(" ")
        char parts.[1], char parts.[7]

    let parseInput fileName = 
        fileName |> readLinesAs parseLine

    let getReadyInstructions instructions = 
        let parents = instructions |> Seq.map fst |> Set.ofSeq
        let children = instructions |> Seq.map snd |> Set.ofSeq
        Set.difference parents children |> Seq.sort

    let getLastInstruction instructions =
        instructions |> Seq.map snd |> Seq.head

    let getAllSteps instructions = 
        let parents = instructions |> Seq.map fst |> Set.ofSeq
        let children = instructions |> Seq.map snd |> Set.ofSeq
        Set.union parents children

    let rec printInstructions instructions =
        let nextInstruction = getReadyInstructions instructions |> Seq.head
        let remainingInstructions = instructions |> Seq.filter (fun (p, _) -> p <> nextInstruction)

        match Seq.length remainingInstructions with
            | l when l = 0 -> string nextInstruction + (getLastInstruction instructions |> string)
            | _ -> string nextInstruction + printInstructions remainingInstructions

    let SECONDS = 60
    let MAX_WORKERS = 5

    let getConstructonTime (c: char) =
        (int c) - 64 + SECONDS

    let assignWorker (workers: (char * int) list) (instruction: char) =
        let alreadyAssigned = workers |> List.map fst |> List.contains instruction
        match alreadyAssigned with
            | true -> workers
            | false -> List.append workers [(instruction, getConstructonTime instruction)]

    let doWork (workers: (char * int) list) =
        List.map (fun (i, c) -> (i, c - 1)) workers

    let printState (instructions: seq<char * char>, workers: (char * int) list, second: int) =
        printfn "%i" second
        instructions |> Seq.map fst |> Seq.iter (fun c -> printfn "Instruction: %c" c)
        workers |> List.iter (fun (c, i) -> printfn "Worker: %c,%i" c i)
        printfn "------------------------"

    let scanInstructions (instructions: seq<char * char>, workers: (char * int) list, allSteps: Set<char>, second: int) (currentSecond: int) =
        let worked = doWork workers
        let completedInstructions = worked |> List.filter (fun (_, c) -> c = 0) |> List.map fst
        let availableWorkers = worked |> List.filter (fun (_, c) -> c <> 0)
        
        let nextInstructions = instructions |> Seq.filter (fun (c, _) -> not (List.contains c completedInstructions))
        let nextSteps = Set.filter (fun c -> not (List.contains c completedInstructions)) allSteps

        let nextWorkers = 
            match Set.count nextSteps with
                | 1 -> assignWorker availableWorkers (Seq.head nextSteps)
                | _ ->
                    getReadyInstructions nextInstructions
                    |> Seq.scan assignWorker availableWorkers
                    |> Seq.takeWhile (fun ws -> List.length ws <= MAX_WORKERS)
                    |> Seq.last

        (nextInstructions, nextWorkers, nextSteps, currentSecond)
        
    [<Solution(2018, 7, 1)>]
    let part1 fileName = 
        fileName
        |> parseInput
        |> printInstructions

    [<Solution(2018, 7, 2)>]
    let part2 (fileName: string) =
        let instructions = 
            fileName 
            |> parseInput

        let allSteps = getAllSteps instructions

        Seq.initInfinite (fun n -> n + 1)
        |> Seq.scan scanInstructions (instructions, List.empty, allSteps, 0)
        |> Seq.takeWhile (fun (_, workers, steps, _) -> Seq.length steps <> 0 || List.length workers > 0)
        |> (Seq.last >> (fun (_, _, _, seconds) -> seconds))





        
        

