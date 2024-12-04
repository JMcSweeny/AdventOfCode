namespace Soltions2024

module Day3 =
    open Common
    open System.Text.RegularExpressions

    let collectMatches (m: MatchCollection) =
        m |> Seq.collect (fun m -> m.Groups |> Seq.map (fun g -> g.Value))

    type Instruction =
        | Multiply of int
        | Do
        | Don't

    let classifyInstruction (i: string) =
        match i with
        | s when s.StartsWith("mul") -> Multiply(Regex.Matches(s, "[0-9]{1,3}") |> collectMatches |> Seq.map int |> Seq.reduce (*))
        | "don't()" -> Don't
        | "do()" -> Do
        | _ -> raise (System.Exception("Unknown Instruction"))


    let parseMemory (memory: string) = 
        Regex.Matches(memory, "mul\([0-9]{1,3}\,[0-9]{1,3}\)")
        |> collectMatches
        |> Seq.map classifyInstruction
        |> Seq.choose (function
            | Multiply n -> Some n
            | _ -> None
        )
        |> Seq.sum

    let parseWithConditons (memory: string) =
        Regex.Matches(memory, "mul\([0-9]{1,3}\,[0-9]{1,3}\)|don\'t\(\)|do\(\)")
        |> collectMatches
        |> Seq.map classifyInstruction
        |> Seq.fold (fun (sum, enabled) instruction -> 
            match instruction with
            | Multiply n when enabled = true -> sum + n, enabled
            | Do -> sum, true
            | Don't -> sum, false
            | _ -> sum, enabled
        ) (0, true)

    [<Solution(2024, 3, 1)>]
    let part1 fileName =
        fileName
        |> readText
        |> parseMemory

    [<Solution(2024, 3, 2)>]
    let part2 fileName =
        fileName
        |> readText
        |> parseWithConditons
        |> fst



