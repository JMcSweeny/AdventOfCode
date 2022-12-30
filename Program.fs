// Learn more about F# at http://fsharp.org

open System.Diagnostics
open System.IO
open System.Reflection
open Common

let parseProgramNumber (input: string) = 
    let parts = input.Split('.') |> Array.map int
    (parts.[0], parts.[1], parts.[2])

let getSolutions() =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Array.collect (fun t -> t.GetMethods())
    |> Array.choose (fun mi -> 
        match mi.GetCustomAttribute(typeof<SolutionAttribute>) with
        | null -> None
        | attr -> Some (attr :?> SolutionAttribute, mi)) 
    |> Array.toList

let runProgram (year, day, part) =
    printfn "Running program %i.%i.%i" year day part

    let tryFindSolution = List.tryFind (fst >> (fun (s: SolutionAttribute) -> s.Year = year && s.Day = day && s.Part = part))
    let inputFilePath = Path.Combine("Input", string year, sprintf "Day%i.txt" day)

    match getSolutions() |> tryFindSolution with
        | Some (_, mi) -> 
            let stopWatch = Stopwatch.StartNew()
            let result = mi.Invoke(null, [|inputFilePath|]) |> string
            (result, stopWatch.ElapsedMilliseconds)
        | None -> failwithf "Could not find solution for %i.%i.%i" year day part

let printProgramResults (result, timeElapsed) =
    printfn "Result: %s" result
    printfn "Program completed in %ims" timeElapsed
    
[<EntryPoint>]
let main argv =
    argv.[0]
    |> parseProgramNumber
    |> runProgram
    |> printProgramResults
    0 // return an integer exit code
