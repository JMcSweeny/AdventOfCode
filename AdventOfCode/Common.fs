module Common

open System.IO
    
let readLines filePath =
    File.ReadAllLines(filePath)

let readLinesAs fn = readLines >> Seq.map fn

type SolutionAttribute(year: int, day: int, part: int) =
    inherit System.Attribute()
    member this.Year = year
    member this.Day = day
    member this.Part = part
    

