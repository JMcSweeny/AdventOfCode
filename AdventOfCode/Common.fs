module Common

open System.IO

let getFullPath filePath =
    let baseDirectory = Directory.GetParent(__SOURCE_DIRECTORY__)
    Path.Combine(baseDirectory.FullName, filePath)
    
let readLines filePath =
    File.ReadAllLines(getFullPath filePath)

let readLinesAs fn = readLines >> Seq.map fn

type SolutionAttribute(year: int, day: int, part: int) =
    inherit System.Attribute()
    member this.Year = year
    member this.Day = day
    member this.Part = part
    

