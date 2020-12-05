module Common

open System.IO
    
let readLines filePath =
    File.ReadAllLines(filePath)

let readText filePath = 
    File.ReadAllText(filePath)

let readLinesAs fn = readLines >> Seq.map fn

type SolutionAttribute(year: int, day: int, part: int) =
    inherit System.Attribute()
    member this.Year = year
    member this.Day = day
    member this.Part = part

let split (seperator: string) (s:string) = s.Split(seperator)
let replace (o: string) (n: string) (s: string) = s.Replace(o, n)
let trim (s: string) = s.Trim()
    

