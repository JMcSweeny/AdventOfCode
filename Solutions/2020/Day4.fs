namespace Soltions2020

open System.Text.RegularExpressions

module Day4 = 

    open Common
    open System

    let parseField =
        split ":"
        >> (fun parts -> parts.[0], parts.[1])

    let parsePassportFields =
        split " "
        >> Seq.map parseField
        >> Map.ofSeq
        >> Map.filter (fun k _ -> k <> "cid")

    let parsePassports =
        split (Environment.NewLine + Environment.NewLine)
        >> Seq.map (replace Environment.NewLine " ")
        >> Seq.map parsePassportFields

    let validatePassport isValidEntry =
        Map.filter isValidEntry
        >> Map.count
        >> (=) 7

    let validatePassportField (k: string) (v: string) =
        match k with
        | "byr" -> Regex.IsMatch(v, "^(19[2-9][0-9]|200[0-2])$")
        | "iyr" -> Regex.IsMatch(v, "^(201[0-9]|2020)$")
        | "eyr" -> Regex.IsMatch(v, "^(202[0-9]|2030)$")
        | "hgt" -> Regex.IsMatch(v, "^(1[5-8][0-9]|19[0-3])cm$|^(59|6[0-9]|7[0-6])in$")
        | "hcl" -> Regex.IsMatch(v, "^(#[a-f0-9]{6})$")
        | "ecl" -> Regex.IsMatch(v, "^(amb|blu|brn|gry|grn|hzl|oth)$")
        | "pid" -> Regex.IsMatch(v, "^(\d{9})$")
        | _ -> false
        
    [<Solution(2020, 4, 1)>]
    let part1 fileName =
        fileName 
        |> readText
        |> parsePassports
        |> Seq.filter (validatePassport (fun _ _ -> true))
        |> Seq.length

    [<Solution(2020, 4, 2)>]
    let part2 fileName =
        fileName 
        |> readText
        |> parsePassports
        |> Seq.filter (validatePassport validatePassportField)
        |> Seq.length




