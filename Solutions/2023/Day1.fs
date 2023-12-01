namespace Soltions2023

module Day1 =
    open Common
    open System

    let replacements = 
        [ "one",   "1";
          "two",   "2";
          "three", "3";
          "four",  "4";
          "five",  "5";
          "six",   "6";
          "seven", "7";
          "eight", "8";
          "nine",  "9";
          "ten",   "10" ]

    let replaceStringWithDigit s = 
        replacements 
        |> Seq.fold (fun curr (digitAsString, digit) ->
            replace digitAsString (digitAsString.[0..0] + digit + digitAsString.[1..digitAsString.Length]) curr
        ) s

    let firstAndLastDigit = 
        String.filter Char.IsDigit >> (fun s -> s.[0..0] + s.[s.Length-1..s.Length]) >> string >> int

    [<Solution(2023, 1, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs firstAndLastDigit
        |> Seq.sum

    [<Solution(2023, 1, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs (replaceStringWithDigit >> firstAndLastDigit)
        |> Seq.sum
