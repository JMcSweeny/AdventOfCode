namespace Soltions2024

module Day5 =
    open Common

    type Rule =
    | Before of int
    | After of int

    let parseRule (rules: Map<int, list<Rule>>) (line: string) =
        let pageNumbers =
            line
            |> split ("|")
            |> Array.map int

        rules
        |> Map.change pageNumbers.[0] (fun v -> 
            match v with
            | Some r -> Some (r @ [Before(pageNumbers.[1])])
            | None -> Some([Before(pageNumbers.[1])])
        )
        |> Map.change pageNumbers.[1] (fun v -> 
            match v with
            | Some r -> Some (r @ [After(pageNumbers.[0])])
            | None -> Some([After(pageNumbers.[0])])
        )

    let parsePages = split(",") >> Seq.map int >> Seq.toList

    let parseLine (rules: Map<int, list<Rule>>, pagesList: list<list<int>>) (line: string) =
        match line with
        | l when l.Contains('|') -> parseRule rules l, pagesList
        | l when l.Contains(',') -> rules, pagesList @ [parsePages l]
        | _ -> rules, pagesList

    let validateRule (beforePages: list<int>) (afterPages: list<int>) (rule: Rule) =
        match rule with
        | Before r -> not (beforePages |> List.contains r)
        | After r -> not (afterPages |> List.contains r)

    let pagesInCorrectOrder (rules: Map<int, list<Rule>>) (pages: list<int>) =
        let splitPages index =
            match index with
            | i when i = 0 -> [], pages.[i + 1..]
            | i when i = pages.Length - 1 -> pages.[..i - 1], []
            | i -> pages.[..i - 1], pages.[i + 1..]

        pages
        |> List.mapi (fun i page -> 
            let beforePages, afterPages = splitPages i
            match rules.TryFind page with
            | Some r -> r |> Seq.forall (validateRule beforePages afterPages)
            | None -> true
        )
        |> List.forall (fun p -> p)
        
    let getSumOfCorrectPages (rules: Map<int, list<Rule>>, pagesList: list<list<int>>) =
        pagesList
        |> Seq.filter (pagesInCorrectOrder rules)
        |> Seq.sumBy (fun l -> l.[l.Length / 2])

    let comparePages (rules: Map<int, list<Rule>>) (page1: int) (page2: int) =
        match rules.TryFind page1 with
        | Some r when r |> Seq.exists (fun r -> r = Before(page2)) -> -1
        | Some r when r |> Seq.exists (fun r -> r = After(page2)) -> 1
        | _ -> 0

    let getSumOfIncorrectPages (rules: Map<int, list<Rule>>, pagesList: list<list<int>>) =
        pagesList
        |> Seq.filter (fun l -> not (pagesInCorrectOrder rules l))
        |> Seq.map (List.sortWith (comparePages rules))
        |> Seq.sumBy (fun l -> l.[l.Length / 2])

    [<Solution(2024, 5, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine (Map.empty, List.empty)
        |> getSumOfCorrectPages

    [<Solution(2024, 5, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold parseLine (Map.empty, List.empty)
        |> getSumOfIncorrectPages
