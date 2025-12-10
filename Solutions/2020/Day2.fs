namespace Solutions2020

module Day2 = 

    open Common

    type Policy = {
        minOccurrences: int;
        maxOccurrences: int;
        requiredLetter: char;
        password: string;
    }

    let parseLineAsPolicy (line: string) =
        let parts = line.Split(' ')
        let occurrences = parts.[0].Split('-')
        let requiredLetter = parts.[1].[0]
        let password = parts.[2]
        {
            minOccurrences = int occurrences.[0];
            maxOccurrences = int occurrences.[1];
            requiredLetter = requiredLetter;
            password = password;
        }

    let countOccurrences x = Seq.filter ((=) x) >> Seq.length

    let isValidPassword (policy: Policy) =
        let occurrences = policy.password |> countOccurrences policy.requiredLetter
        occurrences >= policy.minOccurrences && occurrences <= policy.maxOccurrences

    let isValidPassword2 (policy: Policy) =
        [policy.password.[policy.minOccurrences - 1]; policy.password.[policy.maxOccurrences - 1]]
        |> countOccurrences policy.requiredLetter
        |> (=) 1

        
    [<Solution(2020, 2, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLineAsPolicy
        |> Seq.filter isValidPassword
        |> Seq.length

    [<Solution(2020, 2, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLineAsPolicy
        |> Seq.filter isValidPassword2
        |> Seq.length


