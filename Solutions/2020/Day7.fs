namespace Soltions2020

module Day7 = 
    open Common

    type Bag = {
        name: string;
        children: seq<int * Bag>;
    }

    let parseChild = 


    let parseChildren (childrenString: string) =
        match trim childrenString with
        | "no other" -> []
        | _ -> 


    let bag = 
        replaceMany [" bags"; " bag"; "."] "" 
        >> split "contain" 
        >> (fun parts -> { name = trim parts.[0]; children = [] })

    [<Solution(2020, 7, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs bag
        |> Seq.iter (printfn "%A")