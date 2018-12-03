namespace AdventOfCode

module Day2 =
    open System.IO
    open System.Text.RegularExpressions

    let parseInput args =
        match args with
        | [] ->
            None
        | [filepath] ->
            Some(File.ReadAllLines filepath |> List.ofArray)

    let charTotals (boxID:string) =
        boxID 
        |> Seq.toList
        |> Seq.countBy (fun x -> x)
        |> Seq.groupBy (fun x -> snd x)
        |> Seq.map (fun x -> (fst x, (snd x |> Seq.map fst)))

    let hits = [2;3]

    //let compare 

    let findValue (col: seq<int * seq<char>>) =
        col 
        |> Seq.map fst
        |> Seq.filter (fun x -> Seq.contains x hits)

    let execute (args: string list) =
        match args |> parseInput with
        | None ->
            printfn "%s" "specify a file"
        | Some(boxes) ->
            Seq.map charTotals boxes
            |> Seq.map findValue
            |> Seq.filter (fun x -> not (Seq.isEmpty x))
            |> Seq.collect (fun x -> x)
            |> Seq.countBy (fun x -> x)
            |> Seq.map snd
            |> Seq.reduce (fun x y -> x * y)
            //|> List.ofSeq //Gimmie the whole list, not just the first bits of the seq
            |> printf "%A"