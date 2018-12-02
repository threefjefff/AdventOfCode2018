namespace AdventOfCode
open System.IO
module Day1 = 
    let readFile filepath =
        File.ReadAllLines filepath |> List.ofSeq |> List.map int

    let inputAsInts args =
        match args with
        | [filepath] ->
            printfn "Attempting to read filepath %s" filepath
            readFile filepath
        | _ ->
            List.map int args 

    let runningTotal (inputs: int list) =
        inputs 
        |> List.mapFold (fun total next -> (total + next, total + next)) 0

    let rec findDuplicate list =
        match list with
        | first::tail ->
            match List.exists (fun x -> x == first) tail with
            | true ->
                Some(first)
            | false ->
                findDuplicate tail
        | [] ->
            None

    let execute (args: string list)=
        let result = inputAsInts args |> runningTotal
        result |> snd |> printfn "Total is %d"
        result 
        |> fst 
        |> findDuplicate
        |> printfn "Running list is %A"
        