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
    
    let execute (args: string list)=
        inputAsInts args |> List.sum |> printfn "Total is %d"
