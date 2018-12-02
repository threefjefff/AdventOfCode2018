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
   
    type ResultOrList = Result of int option | List of int list

    let rec findDuplicate list:ResultOrList =
        match list with
        | first::tail ->
            match List.exists (fun x -> x = first) tail with
            | true ->
                Result first
            | false ->
                match findDuplicate tail with
                    | Result x ->
                        match x with
                        | None ->
                            findDuplicate (list::list)
        | [] ->
            Result None

    let execute (args: string list)=
        let result = inputAsInts args |> runningTotal
        result
        |> printfn "%A"
        result 
        |> snd 
        |> printfn "Total is %d"
        result 
        |> fst 
        |> findDuplicate
        |> printfn "First duplicate is %A"
        