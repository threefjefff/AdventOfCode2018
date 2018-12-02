open AdventOfCode

let dayArg argList = 
    match argList with
    | [] ->
        None
    | first :: rest ->
        Some(first |> int, rest)

let executeDay day args =
    match day with
    | 1 ->
        printfn "day 1, args: %A" args
        Day1.execute args
    | _ ->
        printfn "This day isn't ready yet!"

[<EntryPoint>]
let main argv =
    let day = List.ofSeq argv |> dayArg
    match day with
    | Some (day, additionalArgs) ->
        executeDay day additionalArgs
    | None ->
        printfn "Enter with <day> argument to run that day's solution!"
    0 // Return an integer exit code