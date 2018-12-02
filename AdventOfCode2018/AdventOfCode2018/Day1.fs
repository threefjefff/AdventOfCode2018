namespace AdventOfCode
open System.IO
module Day1 = 
    let readFile filepath =
        File.ReadAllLines filepath |> Seq.map int

    let inputAsInts args =
        match args with
        | [filepath] ->
            printfn "Attempting to read filepath %s" filepath
            readFile filepath
        | _ ->
            Seq.map int args 

    let runningTotal (inputs: int seq) =
        inputs 
        |> Seq.mapFold (fun total next -> (total + next, total + next)) 0
        |> fst

    //Credit goes to reddit. I am a bad person.
    //1. After a set repeats, it's increases all values by the offset
    //2. The first repeated value has to be in the original set 
    //Therefore. Get the original sequence, find the offset, search the offset values for a match, continue offsetting till you do
    let findRepetition changes =
        let runningTotal = //Find the original sequence
            Seq.scan (+) 0 changes
            |> Seq.tail
            |> Seq.toArray
        let possibleValues = Set.ofArray runningTotal //And keep them, because we know the answer is one of these
        let offset = Array.last runningTotal //Find the offset (the last value of the running total)
        let rec iterate sums =
            let newSums = (Array.map ((+) offset) sums) //Get the new offset sequence
            let firstMatch = Array.tryFind (fun i -> Set.contains i possibleValues) newSums //Try to get a value
            match firstMatch with
            | Some x -> x //Stop if you find one
            | None -> iterate newSums //Or go for another rotation if you don't
        iterate runningTotal

    let execute (args: string list)=
        inputAsInts args |> Seq.sum |> printfn "Total is %d"
        inputAsInts args |> findRepetition |> printfn "First duplicate is %O"
        