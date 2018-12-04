namespace AdventOfCode

module Day2 =
    open System.IO
    open System

    let parseInput args =
        match args with
        | [] ->
            None
        | [filepath] ->
            Some(File.ReadAllLines filepath |> List.ofArray)

    //counts of individual characters, and the list of chars with those counts
    let charTotals (boxID:string) =
        boxID 
        |> Seq.toList
        |> Seq.countBy (fun x -> x)
        |> Seq.groupBy (fun x -> snd x)
        |> Seq.sortBy (fun x -> fst x)
        |> Seq.map (fun x -> (fst x, (snd x |> Seq.map fst |> Seq.sort)))

    //filter out any counts that aren't ones we care about
    let notSingles (col: seq<int * seq<char>>) =
        col 
        |> Seq.filter (fun x -> not ((x |> fst) = 1))

    let isListsEqual (a: seq<'a>) (b:seq<'a>) =
        (Seq.compareWith Operators.compare a b) = 0

    let checksum boxes =
        boxes 
        |> Seq.map charTotals
        |> Seq.map notSingles
        |> Seq.map (fun x -> Seq.map fst x)
        |> Seq.filter (fun x -> not (Seq.isEmpty x)) //some words don't have counts we care about
        |> Seq.collect (fun x -> x) //stuff them all the individual word arrays into one big one
        |> Seq.countBy (fun x -> x) //count them by number
        |> Seq.map snd //Grab the count totals
        |> Seq.reduce (fun x y -> x * y) //multiply all the totals together

    let except list1 =
        Seq.filter (fun x -> list1 |> Seq.exists (fun y -> y = x) |> not)

    let compareIds first second =
        let notShared = first |> Seq.toList |> except (second |> Seq.toList)
        let notShared2 = second |> Seq.toList |> except (first |> Seq.toList)
        match ((Seq.length notShared) = 1) && ((Seq.length notShared2) = 1) with
        | true ->
            printfn "notshared %A %A" notShared notShared2
            let notSharedChar = Seq.exactlyOne notShared
            let sharedString = first |> Seq.where (fun x -> not (x = notSharedChar)) |> Seq.toArray |> System.String
            printfn "SharedChars %A" sharedString
            Some (first, second)
        | false ->
            None

    let rec findMatchingPair boxes =
        match boxes with
        | first::rest ->
            let found = rest |> List.map (fun x -> compareIds first x) |> List.choose (fun x -> x)
            match found with
            | [] ->
                findMatchingPair rest
            | found -> Some found
        | [] -> None

    let execute (args: string list) =
        match args |> parseInput with
        | None ->
            printfn "%s" "specify a file"
        | Some(boxes) ->
            //boxes |> checksum |> printfn "%A"
            boxes |> findMatchingPair |> printfn "%A"
            