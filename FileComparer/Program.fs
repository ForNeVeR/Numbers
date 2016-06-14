module FileComparer.Program

open System.IO

let private read path =
    printf "Reading [%s]..." path
    let mutable set = Set.empty
    use reader = new StreamReader (path)
    let stream = reader.BaseStream
    let mutable percent = 0
    let mutable line = reader.ReadLine ()
    while not <| isNull line do
        set <- Set.add line set
        line <- reader.ReadLine ()
        
        let percent' = int <| (double stream.Position) / (double stream.Length) * 100.0
        if percent <> percent' then
            percent <- percent'
            printf "\rReading [%s]... %d%%" path percent    
    
    printfn ""
    set

let private printDiff name1 name2 diff =
    printfn "Difference between [%s] and [%s]:" name1 name2
    if Set.isEmpty diff then
        printfn "[empty]"
    else
        Set.iter (printf "%s") diff

let private compare path1 path2 =
    let data1 = read path1
    let data2 = read path2

    printfn "Calculating set difference [%s] / [%s]..." path2 path1
    let diff1 = Set.difference data2 data1

    printfn "Calculating set difference [%s] / [%s]..." path1 path2
    let diff2 = Set.difference data1 data2

    printDiff path1 path2 diff1
    printDiff path1 path2 diff1

[<EntryPoint>]
let main = function
    | [| file1; file2 |] ->
        compare file1 file2
        0
    | _ ->
        printfn "Usage: FileComparer file1.txt file2.txt"
        -1
