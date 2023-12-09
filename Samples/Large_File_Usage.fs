
// The following example uses Seq.splitBy and Seq.tryHeadTail 
// twice to split the input file first in 2 and then into several pieces
// collecting the output file names in the first section and the content in the subsequent pieces
// it can split a huge 2.2GB file into several subfiles without having to rescan the file
// or cache in memory any portions of the file apart from the current line

// the first Seq.tryHeadTail operates on a seq<seq<string>>. Because of the nested seq it must use 2
// separate functions to process first the head and then the tail without backtracking,
// the result from the first function is passed to the second:
//     |> Seq.tryHeadTail (fun head -> ...; filelist) (fun filelist tail -> ...)

// the second Seq.tryHeadTail operates on a seq<string>, because it is not nested seqs it can be called simply:
//     |> Seq.tryHeadTail id (fun head tail -> ...)

open System.IO

let splitFile path file =
    File.ReadLines file
    //|> Seq.mapi    (fun i v -> printfn "%d -> %s" i v; v)
    |> Seq.skip 1 
    |> Seq.splitBy    (fun l    -> l.EndsWith "---*/") Seq.SplitByOption.IncludeInSecond
    |> Seq.tryHeadTail(fun head ->
        let filelist = head |> Seq.map (fun s-> s.Split('.') |> Seq.last) |> Seq.toArray
        for file in filelist do printfn "file: %A" file
        printfn "--------"
        filelist
    ) (fun filelist tail ->
        tail
        |> Seq.collect id
        |> Seq.splitBy (fun l -> l.EndsWith " rows affected)" || l = "(1 row affected)") Seq.SplitByOption.IncludeInFirst
        |> Seq.map (Seq.skip 1)
        |> Seq.iteri(fun i s ->
            if i < filelist.Length then
                s 
                |> Seq.tryHeadTail id (fun head tail -> 
                    printfn "%s => %s" filelist.[i] head
                    File.WriteAllLines(path + filelist.[i] + ".rpt", Seq.append [head] tail) 
                )
                |> Option.defaultValue ()
        )
    )
    |> Option.defaultValue ()
