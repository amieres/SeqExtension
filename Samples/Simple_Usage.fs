/////////// Simple usage

[ -1 ; 0 ; 1; 2; 3; -1; 3; 5; 7; -1; 2; 3; 9 ; -1] 
|> Seq.map  (fun x -> printfn "---> %A" x; x)
|> Seq.splitBy ((=) -1) Seq.SplitByOption.Exclude
|> Seq.skip 2
//|> Seq.take 2
|> Seq.iter (fun s -> 
    let s = Seq.cache s
    // this Seq.cache/Seq.toArray is required because Seq.length (Seq.isEmpty) consumes (partly) the sequence 
    // and then the next Seq.*** operation needs to restart
    // to avoid this behaviour Seq.cache can be used or Seq.tryHeadTail
    Seq.length s |> printfn " ==> %A %A" (Seq.toList s) ) 

"Hello friend how are you? Good "
|> Seq.toArray
|> Seq.map  (fun x -> printfn "---> %A" x; x)
|> Seq.splitBy ((=) ' ') Seq.SplitByOption.IncludeInFirst
//|> Seq.iter (printfn "%A")
|> Seq.skip 3
//|> Seq.take 2
|> Seq.iter (fun s -> 
    let s = Seq.cache s
    Seq.length s |> printfn " ==> %A %A" (System.String(Seq.toArray s)) ) 


[ 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ]
|> Seq.map  (fun x -> printfn "---> %A" x; x)
|> Seq.splitAt  2
|> printfn "%A"

[ 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ]
|> Seq.map  (fun x -> printfn "---> %A" x; x)
|> Seq.tryHeadTail id     (fun head tail -> printfn "head = %A, tail = %A" head tail )
|> Option.defaultWith (fun ()        -> printfn "No Head Tail"                   )

