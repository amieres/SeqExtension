/// Implements Seq.splitBy, Seq.splitAt, Seq.tryHeadTail
module Seq
    let rtn = Seq.singleton
    let insertO  vSO              = vSO |> Option.map(Seq.map Some) |> Option.defaultWith(fun () -> rtn None)
    let insertR (vSR:Result<_,_>) = vSR |> function | Error m -> rtn (Error m) | Ok v -> Seq.map Ok v
    let absorbO  vOS              = vOS |> Seq.choose id
    let absorbR  vOS              = vOS |> Seq.choose (function Ok v -> Some v |_-> None)
    let ofOption vO = 
        match vO with
        | Some v -> Seq.singleton v
        | None   -> Seq.empty    

    type [< RequireQualifiedAccess >] SplitByOption = Exclude | IncludeInFirst | IncludeInSecond

    type [< RequireQualifiedAccess >] private SplitSubUnfoldState<'T> = 
    | PostValue of 'T * SplitSubUnfoldState<'T>
    | Start     of seqNo: int * start: int
    | Started   of tryNext: (unit -> 'T option) * bingo: ('T -> unit) * finish: (unit -> unit) 
    | Finish

    type [< RequireQualifiedAccess >] private SplitUnfoldState<'T> = {
        enumerator          : System.Collections.Generic.IEnumerator<'T>
        mutable currentPos  : int
        mutable isDone      : bool
        mutable splitterO   : 'T     option
        mutable currentSeqO : 'T seq option
    }

    let getSplitById =
        let mutable id = 100
        fun () -> id <- id + 1 ; id

    /// Straight scan through is efficient, reusing seqs causes rescan from beginning
    let splitBy f opt (input: 'a seq) =
        let splitById = getSplitById()
        let getEnumerator() = input.GetEnumerator()
        let startingState() = 
            {
                SplitUnfoldState.enumerator   = getEnumerator()
                SplitUnfoldState.currentPos   = 0
                SplitUnfoldState.isDone       = false
                SplitUnfoldState.splitterO    = None
                SplitUnfoldState.currentSeqO  = None
            }, 0

        Seq.unfold(fun stateO ->
            let state, currentSeqNo = stateO |> Option.defaultWith startingState

            let tryNextMain() : 'a option=
                if state.isDone then None else
                if state.enumerator.MoveNext() then
                    state.currentPos  <- state.currentPos + 1
                    Some state.enumerator.Current
                else
                    state.enumerator.Dispose()
                    None
            let bingo  v  = state.splitterO <- Some v ; state.currentSeqO <- None
            let finish () = state.splitterO <- None   ; state.currentSeqO <- None ; state.isDone <- true   

            let tryNextSub start  =
                printfn "%A new sub enumerator %A" splitById start
                let en = getEnumerator()
                for i in 0..start - 1 do (en.MoveNext() |> ignore<bool>)
                fun () ->
                    if en.MoveNext()   then Some en.Current
                    else                    en.Dispose()
                                            None

            let subUnFold(tryNext: unit -> 'a option, bingo, finish) = 
                match tryNext() with
                | None      ->  finish()
                                None
                | Some v    ->
                    if f v then bingo v
                                if opt = SplitByOption.IncludeInFirst 
                                then Some( v, SplitSubUnfoldState.Finish                          )
                                else None
                    else             Some( v, SplitSubUnfoldState.Started(tryNext, bingo, finish) )

            //printfn "%i Unfold %A %A" splitById currentSeqNo state.currentPos
            while state.currentSeqO |> Option.isSome do 
                //printfn "%i skipping %A %A" splitById currentSeqNo state.currentPos
                subUnFold(tryNextMain, bingo , finish) |> ignore
            if state.isDone then None else
            if opt <> SplitByOption.IncludeInSecond then state.splitterO <- None
            let s0 =
                let start = SplitSubUnfoldState.Start(currentSeqNo, state.currentPos)
                match state.splitterO, opt with
                | Some v, SplitByOption.IncludeInSecond -> SplitSubUnfoldState.PostValue(v, start)
                | _                                     ->                                  start
                |> Seq.unfold(function
                    | SplitSubUnfoldState.PostValue(v, next)                 -> Some(v, next)
                    | SplitSubUnfoldState.Finish                             -> None
                    | SplitSubUnfoldState.Started(tryNext, bingo, finish)    -> subUnFold(tryNext, bingo, finish)
                    | SplitSubUnfoldState.Start(seqNo, myStart)              -> //printfn "%d Starting %d at %d = %d" splitById seqNo myStart state.currentPos
                                                                                if state.currentPos = myStart // && not state.isDone 
                                                                                then subUnFold(tryNextMain       , bingo , finish)
                                                                                else subUnFold(tryNextSub myStart, ignore, ignore)
                )
            state.currentSeqO <- Some s0
            Some (s0, Some(state, currentSeqNo + 1) )
        ) None

    let splitAt n s =
        s
        |> Seq.mapi(fun i v -> (i,v))
        |> splitBy (fst >> ((=) n ) (* >>! printfn " split %i %A" n *) ) SplitByOption.IncludeInSecond
        |> Seq.map (Seq.map snd)
        |> Seq.truncate 2

    let tryHeadTail fhead ftail s =
        ( Choice1Of3(), splitAt 1 s )
        ||> Seq.fold(function
            | Choice1Of3 (          ) -> Seq.tryHead >> Option.map fhead >> Choice2Of3
            | Choice2Of3 (Some headv) -> ftail    headv                  >> Choice3Of3
            | result                  -> fun _ -> result
        ) 
        |> function
        | Choice2Of3 (Some headv) -> ftail headv Seq.empty |> Some
        | Choice3Of3 v            -> Some v
        | _                       -> None

/////////// Sample Usage
//
//    [1..24] 
//    |> Seq.mapi  (fun i x -> printfn $"{i}: {x}"; x)
//    |> splitBy (fun n -> n % 3 = 0) SplitByOption.Exclude
//    // |> splitAt 4
//    |> Seq.map    Seq.cache // required because multiple uses of seq would force restarting from the beginning
//    |> Seq.filter (fun s -> Seq.length s < 100)
//    |> Seq.filter (fun s -> Seq.isEmpty s |> not)
//    |> Seq.filter (Seq.isEmpty >> not >>! print )
//    // |> Seq.skip 1
//    // |> Seq.take 2
//    |> Seq.iter (fun s -> 
//        //let s = Seq.cache s
//        printfn " ==> %A %A" (Seq.toList s) (Seq.length s)
//    )

//  let lines = 
//       """
//  10		CBR	280					
//  130	SALES REVENUE	DES			NP			
//  160	Net Sales Stock Warehouse			C	NP		8???2 +(00300) +(80020) +(80080)	B22/CPO"""
//      |> Seq.mapi    (fun i x -> printfn $"{i}: {x}"; x)
//      |> Seq.splitBy (fun   a -> a = '\n' || a = '\r') Seq.SplitByOption.Exclude
//      |> Seq.map     Seq.cache
//      |> Seq.filter (Seq.isEmpty >> not)
//      |> Seq.take 2
//  
//  lines 
//  |> Seq.iter (fun ar -> new string(Seq.toArray ar) |> (+) "***** " |> print)

