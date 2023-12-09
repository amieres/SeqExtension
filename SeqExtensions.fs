module Seq
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

    /// Straight scan through is efficient, reusing seqs causes rescan from beginning
    let splitBy f opt (input: 'a seq) =
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
                let en = getEnumerator()
                for i in 0..start - 1 do (en.MoveNext() |> ignore)
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

            //printfn "Unfold %A" currentSeqNo
            while state.currentSeqO |> Option.isSome do 
                //printfn "skipping"
                subUnFold(tryNextMain, bingo , finish) |> ignore
            if state.isDone then None else
            if opt <> SplitByOption.IncludeInSecond then state.splitterO <- None
            let s0 =
                let start = SplitSubUnfoldState.Start(currentSeqNo, state.currentPos)
                match state.splitterO, opt with
                | Some v, SplitByOption.IncludeInSecond -> SplitSubUnfoldState.PostValue(v, start)
                | _                                   ->                                  start
                |> Seq.unfold(function
                    | SplitSubUnfoldState.PostValue(v, next)                 -> Some(v, next)
                    | SplitSubUnfoldState.Finish                             -> None
                    | SplitSubUnfoldState.Started(tryNext, bingo, finish)    -> subUnFold(tryNext, bingo, finish)
                    | SplitSubUnfoldState.Start(seqNo, myStart)              -> //printfn "Starting %d at %d = %d" seqNo myStart state.currentPos
                                                                                if state.currentPos = myStart // && not state.isDone 
                                                                                then subUnFold(tryNextMain       , bingo , finish)
                                                                                else subUnFold(tryNextSub myStart, ignore, ignore)
                )
            state.currentSeqO <- Some s0
            Some (s0, Some(state, currentSeqNo + 1) )
        ) None

    let splitAt n s =
        s
        |> Seq.mapi(fun i v -> i,v)
        |> splitBy (fst >> ((=) n )) SplitByOption.IncludeInSecond
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
        | Choice2Of3 (Some headv) -> Seq.empty |> ftail headv |> Some
        | Choice3Of3 v            -> Some v
        | _                       -> None
