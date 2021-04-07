open TennisKata.Tiebreak


let shouldOutput expected observed: unit =
    if expected = observed then
        $"Success: {expected} equals {observed}" |> printfn "%s"
    else
        $"Failure: {expected} does not equal {observed}" |> printfn "%s"

[<EntryPoint>]
let main _ =
    decide (GameInProgress {Player1=0;Player2=0}) (ScorePoint (ScoringPlayer.from Player1))
    |> printfn "%A"
    0
