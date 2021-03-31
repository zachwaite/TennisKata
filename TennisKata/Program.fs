open Game
 
let accumulate
    (init:GameState)
    (pointWinners:PointWinEvent list): GameState =
    pointWinners |> List.fold evolveGameState init
    
let accumulate' = accumulate LoveAll


let shouldOutput expected observed: unit =
    if expected = observed then
        $"Success: {expected} equals {observed}" |> printfn "%s"
    else
        $"Failure: {expected} does not equal {observed}" |> printfn "%s"

[<EntryPoint>]
let main _ =
    [
        Player1
        Player1
        Player1
        Player2
        Player2
        Player2
        Player1
        Player2
        Player2
        Player2
        Player2
    ]
    |> accumulate'
    |> shouldOutput WinnerPlayer2
    0