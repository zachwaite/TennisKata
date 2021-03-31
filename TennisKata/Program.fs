type GameState =
    | LoveAll          // 0, 0 - initial state
    | LoveFifteen       // 0, 15
    | FifteenLove       // 15, 0
    | LoveThirty        // 0, 30
    | ThirtyLove        // 30, 0
    | LoveForty         // 0, 40
    | FortyLove         // 40, 0
    | FifteenAll        // 15, 15
    | FifteenThirty     // 15, 30
    | ThirtyFifteen     // 30, 15
    | FifteenForty      // 15, 40
    | FortyFifteen      // 40, 15
    | ThirtyAll         // 30, 30
    | ThirtyForty       // 30, 40
    | FortyThirty       // 40, 30
    | Deuce             // 40, 40
    | AdvantagePlayer1  // 40+, 40 
    | AdvantagePlayer2  // 40, 40+
    | WinnerPlayer1     // terminal state
    | WinnerPlayer2     // terminal state

type PointWinEvent =
    | Player1
    | Player2

let evolveGameState
    (beginningState:GameState)
    (winner:PointWinEvent): GameState =
    match beginningState, winner with
    | LoveAll, Player1 -> FifteenLove
    | LoveAll, Player2 -> LoveFifteen
    | LoveFifteen, Player1 -> FifteenAll
    | LoveFifteen, Player2 -> LoveThirty
    | LoveThirty, Player1 -> FifteenThirty
    | LoveThirty, Player2 -> LoveForty
    | LoveForty, Player1 -> FifteenForty
    | LoveForty, Player2 -> WinnerPlayer2
    | FifteenLove, Player1 -> ThirtyLove
    | FifteenLove, Player2 -> FifteenAll
    | FifteenAll, Player1 -> ThirtyFifteen
    | FifteenAll, Player2 -> FifteenThirty
    | FifteenThirty, Player1 -> ThirtyAll
    | FifteenThirty, Player2 -> FifteenForty
    | FifteenForty, Player1 -> ThirtyForty
    | FifteenForty, Player2 -> WinnerPlayer2
    | ThirtyLove, Player1 -> FortyLove
    | ThirtyLove, Player2 -> ThirtyFifteen
    | ThirtyFifteen, Player1 -> FortyFifteen
    | ThirtyFifteen, Player2 -> ThirtyAll
    | ThirtyAll, Player1 -> FortyThirty
    | ThirtyAll, Player2 -> ThirtyForty
    | ThirtyForty, Player1 -> Deuce
    | ThirtyForty, Player2 -> WinnerPlayer2
    | FortyLove, Player1 -> WinnerPlayer1
    | FortyLove, Player2 -> FortyFifteen
    | FortyFifteen, Player1 -> WinnerPlayer1
    | FortyFifteen, Player2 -> FortyThirty
    | FortyThirty, Player1 -> WinnerPlayer1
    | FortyThirty, Player2 -> Deuce
    | Deuce, Player1 -> AdvantagePlayer1
    | Deuce, Player2 -> AdvantagePlayer2
    | AdvantagePlayer1, Player1 -> WinnerPlayer1
    | AdvantagePlayer1, Player2 -> Deuce
    | AdvantagePlayer2, Player1 -> Deuce
    | AdvantagePlayer2, Player2 -> WinnerPlayer2
    | WinnerPlayer1, _ -> WinnerPlayer1
    | WinnerPlayer2, _ -> WinnerPlayer2
 
let accumulate
    (init:GameState)
    (pointWinners:PointWinEvent list): GameState =
    pointWinners |> List.fold evolveGameState init

[<EntryPoint>]
let main _ =
    let pointWinOutcomes = [
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
    let accumulate' = accumulate LoveAll
    pointWinOutcomes |> accumulate' |> printfn "%A"
    0