type GameState =
    | ZeroZero          // 0, 0
    | ZeroFifteen       // 0, 15
    | FifteenZero       // 15, 0
    | ZeroThirty        // 0, 30
    | ThirtyZero        // 30, 0
    | ZeroForty         // 0, 40
    | FortyZero         // 40, 0
    | FifteenFifteen    // 15, 15
    | FifteenThirty     // 15, 30
    | ThirtyFifteen     // 30, 15
    | FifteenForty      // 15, 40
    | FortyFifteen      // 40, 15
    | ThirtyThirty      // 30, 30
    | ThirtyForty       // 30, 40
    | FortyThirty       // 40, 30
    | Deuce             // 40, 40
    | AdvantagePlayer1  // 40+, 40 
    | AdvantagePlayer2  // 40, 40+
    | WinnerPlayer1     // 
    | WinnerPlayer2     // 

type Victor =
    | Player1
    | Player2

let handleGameOutcome (state:GameState) (victor:Victor): GameState =
    match state, victor with
    | ZeroZero, Player1 -> FifteenZero
    | ZeroZero, Player2 -> ZeroFifteen
    | ZeroFifteen, Player1 -> FifteenFifteen
    | ZeroFifteen, Player2 -> ZeroThirty
    | ZeroThirty, Player1 -> FifteenThirty
    | ZeroThirty, Player2 -> ZeroForty
    | ZeroForty, Player1 -> FifteenForty
    | ZeroForty, Player2 -> WinnerPlayer2
    | FifteenZero, Player1 -> ThirtyZero
    | FifteenZero, Player2 -> FifteenFifteen
    | FifteenFifteen, Player1 -> ThirtyFifteen
    | FifteenFifteen, Player2 -> FifteenThirty
    | FifteenThirty, Player1 -> ThirtyThirty
    | FifteenThirty, Player2 -> FifteenForty
    | FifteenForty, Player1 -> ThirtyForty
    | FifteenForty, Player2 -> WinnerPlayer2
    | ThirtyZero, Player1 -> FortyZero
    | ThirtyZero, Player2 -> ThirtyFifteen
    | ThirtyFifteen, Player1 -> FortyFifteen
    | ThirtyFifteen, Player2 -> ThirtyThirty
    | ThirtyThirty, Player1 -> FortyThirty
    | ThirtyThirty, Player2 -> ThirtyForty
    | ThirtyForty, Player1 -> Deuce
    | ThirtyForty, Player2 -> WinnerPlayer2
    | FortyZero, Player1 -> WinnerPlayer1
    | FortyZero, Player2 -> FortyFifteen
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

[<EntryPoint>]
let main _ =
    handleGameOutcome ZeroZero Player1 |> printfn "%A"
    0