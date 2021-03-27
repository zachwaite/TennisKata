open System

type Player =
    | Player1
    | Player2

type Score =
    | Zero
    | Fifteen
    | Thirty
    | Forty
    
type GameInProgress = {
    Player1Score:Score
    Player2Score:Score
}

type GameWithAdvantage =
    | AdvantagePlayer1
    | AdvantagePlayer2

type GameCompleted = {
    Winner:Player
}

type Game =
    | GameInProgress of GameInProgress
    | GameWithAdvantage of GameWithAdvantage
    | GameCompleted of GameCompleted

let initGame =
    GameInProgress {Player1Score=Zero; Player2Score=Zero;}

type WinEvent =
    | Player1
    | Player2

let handleWinEvent winEvent game: Game =
    match winEvent, game with
    | Player1, GameInProgress {Player1Score=Zero; Player2Score=player2Score} -> 
        GameInProgress {Player1Score=Fifteen; Player2Score=player2Score}
    | Player1, GameInProgress {Player1Score=Fifteen; Player2Score=player2Score} ->
        GameInProgress {Player1Score=Thirty; Player2Score=player2Score}
    | Player1, GameInProgress {Player1Score=Thirty; Player2Score=player2Score} -> 
        GameInProgress {Player1Score=Forty; Player2Score=player2Score}
    | Player1, GameInProgress {Player1Score=Forty; Player2Score=player2Score} ->
        if player2Score = Forty then
            GameWithAdvantage AdvantagePlayer1
        else GameCompleted {Winner=Player.Player1}
    | Player1, GameWithAdvantage AdvantagePlayer1 ->
        GameCompleted {Winner=Player.Player1}
    | Player1, GameWithAdvantage AdvantagePlayer2 ->
        GameInProgress {Player1Score=Forty; Player2Score=Forty}
    | Player2, GameInProgress {Player2Score=Zero; Player1Score=player1Score} -> 
        GameInProgress {Player2Score=Fifteen; Player1Score=player1Score}
    | Player2, GameInProgress {Player2Score=Fifteen; Player1Score=player1Score} -> 
        GameInProgress {Player2Score=Thirty; Player1Score=player1Score}
    | Player2, GameInProgress {Player2Score=Thirty; Player1Score=player1Score} -> 
        GameInProgress {Player2Score=Forty; Player1Score=player1Score}
    | Player2, GameInProgress {Player2Score=Forty; Player1Score=player1Score} ->
        if player1Score = Forty then
            GameWithAdvantage AdvantagePlayer2
        else GameCompleted {Winner=Player.Player2}
    | Player2, GameWithAdvantage AdvantagePlayer2 ->
        GameCompleted {Winner=Player.Player2}
    | Player2, GameWithAdvantage AdvantagePlayer1 ->
        GameInProgress {Player1Score=Forty; Player2Score=Forty}
    | _, (GameCompleted _ as gameCompleted) -> gameCompleted


[<EntryPoint>]
let main _ =
    initGame
    |> handleWinEvent Player1
    |> handleWinEvent Player1
    |> handleWinEvent Player1
    |> handleWinEvent Player2
    |> handleWinEvent Player2
    |> handleWinEvent Player2
    |> handleWinEvent Player1
    |> handleWinEvent Player2
    |> handleWinEvent Player2
    |> handleWinEvent Player2
    |> Console.WriteLine
    0