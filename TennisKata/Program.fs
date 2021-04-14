open TennisKata
open TennisKata.Tiebreak

type Player =
    | Player1
    | Player2

module RegularGame =
    type State =
        | ``Love - Love``
        | ``Love - Fifteen``
        | ``Fifteen - Love``
        | ``Love - Thirty``
        | ``Thirty - Love``
        | ``Love - Forty``
        | ``Forty - Love``
        | ``Fifteen - Fifteen``
        | ``Fifteen - Thirty``
        | ``Thirty - Fifteen``
        | ``Fifteen - Forty``
        | ``Forty - Fifteen``
        | ``Thirty - Thirty``
        | ``Thirty - Forty``
        | ``Forty - Thirty``
        | Deuce
        | Advantage of Player
        | GameWon of Player

    let start () = ``Love - Love``

    let registerOutcome state ballWinner =
        match state, ballWinner with
        | ``Love - Love``, Player1 -> ``Fifteen - Love``
        | ``Love - Love``, Player2 -> ``Love - Fifteen``
        | ``Love - Fifteen``, Player1 -> ``Fifteen - Fifteen``
        | ``Love - Fifteen``, Player2 -> ``Love - Thirty``
        | ``Fifteen - Love``, Player1 -> ``Thirty - Love``
        | ``Fifteen - Love``, Player2 -> ``Fifteen - Fifteen``
        | ``Love - Thirty``, Player1 -> ``Fifteen - Thirty``
        | ``Love - Thirty``, Player2 -> ``Love - Forty``
        | ``Thirty - Love``, Player1 -> ``Forty - Love``
        | ``Thirty - Love``, Player2 -> ``Thirty - Fifteen``
        | ``Love - Forty``, Player1 -> ``Fifteen - Forty``
        | ``Love - Forty``, Player2 -> GameWon Player2
        | ``Forty - Love``, Player1 -> GameWon Player1
        | ``Forty - Love``, Player2 -> ``Forty - Fifteen``
        | ``Fifteen - Fifteen``, Player1 -> ``Thirty - Fifteen``
        | ``Fifteen - Fifteen``, Player2 -> ``Fifteen - Thirty``
        | ``Fifteen - Thirty``, Player1 -> ``Thirty - Thirty``
        | ``Fifteen - Thirty``, Player2 -> ``Fifteen - Forty``
        | ``Thirty - Fifteen``, Player1 -> ``Forty - Fifteen``
        | ``Thirty - Fifteen``, Player2 -> ``Thirty - Thirty``
        | ``Fifteen - Forty``, Player1 -> ``Thirty - Forty``
        | ``Fifteen - Forty``, Player2 -> GameWon Player2
        | ``Forty - Fifteen``, Player1 -> GameWon Player1
        | ``Forty - Fifteen``, Player2 -> ``Forty - Thirty``
        | ``Thirty - Thirty``, Player1 -> ``Forty - Thirty``
        | ``Thirty - Thirty``, Player2 -> ``Thirty - Forty``
        | ``Thirty - Forty``, Player1 -> Deuce
        | ``Thirty - Forty``, Player2 -> GameWon Player2
        | ``Forty - Thirty``, Player1 -> GameWon Player1
        | ``Forty - Thirty``, Player2 -> Deuce
        | Deuce, Player1 -> Advantage Player1
        | Deuce, Player2 -> Advantage Player2
        | Advantage Player1, Player1 -> GameWon Player1
        | Advantage Player1, Player2 -> Deuce
        | Advantage Player2, Player1 -> Deuce
        | Advantage Player2, Player2 -> GameWon Player2
        | GameWon _, _ -> failwith "The game has concluded."

module TieBreak =
    type Score = { Player1: uint; Player2: uint }

    type State =
        | Score of Score
        | Won of Player

    let start () = Score { Player1 = 0u; Player2 = 0u }

    let private (|Leads|_|) player score =
        match player, score with
        | Player1,
          { Player1 = player1Score
            Player2 = player2Score } when player1Score > player2Score -> Some()
        | Player2,
          { Player1 = player1Score
            Player2 = player2Score } when player2Score > player1Score -> Some()
        | _ -> None

    let private (|ScoredAtLeast6Points|_|) player score =
        match player, score with
        | Player1, { Player1 = score } when score >= 6u -> Some()
        | Player2, { Player2 = score } when score >= 6u -> Some()
        | _ -> None

    let private updateScore score ballWinner =
        match ballWinner with
        | Player1 ->
            { score with
                  Player1 = score.Player1 + 1u }
        | Player2 ->
            { score with
                  Player2 = score.Player2 + 1u }

    let registerOutcome state ballWinner =
        match state with
        | Score score ->
            match score with
            | Leads ballWinner & ScoredAtLeast6Points ballWinner -> Won ballWinner
            | _ -> Score(updateScore score ballWinner)
        | Won _ ->
            failwith
                "It's not possible to update the points during this state of the game."


module Set =
    type Score = { Player1: uint; Player2: uint }

    type State =
        | Score of Score
        | Tiebreak
        | Won of Player

    let start () = Score { Player1 = 0u; Player2 = 0u }

    let private (|Leads|_|) player score =
        match player, score with
        | Player1,
          { Player1 = player1Score
            Player2 = player2Score } when player1Score > player2Score -> Some()
        | Player2,
          { Player1 = player1Score
            Player2 = player2Score } when player2Score > player1Score -> Some()
        | _ -> None

    let private (|ScoredAtLeast5Points|_|) player score =
        match player, score with
        | Player1, { Player1 = score } when score >= 5u -> Some()
        | Player2, { Player2 = score } when score >= 5u -> Some()
        | _ -> None

    let private (|Tie66|NotTie|) score =
        match score with
        | { Player1 = 6u; Player2 = 6u } -> Tie66
        | _ -> NotTie

    let private updateScore score gameWinner =
        match gameWinner with
        | Player1 ->
            { score with
                  Player1 = score.Player1 + 1u }
        | Player2 ->
            { score with
                  Player2 = score.Player2 + 1u }

    let registerOutcome state gameWinner =
        match state with
        | Score score ->
            match score with
            | Leads gameWinner & ScoredAtLeast5Points gameWinner -> Won gameWinner
            | _ ->
                let updatedScore = updateScore score gameWinner

                match updatedScore with
                | Tie66 -> Tiebreak
                | NotTie -> Score(updatedScore)
        | Tiebreak -> Won gameWinner
        | Won _ ->
            failwith
                "It's not possible to update the points during this state of the game."

module SetTracking =
    type GameState =
        | RegularGameState of RegularGame.State
        | TiebreakGameState of TieBreak.State

    type SetState = SetState of Set.State

    type State =
        { GameState: GameState
          SetState: SetState }

    let start () =
        { GameState = (RegularGameState(RegularGame.start ()))
          SetState = SetState(Set.start ()) }
     
    let (|ShouldBeginTiebreak|ShouldBeginRegular|) (setState) =
        match setState with
        | Set.State.Tiebreak -> ShouldBeginTiebreak
        | _ -> ShouldBeginRegular
    
    let registerOutcome state ballWinner =
        match state.SetState with
        | SetState (Set.State.Won _) -> failwith "Set is already complete"
        | _ ->
            let newGameState =
                match state.GameState with
                | RegularGameState x ->
                    RegularGameState (RegularGame.registerOutcome x ballWinner)
                | TiebreakGameState x ->
                    TiebreakGameState (TieBreak.registerOutcome x ballWinner)
            let (SetState setState) = state.SetState        
            match newGameState with
            | RegularGameState (RegularGame.State.GameWon player) ->
                 let newSetState = Set.registerOutcome setState player
                 match newSetState with
                 | ShouldBeginRegular ->
                     { SetState=SetState (Set.registerOutcome setState player);
                       GameState=RegularGameState(RegularGame.start ())}
                 | ShouldBeginTiebreak ->
                     { SetState=SetState (Set.registerOutcome setState player);
                       GameState=TiebreakGameState(TieBreak.start ())}
            | RegularGameState _ ->
                 { state with GameState=newGameState; }
            | TiebreakGameState (TieBreak.State.Won player) ->
                 let newSetState = Set.registerOutcome setState player
                 match newSetState with
                 | ShouldBeginRegular ->
                     { SetState=SetState (Set.registerOutcome setState player);
                       GameState=RegularGameState(RegularGame.start ())}
                 | ShouldBeginTiebreak ->
                     { SetState=SetState (Set.registerOutcome setState player);
                       GameState=TiebreakGameState(TieBreak.start ())}
            | TiebreakGameState _ ->
                 { state with GameState=newGameState; }

// Switch into Tiebreak when set score = 6 to 6
// After tiebreak game concludes, switch back to regular games
// What happens when set is already won? -> raise


[<EntryPoint>]
let main _ =
    let winPlayer1 = [Player1;Player1;Player1;Player1;]
    let winPlayer2 = [Player2;Player2;Player2;Player2;]
    let winTiebreakPlayer1 = [Player1;Player1;Player1;Player1;Player1;Player1;Player1;]
    let win5Player1 = winPlayer1 @ winPlayer1 @ winPlayer1 @ winPlayer1 @ winPlayer1
    let win5Player2 = winPlayer2 @ winPlayer2 @ winPlayer2 @ winPlayer2 @ winPlayer2
    win5Player1 @ win5Player2 @ winPlayer1 @ winPlayer2 @ winTiebreakPlayer1
    |> List.fold SetTracking.registerOutcome (SetTracking.start ())
    |> printfn "%A"
    0

