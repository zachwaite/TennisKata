module Tiebreak

open System

type Player =
    | Player1
    | Player2

type Score = Score of uint

type GameInProgress = {
    Player1Score: Score
    Player2Score: Score
}

type TiebreakState =
    | InProgess of GameInProgress
    | Complete of Player

let (|AtLeastOnePlayerScored7|_|) (winner:Player) (state:TiebreakState) =
    if state.Player1Score >= Score 7u
        || state.Player2Score >= Score 7u
    then Some ()
    else None

let (|ScoreDifferenceAtLeast2|_|) (state:TiebreakState) =
    let (Score player1Score) = state.Player1Score
    let (Score player2Score) = state.Player2Score
    if abs (int64 player1Score - int64 player2Score) > 1L
    then Some ()
    else None

// let update state player =
//     let newState = state // add point to a winning player
//     // try to determine if there is a winner
//     // if not, return newState
//     // if we do, return Winner player
//     state

let update (originalState:TiebreakState) (winner:Player): TiebreakState =
    match originalState, winner with
    | AtLeastOnePlayerScored7 Player1 & ScoreDifferenceAtLeast2, Player1 ->
        Complete Player1
    | AtLeastOnePlayerScored7 & ScoreDifferenceAtLeast2, Player2 -> Complete Player2
