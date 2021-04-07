module TennisKata.Tiebreak

type TiebreakGameError = TiebreakGameError of string

type Player =
    | Player1
    | Player2

type Winner = Winner of Player
type ScoringPlayer = ScoringPlayer of Player

type GameScore = {
    Player1:int
    Player2:int
}

type TiebreakGameCommand =
    | StartGame
    | ScorePoint of ScoringPlayer

type TiebreakGameEvent =
    | GameStarted
    | PointScored of ScoringPlayer
    | GameWon of Winner

type TiebreakGameState =
    | GameScheduled
    | GameInProgress of GameScore
    | GameOver of Winner

let updateScore (score:GameScore) (player:ScoringPlayer): GameScore =
    match player with
    | ScoringPlayer(Player1) -> {score with Player1=score.Player1 + 1}
    | ScoringPlayer(Player2) -> {score with Player1=score.Player1 + 1}

let (|WinningBy2|_|) (score:GameScore) = 
    if System.Math.Abs(score.Player1 - score.Player2) > 1 then
        Some WinningBy2
    else
        None

let (|Player1Has7|Player2Has7|BothOrNeitherHas7|) (score:GameScore) =
    if score.Player1 >= 7 && score.Player2 < 7 then
        Player1Has7
    elif score.Player2 >= 7 && score.Player1 < 7 then
        Player2Has7
    else
        BothOrNeitherHas7

let stateFromScore (score:GameScore): TiebreakGameState =
    match score with
    | WinningBy2 & Player1Has7 -> GameOver(Winner(Player1))
    | WinningBy2 & Player2Has7 -> GameOver(Winner(Player2))
    | _ -> GameInProgress score


let evolve (state:TiebreakGameState) (event:TiebreakGameEvent): TiebreakGameState =
    match state, event with
    | GameScheduled, GameStarted -> GameInProgress {Player1=0;Player2=0;}
    | GameScheduled, _ -> failwith "Cannot score in a game that hasn't started"
    | _, GameStarted -> failwith "Game can only be started from state GameScheduled"
    | (GameInProgress score), (PointScored(ScoringPlayer Player1)) ->
        (updateScore score (ScoringPlayer(Player1))) |> stateFromScore
    | (GameInProgress score), (PointScored(ScoringPlayer Player2)) ->
        (updateScore score (ScoringPlayer(Player2))) |> stateFromScore
    | (GameInProgress _), GameWon player -> GameOver player
    | _, GameWon _ -> failwith "Only games in progress can be won"
    | GameOver _, _ -> failwith "Cannot update a game that is over"


// Infrastructure level concerns, not domain
let hydrate (init:TiebreakGameState) (events:TiebreakGameEvent list): TiebreakGameState =
    events |> List.fold evolve init

let tryHydrate (init:TiebreakGameState) (events:TiebreakGameEvent list): Result<TiebreakGameState, TiebreakGameError> =
    try
        Ok (hydrate init events)
    with
        | ex -> ex.ToString() |> TiebreakGameError |> Error

let decide (state:TiebreakGameState) (command:TiebreakGameCommand): Result<TiebreakGameEvent list, TiebreakGameError> =
    match state, command with
    | state, StartGame -> 
        match (tryHydrate state [GameStarted;]) with
        | Ok _ -> Ok [GameStarted;]
        | Error err -> Error err
    | state, ScorePoint(ScoringPlayer player) ->
        match (tryHydrate state [PointScored(ScoringPlayer player);]) with
        | Ok _ -> Ok [PointScored(ScoringPlayer player);]
        | Error err -> Error err
