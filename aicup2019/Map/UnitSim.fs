namespace Robocop.Map

open System.Numerics
open AiCup2019.Model
open Robocop.Utils

type UnitSim(gameLength, players) =
    let turnLog : Option<Unit> [,]= Array2D.create players gameLength None
    let mutable currTurnNum = 0

    let predictFuture id turn =
        let prevTurn, currTurn = Vector2.fromVec2Double turnLog.[id % players, turn - 2].Value.Position, 
                                 Vector2.fromVec2Double turnLog.[id % players, turn - 1].Value.Position
        currTurn - prevTurn

    member _.AddTurn unit turn = 
        currTurnNum <- turn
        turnLog.[unit.Id % players, turn] <- Some unit

    member _.Predict unit turn = 
        match turn with
            | t when t <= currTurnNum -> Vector2.Zero
            | _ -> predictFuture unit.Id turn