namespace Robocop.Map

open System.Numerics
open AiCup2019.Model
open Robocop.Utils

type UnitSim(gameLength) =
    let turnLog : Option<Unit> array= Array.create gameLength None
    let mutable currTurnNum = 0

    let predictFuture turn =
        let prevTurn, currTurn = Vector2.fromVec2Double turnLog.[turn - 1].Value.Position, 
                                 Vector2.fromVec2Double turnLog.[turn - 2].Value.Position
        currTurn - prevTurn

    member _.AddTurn turn unit = 
        currTurnNum <- turn
        turnLog.[turn] <- Some unit

    member _.Predict turn = 
        match turn with
            | t when t <= currTurnNum -> Vector2.Zero
            | _ -> predictFuture turn