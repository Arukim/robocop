namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils
open System.Numerics

module Controller =
    let makeMove (unit:Unit) (source:Vector2) (tgt:Cell) =
        let target = tgt.toMidBottom

        let checkJump =
            match unit.OnLadder with
                | true -> match target.Y - source.Y with
                            | x when x > 0.1f -> (true,false)
                            | x when x < 0.1f -> (false, true)
                            | _ -> (false, false)
                | false -> match unit.OnGround with
                                | true -> (target.Y - source.Y > 0.5f,target.Y - source.Y < -2.0f)
                                | false -> (target.Y - source.Y > -1.0f && unit.JumpState.CanJump,target.Y - source.Y < -1.0f)


        let (jump, jumpDown) = checkJump

        Logger.drawText(sprintf "From %A to %A jump %A jumpDown %A" source tgt jump jumpDown)
        Logger.cellHighlight tgt.toCenter Palette.Red
        // slow down near tgt??
        let vel = match target.X - source.X with
                    | t when t > 0.1f -> 10.0
                    | t when t < 0.1f -> -10.0                    
                    | _ -> 0.0
        (jump, jumpDown, vel)

