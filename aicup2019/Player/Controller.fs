namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils
open System.Numerics

module Controller =

    let defaultMove unit (source:Vector2) (target:Vector2) (tgt:Cell)  =
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
                    | t when t > 0.0f -> 10.0
                    | t when t < 0.0f -> -10.0                    
                    | _ -> 0.0
        (jump, jumpDown, vel)

    let jumpDownMove (tiles: Tile[][]) (src:Cell) =
        if tiles.[src.X].[src.Y-1] = Tile.Platform then
            (false, true, 0.0)
        else
            if tiles.[src.X-1].[src.Y-1] = Tile.Wall then
                (false, false, 10.0)
            else
                (false, false, -10.0)

    let makeMove (tiles: Tile[][]) (unit:Unit) (src:Cell) (tgt:Cell) =
        let source, target = new Vector2(single unit.Position.X, single unit.Position.Y), tgt.toMidBottom
       
        match tgt.Y < src.Y && unit.OnGround with
                | true -> (jumpDownMove tiles src)
                | false -> (defaultMove unit source target tgt)

