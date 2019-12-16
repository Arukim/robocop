namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils
open System.Numerics
open Robocop.Core

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

        //Logger.drawText(sprintf "From %A to %A jump %A jumpDown %A" source tgt jump jumpDown)
        Logger.cellHighlight tgt.toCenter Palette.GreenYellow
        // slow down near tgt??
        let dir = match target.X - source.X with
                    | t when t > 0.0f -> 1.0f
                    | t when t < 0.0f -> -1.0f                
                    | _ -> 0.0f

        let spd = match abs (target.X - source.X) with
                    | t when t > Constants.One_Tick_Move -> 10.0f
                    | d -> d * single Constants.Ticks_Per_Second

        let tgt = float (dir * spd)

        (jump, jumpDown, tgt)

    let jumpDownMove (tiles: Tile[][]) (src:Cell) =
        if tiles.[src.X-1].[src.Y-1] = Tile.Wall then
            (false, true, 10.0)
        else
            (false, true, -10.0)

    let makeMove (tiles: Tile[][]) (unit:Unit) (src:Cell) (tgt:Cell) =
        let source, target = new Vector2(single unit.Position.X, single unit.Position.Y), tgt.toCenter
       
        match tgt.Y < src.Y && unit.OnGround with
                | true -> (jumpDownMove tiles src)
                | false -> (defaultMove unit source target tgt)

