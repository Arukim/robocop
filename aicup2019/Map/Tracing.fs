namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils
open System.Numerics
open System

module Tracing =

    let castRay (tiles:Tile[][]) (start: single*single) (finish: single*single) =
        let vFrom, vTo = Vector2.fromTuple start, Vector2.fromTuple finish
        let inc: Vector2 = vFrom |> Vector2.sub vTo
                                 |> Vector2.Normalize 
                                 |> Vector2.mulS 0.5f
        
            
        let v = Vector2.fromTuple start + Vector2.One * 0.5f
        let cell = {X = Int32.MaxValue; Y = Int32.MaxValue}: Cell

        let rec trace (tiles:Tile[][]) (vTo: Vector2) prevCell pos =
            seq { 
                let newPos = pos + inc
                let newCell = {X = int newPos.X; Y = int newPos.Y}: Cell
                if newCell <> prevCell then yield newCell
                if Vector2.Distance(newPos, vTo) > 0.2f && tiles.[newCell.X].[newCell.Y] <> Tile.Wall then
                    yield! trace tiles vTo newCell newPos
            }

        trace tiles vTo cell v

    let castRay2 (tiles:Tile[][]) collisionFilter (start: Vector2) (finish: Vector2) =
           let inc: Vector2 = start |> Vector2.sub finish
                                    |> Vector2.Normalize 
                                    |> Vector2.mulS 0.1f

           let rec trace (tiles:Tile[][]) (inc: Vector2) (vTo: Vector2) pos =
                let newPos = pos + inc
                let newCell = {X = int newPos.X; Y = int newPos.Y}: Cell
                if Vector2.DistanceSquared(newPos, vTo) <= 0.2f*0.2f || collisionFilter tiles.[newCell.X].[newCell.Y] then
                    pos
                else trace tiles inc vTo newPos

           trace tiles inc finish start            

    let buildTraceMap (game: Game) start =
        game.Level.Tiles 
            |> Matrices.allBorders
            |> Seq.map(fun x -> single (fst x), single (snd x))
            |> Seq.map(fun x -> castRay game.Level.Tiles start x)
            |> Seq.concat
            |> Seq.distinct

    let traceHitFrame (tiles:Tile[][]) (from:Vec2Double) (target:Vec2Double) =
        let collisionFilterJump cell = Tile.Wall
        let vFrom = new Vector2(single from.X + 0.5f, single from.Y + 1.0f)
        let vTargets = seq {
            let x,y = single target.X, single target.Y
            for i = 0 to 9 do yield new Vector2(x, y + i*0.2f)
            for i = 0 to 4 do yield new Vector2(x + i*0.2f, y + 2.0f)
            for i = 0 to 9 do yield new Vector2(x + 1.0f, y + 2.0f - i*0.2f)
            for i = 0 to 4 do yield new Vector2(x + 1.0f - i*0.2f, y)
        }

        let traces = vTargets |> Seq.map(fun target -> castRay2 tiles vFrom target)

