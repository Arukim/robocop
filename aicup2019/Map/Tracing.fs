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

    let castRay2 (tiles:Tile[][]) (start: Vector2) (finish: Vector2) =
           let inc: Vector2 = start |> Vector2.sub finish
                                    |> Vector2.Normalize 
                                    |> Vector2.mulS 0.1f

           let rec trace (tiles:Tile[][]) (inc: Vector2) (vTo: Vector2) pos =
                let newPos = pos + inc
                let newCell = {X = int newPos.X; Y = int newPos.Y}: Cell
                if Vector2.DistanceSquared(newPos, vTo) <= 0.2f*0.2f || tiles.[newCell.X].[newCell.Y] = Tile.Wall then
                    pos
                else trace tiles inc vTo newPos

           let tail = trace tiles inc finish start            
           tail

    let buildTraceMap (game: Game) start =
        game.Level.Tiles 
            |> Matrices.allBorders
            |> Seq.map(fun x -> single (fst x), single (snd x))
            |> Seq.map(fun x -> castRay game.Level.Tiles start x)
            |> Seq.concat
            |> Seq.distinct

