namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils
open System.Numerics
open System

module Tracing =

    type cell = {X: int; Y: int}


    let private castRay (tiles:Tile[][]) (start: single*single) (finish: single*single) =
        let vFrom, vTo = Vector2.fromTuple start, Vector2.fromTuple finish
        let inc:Vector2 = vFrom |> Vector2.sub vTo
                                |> Vector2.Normalize 
                                |> Vector2.mulS 0.5f
        
            
        let v = Vector2.fromTuple start + Vector2.One * 0.5f
        let cell = {X = Int32.MaxValue; Y = Int32.MaxValue}

        let rec trace (tiles:Tile[][]) (vTo:Vector2) prevCell pos =
            seq { 
                let newPos = pos + inc
                let newCell = {X = int newPos.X; Y = int newPos.Y}
                if newCell <> prevCell then yield newCell
                if Vector2.Distance(newPos, vTo) > 0.2f && tiles.[newCell.X].[newCell.Y] <> Tile.Wall then
                    yield! trace tiles vTo newCell newPos
            }

        trace tiles vTo cell v


    let buildTraceMap (game: Game) start =
        game.Level.Tiles 
            |> Matrices.allBorders
            |> Seq.map(fun x -> single (fst x), single (snd x))
            |> Seq.map(fun x -> castRay game.Level.Tiles start x)
            |> Seq.concat
            |> Seq.distinct

