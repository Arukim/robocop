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

           let rec trace (inc: Vector2) (vTo: Vector2) pos =
                let newPos = pos + inc
                let newCell = {X = int newPos.X; Y = int newPos.Y}: Cell
                if Vector2.DistanceSquared(newPos, vTo) <= 0.2f*0.2f || collisionFilter tiles.[newCell.X].[newCell.Y] then
                    pos
                else trace inc vTo newPos

           trace inc finish start            
    
    let castRay3 collisionFilter (start: Vector2) (finish: Vector2) =
        let inc: Vector2 = start |> Vector2.sub finish
                                 |> Vector2.Normalize 
                                 |> Vector2.mulS 0.01f

        let rec trace (inc: Vector2) (vTo: Vector2) pos =            
             let newPos = pos + inc
             if Vector2.Distance(newPos, vTo) <= 0.1f || collisionFilter newPos then
                 newPos
             else trace inc vTo newPos

        trace inc finish start

    let buildTraceMap (game: Game) start =
        game.Level.Tiles 
            |> Matrices.allBorders
            |> Seq.map(fun x -> single (fst x), single (snd x))
            |> Seq.map(fun x -> castRay game.Level.Tiles start x)
            |> Seq.concat
            |> Seq.distinct

    let traceHitFrame (tiles:Tile[][]) (from:Vec2Double) (target:Vec2Double) =
        let filter (target: Vector2) (pos:Vector2) = 
            tiles.[int pos.X].[int pos.Y] = Tile.Wall ||
                ((pos.X > target.X - 0.7f) && (pos.X < target.X + 0.7f) &&
                 (pos.Y > target.Y - 1.2f) && (pos.Y < (target.Y + 1.2f)))
                
        let vFrom, vTarget = new Vector2(single from.X, single from.Y + 1.0f), new Vector2(single target.X, single target.Y + 1.0f)
        let vTargets = seq {
                            let x,y = single target.X, single target.Y
                            for i = 0 to 10 do yield new Vector2(x - 0.7f, y + (single i)*0.2f)
                            for i = 0 to 6 do yield new Vector2(x - 0.7f + (single i)*0.2f, y + 2.2f)
                            for i = 0 to 10 do yield new Vector2(x + 0.7f, y + 2.2f - (single i)*0.2f)
                            for i = 0 to 6 do yield new Vector2(x + 0.7f - (single i)*0.2f, y)
                        } 
                        |> Array.ofSeq

        let a, b = vTarget, vTarget + Vector2.crossNorm vTarget vFrom
        let traces = vTargets |> Array.map(fun target -> castRay3 (filter vTarget) vFrom target)
                              |> Array.filter(fun t -> abs (t.X - vTarget.X) < 0.75f && abs (t.Y - vTarget.Y) < 1.25f)
                              |> Array.map(fun x -> Geom.closestPointToLine a b x)
                              |> Array.ofSeq        
        
        traces |> Seq.iter(fun x -> Logger.drawDot x Palette.OrangeRed)

        Logger.drawDotD vFrom

        let cross = seq { for a in traces do
                           for b in traces do 
                                if a <> b then yield (a, b);}
                                /// todo vectors are on same line
        cross |> Seq.sortByDescending(fun (a,b) -> Vector2.DistanceSquared(a,b))
              |> Seq.tryHead

        




