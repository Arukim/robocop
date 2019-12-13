namespace Robocop.Map

open System.Numerics
open AiCup2019.Model
open Robocop.Core
open Robocop.Utils

type TraceParams = {Source: Vector2; BulletSpeed: single; Direction: Vector2; Spread: single; Count: int;}
type TargetParams = {Pos: Vector2; Direction: Vector2}

module Oracle =
    let traceFuture (game: Game) (trace: TraceParams) (target: TargetParams) =
        let tiles = game.Level.Tiles 

        let bulletDiscretion = 10

        let rotation i = Quaternion.CreateFromYawPitchRoll(0.0f, 0.0f, (single i) * trace.Spread / (single trace.Count))

        let initialBullets = seq { for i in -trace.Count/2 .. trace.Count/2 -> (trace.Source,  Vector2.Normalize(Vector2.Transform(trace.Direction, rotation i))*(trace.BulletSpeed / single bulletDiscretion))}
                            |> List.ofSeq

        let unitMove = target.Direction / single bulletDiscretion
                        
        let checkWallHit (pos:Vector2) = 
            tiles.[int pos.X].[int pos.Y] = Tile.Wall


        let checkUnitXHit x y =
            let a, b, c = (int(x - 0.5f), int(x + 0.5f), int y)
            tiles.[a].[c] = Tile.Wall || tiles.[b].[c] = Tile.Wall
        
        let checkUnitYHit x y =
            let a, b, c = (int(y - 1.0f), int(y + 1.0f), int x)
            tiles.[c].[a] = Tile.Wall || tiles.[c].[b] = Tile.Wall

        let checkHit (unit:Vector2) (delta:Vector2) =
            let newPos = unit + delta
            new Vector2(
                match checkUnitXHit newPos.X newPos.Y with
                    | true -> unit.X
                    | false -> newPos.X
                ,
                match checkUnitYHit newPos.X newPos.Y with
                    | true -> unit.Y
                    | false -> newPos.Y
            )
            
            

        let checkTargetHit (tgt:Vector2) (bullet:Vector2) =
            abs (tgt.X - bullet.X) < 0.5f && abs (tgt.Y - bullet.Y) < 1.0f

        let rec sim unit (bullets: (Vector2*Vector2) list) =
            seq {
                let newUnit = checkHit unit unitMove
                
                let (hits, misses) = bullets |> List.choose(fun (p,d) ->
                                                                let newPos = p + d
                                                                match checkWallHit newPos with
                                                                    | false -> Some (newPos,d)
                                                                    | _ -> None)
                                         |> List.partition(fun (p,_) -> checkTargetHit newUnit p)
                
                yield! hits

                if not misses.IsEmpty then yield! sim newUnit misses
            }

        sim target.Pos initialBullets |> Array.ofSeq



