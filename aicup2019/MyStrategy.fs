namespace AiCup2019

open AiCup2019.Model
open Robocop.Map

type MyStrategy() =
    static member DistanceSqr (a: Vec2Double, b: Vec2Double) = 
        (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)
            
    member this.getAction(unit: Unit, game: Game, debug: Debug) =
        let nearestEnemy = game.Units |> Array.filter(fun u -> u.PlayerId <> unit.PlayerId)
                                        |> Array.sortBy(fun u -> MyStrategy.DistanceSqr(u.Position, unit.Position))
                                        |> Seq.tryFind(fun _ -> true)

        let nearestWeapon = game.LootBoxes |> Array.choose(fun b -> match b.Item with
                                                                        | Item.Weapon _ -> Some b.Position
                                                                        | _ -> None)
                                            |> Array.sortBy(fun p -> MyStrategy.DistanceSqr(p, unit.Position))
                                            |> Seq.tryFind(fun _ -> true)
        
        let nearestHealthPack = game.LootBoxes |> Array.choose(fun b -> match b.Item with
                                                                        | Item.HealthPack _ -> Some b.Position
                                                                        | _ -> None)
                                                |> Array.sortBy(fun p -> MyStrategy.DistanceSqr(p, unit.Position))
                                                |> Seq.tryFind(fun _ -> true)

        //Tracing.buildTraceMap game (single (unit.Position.X),single (unit.Position.Y))
        //    |> Seq.iter(fun cell -> debug.draw(CustomData.Rect {
        //                                            Pos = {X = (single cell.X) + 0.5f; Y = (single cell.Y) + 0.5f}
        //                                            Size = {X = 0.1f; Y = 0.1f}
        //                                            Color = {R = 0.0f; G = 200.0f; B = 0.0f; A = 255.0f}
        //                                            }))

        let mutable targetPos = unit.Position

        if not unit.Weapon.IsSome && nearestWeapon.IsSome then
            targetPos <- nearestWeapon.Value
        else if unit.Health < 66 && nearestHealthPack.IsSome then
            targetPos <- nearestHealthPack.Value
        else if nearestEnemy.IsSome then
            targetPos <- nearestEnemy.Value.Position

        debug.draw(CustomData.Log {Text = sprintf "Target pos: %A" targetPos })

        let aim: Vec2Double = match nearestEnemy with
                                | Some x -> { X = x.Position.X - unit.Position.X; Y = x.Position.Y - unit.Position.Y} 
                                | None -> { X = 0.0; Y = 0.0 }

        let mutable jump = targetPos.Y > unit.Position.Y

        if targetPos.X > unit.Position.X && game.Level.Tiles.[(int unit.Position.X + 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true
        if targetPos.X < unit.Position.X && game.Level.Tiles.[(int unit.Position.X - 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true
                        
        {
            Velocity = targetPos.X - unit.Position.X
            Jump = jump
            JumpDown = not jump
            Aim = aim
            Shoot = true
            SwapWeapon = false
            PlantMine = false
        }          
