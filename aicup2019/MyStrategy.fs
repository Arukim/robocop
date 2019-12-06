namespace AiCup2019

open AiCup2019.Model
open Robocop.Map
open System

type MyStrategy() =
    let location: Zones.Location = new Zones.Location();
    let elapsed msg f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
#if DEBUG
        printfn "%s elapsed Time: %i ms " msg timer.ElapsedMilliseconds
#endif
        returnValue
    static member DistanceSqr (a: Vec2Double, b: Vec2Double) = 
        (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)

            
    member this.getAction(unit: Unit, game: Game, debug: Debug) =
        
        elapsed "Location init" (fun () -> location.Parse(game.Level.Tiles))       

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
        
        //let upJumps = elapsed(fun() -> game.Level.Tiles |> location.EdgeUpGroundParse)

        //upJumps |> Seq.iter (fun edges ->
        //                           let p1, p2 = edges
        //                           debug.draw(CustomData.Line {
        //                               P1 = {X = p1.X; Y = p1.Y}
        //                               P2 = {X = p2.X; Y = p2.Y}
        //                               Width = 0.05f
        //                               Color = {R = 0.0f; G = 0.5f; B =0.0f; A = 1.0f}
        //                               }))
                                       
        //printfn "downjumps count %d" (upJumps |> Seq.length)
        
       

        //let downJumps = elapsed(fun () -> game.Level.Tiles |> location.EdgeDownGroundParse)
        //printfn "downjumps count %d" (downJumps |> Seq.length)
        
        //downJumps
        //    |> Seq.iter (fun edges ->
        //                            let p1, p2 = edges
        //                            debug.draw(CustomData.Line {
        //                                P1 = {X = p1.X; Y = p1.Y}
        //                                P2 = {X = p2.X; Y = p2.Y}
        //                                Width = 0.05f
        //                                Color = {R = 0.0f; G = 0.0f; B =0.75f; A = 0.5f}
        //                                }))

        //let groundLines = elapsed(fun () -> game.Level.Tiles 
        //                                   |> location.GroundsAndPlatformsParse)
        //printfn "groundLines count %d" (groundLines |> Seq.length)


        
        elapsed "Path map" (fun () -> game.Level.Tiles |> location.buildPathMap)

        //groundLines |> Seq.iter (fun edges ->
        //                       let p1, p2 = edges
        //                       debug.draw(CustomData.Line {
        //                           P1 = {X = p1.X; Y = p1.Y}
        //                           P2 = {X = p2.X; Y = p2.Y}
        //                           Width = 0.1f
        //                           Color = {R = 0.0f; G = 0.9f; B = 0.9f; A = 1.0f}
        //                           }))
                                                                      

        location.Grounds 
            |> Seq.iteri (fun i p ->
                                    let cell = p.Cells |> Seq.head
                                    debug.draw(CustomData.Rect {
                                        Pos = {X = (single cell.X); Y = (single cell.Y) + 0.5f}
                                        Size = {X = single (p.Cells |> Seq.length); Y = 0.25f}
                                        Color = {R = (100.0f + single (25 * i))/ 255.0f ; G = (100.0f)/ 255.0f; B = 0.0f; A = 1.0f}
                                        }))

        location.Ladders 
            |> Seq.iteri (fun i p ->
                                    let cell = p.Cells |> Seq.head
                                    debug.draw(CustomData.Rect {
                                        Pos = {X = (single cell.X)  + 0.5f; Y = (single cell.Y)}
                                        Size = {X = 0.25f; Y = single (p.Cells |> Seq.length)}
                                        Color = {R = (100.0f + single (25 * i))/ 255.0f ; G = 0.0f; B = (100.0f)/ 255.0f; A = 1.0f}
                                        }))

        location.Platforms 
                  |> Seq.iteri (fun i p ->
                                          let cell = p.Cells |> Seq.head
                                          debug.draw(CustomData.Rect {
                                              Pos = {X = (single cell.X); Y = (single cell.Y) + 0.5f}
                                              Size = {X = single (p.Cells |> Seq.length); Y = 0.25f}
                                              Color = {R = (100.0f + single (25 * i))/ 255.0f ; G = 0.0f; B = (100.0f + single (25 * i)); A = 1.0f}
                                              }))

        let mutable targetPos = unit.Position

        if not unit.Weapon.IsSome && nearestWeapon.IsSome then
            targetPos <- nearestWeapon.Value
        else if unit.Health < 90 && nearestHealthPack.IsSome then
            targetPos <- nearestHealthPack.Value
        else if nearestEnemy.IsSome then
            targetPos <- nearestEnemy.Value.Position

        //debug.draw(CustomData.Log {Text = sprintf "Target pos: %A" targetPos })

        let aim: Vec2Double = match nearestEnemy with
                                | Some x -> { X = x.Position.X - unit.Position.X; Y = x.Position.Y - unit.Position.Y} 
                                | None -> { X = 0.0; Y = 0.0 }

        let mutable jump = targetPos.Y > unit.Position.Y

        if targetPos.X > unit.Position.X && game.Level.Tiles.[(int unit.Position.X + 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true
        if targetPos.X < unit.Position.X && game.Level.Tiles.[(int unit.Position.X - 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true        
        
        let checkGrenadeSafety (pos:Vec2Double) (aim:Vec2Double) tiles =
            let dist = Tracing.castRay tiles ((single pos.X),(single pos.Y)) ((single aim.X),(single aim.Y)) |> Seq.length
            debug.draw(CustomData.Log {Text = sprintf "Dist is: %A" dist })
            printf "Dist is: %A\n" dist
            dist > 3

        let shoot = match unit.Weapon with
                        | Some x when x.Typ = WeaponType.RocketLauncher -> checkGrenadeSafety unit.Position aim game.Level.Tiles
                        | _ -> true

        let maxVel curr tgt =
            if tgt > curr then 10.0 else -10.0

        let velocity = if (unit.Position.Y - targetPos.Y) >= -0.5 then maxVel unit.Position.X targetPos.X else targetPos.X - unit.Position.X

        {
            Velocity = velocity
            Jump = jump
            JumpDown = not jump
            Aim = aim
            Shoot = shoot
            SwapWeapon = false
            PlantMine = false
            Reload = false
        }          
