﻿namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open System.Numerics
open Robocop.Utils
open Robocop.Core

type Warrior(armory: Armory, props: Properties, initial: Unit, id: int) =
    let marksman = new Marksman(props)
    let mutable pathfind: Map<Cell,Cell> = Map.empty  
    let mutable distMap: Map<Cell,single> = Map.empty
    let mutable startPos: Option<Vec2Double> = None
    let mutable path: Cell[] = Array.empty<Cell>
    let mutable nextStep: int = 0
    let mutable targetWeapon: Option<Vec2Double> = None
    let mutable targetMine: Option<Vec2Double> = None

    member _.DoTurn (unit:Unit) (game:Game) (location:Location) =   
        let nextLeg myPos = 
            path.Length - 1 > nextStep && Vector2.dist myPos path.[nextStep].toCenter < 0.1f
        if startPos.IsNone then
            startPos <- Some unit.Position
            
        let (shoot, aim, reload) = Diag.elapsedRelease "Marskman calc" (fun () -> marksman.TurnParse game unit)

        let myPos = new Vector2(single unit.Position.X, single unit.Position.Y)
        let myCell = Cell.fromVector unit.Position
        
        let mask = game.Units 
                        |> Array.filter(fun u -> u.Id <> initial.Id) 
                        |> Array.filter(fun u -> Vector2.Distance(Vector2.fromVec2Double unit.Position, Vector2.fromVec2Double u.Position) < 4.0f)
                        |> Array.map(fun u -> let p = Cell.fromVector u.Position
                                              [|p; p.up|])
                        |> Array.collect(fun x -> x)
                        |> Array.append (game.Mines |> Array.map(fun x -> (Cell.fromVector x.Position)))

        let tempMap = Diag.elapsedRelease "Build map" (fun () -> location.BuildMaskedMap mask)

        let jumpLeft = if (unit.OnGround || unit.OnLadder) then Constants.Max_Jump else (single unit.JumpState.MaxTime) * Constants.Max_Speed
          
        Logger.drawText(sprintf "jumpLeft %A" jumpLeft)

        Diag.elapsedRelease "Path graph" (fun () -> let newPath = Pathfinder.dijkstra tempMap (Cell.fromVector unit.Position) jumpLeft
                                                    match fst newPath |> Seq.isEmpty with
                                                        | false -> pathfind <- fst newPath
                                                                   distMap <- (snd newPath) |> Map.map(fun k v -> v.Dist)
                                                        | _ -> ignore())

        if unit.Weapon.IsNone then
            if targetWeapon.IsNone then
                targetWeapon <- armory.SelectWeapon(distMap)
            else                
                if not (armory.HasWeapon targetWeapon.Value) then
                     targetWeapon <- armory.SelectWeapon(distMap)
        else
            targetWeapon <- None

        if targetMine.IsSome then
            if not (armory.HasMine targetMine.Value) then
                targetMine <- None


        let mutable targetPos = unit.Position

        if targetWeapon.IsSome then
            targetPos <- targetWeapon.Value
        else
            let nearestEnemy = game.Units |> Array.filter(fun u -> u.PlayerId <> unit.PlayerId)
                                            |> Array.sortBy(fun u -> Pathfinder.findDistance distMap (Cell.fromVector u.Position))
                                            |> Seq.tryFind(fun _ -> true)
            if unit.Health < 85 then
                let nearestHealthPack = game.LootBoxes |> Array.choose(
                                                            fun b -> match b.Item with
                                                                        | Item.HealthPack _ -> Some b.Position
                                                                        | _ -> None)
                                                        |> Array.sortBy(fun p -> Pathfinder.findDistance distMap (Cell.fromVector p))
                                                        |> Seq.tryFind(fun _ -> true)
                if nearestHealthPack.IsSome then
                    targetPos <- nearestHealthPack.Value
                    else
                    targetPos <- startPos.Value
            else if armory.HasMines && unit.Mines < 2 then
                if targetMine.IsNone then
                    targetMine <- armory.SelectMine distMap

                if targetMine.IsSome then
                    targetPos <- targetMine.Value

            else if nearestEnemy.IsSome then
                targetPos <- nearestEnemy.Value.Position

        let mutable plantMine = false
        //targetPos <- {X=15.0;Y=26.0}
        if nextLeg myPos then 
            nextStep <- nextStep + 1
               
        if targetPos = startPos.Value then plantMine <- true

        let newPath = Diag.elapsedRelease "find path" (fun () -> Pathfinder.findPath pathfind myCell (Cell.fromVector targetPos) |> Seq.rev |> Array.ofSeq)

        if newPath <> path && path |> Array.skip (nextStep - 1) <> newPath then
            path <- newPath
            nextStep <- 1
        

        let color = if id = 0 then Palette.HotPink else Palette.LawnGreen
        path |> Seq.pairwise  |> Seq.iter  (fun (a,b) -> Logger.drawLine a.toCenter b.toCenter color)

        let nextTile = match path.Length with
                             | 1 -> path |> Seq.head
                             | x when x > 0 -> path |> Seq.skip nextStep |> Seq.head
                             | _ -> myCell

        let (jump, jumpDown, velocity) = Controller.makeMove game.Level.Tiles unit myCell nextTile

        {
            Velocity = velocity
            Jump = jump
            JumpDown = jumpDown
            Aim = aim
            Shoot = shoot
            SwapWeapon = false
            PlantMine = plantMine
            Reload = reload
        }
