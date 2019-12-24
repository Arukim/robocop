namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open System.Numerics
open Robocop.Utils
open Robocop.Core

type Warrior(armory: Armory, unitSim: UnitSim, props: Properties, initial: Unit, id: int) =
    let marksman = new Marksman(unitSim, props)
    let controller = new Controller()
    let mutable routeMap: Map<Cell,Link> = Map.empty  
    let mutable distMap: Map<Cell,single> = Map.empty
    let mutable startPos: Option<Vec2Double> = None
    let mutable targetWeapon: Option<Vec2Double> = None
    let mutable targetMine: Option<Vec2Double> = None
    let mutable prevCell = {X=0;Y=0}:Cell

    member _.DoTurn (unit:Unit) (game:Game) (location:Location) (evasion:PlayerModel[])=
        if startPos.IsNone then
            startPos <- Some unit.Position

        let mid = game.Level.Tiles.Length / 2

        let (shoot, aim, reload) = Diag.elapsedRelease "Marskman calc" (fun () -> marksman.TurnParse game unit)

        let myPos = new Vector2(single unit.Position.X, single unit.Position.Y)
        let myCell = Cell.fromVector unit.Position
           
        let cellChanged = prevCell <> myCell     
        let mask = game.Units 
                        |> Array.filter(fun u -> u.Id <> initial.Id) 
                        |> Array.filter(fun u -> abs(mid - int u.Position.X) > abs(mid - int unit.Position.X) && unit.PlayerId = u.PlayerId)
                        |> Array.map(fun u -> let p = Cell.fromVector u.Position
                                              [|p; p.up|])
                        |> Array.collect(fun x -> x)
                        |> Array.append (game.Mines |> Array.map(fun x -> (Cell.fromVector x.Position)))

                        
                        
        let mutable plantMine = false

        let tempMap = Diag.elapsedRelease "Build map" (fun () -> location.BuildMaskedMap mask)

        let jumpTimeLeft = if (unit.OnGround || unit.OnLadder) then Constants.Max_Jump else (single unit.JumpState.MaxTime) * Constants.Max_Speed
          
        Diag.elapsedRelease "Path graph" (fun () -> let newPath = Pathfinder.dijkstra tempMap (Cell.fromVector unit.Position) jumpTimeLeft
                                                    match fst newPath |> Seq.isEmpty with
                                                        | false -> routeMap <- fst newPath
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
            else if armory.HasMines && unit.Mines < 1 then
                if targetMine.IsNone then
                    targetMine <- armory.SelectMine distMap

                if targetMine.IsSome then
                    targetPos <- targetMine.Value

            else if nearestEnemy.IsSome then
                targetPos <- nearestEnemy.Value.Position
        //targetPos <- {X=15.0;Y=26.0}
               
        if targetPos = startPos.Value then plantMine <- true

        let newPath = Pathfinder.findPath routeMap myCell (Cell.fromVector targetPos) |> Seq.rev |> Array.ofSeq
        
        controller.PrepareMove unit
        controller.CurrentPath <- newPath       

        let color = if id = 0 then Palette.HotPink else Palette.LawnGreen
        controller.CurrentPath |> Seq.iter  (fun link -> Logger.drawLine link.Source.toCenter link.Target.toCenter color)

        let mutable (jump, jumpDown, velocity) = match controller.MakeMove game.Level.Tiles unit with
                                                        | Some x -> x
                                                        | _ -> (false,false, 0.0)
        
        if not (evasion |> Array.forall(fun x -> x.DamageReceived = 0)) then


            let bestEvasion = snd (evasion 
                                |> Array.filter(fun x -> unit.JumpState.Speed = 0.0 || (x.Velocity <> Vector2.Zero))
                                |> Array.groupBy(fun x -> x.DamageReceived)
                                |> Array.sortBy(fun d -> fst d)
                                |> Array.head)

            let move = bestEvasion |> Array.sortBy(fun x -> abs ((float x.Velocity.X) - velocity)) |> Array.head
            let evade = move.Velocity                
            jump <- evade.Y > 0.0f
            jumpDown <- evade.Y < 0.0f
            velocity <- match evade.X with
                            | 0.0f -> 0.0
                            | x when x > 0.0f -> 10.0
                            | _ -> -10.0

        
        Logger.drawText (sprintf "J: %A JD: %A V: %A" jump jumpDown velocity)

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
