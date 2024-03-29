﻿namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open System.Numerics
open Robocop.Utils

type Marksman(unitSim: UnitSim, props: Properties) =
    let predictPos game (unit:Unit) (enemy:Unit) (weapon:Weapon) =
        let comrad = game.Units |> Array.tryFind(fun x -> x.PlayerId = unit.PlayerId && x.Id <> unit.Id)
        let comradPos = match comrad with 
                            | Some u -> Vector2(single u.Position.X, single u.Position.Y + 1.0f)
                            | _ -> Vector2(-10.0f,-10.0f)
        let comradDirection = match comrad with 
                                   | Some u -> unitSim.Predict u (game.CurrentTick + 1)
                                   | _ -> Vector2.Zero

        let tgtDirection = unitSim.Predict enemy (game.CurrentTick + 1)
        let tgtPosition = Vector2(single enemy.Position.X, single enemy.Position.Y + 1.0f)
        let unitPos = Vector2(single unit.Position.X, single unit.Position.Y + 1.0f)

        let lastAngle = match weapon.LastAngle with
                            | Some x -> x
                            | _ -> 0.0

        let direction = new Vector2(single (cos lastAngle), single (sin lastAngle))

        let targetParams = {Pos = tgtPosition; Direction = tgtDirection; ComradPos = comradPos; ComradDirection = comradDirection;}
        let traceParams  = {
            Source = unitPos 
            BulletSpeed = single weapon.Parameters.Bullet.Speed / (single props.TicksPerSecond)
            BulletSize = single weapon.Parameters.Bullet.Size
            BulletExplosionRadius = match weapon.Parameters.Explosion with Some e -> single e.Radius | _ -> 0.0f
            Direction = direction
            Spread = single weapon.Spread
            Count = 100}
    
        let hits = Oracle.traceSectorHit game traceParams targetParams

        hits |> Seq.iter (fun (x,_) -> Logger.drawDot x Palette.DarkRed)

        let trace360Params = {
            Source = unitPos
            BulletSpeed = single weapon.Parameters.Bullet.Speed / (single props.TicksPerSecond)
            BulletSize = single weapon.Parameters.Bullet.Size
            BulletExplosionRadius = match weapon.Parameters.Explosion with Some e -> single e.Radius | _ -> 0.0f
            Direction = Vector2.UnitY
            Spread = single System.Math.PI * 2.0f
            Count = 360
        }
        let hits360 = Oracle.traceSectorHit game trace360Params targetParams
        //hits360 |> Seq.iter (fun (x,_) -> Logger.drawDot x Palette.BlueViolet)
        (hits, hits360)

    static member DistanceSqr (a: Vec2Double, b: Vec2Double) = 
        (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)

        
    static member WeaponPreference (t: WeaponType) =
        match t with
            //| WeaponType.RocketLauncher -> 0.0f
            | _ -> 0.0f

    member _.TurnParse (game:Game) (unit:Unit) =
        let myWeapon = unit.Weapon
        let nearestEnemy = game.Units |> Array.filter(fun u -> u.PlayerId <> unit.PlayerId)
                                               |> Array.sortBy(fun u -> Marksman.DistanceSqr(u.Position, unit.Position))
                                               |> Seq.tryFind(fun _ -> true)

        let mutable shoot = false
        let mutable reload = false
        let mutable angle = {X=0.0; Y=0.0}:Vec2Double
        if nearestEnemy.IsSome && myWeapon.IsSome then
            let weapon = myWeapon.Value

            
            let hits, hits360 = predictPos game unit nearestEnemy.Value myWeapon.Value 
            let lastAngle = match weapon.LastAngle with
                            | Some a -> {X=cos a; Y= sin a}:Vec2Double
                            | _ -> {X=0.0; Y=0.0}:Vec2Double

            let hitRate = match weapon.Typ with
                                | WeaponType.Pistol -> 80
                                | WeaponType.AssaultRifle -> 88
                                | WeaponType.RocketLauncher -> 90
                                | _ -> 90
            shoot <- hits |> Array.length > hitRate && weapon.FireTimer.IsNone
                       
            angle <- match not (hits360 |> Array.isEmpty) && not shoot with
                        | true -> let avgX = double (hits360 |> Array.averageBy(fun (_,x) -> x.X))
                                  let avgY = double (hits360 |> Array.averageBy(fun (_,x) -> x.Y))
                                  let a = new Vector2(single unit.Position.X, single unit.Position.Y + 1.0f)
                                  let n = new Vector2(a.X + single avgX * 50.0f, a.Y + single avgY * 50.0f)
                                  Logger.drawLine a n Palette.LawnGreen
                                  {X = avgX * 50.0; Y= avgY * 50.0}:Vec2Double
                        | _ -> lastAngle
            if hits360 |> Array.isEmpty then
                if weapon.Magazine < weapon.Parameters.MagazineSize / 2 then
                    reload <- true
    
            //Logger.drawText(sprintf "Hits %A, Hits360 %A, shoot %A" (hits |> Seq.length) (hits360 |> Seq.length) shoot)
        
        Logger.drawText(sprintf "Shoot %A, Angle %A, Reload %A" shoot angle reload)
        (shoot, angle, reload)


