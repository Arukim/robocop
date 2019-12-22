namespace Robocop.Map

open System.Numerics
open AiCup2019.Model
open Robocop.Core
open Robocop.Utils

type TraceParams = {Source: Vector2; BulletSpeed: single; BulletSize: single; BulletExplosionRadius: single;
                    Direction: Vector2; Spread: single; Count: int;}
type TargetParams = {Pos: Vector2; Direction: Vector2; ComradPos: Vector2; ComradDirection: Vector2}

type SimModel = 
    val mutable Pos: Vector2
    val mutable IsMoving : bool
    val Velocity: Vector2
    val Id: int

    new(id, pos, vel) = {Id=id; Pos = pos; Velocity = vel; IsMoving = true}


type BulletModel =
    inherit SimModel
    
    val Size: single
    val Damage: int
    new(id, pos, vel, size, dmg) = {inherit SimModel(id, pos, vel); Size = size; Damage = dmg}

type PlayerModel =
    inherit SimModel

    val mutable DamageReceived: int
    val HitBy : array<bool>
    new(id, pos, vel, bullets) = {inherit SimModel(id, pos, vel); DamageReceived = 0; HitBy=Array.create bullets false}

module Oracle =
    let bulletDiscretion = 10
    
    let checkBulletWallHit (tiles:Tile[][]) bulletSize (pos:Vector2) = 
        let bulletHalf = bulletSize / 2.0f
        tiles.[int (pos.X - bulletHalf)].[int (pos.Y + bulletHalf)] = Tile.Wall ||
        tiles.[int (pos.X + bulletHalf)].[int (pos.Y - bulletHalf)] = Tile.Wall ||
        tiles.[int (pos.X - bulletHalf)].[int (pos.Y - bulletHalf)] = Tile.Wall ||
        tiles.[int (pos.X + bulletHalf)].[int (pos.Y + bulletHalf)] = Tile.Wall
    
    let checkTargetHit (tgt:Vector2) (bullet:Vector2) =
        abs (tgt.X - bullet.X) < 0.5f && abs (tgt.Y - bullet.Y) < 1.0f

    let traceSectorHit (game: Game) (trace: TraceParams) (target: TargetParams) =
        let tiles = game.Level.Tiles 

        let rotation i = Quaternion.CreateFromYawPitchRoll(0.0f, 0.0f, (single i) * trace.Spread / (single trace.Count))

        let initialBullets = seq { for i in -trace.Count/2 .. trace.Count/2 -> (trace.Source,  Vector2.Normalize(Vector2.Transform(trace.Direction, rotation i))*(trace.BulletSpeed / single bulletDiscretion))}
                            |> List.ofSeq

        let unitMove = target.Direction / single bulletDiscretion
        let comardMove = target.ComradDirection / single bulletDiscretion        

        let minX, maxX = (0, tiles.Length)
        let minY, maxY = (0, tiles.[0].Length)
        let sanityCheckX x = x <= minX || x >= maxX
        let sanityCheckY y = y <= minY || y >= maxY

        let checkUnitXHit x y =
            let a, b, c = (int(x - 0.5f), int(x + 0.5f), int y)
            sanityCheckX a || sanityCheckY c || tiles.[a].[c] = Tile.Wall || tiles.[b].[c] = Tile.Wall
        
        let checkUnitYHit x y =
            let a, b, c = (int(y - 0.9f), int(y + 0.9f), int x)
            sanityCheckX c || sanityCheckY a || tiles.[c].[a] = Tile.Wall || tiles.[c].[b] = Tile.Wall

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

        let checkExplosion (unit:Vector2) (exp:Vector2) =
            if trace.BulletExplosionRadius > 0.0f then
                let dx, dy = abs(unit.X - exp.X), abs(unit.Y - exp.Y)
                dx < trace.BulletExplosionRadius + 0.5f
                    && dy < trace.BulletExplosionRadius + 0.9f
            else
                false

        let rec sim unit comrad (bullets: (Vector2*Vector2) list) =
            seq {
                let newUnit = checkHit unit unitMove

                let newComrad = checkHit comrad comardMove
                

                let (walls, others) = bullets |> List.map(fun (p,d) -> (p+d,d))
                                              |> List.partition(fun (p,_) -> checkBulletWallHit tiles trace.BulletSize p || checkTargetHit newComrad p)

                let expHits = walls |> List.filter(fun (p, _) -> checkExplosion newUnit p)

                let (hits, misses) = others |> List.partition(fun (p,_) -> checkTargetHit newUnit p)

                yield! expHits
                yield! hits

                if not misses.IsEmpty then yield! sim newUnit newComrad misses
            }

        sim target.Pos target.ComradPos initialBullets |> Array.ofSeq

    
    let velocities = 
        [|
            Vector2(0.0f, 0.0f)
            Vector2(10.0f,0.0f)
            Vector2(10.0f,10.0f)
            Vector2(0.0f, 10.0f)
            Vector2(-10.0f,10.0f)
            Vector2(-10.0f,0.0f)
            Vector2(-10.0f, -10.0f)
            Vector2(0.0f, -10.0f)
            Vector2(10.0f, -10.0f)
        |]

    let traceEvasion maxDuration myId (game: Game) =
        let tiles = game.Level.Tiles 
        
        let minX, maxX = (0, tiles.Length)
        let minY, maxY = (0, tiles.[0].Length)
        let sanityCheckX x = x <= minX || x >= maxX
        let sanityCheckY y = y <= minY || y >= maxY

        let checkUnitXHit x y =
            let a, b, c = (int(x - 0.5f), int(x + 0.5f), int y)
            sanityCheckX a || sanityCheckY c || sanityCheckX b || tiles.[a].[c] = Tile.Wall || tiles.[b].[c] = Tile.Wall
        
        let checkUnitYHit x y =
            let a, b, c = (int(y - 1.0f), int(y + 1.0f), int x)
            sanityCheckX c || sanityCheckY a || sanityCheckY b || tiles.[c].[a] = Tile.Wall || tiles.[c].[b] = Tile.Wall

        let checkUnitHit (p:Vector2) =
            checkUnitXHit p.X p.Y && checkUnitYHit p.X p.Y

        let discretion =single( Constants.Ticks_Per_Second * bulletDiscretion)
        
        let bullets = game.Bullets |> Array.mapi(fun i x -> 
            BulletModel(i, Vector2.fromVec2Double x.Position, (Vector2.fromVec2Double x.Velocity) / discretion, single x.Size, x.Damage))

        let players = game.Units 
                            |> Array.filter(fun x -> x.PlayerId = myId)
                            |> Array.mapi(fun _ x -> velocities |> Array.map(fun v -> 
                                PlayerModel(x.Id, Vector2(single x.Position.X, single x.Position.Y + 1.0f), v / discretion, bullets.Length)))
                            |> Array.collect(fun x -> x)

        for _ in 1..maxDuration * bulletDiscretion do        
            for p in players |> Seq.filter(fun x -> x.IsMoving) do
                let newPos = p.Pos + p.Velocity
                if not (checkUnitHit newPos) then   
                    p.Pos <- newPos
                else
                    p.IsMoving <- false
                    
            for b in bullets |> Seq.filter(fun x -> x.IsMoving) do
                let newPos = b.Pos + b.Velocity
                if not (checkBulletWallHit tiles b.Size newPos) then
                    b.Pos <- newPos
                    for p in players do
                        if checkTargetHit p.Pos b.Pos then
                            p.HitBy.[b.Id] <- true                 
                else
                    b.IsMoving <- false

        for p in players do
            for i in 0..p.HitBy.Length-1 do
                let h = p.HitBy.[i]
                p.DamageReceived <- p.DamageReceived + (match h with true -> bullets.[i].Damage | _ -> 0)           

        players


