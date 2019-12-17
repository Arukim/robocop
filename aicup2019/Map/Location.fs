namespace Robocop.Map

open AiCup2019.Model
open Robocop.Core
open Robocop.Utils
open System.Numerics
open System


type Location() =
       let genCell x y v = ({X=x;Y=y}:Cell),v 
       let collisionFilterFall cell =
           cell = Tile.Wall || cell = Tile.Ladder || cell = Tile.Platform        
       let collisionFilterJump cell =
           cell = Tile.Wall
       let collisionFilterGrounds cell =
           cell = Tile.Wall || cell = Tile.Platform 
       
       


       let grounds = Array.empty<ZoneGround>
       let ladders = Array.empty<ZoneLadder>
       let platforms = Array.empty<ZonePlatform>
       let jumpPads = Array.empty<ZoneJumpPad>
       let mutable parsed = false

       member val BasePathMap = Array.empty with get,set
       member val Grounds : ZoneGround[] = grounds with get, set
       member val Ladders : ZoneLadder[] = ladders with get, set
       member val Platforms : ZonePlatform[] = platforms with get, set
       member val JumpPads : ZoneJumpPad[] = jumpPads with get, set

       member this.JumpDownParse (tiles:Tile[][]) =      
            let getDowns (cell:Cell) =
                let (left, down, right) = cell.down.left, cell.down, cell.down.right
                seq {
                    if down.Y >= 0 && tiles.[down.X].[down.Y] = Tile.Empty then
                        yield (cell, down)
                        if left.X >= 0 && tiles.[left.X].[left.Y] = Tile.Empty then
                            yield (cell, left)                            
                        if right.Y < tiles.Length && tiles.[right.X].[right.Y] = Tile.Empty then
                            yield (cell, right)
                }            
            
            let ladders = this.Ladders |> Seq.collect(fun x -> x.Standable |> Seq.skip 1)
                                       |> Seq.map(fun x -> (x, x.down))

            tiles |> Matrices.allTilesG genCell
                  |> Seq.choose(fun (cell, t) -> 
                        match t with
                            | Tile.Empty -> Some (getDowns cell)
                            | _ -> None)
                  |> Seq.collect(fun x -> x)
                  |> Seq.append ladders
                 
    
        member this.JumpUpParse (tiles:Tile[][]) =  
            let getUps (cell:Cell) =
                let (left, up, right) = cell.up.left, cell.up, cell.up.right
                seq {
                    if up.Y < tiles.Length && tiles.[up.X].[up.Y] <> Tile.Wall then
                        yield (cell, up)
                        if left.X >= 0 && tiles.[left.X].[left.Y] <> Tile.Wall  then
                            yield (cell, left)                            
                        if right.Y < tiles.Length && tiles.[right.X].[right.Y] <> Tile.Wall then
                            yield (cell, right)
                } 
                         
            tiles |> Matrices.allTilesG genCell
                  |> Seq.choose(fun (cell, t) -> 
                        match t with
                            | Tile.Empty -> Some (getUps cell)
                            | Tile.Platform -> Some (getUps cell)
                            | _ -> None)
                  |> Seq.collect(fun x -> x)

        member this.EdgeDownGroundParse (tiles:Tile[][]) =
            let edges = this.Grounds |> Seq.map(fun x -> let a,b = x.EdgeCells tiles
                                                         seq {
                                                            match a with Some t -> yield (t.up, t.up.left) | None -> ignore()                                                                    
                                                            match b with Some t -> yield (t.up, t.up.right) | None -> ignore()                                                              
                                                         })                                    
                                    |> Seq.collect(fun x -> x)

            let ladders = this.Ladders |> Seq.collect(fun x -> x.Standable)
                                            |> Seq.map(fun x ->
                                                            let (l,r) = x.left, x.right
                                                            seq {
                                                                if tiles.[l.X].[l.Y] = Tile.Empty then yield (x, l); yield (l, x)
                                                                if tiles.[r.X].[r.Y] = Tile.Empty then yield (x, r); yield (r, x)
                                                            })
                                        |> Seq.collect(fun x -> x)
           

            let platforms = this.Platforms |> Seq.collect(fun x -> x.Cells)
                                           |> Seq.map(fun x -> [(x, x.down);(x.up, x)])
                                           |> Seq.collect(fun x -> x)

            
            edges |> Seq.append ladders
                  |> Seq.append platforms         
               
       member this.GroundsAndPlatformsParse (tiles:Tile[][]) =
        
           let ladders = this.Ladders |> Seq.collect(fun x -> x.Standable)
                                      |> Seq.map(fun x -> (x, x.up))

           this.Grounds |> Seq.map (fun x -> (x.Standable(tiles) |> Seq.pairwise))
                        |> Seq.append (this.Platforms |> Seq.map(fun x -> x.Standable(tiles) |> Seq.pairwise))
                        |> Seq.collect(fun x -> x)
                        |> Seq.append ladders

       
       member this.JumpPadsParse () =
            this.JumpPads |> Seq.map(fun x -> x.Cell, x.TargetCell)            

       member this.drawPathMap =
            ignore()
            //this.BasePathMap |> Map.iter(
            //                    fun _ b -> b |> Seq.iter(fun link -> 
            //                                                let color = match link.Type with
            //                                                                | ConnectionType.JumpDown -> Palette.DarkSlateBlue
            //                                                                | ConnectionType.JumpUp -> Palette.LightSeaGreen
            //                                                                | ConnectionType.JumpUpTouch -> Palette.DarkOliveGreen
            //                                                                | ConnectionType.JumpPad -> Palette.GreenYellow
            //                                                                | ConnectionType.Walk -> Palette.CornflowerBlue
            //                                                                | ConnectionType.JumpDownTouch -> Palette.RoyalBlue
            //                                                Logger.drawLine link.Source.toCenter link.Target.toCenter color
            //                                                )
                                    
                                    
            //                        )

       member this.buildBasePathMap (tiles:Tile[][]) =
           let pathsUp = tiles |> this.JumpUpParse
           let cliffDown = tiles |> this.EdgeDownGroundParse |> Array.ofSeq
           let pathsDown = tiles |> this.JumpDownParse
           let pathsGround = tiles |> this.GroundsAndPlatformsParse
           let jumpPads = this.JumpPadsParse()

           let standables = this.Platforms |> Seq.collect(fun x -> x.WalkCells) |> Array.ofSeq
                  
           this.BasePathMap <- pathsUp |> Seq.map(fun (a,b) -> 
                                                       let t = if (standables |> Array.contains b) then ConnectionType.JumpUpTouch else ConnectionType.JumpUp
                                                       {
                                                           Source= a
                                                           Target= b
                                                           Type= t
                                                           Dist= Cell.dist a b
                                                       })
                               |> Seq.append(
                                   jumpPads |> Seq.map(fun (a,b) -> 
                                                        {
                                                            Source = a
                                                            Target = b
                                                            Type=ConnectionType.JumpPad
                                                            Dist = Vector2.dist a.toCenter b.toCenter
                                                        }))
                               |> Seq.append(
                                   cliffDown |> Array.truncate((Array.length cliffDown) - 1) 
                                             |> Seq.map(fun (a,b) ->
                                                           {
                                                              Source=a
                                                              Target=b
                                                              Type=ConnectionType.JumpDown
                                                              Dist=Cell.dist a b 
                                                           }))
                               |> Seq.append(
                                    cliffDown |> Array.skip((Array.length cliffDown) - 1)  
                                              |> Seq.map(fun (a,b) ->
                                                  {
                                                     Source=a
                                                     Target=b
                                                     Type=ConnectionType.JumpDownTouch
                                                     Dist=Cell.dist a b 
                                                  }))
                               |> Seq.append(
                                   pathsDown |> Seq.map(fun (a,b) -> 
                                                            {
                                                                Source=a
                                                                Target=b
                                                                Type=ConnectionType.JumpDown
                                                                Dist = Cell.dist a b 
                                                            }))
                               |> Seq.append(
                                   pathsGround |> Seq.map(fun (a,b) ->
                                                       seq {
                                                                   yield {
                                                                        Source=a
                                                                        Target=b
                                                                        Type=ConnectionType.Walk
                                                                        Dist = 1.0f 
                                                                    }                                                               
                                                                   yield {
                                                                       Source=b
                                                                       Target=a
                                                                       Type=ConnectionType.Walk
                                                                       Dist = 1.0f 
                                                                   }
                                                           })
                                                |> Seq.collect(fun x -> x))
                               |> Seq.filter(fun l -> tiles.[l.Source.X].[l.Source.Y] <> Tile.JumpPad || l.Type = ConnectionType.JumpPad)
                               |> Array.ofSeq

       member this.BuildMaskedMap (mask:array<Cell>) =
            this.BasePathMap |> Array.filter(fun x -> not (mask |> Array.contains(x.Target)))
                             |> Array.groupBy(fun l -> l.Source)
                             |> Map.ofSeq

       /// A method to populate map with static items like walls, ladder, etc
       member this.Parse tiles =
           if not parsed then
               let cells = tiles |> Matrices.allTilesG 
                            (fun x y tile -> { Cell= {X=x;Y=y}; Tile = tile})
                            |> Seq.filter (fun x -> x.Tile <> Tile.Empty)
           
               this.Grounds <- cells |> Seq.filter (fun x -> x.Tile = Tile.Wall)
                                     |> Seq.map (fun x -> x.Cell)
                                     |> Zones.buildGrounds (fun set -> new ZoneGround(set |> Array.ofSeq))
                                     |> Array.ofSeq

               this.Ladders <- cells |> Seq.filter (fun x -> x.Tile = Tile.Ladder)
                                     |> Seq.map (fun x -> x.Cell)
                                     |> Zones.buildLadders (fun set -> new ZoneLadder(set |> Array.ofSeq))
                                     |> Array.ofSeq
           
               this.Platforms <- cells |> Seq.filter (fun x -> x.Tile = Tile.Platform)
                                       |> Seq.map (fun x -> x.Cell)
                                       |> Zones.buildPlatforms (fun set -> new ZonePlatform(set |> Array.ofSeq))
                                       |> Array.ofSeq
               
               this.JumpPads <- cells |> Seq.filter(fun x -> x.Tile = Tile.JumpPad)
                                      |> Seq.map (fun x -> 
                                                        let a, b = x.Cell.toCenter, ({X= x.Cell.X; Y= x.Cell.Y + Constants.Max_Jump_Pad}:Cell).toCenter
                                                        let trace = Tracing.castRay2 tiles collisionFilterJump a b
                                                        new ZoneJumpPad(x.Cell, {X=int trace.X; Y = int trace.Y}))
                                      |> Array.ofSeq
                               
               //let g = this.Grounds |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ground x))) 
               //let l = this.Ladders |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ladder x))) 
               //let p = this.Platforms |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Platform x))) 
               
               parsed <- true