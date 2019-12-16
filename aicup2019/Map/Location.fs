﻿namespace Robocop.Map

open AiCup2019.Model
open Robocop.Core
open Robocop.Utils
open System.Numerics
open System


type Location() =
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

       member val BasePathMap = Map.empty with get,set
       member val Grounds : ZoneGround[] = grounds with get, set
       member val Ladders : ZoneLadder[] = ladders with get, set
       member val Platforms : ZonePlatform[] = platforms with get, set
       member val JumpPads : ZoneJumpPad[] = jumpPads with get, set

       member _.DownParse (tiles:Tile[][]) =    
            let genCell x y v = ({X=x;Y=y}:Cell),v   
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
              
            tiles |> Matrices.allTilesG genCell
                  |> Seq.choose(fun (cell, t) -> 
                        match t with
                            | Tile.Empty -> Some (getDowns cell)
                            | _ -> None)
                  |> Seq.collect(fun x -> x)
                 
    
        member this.EdgeUpGroundParse (tiles:Tile[][]) =        
            let fromGrounds = this.Grounds |> Seq.collect(fun x -> x.Standable(tiles))
                                           |> Seq.append (this.Platforms |> Seq.collect(fun x -> x.Standable(tiles)))
                                     |> Seq.map(fun x -> seq { x.toCenter})
                                     |> Seq.collect(fun x -> x)

            //fromGrounds |> Seq.iter (fun p -> Logger.drawDot p Palette.Yellow)                       
            
            let ladders = this.Ladders |> Seq.collect(fun x -> x.Standable)
                                           |> Seq.map(fun x -> seq { x.toCenter})
                                           |> Seq.collect(fun x -> x)

             
            //ladders |> Seq.iter (fun p -> Logger.drawDot p Palette.AliceBlue)

            let toGrounds = this.Grounds |> Seq.map(fun x -> let a,b = x.EdgeCells tiles
                                                             seq {
                                                                     match a with Some t -> yield! seq {t.up.toCenter} | None -> ignore()
                                                                     match b with Some t -> yield! seq {t.up.toCenter } | None -> ignore()
                                                             })
                                                |> Seq.append (this.Platforms 
                                                                 |> Seq.collect(fun x -> x.Cells) 
                                                                 |> Seq.map(fun x -> seq { x.up.toCenter}))
                                     |> Seq.collect(fun x -> x)
            
            //toGrounds |> Seq.iter (fun p -> Logger.drawDot p Palette.OrangeRed)

            let crossBasic = seq { for a in fromGrounds |> Seq.append ladders do
                                    for b in toGrounds |> Seq.append ladders do
                                        if a <> b then yield a, b }

            crossBasic
                |> Seq.filter(fun (a,b) -> a.Y <= b.Y && b.Y - a.Y <= Constants.Max_Jump && Math.Abs(b.X - a.X) <= Constants.Max_Jump)
                |> Seq.choose(fun (a,b) ->
                                            let trace = Tracing.castRay2 tiles collisionFilterJump a b
                                            let diff = Vector2.Distance(trace, b)
                                            match diff with
                                                | x when x < 1.0f -> Some(a, trace)
                                                | _ -> None)
                |> Seq.groupBy (fun (a,b) -> {| Ax = int a.X; Ay = int a.Y; Bx = int b.X; By = int b.Y|} ) 
                |> Seq.map(fun (_,v) -> v |> Seq.head)

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
                                           |> Seq.map(fun x -> (x, x.down))
            
            edges |> Seq.append ladders
                  |> Seq.append platforms         
               
       member this.GroundsAndPlatformsParse (tiles:Tile[][]) =
           this.Grounds |> Seq.map (fun x -> (x.Standable(tiles) |> Seq.map (fun x -> x.toCenter) |> Seq.pairwise))
                        |> Seq.append (this.Platforms |> Seq.map(fun x -> x.Standable(tiles) |> Seq.map (fun x -> x.toCenter) |> Seq.pairwise))
                        |> Seq.map (fun p -> p 
                                               |> Seq.choose(fun (a,b) -> 
                                                   let trace = Tracing.castRay2 tiles collisionFilterGrounds a b
                                                   let diff = Vector2.Distance(trace, b)
                                                   match diff with
                                                       | x when x < 1.0f -> Some(a,trace)
                                                       | _ -> None))                                                
                        |> Seq.collect(fun x -> x)
                        |> Seq.groupBy (fun (a,b) -> {| Ax = a.X; Ay = int a.Y; Bx = int b.X; By = int b.Y |} )
                        |> Seq.map(fun (_,v) -> v |> Seq.head)

       
       member this.JumpPadsParse () =
            this.JumpPads |> Seq.map(fun x -> x.Cell, x.TargetCell)            

       member this.drawPathMap =
            this.BasePathMap |> Map.iter(
                                fun _ b -> b |> Seq.iter(fun link -> 
                                                            let color = match link.Type with
                                                                            | ConnectionType.JumpDown -> Palette.DarkSlateBlue
                                                                            | ConnectionType.JumpUp -> Palette.LightSeaGreen
                                                                            | ConnectionType.JumpPad -> Palette.GreenYellow
                                                                            | ConnectionType.Walk -> Palette.CornflowerBlue
                                                            Logger.drawLine link.Source.toCenter link.Target.toCenter color
                                                            )
                                    
                                    
                                    )

       member this.buildBasePathMap (tiles:Tile[][]) =
           let pathsUp = tiles |> this.EdgeUpGroundParse
           let cliffDown = tiles |> this.EdgeDownGroundParse
           let pathsDown = tiles |> this.DownParse
           let pathsGround = tiles |> this.GroundsAndPlatformsParse
           let jumpPads = this.JumpPadsParse()
                  
           this.BasePathMap <- pathsUp |> Seq.map(fun (a,b) -> 
                                                       {
                                                           Source=Cell.fromVector a
                                                           Target=Cell.fromVector b
                                                           Type=ConnectionType.JumpUp
                                                           Dist= Vector2.dist a b
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
                                   cliffDown |> Seq.map(fun (a,b) ->
                                                           {
                                                              Source=a
                                                              Target=b
                                                              Type=ConnectionType.JumpDown
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
                                                               let ca, cb = Cell.fromVector a, Cell.fromVector b
                                                               if tiles.[ca.X].[ca.Y] <> Tile.JumpPad then
                                                                   yield {
                                                                               Source=ca
                                                                               Target=Cell.fromVector b
                                                                               Type=ConnectionType.Walk
                                                                               Dist = 1.0f 
                                                                           }
                                                               if tiles.[cb.X].[cb.Y] <> Tile.JumpPad then
                                                                   yield {
                                                                       Source=cb
                                                                       Target=Cell.fromVector a
                                                                       Type=ConnectionType.Walk
                                                                       Dist = 1.0f 
                                                                   }
                                                           })
                                                |> Seq.collect(fun x -> x))
                               |> Seq.filter(fun l -> tiles.[l.Source.X].[l.Source.Y] <> Tile.JumpPad || l.Type = ConnectionType.JumpPad)
                               |> Seq.groupBy(fun l -> l.Source)
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
                               
               let g = this.Grounds |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ground x))) 
               let l = this.Ladders |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ladder x))) 
               let p = this.Platforms |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Platform x))) 
               
               parsed <- true