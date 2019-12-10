namespace Robocop.Map

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
       let mutable parsed = false

       member val PathMap = Map.empty with get,set
       member val Grounds : ZoneGround[] = grounds with get, set
       member val Ladders : ZoneLadder[] = ladders with get, set
       member val Platforms : ZonePlatform[] = platforms with get, set


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

           let cross = seq { for a in fromGrounds |> Seq.append ladders do
                               for b in toGrounds |> Seq.append ladders do
                                   if a <> b then yield a, b }                                        

           cross
               |> Seq.filter(fun (a,b) -> a.Y < b.Y && b.Y - a.Y <= Constants.Max_Jump && Math.Abs(b.X - a.X) <= Constants.Max_Jump)
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
                                                               match a with Some t -> 
                                                                                yield t.left.left.toCenter;
                                                                                yield t.up.left.toCenter; | None -> ignore()
                                                               match b with Some t ->                                                                
                                                                                yield t.up.right.toCenter;
                                                                                yield t.right.right.toCenter;| None -> ignore()
                                                        })                                    
                                    |> Seq.collect(fun x -> x)
           let laddersFrom = this.Ladders |> Seq.collect(fun x -> x.Standable)
                                      |> Seq.map(fun x -> seq { x.toRightMidDelta; x.toLeftMid})
                                      |> Seq.collect(fun x -> x)
           
           let laddersTo = this.Ladders |> Seq.collect(fun x -> x.Standable)
                                      |> Seq.map(fun x -> x.toCenter)

           let platformsSource = this.Platforms |> Seq.collect(fun x -> x.Cells)
                                          |> Seq.map(fun x -> x.down.toCenter)
           let platformsTarget = this.Platforms |> Seq.collect(fun x -> x.Standable(tiles))
                                           |> Seq.map(fun x -> x.toMidTopDelta)
           
           let cross = seq { for a in edges |> Seq.append laddersFrom |> Seq.append platformsSource do
                                          for b in this.Grounds
                                                           |> Seq.collect (fun x -> x.Cells) 
                                                           |> Seq.map (fun c -> c.toMidTop)
                                                           |> Seq.append platformsTarget
                                                           |> Seq.append laddersTo do                                 
                                           if a <> b then yield a,b }                                                          
           cross                
               |> Seq.filter (fun (a,b) -> a.Y > b.Y && a.Y - b.Y > Math.Abs(b.X - a.X))
               |> Seq.choose (fun (a,b) ->
                   let trace = Tracing.castRay2 tiles collisionFilterFall a b
                   let diff = Vector2.Distance(trace, b)
                   match diff with
                       | x when x < 1.0f -> Some(a,trace)
                       | _ -> None)
               |> Seq.groupBy (fun (a,b) -> {| Ax = int a.X; Ay = int a.Y; Bx = int b.X; By = int b.Y|} ) 
               |> Seq.map(fun (_,v) -> v |> Seq.head)
               |> Seq.append(this.Platforms 
                                |> Seq.collect(fun x -> x.Cells)
                                |> Seq.map (fun c -> (c.up.toCenter, c.toCenter)))
               
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
       
       member this.buildPathMap (tiles:Tile[][])  =
           let pathsUp = tiles |> this.EdgeUpGroundParse
           let pathsDown = tiles |> this.EdgeDownGroundParse
           let pathsGround = tiles |> this.GroundsAndPlatformsParse

           //pathsUp |> Seq.iter (fun edges ->
           //    let p1, p2 = edges
           //    Logger.drawLine p1 p2 Palette.LightSeaGreen)

           //pathsDown |> Seq.iter (fun edges ->
           //              let p1, p2 = edges
           //              Logger.drawLine p1 p2 Palette.DarkSlateBlue)

           pathsGround |> Seq.iter (fun edges ->
                           let p1, p2 = edges
                           Logger.drawLine p1 p2 Palette.CornflowerBlue)
       
           this.PathMap <- pathsUp |> Seq.map(fun (a,b) -> 
                                                   Cell.fromVector a,
                                                   {
                                                       Target=Cell.fromVector b
                                                       Type=ConnectionType.JumpUp
                                                       Dist = Vector2.dist a b
                                                       })
                               |> Seq.append(
                                   pathsDown |> Seq.map(fun (a,b) ->
                                                           Cell.fromVector a,
                                                           {
                                                              Target=Cell.fromVector b
                                                              Type=ConnectionType.JumpDown
                                                              Dist = Vector2.dist a b 
                                                           }))
                               |> Seq.append(
                                   pathsGround |> Seq.map(fun (a,b) ->
                                                           let va, vb = Cell.fromVector a, Cell.fromVector b
                                                           seq {
                                                               yield Cell.fromVector a,
                                                                       {
                                                                           Target=Cell.fromVector b
                                                                           Type=ConnectionType.Walk
                                                                           Dist = 1.0f 
                                                                       }
                                                               yield Cell.fromVector b,
                                                               {
                                                                   Target=Cell.fromVector a
                                                                   Type=ConnectionType.Walk
                                                                   Dist = 1.0f 
                                                               }
                                                           })
                                                |> Seq.collect(fun x -> x))
                               |> Seq.groupBy(fun (k,_) -> k)
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
                               
               let g = this.Grounds |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ground x))) 
               let l = this.Ladders |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Ladder x))) 
               let p = this.Platforms |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zones.Zone.Platform x))) 
               
               parsed <- true