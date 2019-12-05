namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils
open System.Numerics
open System

module Zones =
    let maxJump = 5.0f

    type CellTile = {Cell: Cell; Tile: Tile}

    type ConnectionType = Walk | JumpUp | JumpDown

    type Connection = {Type: ConnectionType}

    type Link<'a> = {Target: 'a; Connection: Connection}        

    type ZoneGround(cells: array<Cell>) =
        member _.Cells = cells
        member _.EdgeCells (tiles:Tile[][]) = 
                let head, tail = cells |> Array.head, cells |> Array.last
                let a = if tiles.[head.X - 1].[head.Y] = Tile.Empty then Some head else None
                let b = if tiles.[tail.X + 1].[tail.Y] = Tile.Empty then Some tail else None
                a,b

    type ZoneLadder(cells: array<Cell>) =
        member _.Cells = cells
        member _.TopBottomEdges=
            let head, tail = cells |> Array.head, cells |> Array.last
            seq { single head.X, single head.Y + 0.5f; single tail.X, single tail.Y + 1.0f}
        
    type ZonePlatform(cells: array<Cell>) =
        member _.Cells = cells


    type Zone = Ground of ZoneGround | Ladder of ZoneLadder | Platform of ZonePlatform
    
    /// <summary> builds horizontal walls from input cells
    /// applying <c> g </c> function on results
    /// </summary>
    let buildGrounds g (cells: seq<Cell>) = 
        cells
            |> Seq.groupBy (fun x -> x.X) 
            |> Seq.map (fun kv -> snd kv
                                    |> Seq.rev
                                    |> Seq.pairwise
                                    |> Seq.filter (fun (a,b) -> a.Y - b.Y > 2)
                                    |> Seq.map (fun (_,b) -> b))
            |> Seq.collect(fun x -> x)
                                    |> Seq.groupBy (fun x -> x.Y)
            |> Seq.map (fun kv -> snd kv
                                    |> Seq.map (fun s -> seq {s;s})
                                    |> Seq.collect (fun x -> x)
                                    |> Seq.pairwise
                                    |> Seq.splitBy (fun (a,b) -> b.X - a.X > 1)
                                    |> Seq.map (fun x -> x 
                                                        |> Seq.map (fun t -> match t with
                                                                                | (a,b) when b.X - a.X < 2 -> seq{a; b}
                                                                                | (_) -> Seq.empty)
                                                        |> Seq.collect (fun x -> x)
                                                        |> Seq.distinct
                                                        |> g))
            |> Seq.collect(fun x -> x)

    let buildLadders g (cells: seq<Cell>) =
        cells
            |> Seq.groupBy (fun x -> x.X)
            |> Seq.map (fun kv -> snd kv
                                |> Seq.map (fun s -> seq {s;s})
                                |> Seq.collect (fun x -> x)
                                |> Seq.pairwise
                                |> Seq.splitBy (fun (a,b) -> b.Y - a.Y > 1)
                                |> Seq.map (fun x -> x 
                                                        |> Seq.map (fun t -> match t with
                                                                                | (a,b) when b.Y - a.Y < 2 -> seq{a; b}
                                                                                | (_) -> Seq.empty)
                                                        |> Seq.collect (fun x -> x)
                                                        |> Seq.distinct
                                                        |> g))            
            |> Seq.collect(fun x -> x)
    
    let buildPlatforms g (cells: seq<Cell>) =
        cells
            |> Seq.groupBy (fun x -> x.Y)
            |> Seq.map (fun kv -> snd kv
                                |> Seq.map (fun s -> seq {s;s})
                                |> Seq.collect (fun x -> x)
                                |> Seq.pairwise
                                |> Seq.splitBy (fun (a,b) -> b.X - a.X > 1)
                                |> Seq.map (fun x -> x 
                                                        |> Seq.map (fun t -> match t with
                                                                                | (a,b) when b.X - a.X < 2 -> seq{a; b}
                                                                                | (_) -> Seq.empty)
                                                        |> Seq.collect (fun x -> x)
                                                        |> Seq.distinct
                                                        |> g))            
            |> Seq.collect(fun x -> x)

    type Location() =
        let collisionFilter cell =
            cell = Tile.Wall || cell = Tile.Ladder || cell = Tile.Platform
        let grounds = Array.empty<ZoneGround>
        let ladders = Array.empty<ZoneLadder>
        let platforms = Array.empty<ZonePlatform>
        let map = Map.empty
        let mutable parsed = false
        member val Grounds : ZoneGround[] = grounds with get, set
        member val Ladders : ZoneLadder[] = ladders with get, set
        member val Platforms : ZonePlatform[] = platforms with get, set
        member val Mapping : Map<Cell,Zone> = map with get,set

        member this.isOnPlatform vec =
            let cell:Cell = {X = (int vec.X); Y = (int vec.Y) - 1}

            match this.Mapping.TryFind(cell) with
                | Some x -> match x with
                                    | Platform _ -> true
                                    | _ -> false
                | _ -> false

        member this.EdgeUpGroundParse (tiles:Tile[][]) =        
            let edges = this.Grounds |> Seq.map(fun x -> let a,b = x.EdgeCells tiles
                                                         seq {
                                                                match a with Some t -> yield t.toLeftTop | None -> ignore()
                                                                match b with Some t -> yield t.toRightTop | None -> ignore()
                                                         }) 
                                     |> Seq.collect(fun x -> x)

            let cross = seq { for a in edges do
                                for b in edges do
                                    if a <> b then yield a, b }
                                        |> Array.ofSeq

            cross
                |> Array.filter(fun (a,b) -> a.Y < b.Y && (Vector2.dist a b) <= maxJump)
                |> Array.choose(fun (a,b) ->
                                            let trace = Tracing.castRay2 tiles collisionFilter a b
                                            let diff = Vector2.Distance(trace, b)
                                            match diff with
                                                | x when x < 1.0f -> Some(a, trace)
                                                | _ -> None)

        member this.EdgeDownGroundParse (tiles:Tile[][]) =
            let edges = this.Grounds |> Seq.map(fun x -> let a,b = x.EdgeCells tiles
                                                         seq {
                                                                match a with Some t -> yield! seq {t.toLeftTopDelta ; t.toLeftBottomDelta;} | None -> ignore()
                                                                match b with Some t -> yield! seq {t.toRightTopDelta; t.toRightBottomDelta;} | None -> ignore()
                                                         })                                    
                                     |> Seq.collect(fun x -> x)
            let ladders = this.Ladders |> Seq.collect(fun x -> x.Cells)
                                       |> Seq.map(fun x -> seq { x.toMidTopDelta; x.toLeftTop; x.toRightMid})
                                       |> Seq.collect(fun x -> x)
            let platformsSource = this.Platforms |> Seq.collect(fun x -> x.Cells)
                                           |> Seq.map(fun x -> x.toMidBottom)
            let platformsTarget = this.Platforms |> Seq.collect(fun x -> x.Cells)
                                            |> Seq.map(fun x -> x.toMidTopDelta)
            
            let cross = seq { for a in edges |> Seq.append ladders |> Seq.append platformsSource do
                                           for b in this.Grounds
                                                            |> Seq.collect (fun x -> x.Cells) 
                                                            |> Seq.map (fun c -> c.toMidTop)
                                                            |> Seq.append platformsTarget
                                                            |> Seq.append ladders do                                 
                                            if a <> b then yield a,b }                                                          
            cross                
                |> Seq.filter (fun (a,b) -> a.Y > b.Y && a.Y - b.Y > Math.Abs(b.X - a.X))
                |> Seq.choose (fun (a,b) ->
                    let trace = Tracing.castRay2 tiles collisionFilter a b
                    let diff = Vector2.Distance(trace, b)
                    match diff with
                        | x when x < 1.0f -> Some(a,trace)
                        | _ -> None)
                |> Seq.groupBy (fun (a,b) -> {| Ax = int a.X; Ay = int a.Y; Bx = int b.X; By = int b.Y|} ) 
                |> Seq.map(fun (_,v) -> v |> Seq.head)
                
        member this.GroundsAndPlatformsParse (tiles:Tile[][]) =
            let getPoints (cells:seq<Cell>) = 
                seq {
                    let head, tail = cells |> Seq.head, cells |> Seq.last
                    yield head.toLeftNextMid
                    yield! (cells |> Seq.map (fun x -> x.toCenterUp))
                    yield tail.toRightNextMid
                } |> Seq.pairwise

            this.Grounds |> Seq.map (fun x -> getPoints x.Cells)
                         |> Seq.append (this.Platforms |> Seq.map(fun x -> getPoints x.Cells))
                         |> Seq.map (fun p -> p 
                                                |> Seq.choose(fun (a,b) -> 
                                                    let trace = Tracing.castRay2 tiles collisionFilter a b
                                                    let diff = Vector2.Distance(trace, b)
                                                    match diff with
                                                        | x when x < 1.0f -> Some(a,trace)
                                                        | _ -> None))                                                
                         |> Seq.collect(fun x -> x)
                         |> Seq.groupBy (fun (a,b) -> {| Ax = a.X; Ay = int a.Y; Bx = int b.X; By = int b.Y |} )
                         |> Seq.map(fun (_,v) -> v |> Seq.head)
                         //|> Seq.append this.Platforms |> Seq.map(fun x -> getPoints x.Cells) 
                         //                             |> Seq.collect(fun x -> x)
                         //|> ()


        member this.Parse tiles =
            if not parsed then
                let cells = tiles |> Matrices.allTilesG 
                             (fun x y tile -> { Cell= {X=x;Y=y}; Tile = tile})
                             |> Seq.filter (fun x -> x.Tile <> Tile.Empty)
            
                this.Grounds <- cells |> Seq.filter (fun x -> x.Tile = Tile.Wall)
                                      |> Seq.map (fun x -> x.Cell)
                                      |> buildGrounds (fun set -> new ZoneGround(set |> Array.ofSeq))
                                      |> Array.ofSeq

                this.Ladders <- cells |> Seq.filter (fun x -> x.Tile = Tile.Ladder)
                                      |> Seq.map (fun x -> x.Cell)
                                      |> buildLadders (fun set -> new ZoneLadder(set |> Array.ofSeq))
                                      |> Array.ofSeq
            
                this.Platforms <- cells |> Seq.filter (fun x -> x.Tile = Tile.Platform)
                                        |> Seq.map (fun x -> x.Cell)
                                        |> buildPlatforms (fun set -> new ZonePlatform(set |> Array.ofSeq))
                                        |> Array.ofSeq
                                
                let g = this.Grounds |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c, Zone.Ground x))) 
                let l = this.Ladders |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c,Zone.Ladder x))) 
                let p = this.Platforms |> Seq.collect(fun x -> x.Cells |> Seq.map(fun c -> (c,Zone.Platform x))) 

                this.Mapping <- Seq.concat [g; l; p;] |> Map.ofSeq

                parsed <- true
    
