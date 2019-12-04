namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils
open System.Numerics
open System

module Zones =
    type CellTile = {Cell: Cell; Tile: Tile}

    type ConnectionType = Walk | JumpUp | JumpDown

    type Connection = {Type: ConnectionType}

    type Link<'a> = {Target: 'a; Connection: Connection}
        

    type ZoneGround(cells: array<Cell>) =
        member _.Cells = cells
        member _.TopEdges=
            let head, tail = cells |> Array.head, cells |> Array.last
            seq { single (head.X), single (head.Y + 1); (single tail.X) + 1.0f, single (tail.Y + 1)}

    type ZoneLadder(cells: array<Cell>) =
        member _.Cells = cells
        
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

        member this.EdgeParse (tiles:Tile[][]) =
            let edges = this.Grounds |> Seq.map(fun x -> x.TopEdges)
                                     |> Seq.collect(fun x -> x)

            let cross = seq { for a in edges do
                                for b in edges do
                                    if a <> b then yield Vector2.fromTuple a,Vector2.fromTuple b }

            cross |> Array.ofSeq |> Array.choose(fun x ->
                                                        let a, b = x
                                                        let trace = Tracing.castRay2 tiles a b
                                                        let diff = Vector2.Distance(trace, b)
                                                        match diff with
                                                            | x when x < 1.0f -> Some(a, trace)
                                                            | _ -> None)


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
    
