namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils

module Zones =
    type CellTile = {Cell: Cell; Tile: Tile}

    type ConnectionType = Walk | JumpUp | JumpDown

    type Connection = {Type: ConnectionType}

    type Link<'a> = {Target: 'a; Connection: Connection}
        
    type Zone() = 
        member _.Neighbours : List<Link<Zone>> = []
        member _.Cells : List<Cell> = []

    type ZoneGround(cells: seq<Cell>) =
        member _.Cells = cells

    type ZoneLadder(cells: seq<Cell>) =
        member _.Cells = cells
        
    type ZonePlatform(cells: seq<Cell>) =
        member _.Cells = cells
    
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
        member val Grounds : ZoneGround[] = grounds with get, set
        member val Ladders : ZoneLadder[] = ladders with get, set
        member val Platforms : ZonePlatform[] = platforms with get, set

        member this.Parse tiles =
            let cells = tiles |> Matrices.allTilesG 
                         (fun x y tile -> { Cell= {X=x;Y=y}; Tile = tile})
                         |> Seq.filter (fun x -> x.Tile <> Tile.Empty)
            
            this.Grounds <- cells |> Seq.filter (fun x -> x.Tile = Tile.Wall)
                                  |> Seq.map (fun x -> x.Cell)
                                  |> buildGrounds (fun set -> new ZoneGround(set))
                                  |> Array.ofSeq

            this.Ladders <- cells |> Seq.filter (fun x -> x.Tile = Tile.Ladder)
                                  |> Seq.map (fun x -> x.Cell)
                                  |> buildLadders (fun set -> new ZoneLadder(set))
                                  |> Array.ofSeq
            
            this.Platforms <- cells |> Seq.filter (fun x -> x.Tile = Tile.Platform)
                                    |> Seq.map (fun x -> x.Cell)
                                    |> buildPlatforms (fun set -> new ZonePlatform(set))
                                    |> Array.ofSeq
    
