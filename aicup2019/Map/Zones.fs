namespace Robocop.Map

open AiCup2019.Model
open Robocop.Utils

type CellTile = {Cell: Cell; Tile: Tile}
type ConnectionType = Walk | JumpUp | JumpDown

type Link = {Target: Cell; Type: ConnectionType; Dist: single}        

type ZoneGround(cells: array<Cell>) =
    member _.Cells = cells
    member _.Standable (tiles:Tile[][]) =  
        seq { 
                let head, tail = (cells |> Array.head).up.left, (cells |> Array.last).right.up
                match tiles.[head.X].[head.Y] with Tile.Empty -> yield head; | _ -> ignore()
                yield! (cells |> Seq.map(fun x -> x.up))
                match tiles.[tail.X].[tail.Y] with Tile.Empty -> yield tail; | _ -> ignore()
            }
    member _.EdgeCells (tiles:Tile[][]) = 
            let head, tail = cells |> Array.head, cells |> Array.last
            let a = if tiles.[head.X - 1].[head.Y] = Tile.Empty then Some head else None
            let b = if tiles.[tail.X + 1].[tail.Y] = Tile.Empty then Some tail else None
            a,b

type ZoneLadder(cells: array<Cell>) =
    member _.Cells = cells
    member _.Standable = seq { yield! cells ; yield (cells |> Array.last).up; }
    member _.TopBottomEdges=
        let head, tail = cells |> Array.head, cells |> Array.last
        seq { single head.X, single head.Y + 0.5f; single tail.X, single tail.Y + 1.0f}
    
type ZonePlatform(cells: array<Cell>) =
    member _.Cells = cells
    member _.WalkCells = cells |> Seq.map(fun c -> {X=c.X; Y = c.Y + 1}: Cell)
    
    member _.Standable (tiles:Tile[][]) =  
        seq { 
                let head, tail = (cells |> Array.head).up.left, (cells |> Array.last).right.up
                match tiles.[head.X].[head.Y] with | x  when x = Tile.Empty || x = Tile.Ladder -> yield head; | _ -> ignore()
                yield! (cells |> Seq.map(fun x -> x.up))
                match tiles.[tail.X].[tail.Y] with | x when x = Tile.Empty || x = Tile.Ladder -> yield tail; | _ -> ignore()
            }

module Zones =
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
        

   
    
