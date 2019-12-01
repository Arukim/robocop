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

    type ZonePlatform(cells) =
        member _.Cells = cells

    let buildPlatforms g (cells: seq<Cell>) = 
        cells
            |> Seq.groupBy (fun x -> x.Y) 
            |> Seq.map (fun kv -> snd kv
                                    |> Seq.pairwise
                                    |> Seq.filter (fun (a,b) -> b.Y - a.Y <> 1)
                                    |> Seq.map (fun (_,b) -> b))
            |> Seq.collect(fun x -> x)
                                        |> Seq.groupBy (fun x -> x.X)
            |> Seq.map (fun kv -> snd kv
                                    |> Seq.pairwise
                                    |> Seq.splitBy(fun (a,b) -> b.X - a.X <> 1)
                                    |> Seq.map(fun x -> g x))
            |> Seq.collect(fun x -> x)
    
    type Location() =
        let platforms = Array.empty<ZonePlatform>
        member val Platforms : ZonePlatform[] = platforms with get, set
        member this.Parse tiles =
            let cells = tiles |> Matrices.allTilesG 
                         (fun x y tile -> { Cell= {X=x;Y=y}; Tile = tile})
                         |> Seq.filter (fun x -> x.Tile <> Tile.Empty)
            
            let p = cells |> Seq.filter (fun x -> x.Tile = Tile.Platform)
                                  |> Seq.map (fun x -> x.Cell)
                                  |> buildPlatforms (fun set -> new ZonePlatform(set |> Seq.map (fun (a,_) -> a)))
                                  |> Array.ofSeq
            this.Platforms <- p
            
            0

    
