namespace Robocop.Utils

open AiCup2019.Model
open System
open System.IO

module Dumper =
    let dumpGameMap (game: Game) = 
        let dump = game.Level.Tiles 
                    |> Matrices.rotateConterClockwise
                    |> Array.map(fun row -> 
                        row |> Array.map(fun cell ->
                                 match cell with
                                        | Tile.Empty -> '.'
                                        | Tile.Platform -> '^'
                                        | Tile.Ladder -> 'H'
                                        | Tile.Wall -> '#'
                                        | Tile.JumpPad -> 'T'
                                        | _ -> '*') 
                            |> System.String)

        let hash = (hash game.Level.Tiles).ToString "X"
        sprintf "Map Hash is %s" hash |> Console.WriteLine 

        let name = hash |> sprintf "dumps\%s.txt"

        Directory.CreateDirectory "dumps" |> ignore

        if File.Exists name then Console.WriteLine "!!!!!!!!!!!!repeat!!!!!!!!!!!!!"

        File.WriteAllLines (name, dump)
