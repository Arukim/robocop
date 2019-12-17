namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils
open Robocop.Core

type Strategy(game: Game, myTeam: int) =
    let location = new Location()
    let armory = new Armory(game)
    let mutable lastCalculatedTick = -1
    let mutable unitCounter = -1
    let warriors = game.Units |> Array.choose(
                                fun u ->                                     
                                    match u with 
                                            | x when x.PlayerId = myTeam -> 
                                                    unitCounter <- unitCounter + 1
                                                    Some((x.Id, new Warrior(armory, game.Properties, x, unitCounter)))
                                            | _ -> None)
                              |> Map.ofArray                                  

    let calcTick game = 
        //location.drawPathMap
        ignore()


    let cleanPathMap() =
        let (_, dist) = Pathfinder.dijkstra (location.BuildMaskedMap Array.empty) (Cell.fromVector game.Units.[0].Position) (Constants.Max_Jump * 2.0f)
        location.BasePathMap <- location.BasePathMap |> Seq.filter(fun l -> dist.[l.Source].Dist <> infinityf)
        ignore()

    member _.Init() = 
        Diag.elapsed "Location init" (fun () -> location.Parse game.Level.Tiles)
        Diag.elapsed "Base path map" (fun () -> location.buildBasePathMap game.Level.Tiles)
        Diag.elapsed "Clean path map" (fun () -> cleanPathMap())

    member _.PrepareTurn(game: Game) =
        if lastCalculatedTick <> game.CurrentTick then
            calcTick game.Level.Tiles
            lastCalculatedTick <- game.CurrentTick
            armory.Sync game

    member _.DoTurn(unit: Unit, game: Game) =
        let msg = sprintf "Warrior %A turn" unit.Id

        let warrior = warriors.[unit.Id]

        Diag.elapsed msg (fun () -> warrior.DoTurn unit game location)