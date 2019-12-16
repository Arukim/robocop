namespace AiCup2019

open AiCup2019.Model
open Robocop.Player

type MyStrategy() =
    let mutable strategy:Option<Strategy> = None

    member this.Init(game) =
        strategy <- Some(new Strategy(game))
        strategy.Value.Init()
            
    member this.getAction(unit: Unit, game: Game, debug: Debug) =        
        if not strategy.IsSome then
            this.Init(game, unit.PlayerId)

        let strategy = strategy.Value

        strategy.PrepareTurn(game: Game)

        strategy.DoTurn(unit, game)     