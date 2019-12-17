namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils

type Armory(game: Game) =
    let mutable freeWeapons = game.LootBoxes |> Array.choose(
                                fun lb -> match lb.Item with
                                            | Item.Weapon x -> Some(lb.Position, x.WeaponType)
                                            | _ -> None)
                              

    let mutable freeMines = game.LootBoxes |> Array.choose(
                                fun b -> match b.Item with
                                            | Item.Mine _ -> Some b.Position
                                            | _ -> None)
                             
    member _.Sync (game: Game) =
        freeWeapons <- freeWeapons |> Array.filter(fun (pos, _) -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))
        freeMines <- freeMines |> Array.filter(fun pos -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))

    member _.SelectWeapon dist =
        let wpn = freeWeapons |> Array.sortBy(fun (pos,t) -> Pathfinder.findDistance dist (Cell.fromVector pos) - Marksman.WeaponPreference(t))
                              |> Seq.tryHead
        if wpn.IsSome then
            freeWeapons <- freeWeapons |> Array.filter(fun (pos,_) -> pos <> fst wpn.Value)

        match wpn with
            | Some (pos, _) -> Some pos
            | _ -> None

    member _.SelectMine dist =
        let mine = freeMines |> Array.sortBy(fun p -> Pathfinder.findDistance dist (Cell.fromVector p))
                                     |> Seq.tryFind(fun _ -> true)
        if mine.IsSome then
            freeMines <- freeMines |> Array.filter(fun m -> m <> mine.Value)

        mine

    member _.HasMines = freeMines.Length > 0

    member _.HasMine (mine:Vec2Double) = freeMines |> Array.map(Cell.fromVector) |> Array.contains(Cell.fromVector(mine))
    member _.HasWeapon (w:Vec2Double) = freeWeapons |> Array.map(fun (pos, _) -> Cell.fromVector pos) |> Array.contains(Cell.fromVector(w))

