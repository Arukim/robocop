namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils

type Armory(game: Game) =
    let mutable freeWeapons = game.LootBoxes |> Array.choose(
                                fun lb -> match lb.Item with
                                            | Item.Weapon x -> Some(lb.Position, x.WeaponType)
                                            | _ -> None) |> List.ofArray

    let mutable selectedWeapons:List<Vec2Double*WeaponType> = List.empty
                              

    let mutable freeMines = game.LootBoxes |> Array.choose(
                                fun b -> match b.Item with
                                            | Item.Mine _ -> Some b.Position
                                            | _ -> None) |> List.ofArray
    
    
    let mutable selectedMines:List<Vec2Double> = List.empty
                             
    member _.Sync (game: Game) =
        freeWeapons <- freeWeapons |> List.filter(fun (pos, _) -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))
        freeMines <- freeMines |> List.filter(fun pos -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))
        selectedWeapons <- selectedWeapons |> List.filter(fun (pos, _) -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))
        selectedMines <- selectedMines |> List.filter(fun pos -> game.LootBoxes |> Array.exists(fun x -> x.Position = pos))

    member _.SelectWeapon dist =
        let wpn = freeWeapons |> List.sortBy(fun (pos,t) -> Pathfinder.findDistance dist (Cell.fromVector pos) - Marksman.WeaponPreference(t))
                              |> List.tryHead
        if wpn.IsSome then
            freeWeapons <- freeWeapons |> List.filter(fun (pos,_) -> pos <> fst wpn.Value)            
            selectedWeapons <- selectedWeapons @ [wpn.Value]

        match wpn with
            | Some (pos, _) -> Some pos
            | _ -> None

    member _.DeselectWeapon dist = 
        let wpn = selectedWeapons |> List.filter(fun (pos,_) -> pos = dist)
                                  |> List.tryHead
        if wpn.IsSome then
            selectedWeapons <- selectedWeapons |> List.filter(fun (pos,_) -> pos <> fst wpn.Value)
            freeWeapons <- freeWeapons @ [wpn.Value]

    member _.SelectMine dist =
        let mine = freeMines |> List.sortBy(fun p -> Pathfinder.findDistance dist (Cell.fromVector p))
                             |> Seq.tryFind(fun _ -> true)
        if mine.IsSome then
            freeMines <- freeMines |> List.filter(fun m -> m <> mine.Value)
            selectedMines <- selectedMines @ [mine.Value]

        mine

    member _.HasMines = freeMines.Length > 0

    member _.HasMine (mine:Vec2Double) = 
        freeMines |> List.append selectedMines
                  |> List.map(Cell.fromVector) 
                  |> List.contains(Cell.fromVector(mine))

    member _.HasWeapon (w:Vec2Double) = 
        freeWeapons |> List.append selectedWeapons
                    |> List.map(fun (pos, _) -> Cell.fromVector pos) 
                    |> List.contains(Cell.fromVector(w))

