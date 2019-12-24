namespace Robocop.Player

open AiCup2019.Model
open Robocop.Map
open Robocop.Utils
open System.Numerics
open Robocop.Core

type Controller() =
    let deltaSize = 0.1f
    let mutable step = 0
    let mutable newStep = true
    let mutable hasTouched = true
    let mutable route : Link array = [||]
    
    let calcVelocity curr tgt = match tgt + 0.5f - curr with
                                        | d when d > deltaSize -> 10.0 
                                        | d when d < -deltaSize -> -10.0
                                        | d -> float (d * single Constants.Ticks_Per_Second)

    let setNextStep () =
        newStep <- true
        hasTouched <- true
        step <- step + 1

    let walk (tiles:Tile[][]) (pos:Vector2) (unit:Unit) target =
        /// detect starting jump pad
        if unit.JumpState.Speed > 0.0 && not unit.JumpState.CanCancel then setNextStep(); (false, false, 0.0)
        else
            let (x,y) = single target.X, single target.Y
            let vel = calcVelocity pos.X x
            let delta = y - pos.Y
            let mutable (jump, jumpDown) = match delta with
                                                | dif when dif > deltaSize -> (true, false)
                                                | dif when dif < -deltaSize -> (false, true)
                                                | _ -> (false, false)
            
            if (tiles.[(int) pos.X].[(int) pos.Y - 1] = Tile.Ladder && tiles.[target.X].[target.Y] = Tile.Empty) then
                if delta <= 0.0f && delta > -deltaSize then jump <- true

            if unit.JumpState.CanCancel 
                && tiles.[(int) pos.X].[(int) pos.Y - 1] = Tile.Empty
                && abs(x - pos.X) > 0.4f then jump <- true; jumpDown <- false            
            (jump, jumpDown, vel)

    let jumpUp (pos:Vector2) target =
        let x = single target.X
        let vel = calcVelocity pos.X x
        (true, false, vel)

    let jumpPad (pos:Vector2) (unit: Unit) target =
        if unit.JumpState.CanCancel then setNextStep(); (false, false, 0.0)
        else jumpUp pos target
            
    let jumpDown (pos:Vector2) (unit: Unit) target =
        let x = single target.X
        let vel = calcVelocity pos.X x
        let jumpDown = unit.JumpState.CanCancel
        (false, jumpDown, vel)

    let jumpUpTouch (pos:Vector2) (unit:Unit) target =
        if newStep then hasTouched <- false
        let x = single target.X
        let vel = calcVelocity pos.X x
        let makeTouch = match int pos.Y > target.Y || unit.OnLadder with
                           | true -> hasTouched <- true; true
                           | _ -> false            
        (not makeTouch, makeTouch, vel)
        
    let jumpDownTouch (pos:Vector2) (unit:Unit) target =
        if newStep then hasTouched <- false
        let (x,y) = single target.X, single target.Y
        let vel = calcVelocity pos.X x
        if pos.Y - y < deltaSize || unit.OnLadder then hasTouched <- true
        (false, true, vel)

    let internalMove (tiles:Tile[][]) (pos:Vector2) (unit:Unit) (link:Link) =        
       match link.Type with
            | LinkType.Walk -> walk tiles pos unit link.Target
            | LinkType.JumpUp -> jumpUp pos link.Target
            | LinkType.JumpPad -> jumpPad pos unit link.Target
            | LinkType.JumpUpTouch -> jumpUpTouch pos unit link.Target
            | LinkType.JumpDown -> jumpDown pos unit link.Target
            | LinkType.JumpDownTouch -> jumpDownTouch pos unit link.Target
            
    member _.CurrentPath with get() = route
    member _.CurrentPath with set (value) = 
        if route |> Array.compareWith(fun a b -> (if a = b then 0 else -1)) value <> 0 then
            route <- value
            step <- 0
            newStep <- true
            hasTouched <- true

    member _.PrepareMove (unit:Unit) =
        let unitCell = Cell.fromVector unit.Position 

        if route.Length = step then true;
        else
            let pos = Vector2.fromVec2Double unit.Position
            let tgt = route.[step].Target.toMidBottom
            Logger.drawText (sprintf "Pos %A Tgt:%A" unit.Position tgt)
            let delta = Vector2.dist tgt pos

            if delta < deltaSize && hasTouched then 
               setNextStep()
            else
                newStep <- false
            step = route.Length || (delta > 1.5f && route.[step].Type <> LinkType.JumpPad)

    member _.MakeMove (tiles:Tile[][]) (unit:Unit) =        
        let pos = Vector2.fromVec2Double unit.Position
        Logger.drawText (sprintf "NS: %A T: %A S: %A" newStep hasTouched step)
        if step < route.Length then 
            Some (internalMove tiles pos unit route.[step])
        else
            None
        

