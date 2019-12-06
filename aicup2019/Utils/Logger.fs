namespace Robocop.Utils

open AiCup2019.Model
open AiCup2019
open System.Numerics

type Logger() =
    static let mutable debug : Option<Debug> = None    
    static member Debug with set(v) = debug <- v
    
    static member drawDot (cell:Vector2) =
        match debug with
            | Some x -> x.draw(CustomData.Rect {
                                        Pos = {X = cell.X - 0.05f; Y = cell.Y - 0.05f}
                                        Size = {X = 0.1f; Y = 0.1f}
                                        Color = {R = 0.0f; G = 200.0f; B = 0.0f; A = 255.0f}
                                        })
            | _ -> ignore()