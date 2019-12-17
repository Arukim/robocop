namespace Robocop.Utils

open AiCup2019.Model
open AiCup2019
open System.Numerics

type Logger() =
    static let mutable debug : Option<Debug> = None    
    static member Debug with set(v) = debug <- v
    
    static member drawDot (cell:Vector2) color =
        match debug with
            | Some x -> x.draw(CustomData.Rect {
                                        Pos = {X = cell.X - 0.05f; Y = cell.Y - 0.05f}
                                        Size = {X = 0.1f; Y = 0.1f}
                                        Color = ColorSingle.from 1.0f color
                                        })
            | _ -> ignore()

    static member cellHighlight (cell:Vector2) color =
           match debug with
               | Some x -> x.draw(CustomData.Rect {
                                           Pos = {X = cell.X-0.5f; Y = cell.Y-0.5f}
                                           Size = {X = 1.0f; Y = 1.0f}
                                           Color = ColorSingle.from 0.5f color
                                           })
               | _ -> ignore()
    
    static member drawLine (source:Vector2) (target:Vector2) color =
        match debug with
        | Some x -> x.draw(CustomData.Line {
                                    P1 = {X = source.X; Y = source.Y}
                                    P2 = {X = target.X; Y = target.Y}
                                    Width = 0.05f
                                    Color = ColorSingle.from 0.5f color
                                    })
        | _ -> ignore()

    static member drawText text =
        match debug with
        | Some x -> x.draw(CustomData.Log {Text = text})
        | _ -> ignore()
    
    static member drawDotD (dot:Vector2) = Logger.drawDot dot Palette.LimeGreen
    static member drawLineD (source:Vector2) (target:Vector2) = Logger.drawLine source target Palette.LimeGreen