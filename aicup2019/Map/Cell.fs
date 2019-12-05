namespace Robocop.Map

open System.Numerics

type Cell = {X: int; Y: int} with
    member this.toLeftBottom = new Vector2(single this.X, single this.Y)
    member this.toLeftBottomDelta = new Vector2(single this.X - 0.5f, single this.Y)
    member this.toRightBottom = new Vector2(single this.X + 1.0f, single this.Y)
    member this.toRightBottomDelta = new Vector2(single this.X + 1.0f + 0.5f, single this.Y)
    member this.toLeftMid = new Vector2(single this.X, single this.Y + 0.5f)
    member this.toLeftTop = new Vector2(single this.X, single this.Y + 1.0f)
    member this.toLeftTopDelta = new Vector2(single this.X - 0.5f, single this.Y + 0.99f)
    member this.toRightMid = new Vector2(single this.X + 1.0f, single this.Y + 0.5f)
    member this.toRightTop = new Vector2(single this.X + 1.0f, single this.Y + 1.0f)
    member this.toRightTopDelta = new Vector2(single this.X + 1.0f + 0.5f, single this.Y + 0.99f)
    member this.toMidBottom = new Vector2(single this.X + 0.5f, single this.Y)    
    member this.toMidTop = new Vector2(single this.X + 0.5f, single this.Y + 1.0f)
    member this.toMidTopDelta = new Vector2(single this.X + 0.5f, single this.Y + 0.99f)
    member this.toLeftNextMid = new Vector2(single this.X - 0.5f, single this.Y + 1.5f)
    member this.toRightNextMid = new Vector2(single this.X + 1.5f, single this.Y + 1.5f)
    member this.toCenter = new Vector2(single this.X + 0.5f, single this.Y + 0.5f)
    member this.toCenterUp = new Vector2(single this.X + 0.5f, single this.Y + 1.5f)
    member this.allEdges = seq {this.toLeftBottom; this.toRightBottom; this.toLeftTop; this.toRightTop }


