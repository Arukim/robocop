namespace Robocop.Map

type Cell = {X: int; Y: int} with
    member this.toLeftBottom = single this.X, single this.Y
    member this.toLeftBottomExtra = single this.X - 0.5f, single this.Y
    member this.toRightBottom = single this.X + 1.0f, single this.Y
    member this.toRightBottomExtra = single this.X + 1.0f + 0.5f, single this.Y
    member this.toLeftMid = single this.X, single this.Y + 0.5f
    member this.toLeftTop = single this.X, single this.Y + 1.0f
    member this.toLeftTopExtra = single this.X - 0.5f, single this.Y + 1.0f
    member this.toRightMid = single this.X + 1.0f, single this.Y + 0.5f
    member this.toRightTop = single this.X + 1.0f, single this.Y + 1.0f
    member this.toRightTopExtra = single this.X + 1.0f + 0.5f, single this.Y + 1.0f
    member this.toMidBottom = single this.X + 0.5f, single this.Y
    member this.toMidTop = single this.X + 0.5f, single this.Y + 1.0f
    member this.allEdges = seq {this.toLeftBottom; this.toRightBottom; this.toLeftTop; this.toRightTop }


