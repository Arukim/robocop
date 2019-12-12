namespace Robocop.Utils

open System.Numerics
open AiCup2019.Model

(* For some bizarre reasons a lot of Vector2 methods do not
   work correctly with pipes. This module add some adhoc
   wrappers to solve this problem.*)
module Vector2 =
    let inline sub a b = Vector2.Subtract(a, b)
    let inline add a b = Vector2.Add(a, b)
    let inline mulS (a:single) b = Vector2.Multiply(a, b)
    let inline fromTuple (v:single*single) = Vector2(fst v, snd v)
    let inline fromVec2Double (v:Vec2Double) = Vector2(single v.X, single v.Y)
    let inline dist a b = Vector2.Distance(a, b)
    let inline crossNorm (a:Vector2) (b: Vector2) = 
        let q = (a - b)
        Vector2.Normalize(new Vector2(q.Y, -q.X))

