namespace Robocop.Utils

open System.Numerics

module Geom =
    /// not in use, but I've already made a test for it
    /// N.B. from the past tournament -> use vector math!
    let inline lineEquation p1 p2 : double*double =
        let k = (snd p1 - snd p2) / (fst p1 - fst p2)
        let b = snd p1 - fst p1 * k
        (k, b)

    let closestPointToLine a b p =
        let ap = p - a
        let ab = b - a
        a + Vector2.Dot(ap,ab) / Vector2.Dot(ab, ab) * ab

