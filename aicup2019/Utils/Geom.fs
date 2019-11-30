namespace Robocop.Utils

module Geom =
    /// not in use, but I've already made a test for it
    /// N.B. from the past tournament -> use vector math!
    let inline lineEquation p1 p2 : double*double =
        let k = (snd p1 - snd p2) / (fst p1 - fst p2)
        let b = snd p1 - fst p1 * k
        (k, b)

