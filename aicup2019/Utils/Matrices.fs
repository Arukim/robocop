namespace Robocop.Utils

module Matrices =
    let rotateConterClockwise tiles =
        let height, width = Array.length tiles, Array.length tiles.[0]
        [|
            for row=0 to width-1 do
                yield [| 
                    for column=0 to height-1 do
                        yield tiles.[column].[(width - row - 1)]
                |]
        |]

    /// generate sequence of all available borders for the given Matrice
    let allBorders tiles =
        let width, height = Array.length tiles, Array.length tiles.[0]
        seq {
            // corners
            yield! seq {(0,0); (width-1, 0); (0,height-1); (height-1,width-1)}
            // top/down
            for n in 1 .. width - 2 do
                yield (n, 0)
                yield (n, height - 1)
            // left/right
            for n in 1 .. height - 2 do
                yield (0, n)
                yield (width - 1, n)
        }
