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

