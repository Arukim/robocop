namespace Robocop.Utils

module Seq =
   let splitBy f input =
        let i = ref 0
        input
        |> Seq.groupBy (fun x ->
        if f x then incr i
        !i)
        |> Seq.map snd
