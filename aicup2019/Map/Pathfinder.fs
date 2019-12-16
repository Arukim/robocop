namespace Robocop.Map

open System.Collections.Generic
open System.Linq
open Robocop.Utils

module Pathfinder =
    // todo rewrite in F# style
    // [PERF] use sorted queue for better performace
    let dijkstra (graph:Map<Cell,seq<Link>>) source =
        let dist: Dictionary<Cell, single> = new Dictionary<Cell, single>()
        let prev: Dictionary<Cell, Option<Cell>> = new Dictionary<Cell, Option<Cell>>()
        let q: Dictionary<Cell, array<Link>> =  Dictionary<Cell, array<Link>>()
        
        graph |> Map.iter (fun k v -> 
                                    dist.[k] <- infinityf
                                    prev.[k] <- None
                                    q.[k] <- (v |> Array.ofSeq))

        dist.[source] <- 0.0f

        while q.Any() do
            let u = q.OrderBy(fun x -> dist.[x.Key])
                     .First()

            q.Remove(u.Key) |> ignore

            for link in u.Value do
                let alt = dist.[u.Key] + link.Dist
                if alt < dist.GetValueOrDefault(link.Target, infinityf) then
                    dist.[link.Target] <- alt
                    prev.[link.Target] <- Some u.Key

        let path = prev |> Seq.choose (fun kv -> match kv.Value with
                                                    | Some v -> Some (kv.Key, v)
                                                    | _ -> None)
                        |> Map.ofSeq
        let distMap = dist |> Seq.map (fun kv -> (kv.Key, kv.Value))
                           |> Map.ofSeq
        (path, distMap)

    let findPath (graph: Map<Cell,Cell>) source target =
        let u = ref target
        let mutable maxDepth = 50
        let path = seq {
            if graph.ContainsKey(u.contents) || u.contents = source then
                let mutable go = true
                while go && maxDepth > 0 do
                    maxDepth <- maxDepth - 1
                    yield u.contents
                    go <- graph.TryGetValue(u.contents, u)
        }
        if maxDepth > 0 then path else Seq.empty
    
    let findDistance (dist:Map<Cell, single>) cell =
        if dist.ContainsKey cell then
            dist.[cell]
        else
            infinityf

