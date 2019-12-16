namespace Robocop.Map

open System.Collections.Generic
open System.Linq
open Robocop.Utils
open Robocop.Core

type DistInfo = {Dist:single; Prev:ConnectionType; JumpLeft:single}

module Pathfinder =
    let infDist = {Dist=infinityf; Prev=ConnectionType.Walk; JumpLeft = 0.0f}
    // todo rewrite in F# style
    // [PERF] use sorted queue for better performace
    let dijkstra (graph:Map<Cell,seq<Link>>) source =
        let dist: Dictionary<Cell, DistInfo> = new Dictionary<Cell, DistInfo>()
        let prev: Dictionary<Cell, Option<Cell>> = new Dictionary<Cell, Option<Cell>>()
        let q: Dictionary<Cell, array<Link>> =  Dictionary<Cell, array<Link>>()
        
        graph |> Map.iter (fun k v -> 
                                    dist.[k] <- infDist
                                    prev.[k] <- None
                                    q.[k] <- (v |> Array.ofSeq))

        dist.[source] <- {Dist=0.0f; Prev=ConnectionType.Walk; JumpLeft = Constants.Max_Jump}
        while q.Any() do
            let u = q.OrderBy(fun x -> dist.[x.Key])
                     .First()

            q.Remove(u.Key) |> ignore

            for link in u.Value do
                let curr = dist.[u.Key]
                if not (curr.Prev = ConnectionType.JumpDown && link.Type = ConnectionType.JumpUp) 
                    && (link.Type <> ConnectionType.JumpUp || link.Dist < curr.JumpLeft) then
                    let alt = curr.Dist + link.Dist
                    let next = dist.GetValueOrDefault(link.Target, infDist)
                    if alt < next.Dist then
                        dist.[link.Target] <- {
                                                Dist=alt; 
                                                Prev=link.Type; 
                                                JumpLeft= match link.Type with
                                                                | ConnectionType.JumpUp -> curr.JumpLeft - link.Dist
                                                                | ConnectionType.Walk -> Constants.Max_Jump
                                                                | _ -> 0.0f}
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

