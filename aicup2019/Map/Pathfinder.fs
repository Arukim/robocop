namespace Robocop.Map

open System.Collections.Generic
open System.Linq
open Robocop.Utils
open Robocop.Core

type DistInfo = {Dist:single; Prev:ConnectionType; JumpLeft:single}
type QueueItem = {Source:Cell; Links: array<Link>}

module Pathfinder =
    let infDist = {Dist=infinityf; Prev=ConnectionType.Walk; JumpLeft = 0.0f}

    let cmp a b =
        if a = b then 0
        else if a > b then 1
        else -1

    // todo rewrite in F# style
    // [PERF] use sorted queue for better performace
    let dijkstra (graph:Map<Cell,array<Link>>) source jumpLeft =
        let dist: Dictionary<Cell, DistInfo> = new Dictionary<Cell, DistInfo>()
        let prev: Dictionary<Cell, Option<Cell>> = new Dictionary<Cell, Option<Cell>>()
        let q: List<single*QueueItem> = new List<single*QueueItem>()
        
        graph |> Map.iter (fun k v -> 
                                    dist.[k] <- infDist
                                    prev.[k] <- None)

        dist.[source] <- {Dist=0.0f; Prev=ConnectionType.Walk; JumpLeft = jumpLeft}
        
        if graph.ContainsKey source then
            q.Add(0.0f,{Source=source;Links=graph.[source]})

        while q.Any() do
            q.Sort(fun a b -> cmp (fst a) (fst b))
            let (_,u) = q.First()
            q.RemoveAt 0 |> ignore

            for link in u.Links do
                let curr = dist.[u.Source]
                if not (curr.Prev = ConnectionType.JumpDown && link.Type = ConnectionType.JumpUp) 
                    && (link.Type <> ConnectionType.JumpUp || link.Dist < curr.JumpLeft) then
                    let alt = curr.Dist + link.Dist
                    let next = dist.GetValueOrDefault(link.Target, infDist)
                    if alt < next.Dist then
                        let links = graph.GetValueOrDefault(link.Target, Array.empty)
                        // either add a new node
                        if next.Dist = infinityf then
                            q.Add(alt,{Source=link.Target;Links=links})
                        else
                            // or make in-place update for existing
                            for i in 0..q.Count - 1 do
                                let (_,it) = q.[i]
                                if it.Source = link.Target then
                                    q.[i] <- (alt,{Source=link.Target;Links=links})
                                    
                        dist.[link.Target] <- {
                                                Dist=alt; 
                                                Prev=link.Type; 
                                                JumpLeft= match link.Type with
                                                                | ConnectionType.JumpUp -> curr.JumpLeft - link.Dist
                                                                | ConnectionType.Walk -> Constants.Max_Jump
                                                                | ConnectionType.JumpUpTouch -> Constants.Max_Jump
                                                                | _ -> 0.0f}
                        prev.[link.Target] <- Some u.Source

        let path = prev |> Seq.choose (fun kv -> match kv.Value with
                                                    | Some v -> Some (kv.Key, v)
                                                    | _ -> None)
                        |> Map.ofSeq
        let distMap = dist |> Seq.map (fun kv -> (kv.Key, kv.Value))
                           |> Map.ofSeq

        //dist |> Seq.choose(fun kv -> match kv.Value.Dist <> infinityf with true -> Some kv.Key | _ -> None)
        //     |> Seq.iter (fun x -> Logger.cellHighlight x.toCenter Palette.Blue)
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

