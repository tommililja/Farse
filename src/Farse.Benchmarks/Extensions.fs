namespace Farse.Benchmarks

open System.Text.Json
open Newtonsoft.Json.Linq

module JToken =

    let asOption fn (t:JToken) =
        match t.Type with
        | JTokenType.Null -> None
        | _ -> Some <| fn t

    let asArray fn (t:JToken) =
        let array = t :?> JArray
        let items = Array.zeroCreate array.Count
        for i in 0 .. array.Count - 1 do
            items[i] <- fn array[i]
        items

module JsonElement =

    let asOption fn (e:JsonElement) =
        match e.ValueKind with
        | JsonValueKind.Null -> None
        | _ -> Some <| fn e

    let asArray fn (e:JsonElement) =
        let items = Array.zeroCreate <| e.GetArrayLength()
        let mutable enumerator, i = e.EnumerateArray(), 0
        while enumerator.MoveNext() do
            items[i] <- fn enumerator.Current
            i <- i + 1
        items