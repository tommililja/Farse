namespace Farse

open System.Text.Json.Nodes
open System.Collections.Generic

type JNum =
    | Int of int
    | Float of float
    | Decimal of decimal
    | Byte of byte

[<NoComparison>]
type Json =
    | JStr of string
    | JNum of JNum
    | JBit of bool
    | JObj of JsonProperty list
    | JArr of Json list
    | JOpt of Json option
    | JNil

and JsonProperty = string * Json

module Json =

    let rec internal getJsonNode = function
        | JStr str -> JsonNode.create str
        | JNum num ->
            match num with
            | Int int -> JsonNode.create int
            | Float float -> JsonNode.create float
            | Decimal decimal -> JsonNode.create decimal
            | Byte byte -> JsonNode.create byte
        | JBit bit -> JsonNode.create bit
        | JObj obj ->
            obj
            |> Seq.map (fun (key, value) ->
                let node = getJsonNode value
                KeyValuePair(key, node)
            )
            |> JsonObject
            |> _.Root
        | JArr arr ->
            arr
            |> Seq.map getJsonNode
            |> Seq.toArray
            |> JsonArray
            |> _.Root
        | JOpt opt ->
            opt
            |> Option.map getJsonNode
            |> Option.defaultValue null
        | JNil -> null

    /// <summary>Converts the Json to a formatted string.</summary>
    /// <remarks>WriteIndented = true</remarks>
    /// <typeparam name="json">The Json to convert.</typeparam>
    let asString = getJsonNode >> JsonNode.asString