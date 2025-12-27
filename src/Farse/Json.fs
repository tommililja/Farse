namespace Farse

open System.Numerics
open System.Text.Json
open System.Text.Json.Nodes
open System.Collections.Generic

type Json =
    | JStr of string
    | JNum of JsonNode
    | JBit of bool
    | JObj of (string * Json) seq
    | JArr of Json seq
    | JNil of Json option

type JsonFormat =
    | Indented of int
    | Custom of JsonSerializerOptions
    | Raw

[<AutoOpen>]
type JNum =

    static member JNum<'a when 'a :> INumber<'a>>(x) =
        JsonValue.Create<'a>(x).Root
        |> Json.JNum

module JNil =

    let some = Some >> JNil

    let none = JNil None

    let internal from fn =
        Option.map fn >> JNil

module JStr =

    let nil = JNil.from JStr

module JNum =

    let nil<'a when 'a :> INumber<'a>> (x:'a option) =
        JNil.from JNum x

module JBit =

    let nil = JNil.from JBit

module Json =

    let rec internal getJsonNode = function
        | JStr str -> JsonNode.create str
        | JNum num -> num
        | JBit bit -> JsonNode.create bit
        | JObj obj ->
            obj
            |> Seq.map (fun (name, json) ->
                let node = getJsonNode json
                KeyValuePair(name, node)
            )
            |> JsonObject.asJsonNode
        | JArr arr ->
            arr
            |> Seq.map getJsonNode
            |> Seq.toArray
            |> JsonArray.asJsonNode
        | JNil nil ->
            nil
            |> Option.map getJsonNode
            |> Option.defaultValue null

    /// <summary>Converts the Json to a JSON string with the given format.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asString format json =
        match getJsonNode json with
        | node when isNull node -> "null"
        | node ->
            let options =
                match format with
                | Indented size -> JsonSerializerOptions.indented size
                | Custom options -> options
                | Raw -> null

            node.ToJsonString(options)