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
    | JNil

type JsonFormat =
    | Indented
    | Custom of JsonSerializerOptions
    | Raw

[<AutoOpen>]
type JNum =

    static member JNum<'a when 'a :> INumber<'a>>(x) =
        JsonValue.Create<'a>(x).Root
        |> Json.JNum

module JNil =

    let inline internal from fn = function
        | Some x -> fn x
        | None -> JNil

module JArr =

    let inline internal from fn =
        Seq.map fn >> JArr

module JStr =

    let nil fn = JNil.from (fn >> JStr)

    let arr fn = JArr.from (fn >> JStr)

module JNum =

    let nil<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) =
        JNil.from (fn >> JNum)

    let arr fn = JArr.from (fn >> JNum)

module JBit =

    let nil fn = JNil.from (fn >> JBit)

    let arr fn = JArr.from (fn >> JBit)

module JObj =

    let nil fn = JNil.from (fn >> JObj)

    let arr fn = JArr.from (fn >> JObj)

module Json =

    let rec private getJsonNode = function
        | JStr str -> JsonValue.Create(str).Root
        | JBit bit -> JsonValue.Create(bit).Root
        | JNum num -> num
        | JObj obj ->
            obj
            |> Seq.map (fun (name, json) ->
                let node = getJsonNode json
                KeyValuePair(name, node)
            )
            |> JsonObject :> JsonNode
        | JArr arr ->
            arr
            |> Seq.map getJsonNode
            |> Seq.toArray
            |> JsonArray :> JsonNode
        | JNil -> null

    /// <summary>Converts the Json to a JSON string with the given format.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asString format json =
        match getJsonNode json with
        | node when isNull node -> "null"
        | node ->
            let options =
                match format with
                | Indented -> JsonSerializerOptions.preset
                | Custom options -> options
                | Raw -> null

            node.ToJsonString(options)