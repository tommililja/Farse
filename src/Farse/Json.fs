namespace Farse

open System.Numerics
open System.Text.Json
open System.Text.Json.Nodes

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

    static member inline JNum<'a when 'a :> INumber<'a>>(x) =
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

    let inline nil fn = JNil.from (fn >> JStr)

    let inline arr fn = JArr.from (fn >> JStr)

module JNum =

    let inline nil<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) =
        JNil.from (fn >> JNum)

    let inline arr fn = JArr.from (fn >> JNum)

module JBit =

    let inline nil fn = JNil.from (fn >> JBit)

    let inline arr fn = JArr.from (fn >> JBit)

module JObj =

    let inline nil fn = JNil.from (fn >> JObj)

    let inline arr fn = JArr.from (fn >> JObj)

module Json =

    let rec private getJsonNode = function
        | JStr str -> JsonValue.Create(str).Root
        | JBit bit -> JsonValue.Create(bit).Root
        | JNum num -> num
        | JObj obj ->
            let object = JsonObject()
            for name, json in obj do
                object.Add(name, getJsonNode json)
            object :> JsonNode
        | JArr arr ->
            let array = JsonArray()
            for json in arr do
                array.Add(getJsonNode json)
            array :> JsonNode
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