namespace Farse

open System.Text.Json.Nodes
open System.Collections.Generic

type JNumber =
    | Int of int
    | Float of float
    | Decimal of decimal
    | Byte of byte

type JValue =
    | JStr of string
    | JNum of JNumber
    | JBit of bool
    | JObj of (string * JValue) seq
    | JArr of JValue seq
    | JOpt of JValue option
    | JNil

module internal JValue =

    let createValue x =
        JsonValue.Create<'a>(x).Root

    let rec getJsonNode = function
        | JStr str -> createValue str
        | JNum num ->
            match num with
            | Int int -> createValue int
            | Float float -> createValue float
            | Decimal decimal -> createValue decimal
            | Byte byte -> createValue byte
        | JBit bit -> createValue bit
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

type JsonString = JsonString of JsonObject

module JsonString =

    /// <summary>Creates a JsonString from a list of properties.</summary>
    /// <example>JsonString.create [ "prop", JStr "text" ]</example>
    /// <param name="props">The list of properties.</param>
    let create props =
        props
        |> List.map (fun (key, value) ->
            let node = JValue.getJsonNode value
            KeyValuePair(key, node)
        )
        |> JsonObject
        |> JsonString

    /// <summary>Converts the JsonString to a formatted string.</summary>
    /// <remarks>IndentSize = 4, WriteIndented = true.</remarks>
    /// <typeparam name="JsonString">The JsonString to convert.</typeparam>
    let asString (JsonString x) =
        x.ToJsonString(JsonSerializerOptions.preset)