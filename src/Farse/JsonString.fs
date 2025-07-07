namespace Farse

open System.Text.Json
open System.Text.Json.Nodes
open System.Collections.Generic

type JNumber =
    | Int of int
    | Float of float
    | Decimal of decimal

type JValue =
    | JStr of string
    | JNum of JNumber
    | JBit of bool
    | JObj of (string * JValue) seq
    | JArr of JValue seq
    | JOpt of JValue option
    | JNil

module internal JValue =

    let private createValue<'a> x =
        JsonValue.Create<'a>(x).Root

    let rec internal getJsonNode = function
        | JStr str -> createValue str
        | JNum num ->
            match num with
            | Int int -> createValue int
            | Float float -> createValue float
            | Decimal decimal -> createValue decimal
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

    let private defaultOptions =
        JsonSerializerOptions(IndentSize = 4, WriteIndented = true)

    let create list =
        list
        |> List.map (fun (key, value) ->
            let node = JValue.getJsonNode value
            KeyValuePair(key, node)
        )
        |> JsonObject
        |> JsonString

    let asString (JsonString x) =
        x.ToJsonString(defaultOptions)