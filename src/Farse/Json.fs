namespace Farse

open System.Numerics
open System.Text.Json.Nodes
open System.Collections.Generic

[<NoComparison>]
type Json =
    | JStr of string
    | JNum of number
    | JBit of bool
    | JObj of JsonProperty seq
    | JArr of Json seq
    | JNil of Json option
    | JNon

and JsonProperty = string * Json

and number internal (value:JsonValue) =

    member _.JsonNode = value.Root

[<AutoOpen>]
module number =

    let JNum<'a when 'a :> INumber<'a>> =
        JsonValue.Create<'a>
        >> number
        >> JNum

module JNil =

    let some = Some >> JNil

    let none = JNil None

    let internal nil fn =
        Option.map fn >> JNil

module JStr =

    let nil = JNil.nil JStr

module JNum =

    let nil<'a when 'a :> INumber<'a>>(x:'a option) =
        JNil.nil JNum x

module JBit =

    let nil = JNil.nil JBit

module Json =

    let rec internal getJsonNode = function
        | JStr str -> JsonNode.create str
        | JNum num -> num.JsonNode
        | JBit bit -> JsonNode.create bit
        | JObj obj ->
            obj
            |> Seq.choose (function
                | _, JNon -> None
                | key, value ->
                    let node = getJsonNode value
                    Some <| KeyValuePair(key, node)
            )
            |> JsonObject
            |> _.Root
        | JArr arr ->
            arr
            |> Seq.choose (function
                | JNon -> None
                | json -> Some <| getJsonNode json
            )
            |> Seq.toArray
            |> JsonArray
            |> _.Root
        | JNil nil ->
            nil
            |> Option.map getJsonNode
            |> Option.defaultValue null
        | JNon -> null

    /// <summary>Converts the Json to a formatted string.</summary>
    /// <remarks>
    ///     WriteIndented = true
    ///     IndentSize = 4
    /// </remarks>
    /// <typeparam name="json">The Json to convert.</typeparam>
    let asString = getJsonNode >> JsonNode.asString