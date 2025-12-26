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
            |> Seq.map (fun (name, json) ->
                let node = getJsonNode json
                KeyValuePair(name, node)
            )
            |> JsonObject
            |> JsonObject.asJsonNode
        | JArr arr ->
            arr
            |> Seq.map getJsonNode
            |> Seq.toArray
            |> JsonArray
            |> JsonArray.asJsonNode
        | JNil nil ->
            nil
            |> Option.map getJsonNode
            |> Option.defaultValue null

    /// <summary>Converts the Json to a JSON string with the given format.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asString format json =
        getJsonNode json
        |> JsonNode.asString format