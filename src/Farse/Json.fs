namespace Farse

#if NET7_0_OR_GREATER
open System.Numerics
#endif

open System.Text.Json.Nodes
open System.Collections.Generic

[<NoComparison>]
type Json =
    | JStr of string
    | JNum of JsonValue
    | JBit of bool
    | JObj of JsonProperty seq
    | JArr of Json seq
    | JNil of Json option
    | JNon

    static member internal nil(fn) =
        Option.map fn >> JNil

and
    [<AutoOpen>]
    JNum =

        #if NET7_0_OR_GREATER

        static member JNum<'a when 'a :> INumber<'a>>(x:'a) =
            JsonValue.Create<'a>(x)
            |> Json.JNum

        #else

        static member JNum<'a>(x:'a) =
            JsonValue.Create<'a>(x)
            |> Json.JNum

        #endif

and JsonProperty = string * Json

module JStr =

    let nil = Json.nil JStr

module JNum =

    #if NET7_0_OR_GREATER

    let nil<'a when 'a :> INumber<'a>>(x:'a option) =
        Json.nil JNum x

    #else

    let nil<'a>(x:'a option) =
        Json.nil JNum x

    #endif

module JBit =

    let nil = Json.nil JBit

module JNil =

    let some = Some >> JNil

    let none = JNil None

module Json =

    let rec internal getJsonNode = function
        | JStr str -> JsonNode.create str
        | JNum num -> num.Root
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
    /// <remarks>WriteIndented = true</remarks>
    /// <typeparam name="json">The Json to convert.</typeparam>
    let asString = getJsonNode >> JsonNode.asString