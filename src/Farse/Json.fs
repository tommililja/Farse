namespace Farse

open System.Text.Json.Nodes
open System.Collections.Generic

[<NoComparison>]
type Json =
    | JStr of string
    | JNum of JNum
    | JBit of bool
    | JObj of JsonProperty seq
    | JArr of Json seq
    | JNil of Json option

    static member internal nil fn =
        Option.map fn >> JNil

and
    [<AutoOpen>]
    JNum =
        | Int of int
        | Float of float
        | Decimal of decimal
        | Byte of byte

        static member internal int = Int >> Json.JNum
        static member internal decimal = Decimal >> Json.JNum
        static member internal float = Float >> Json.JNum
        static member internal byte = Byte >> Json.JNum

        static member JNum(x:int) = int x
        static member JNum(x:decimal) = decimal x
        static member JNum(x:float) = float x
        static member JNum(x:byte) = byte x

        static member nil(x:int option) = Json.nil int x
        static member nil(x:decimal option) = Json.nil decimal x
        static member nil(x:float option) = Json.nil float x
        static member nil(x:byte option) = Json.nil byte x

and JsonProperty = string * Json

module JStr =

    let nil = Json.nil JStr

module JNum =

    let int = JNum.int
    let intNil = Json.nil int
    let float = JNum.float
    let floatNil = Json.nil float
    let decimal = JNum.decimal
    let decimalNil = Json.nil decimal
    let byte = JNum.byte
    let byteNil = Json.nil byte

module JBit =

    let nil = Json.nil JBit

module JNil =

    let none = JNil None

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
        | JNil nil ->
            nil
            |> Option.map getJsonNode
            |> Option.defaultValue null

    /// <summary>Converts the Json to a formatted string.</summary>
    /// <remarks>WriteIndented = true</remarks>
    /// <typeparam name="json">The Json to convert.</typeparam>
    let asString = getJsonNode >> JsonNode.asString