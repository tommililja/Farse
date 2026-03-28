namespace Farse

open System
open System.Diagnostics.CodeAnalysis
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

    let inline internal from map fn = function
        | Some x -> (map >> fn) x
        | None -> JNil

module JArr =

    let empty = JArr Seq.empty

    let inline internal from fn map =
        Seq.map (fn >> map) >> JArr

module JStr =

    let empty = JStr String.Empty

    let inline nil fn = JNil.from fn JStr

    let inline arr fn = JArr.from fn JStr

    let inline singleton fn x =
        Seq.singleton x
        |> arr fn

module JNum =

    let zero = JNum 0

    let inline nil<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) =
        JNil.from fn JNum

    let inline arr fn = JArr.from fn JNum

    let inline singleton fn x =
        Seq.singleton x
        |> arr fn

module JBit =

    let inline nil fn = JNil.from fn JBit

    let inline arr fn = JArr.from fn JBit

    let inline singleton fn x =
        Seq.singleton x
        |> arr fn

module JObj =

    let empty = JObj Seq.empty

    let inline nil fn = JNil.from fn JObj

    let inline arr fn = JArr.from fn JObj

    let inline singleton fn x =
        Seq.singleton x
        |> arr fn

module Json =

    let private serializerOptions =
        JsonSerializerOptions (
            WriteIndented = true,
            IndentSize = 4,
            NewLine = "\n"
        )

    let private documentOptions =
        JsonDocumentOptions (
            AllowTrailingCommas = true,
            CommentHandling = JsonCommentHandling.Skip
        )

    let rec private getJsonNode = function
        | JStr str -> JsonValue.Create(str).Root
        | JBit bit -> JsonValue.Create(bit).Root
        | JNum num -> num
        | JObj obj ->
            let object = JsonObject ()
            for name, json in obj do
                object.Add(name, getJsonNode json)
            object :> JsonNode
        | JArr arr ->
            let array = JsonArray ()
            for json in arr do
                array.Add(getJsonNode json)
            array :> JsonNode
        | JNil -> null

    let rec private getJson (node:JsonNode) =
        match node with
        | node when isNull node -> JNil
        | node ->
            match node.GetValueKind() with
            | Kind.String -> JStr <| node.GetValue()
            | Kind.Number -> Json.JNum node
            | Kind.True | Kind.False -> JBit <| node.GetValue()
            | Kind.Object ->
                node :?> JsonObject
                |> Seq.map (fun kv -> kv.Key, getJson kv.Value)
                |> JObj
            | Kind.Array ->
                node :?> JsonArray
                |> Seq.map getJson
                |> JArr
            | Kind.Null | Kind.Undefined -> JNil

    /// <summary>Converts a JSON string to Json.</summary>
    /// <param name="json">The JSON string to convert.</param>
    let fromString ([<StringSyntax("Json")>] json:string) =
        try
            let node = JsonNode.Parse(json, documentOptions = documentOptions)
            Ok <| getJson node
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error exn

    /// <summary>Converts a JSON stream asynchronously to Json.</summary>
    /// <param name="token">The CancellationToken to use.</param>
    /// <param name="stream">The JSON stream to convert.</param>
    let fromStreamAsync token stream =
        task {
            try
                let! node = JsonNode.ParseAsync(stream, cancellationToken = token, documentOptions = documentOptions)
                return Ok <| getJson node
            with
                | :? JsonException
                | :? ArgumentNullException as exn -> return Error exn
        }

    /// <summary>Converts a Json to a formatted JSON string.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asString format json =
        match getJsonNode json with
        | node when isNull node -> "null"
        | node ->
            let options =
                match format with
                | Indented -> serializerOptions
                | Custom options -> options
                | Raw -> null

            node.ToJsonString(options)