namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Numerics
open System.Text
open System.Text.Json
open System.Text.Json.Nodes

[<NoComparison>]
type Json =
    | JStr of string
    | JNum of string
    | JBit of bool
    | JObj of (string * Json) list
    | JArr of Json list
    | JNil

[<NoComparison>]
type JsonFormat =
    | Indented
    | Custom of JsonSerializerOptions
    | Raw

[<AutoOpen>]
type JNum =

    /// <summary>Creates a Json from a number.</summary>
    /// <param name="number">The number.</param>
    static member inline JNum<'a when 'a :> INumber<'a>>(number:'a) =
        let string = number.ToString("R", CultureInfo.InvariantCulture)
        Json.JNum string

module internal JNil =

    let inline from map fn = function
        | Some x -> (map >> fn) x
        | None -> JNil

module JArr =

    /// <summary>An empty JSON array.</summary>
    let empty = JArr List.empty

    let inline internal from fn map =
        List.ofSeq >> List.map (fn >> map) >> JArr

module JStr =

    /// <summary>An empty JSON string.</summary>
    let empty = JStr String.Empty

    /// <summary>Creates a JSON string from an optional value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JStr x

    /// <summary>Creates a JSON string array from a sequence.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JStr x

    /// <summary>Creates a JSON string array from a value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JNum =

    /// <summary>A JSON number with the value 0.</summary>
    let zero = JNum 0

    /// <summary>Creates a JSON number from an optional value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JNil.from fn JNum x

    /// <summary>Creates a JSON number array from a sequence.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JNum x

    /// <summary>Creates a JSON number array from a value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JBit =

    /// <summary>Creates a JSON bool from an optional value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JBit x

    /// <summary>Creates a JSON bool array from a sequence.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JBit x

    /// <summary>Creates a JSON bool array from a value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JObj =

    /// <summary>An empty JSON object.</summary>
    let empty = JObj List.empty

    /// <summary>Creates a JSON object from an optional value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JObj x

    /// <summary>Creates a JSON object array from a sequence.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JObj x

    /// <summary>Creates a JSON object array from a value.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module Json =

    let private serializerOptions =
        JsonSerializerOptions (
            WriteIndented = true,
            IndentSize = 4,
            NewLine = "\n"
        )

    /// <summary>Sorts all properties in ascending order.</summary>
    /// <param name="json">The Json to sort.</param>
    let rec sort json =
        match json with
        | JObj list ->
            list
            |> List.sortBy fst
            |> List.map (fun (k, v) -> k, sort v)
            |> JObj
        | JArr list ->
            list
            |> List.map sort
            |> JArr
        | other -> other

    /// <summary>Converts a JsonElement to a Json.</summary>
    /// <param name="element">The JsonElement to convert.</param>
    let rec fromElement (element:JsonElement) =
        match element.ValueKind with
        | Kind.String  -> JStr <| element.GetString()
        | Kind.Number -> Json.JNum <| element.GetRawText()
        | Kind.True -> JBit true
        | Kind.False -> JBit false
        | Kind.Object ->
            element.EnumerateObject()
            |> Seq.map (fun prop -> prop.Name, fromElement prop.Value)
            |> Seq.toList
            |> JObj
        | Kind.Array   ->
            element.EnumerateArray()
            |> Seq.map fromElement
            |> Seq.toList
            |> JArr
        | Kind.Null | Kind.Undefined -> JNil

    /// <summary>Parses a JSON string to a Json.</summary>
    /// <param name="json">The JSON string to parse.</param>
    let fromString ([<StringSyntax("Json")>] json:string) =
        try
            use document = JsonDocument.Parse(json, JsonDocumentOptions.preset)
            Ok <| fromElement document.RootElement
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error exn

    /// <summary>Parses a JSON string asynchronously to a Json.</summary>
    /// <param name="token">The CancellationToken to monitor.</param>
    /// <param name="stream">The JSON stream to parse.</param>
    let fromStreamAsync token stream =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream, JsonDocumentOptions.preset, token)
                return Ok <| fromElement document.RootElement
            with
                | :? JsonException
                | :? ArgumentNullException as exn -> return Error exn
        }

    /// <summary>Parses a byte array to a Json.</summary>
    /// <param name="bytes">The byte array to parse.</param>
    let fromBytes (bytes:byte array) =
        Encoding.UTF8.GetString(bytes)
        |> fromString

    /// <summary>Converts a Json to a JsonNode.</summary>
    /// <param name="json">The Json to convert.</param>
    let rec asJsonNode json =
        match json with
        | JStr str -> JsonValue.Create(str).Root
        | JNum str -> JsonNode.Parse(str)
        | JBit bit -> JsonValue.Create(bit).Root
        | JObj obj ->
            let object = JsonObject ()
            for name, json in obj do
                object.Add(name, asJsonNode json)
            object :> JsonNode
        | JArr arr ->
            let array = JsonArray ()
            for json in arr do
                array.Add(asJsonNode json)
            array :> JsonNode
        | JNil -> null

    /// <summary>Converts a Json to a formatted JSON string.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asString format json =
        match asJsonNode json with
        | node when isNull node -> "null"
        | node ->
            let options =
                match format with
                | Indented -> serializerOptions
                | Custom options -> options
                | Raw -> null

            node.ToJsonString(options)

    /// <summary>Writes a Json as a JSON string to a writer.</summary>
    /// <remarks>The JSON string is written raw and not formatted.</remarks>
    /// <param name="writer">The writer to write to.</param>
    /// <param name="json">The Json to write.</param>
    let asStringTo (writer:Utf8JsonWriter) json =
        let rec write = function
            | JStr str -> writer.WriteStringValue(str)
            | JNum str -> writer.WriteRawValue(str)
            | JBit bit -> writer.WriteBooleanValue(bit)
            | JObj obj ->
                writer.WriteStartObject()
                obj
                |> List.iter (fun (k, v) ->
                    writer.WritePropertyName(k)
                    write v
                )
                writer.WriteEndObject()
            | JArr arr ->
                writer.WriteStartArray()
                List.iter write arr
                writer.WriteEndArray()
            | JNil -> writer.WriteNullValue()

        write json

    /// <summary>Converts a Json to a byte array.</summary>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asBytes format json =
        asString format json
        |> Encoding.UTF8.GetBytes