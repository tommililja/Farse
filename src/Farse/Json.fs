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
    /// <example><code>"number", JNum 1</code></example>
    /// <param name="number">The number.</param>
    static member inline JNum<'a when 'a :> INumber<'a>>(number:'a) =
        match typeof<'a> with
        | x when x = typeof<float> -> number.ToString("G17", CultureInfo.InvariantCulture)
        | x when x = typeof<float32> -> number.ToString("G9", CultureInfo.InvariantCulture)
        | x when x = typeof<decimal> -> number.ToString(null, CultureInfo.InvariantCulture)
        | x when x = typeof<bigint> -> number.ToString("R", CultureInfo.InvariantCulture)
        | _ -> number.ToString("D", CultureInfo.InvariantCulture)
        |> Json.JNum

module internal JNil =

    let inline from map fn = function
        | Some x -> (map >> fn) x
        | None -> JNil

module JArr =

    /// <summary>An empty JSON array.</summary>
    /// <example><code>"array", JArr.empty</code></example>
    let empty = JArr List.empty

    let inline internal from fn map =
        List.ofSeq
        >> List.map (fn >> map)
        >> JArr

module JStr =

    /// <summary>An empty JSON string.</summary>
    /// <example><code>"string", JStr.empty</code></example>
    let empty = JStr String.Empty

    /// <summary>Creates a JSON string from an optional value.</summary>
    /// <example><code>"string", JStr.nil _ToUpper() (Some "string")</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JStr x

    /// <summary>Creates a JSON string array from a sequence.</summary>
    /// <example><code>"array", JStr.arr _ToUpper() [ "string" ]</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JStr x

    /// <summary>Creates a JSON string array from a value.</summary>
    /// <example><code>"array", JStr.singleton _ToUpper() "string"</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JNum =

    /// <summary>A JSON number with the value 0.</summary>
    /// <example><code>"number", JNum.zero</code></example>
    let zero = JNum 0

    /// <summary>Creates a JSON number from an optional value.</summary>
    /// <example><code>"number", JNum.nil ((*)2) (Some 1)</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JNil.from fn JNum x

    /// <summary>Creates a JSON number array from a sequence.</summary>
    /// <example><code>"array", JNum.arr ((*)2) [ 1 ]</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr.from fn JNum x

    /// <summary>Creates a JSON number array from a value.</summary>
    /// <example><code>"array", JNum.singleton ((*)2) 1</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JBit =

    /// <summary>Creates a JSON bool from an optional value.</summary>
    /// <example><code>"bool", JBit.nil not (Some true)</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JBit x

    /// <summary>Creates a JSON bool array from a sequence.</summary>
    /// <example><code>"array", JBit.arr not [ true ]</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JBit x

    /// <summary>Creates a JSON bool array from a value.</summary>
    /// <example><code>"array", JBit.singleton not true</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The value.</param>
    let inline singleton fn x =
        List.singleton x
        |> arr fn

module JObj =

    /// <summary>An empty JSON object.</summary>
    /// <example><code>"object", JObj.empty</code></example>
    let empty = JObj List.empty

    /// <summary>Creates a JSON object from an optional value.</summary>
    /// <example><code>"object", JObj.nil (fun obj -> [ "prop", JStr obj.Prop ]) (Some {| Prop = "value" |})</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The optional value.</param>
    let inline nil fn x = JNil.from fn JObj x

    /// <summary>Creates a JSON object array from a sequence.</summary>
    /// <example><code>"array", JObj.arr (fun obj -> [ "prop", JStr obj.Prop ]) [ {| Prop = "value" |} ]</code></example>
    /// <param name="fn">The mapping function.</param>
    /// <param name="x">The sequence.</param>
    let inline arr fn x = JArr.from fn JObj x

    /// <summary>Creates a JSON object array from a value.</summary>
    /// <example><code>"array", JObj.singleton (fun obj -> [ "prop", JStr obj.Prop ]) {| Prop = "value" |}</code></example>
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
    /// <example><code>let sorted = Json.sort json</code></example>
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

    /// <summary>Compares two Json values.</summary>
    /// <remarks>Properties are sorted in ascending order.</remarks>
    /// <example><code>let equal = Json.equal x y</code></example>
    /// <param name="x">The first Json value to compare.</param>
    /// <param name="y">The second Json value to compare.</param>
    let equal x y =
        let x = sort x
        let y = sort y
        x = y

    /// <summary>Converts a JsonElement to a Json.</summary>
    /// <example><code>let json = Json.fromElement element</code></example>
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
    /// <example><code>let result = Json.fromString json</code></example>
    /// <param name="json">The JSON string to parse.</param>
    let fromString ([<StringSyntax("Json")>] json:string) =
        try
            use document = JsonDocument.Parse(json, JsonDocumentOptions.preset)
            Ok <| fromElement document.RootElement
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error exn

    /// <summary>Parses a JSON string asynchronously to a Json.</summary>
    /// <example><code>let! result = Json.fromStreamAsync token stream</code></example>
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
    /// <example><code>let result = Json.fromBytes bytes</code></example>
    /// <param name="bytes">The byte array to parse.</param>
    let fromBytes (bytes:byte array) =
        Encoding.UTF8.GetString(bytes)
        |> fromString

    /// <summary>Converts a Json to a JsonNode.</summary>
    /// <example><code>let node = Json.asJsonNode json</code></example>
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
    /// <example><code>let string = Json.asString Indented json</code></example>
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
    /// <example><code>Json.asStringTo writer json</code></example>
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
    /// <example><code>let bytes = Json.asBytes Indented json</code></example>
    /// <param name="format">The format to use.</param>
    /// <param name="json">The Json to convert.</param>
    let asBytes format json =
        asString format json
        |> Encoding.UTF8.GetBytes