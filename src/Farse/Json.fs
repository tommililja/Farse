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

module Json =

    let private serializerOptions =
        JsonSerializerOptions (
            WriteIndented = true,
            IndentSize = 4,
            NewLine = "\n"
        )

    /// <summary>Sorts all properties in ascending order.</summary>
    /// <example><code>let sorted = Json.sort json</code></example>
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

    /// <summary>Converts a <c>JsonElement</c> into a <c>Json</c>.</summary>
    /// <example><code>let json = Json.fromElement element</code></example>
    let rec fromElement (element:JsonElement) =
        match element.ValueKind with
        | Kind.String -> JStr <| element.GetString()
        | Kind.Number -> JNum <| element.GetRawText()
        | Kind.True -> JBit true
        | Kind.False -> JBit false
        | Kind.Object ->
            element.EnumerateObject()
            |> Seq.map (fun prop -> prop.Name, fromElement prop.Value)
            |> Seq.toList
            |> JObj
        | Kind.Array ->
            element.EnumerateArray()
            |> Seq.map fromElement
            |> Seq.toList
            |> JArr
        | Kind.Null | Kind.Undefined -> JNil

    /// <summary>Parses a <c>string</c> into a <c>Json</c>.</summary>
    /// <example><code>let result = Json.fromString json</code></example>
    let fromString ([<StringSyntax("Json")>] json:string) =
        try
            use document = JsonDocument.Parse(json, JsonDocumentOptions.preset)
            Ok <| fromElement document.RootElement
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error exn

    /// <summary>Parses a UTF-8 encoded <c>Stream</c> asynchronously into a <c>Json</c>.</summary>
    /// <remarks>The <c>Stream</c> is read to completion.</remarks>
    /// <example><code>let! result = Json.fromStreamAsync token stream</code></example>
    let fromStreamAsync token stream =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream, JsonDocumentOptions.preset, token)
                return Ok <| fromElement document.RootElement
            with
                | :? JsonException
                | :? ArgumentNullException as exn -> return Error exn
        }

    /// <summary>Parses a UTF-8 encoded <c>byte array</c> into a <c>Json</c>.</summary>
    /// <example><code>let result = Json.fromBytes bytes</code></example>
    let fromBytes (bytes:byte array) =
        try
            use document = JsonDocument.Parse(bytes, JsonDocumentOptions.preset)
            Ok <| fromElement document.RootElement
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error exn

    /// <summary>Converts a <c>Json</c> to a <c>JsonNode</c>.</summary>
    /// <example><code>let node = Json.asJsonNode json</code></example>
    let rec asJsonNode json =
        match json with
        | JStr str -> JsonValue.Create(str).Root
        | JNum str -> JsonNode.Parse(str)
        | JBit bit -> JsonValue.Create(bit).Root
        | JObj obj ->
            let object = JsonObject()
            // Take last instead of throwing an exception.
            for name, json in obj do object[name] <- asJsonNode json
            object.Root
        | JArr arr ->
            let array = JsonArray()
            for json in arr do array.Add(asJsonNode json)
            array.Root
        | JNil -> null

    /// <summary>Converts a <c>Json</c> to a <c>JsonElement</c>.</summary>
    /// <example><code>let element = Json.asJsonElement json</code></example>
    let asJsonElement = asJsonNode >> JsonSerializer.SerializeToElement

    /// <summary>Converts a <c>Json</c> to a <c>JsonDocument</c>.</summary>
    /// <example><code>use document = Json.asJsonDocument json</code></example>
    let asJsonDocument = asJsonNode >> JsonSerializer.SerializeToDocument

    /// <summary>Converts a <c>Json</c> to a formatted JSON string.</summary>
    /// <example><code>let string = Json.asString Indented json</code></example>
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

    /// <summary>Writes a <c>Json</c> to a <c>Utf8JsonWriter</c>.</summary>
    /// <example>
    /// <code>
    ///    task {
    ///        use writer = new Utf8JsonWriter(ctx.Response.BodyWriter)
    ///        Json.writeTo writer json
    ///        do! writer.FlushAsync()
    ///    }
    /// </code>
    /// </example>
    let writeTo (writer:Utf8JsonWriter) json =
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

    /// <summary>Converts a <c>Json</c> to a UTF-8 encoded <c>byte array</c>.</summary>
    /// <example><code>let bytes = Json.asBytes Indented json</code></example>
    let asBytes format json =
        asString format json
        |> Encoding.UTF8.GetBytes

    /// <summary>Determines whether two <c>Json</c> values are equal.</summary>
    /// <remarks>Properties are compared regardless of order.</remarks>
    /// <example><code>let equal = Json.equal x y</code></example>
    let equal x y =
        let x = sort x
        let y = sort y
        x = y

    /// <summary>Compares two <c>Json</c> values and returns a message when they differ.</summary>
    /// <remarks>Properties are compared regardless of order.</remarks>
    /// <example>
    /// <code>
    ///     match Json.diff x y with
    ///     | Some msg -> failwith msg
    ///     | None -> ()
    /// </code>
    /// </example>
    let diff x y =
        let render = function
            | JStr str -> $"\"%s{str}\""
            | JNum str -> str
            | JBit bit -> bit.ToString().ToLower()
            | JNil -> "null"
            | other ->
                asString Indented other
                |> String.indent 4
                |> (+) "\n"

        let missing fn x y =
            Set.difference x y
            |> Set.toList
            |> List.map fn

        let pathKey path key =
            $"%s{path}.%s{key}"

        let renderDiff x y (path:string) =
            string {
                path
                $"  x: %s{x}"
                $"  y: %s{y}"
            }

        let rec diff path x y =
            match x, y with
            | JStr x, JStr y when x = y -> []
            | JNum x, JNum y when x = y -> []
            | JBit x, JBit y when x = y -> []
            | JArr x, JArr y when x = y -> []
            | JObj x, JObj y ->
                let xKeys, xMap = x |> List.map fst |> Set.ofList, Map.ofList x
                let yKeys, yMap = y |> List.map fst |> Set.ofList, Map.ofList y

                let yMissing =
                    missing (fun key ->
                        pathKey path key
                        |> renderDiff (render xMap[key]) "<missing>"
                    ) xKeys yKeys

                let xMissing =
                    missing (fun key ->
                        pathKey path key
                        |> renderDiff "<missing>" (render yMap[key])
                    ) yKeys xKeys

                let differing =
                    yKeys
                    |> Set.intersect xKeys
                    |> Set.toList
                    |> List.sort
                    |> List.collect (fun key -> diff (pathKey path key) xMap[key] yMap[key])

                yMissing @ xMissing @ differing
            | JArr x, JArr y when x.Length = y.Length ->
                List.zip x y
                |> List.mapi (fun i (x, y) -> diff $"%s{path}[%d{i}]" x y)
                |> List.concat
            | JNil, JNil -> []
            | x, y ->
                path
                |> renderDiff (render x) (render y)
                |> List.singleton

        match diff "$" x y with
        | [] -> None
        | diffs ->
            let list =
                diffs
                |> String.concat "\n\n"
                |> String.indent 2

            Some $"Diff yielded %i{diffs.Length} difference[s].\n\n%s{list}"

[<AutoOpen>]
type JNum =

    /// <summary>Creates a <c>Json</c> from an <c>INumber</c>.</summary>
    /// <example><code>"prop", JNum 1</code></example>
    static member inline JNum<'a when 'a :> INumber<'a>>(number:'a) =
        match typeof<'a> with
        | x when x = typeof<float> -> number.ToString("G17", CultureInfo.InvariantCulture)
        | x when x = typeof<float32> -> number.ToString("G9", CultureInfo.InvariantCulture)
        | x when x = typeof<bigint> -> number.ToString("R", CultureInfo.InvariantCulture)
        | x when x = typeof<Half> -> number.ToString("G5", CultureInfo.InvariantCulture)
        | _ -> number.ToString(null, CultureInfo.InvariantCulture) // Safe default for decimal, integers and custom types.
        |> Json.JNum

module internal JNil =

    let inline from map fn = function
        | Some x -> (map >> fn) x
        | None -> JNil

module JArr =

    /// <summary>An empty JSON array.</summary>
    /// <example><code>"prop", JArr.empty</code></example>
    let empty = JArr List.empty

    let inline internal from fn json seq =
        seq
        |> List.ofSeq
        |> List.map (fn >> json)
        |> JArr

module JStr =

    /// <summary>An empty JSON string.</summary>
    /// <example><code>"prop", JStr.empty</code></example>
    let empty = JStr String.Empty

    /// <summary>Creates a JSON string or null from an optional value.</summary>
    /// <example><code>"prop", JStr.option id (Some "string")</code></example>
    let inline option fn x =
        JNil.from fn JStr x

    /// <summary>Creates a JSON string array from a sequence.</summary>
    /// <example><code>"prop", JStr.array id [ "string" ]</code></example>
    let inline array fn x =
        JArr.from fn JStr x

    /// <summary>Creates a JSON string array from a value.</summary>
    /// <example><code>"prop", JStr.singleton id "string"</code></example>
    let inline singleton fn x =
        JArr.from fn JStr [ x ]

module JNum =

    /// <summary>A JSON number with the value 0.</summary>
    /// <example><code>"prop", JNum.zero</code></example>
    let zero = JNum 0

    /// <summary>Creates a JSON number or null from an optional value.</summary>
    /// <example><code>"prop", JNum.option id (Some 1)</code></example>
    let inline option<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JNil.from fn JNum x

    /// <summary>Creates a JSON number array from a sequence.</summary>
    /// <example><code>"prop", JNum.array id [ 1 ]</code></example>
    let inline array<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr.from fn JNum x

    /// <summary>Creates a JSON number array from a value.</summary>
    /// <example><code>"prop", JNum.singleton id 1</code></example>
    let inline singleton fn x =
        JArr.from fn JNum [ x ]

module JBit =

    /// <summary>Creates a JSON bool or null from an optional value.</summary>
    /// <example><code>"prop", JBit.option id (Some true)</code></example>
    let inline option fn x =
        JNil.from fn JBit x

    /// <summary>Creates a JSON bool array from a sequence.</summary>
    /// <example><code>"prop", JBit.array id [ true ]</code></example>
    let inline array fn x =
        JArr.from fn JBit x

    /// <summary>Creates a JSON bool array from a value.</summary>
    /// <example><code>"prop", JBit.singleton id true</code></example>
    let inline singleton fn x =
        JArr.from fn JBit [ x ]

module JObj =

    /// <summary>An empty JSON object.</summary>
    /// <example><code>"prop", JObj.empty</code></example>
    let empty = JObj List.empty

    /// <summary>Creates a JSON object from a value.</summary>
    /// <example><code>"prop", JObj.from (fun x -> [ "prop", JStr x.Prop ]) x</code></example>
    let inline from fn x =
        JObj <| fn x

    /// <summary>Creates a JSON object or null from an optional value.</summary>
    /// <example><code>"prop", JObj.option (fun x -> [ "prop", JStr x.Prop ]) (Some {| Prop = "value" |})</code></example>
    let inline option fn x =
        JNil.from fn JObj x

    /// <summary>Creates a JSON object array from a sequence.</summary>
    /// <example><code>"prop", JObj.array (fun x -> [ "prop", JStr x.Prop ]) [ {| Prop = "value" |} ]</code></example>
    let inline array fn x =
        JArr.from fn JObj x

    /// <summary>Creates a JSON object array from a value.</summary>
    /// <example><code>"prop", JObj.singleton (fun x -> [ "prop", JStr x.Prop ]) {| Prop = "value" |}</code></example>
    let inline singleton fn x =
        JArr.from fn JObj [ x ]