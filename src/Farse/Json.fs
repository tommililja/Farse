namespace Farse

open System
open System.Buffers
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Numerics
open System.Text
open System.Text.Json
open System.Text.Json.Nodes

/// <summary>Represents a JSON number.</summary>
[<Struct>]
type Number =
    private | Value of string

    override this.ToString() =
        match this with
        | Value null -> "0" // Default struct value.
        | Value value -> value

    static member internal From(x:'a when 'a :> INumber<'a>) =
        if 'a.IsFinite(x)
        then Value <| x.ToString(null, CultureInfo.InvariantCulture)
        else Value "0"

    static member op_Implicit(x:int) = Number.From x
    static member op_Implicit(x:int16) = Number.From x
    static member op_Implicit(x:int64) = Number.From x
    static member op_Implicit(x:Int128) = Number.From x
    static member op_Implicit(x:uint16) = Number.From x
    static member op_Implicit(x:uint32) = Number.From x
    static member op_Implicit(x:uint64) = Number.From x
    static member op_Implicit(x:UInt128) = Number.From x
    static member op_Implicit(x:float) = Number.From x
    static member op_Implicit(x:float32) = Number.From x
    static member op_Implicit(x:decimal) = Number.From x
    static member op_Implicit(x:byte) = Number.From x
    static member op_Implicit(x:sbyte) = Number.From x
    static member op_Implicit(x:Half) = Number.From x
    static member op_Implicit(x:bigint) = Number.From x

/// <summary>Represents a JSON value.</summary>
[<NoComparison>]
type Json =
    /// <summary>A JSON string.</summary>
    /// <remarks><c>null</c> strings are written the same as <c>JNil</c>.</remarks>
    | JStr of string
    /// <summary>A JSON number.</summary>
    /// <remarks>
    ///     Use <c>JNum.number</c> to be explicit or suppress <c>FS3391</c> as <c>Number</c> uses implicit conversions.<br/><br/>
    ///     Formats the number with invariant culture. Floating-point values use the shortest representation that round-trips to the same type.
    ///     NaN and infinity are written as <c>0</c>.
    /// </remarks>
    | JNum of Number
    /// <summary>A JSON boolean.</summary>
    | JBit of bool
    /// <summary>A JSON object.</summary>
    /// <remarks>Property order is preserved.</remarks>
    | JObj of (string * Json) list
    /// <summary>A JSON array.</summary>
    /// <remarks>Element order is preserved.</remarks>
    | JArr of Json list
    /// <summary>A JSON null.</summary>
    | JNil

/// <summary>Represents the JSON string format.</summary>
[<NoComparison>]
type JsonFormat =
    /// <summary>Indented with four spaces.</summary>
    | Indented
    /// <summary>Formatted with the given <c>JsonWriterOptions</c>.</summary>
    | Custom of JsonWriterOptions
    /// <summary>Compact, without whitespace.</summary>
    | Raw

module internal JsonFormat =

    let getOptions = function
        | Indented -> JsonWriterOptions.indented
        | Custom options -> options
        | Raw -> JsonWriterOptions.raw

module Json =

    /// <summary>Sorts all properties in ascending order.</summary>
    /// <example><code>let sorted = Json.sort json</code></example>
    let rec sort json =
        match json with
        | JObj list ->
            list
            |> List.sortBy fst
            |> List.map (fun (key, value) -> key, sort value)
            |> JObj
        | JArr list ->
            list
            |> List.map sort
            |> JArr
        | other -> other

    /// <summary>Converts a <c>JsonElement</c> into a <c>Json</c>.</summary>
    /// <exception cref="System.ArgumentException">Thrown for undefined elements.</exception>
    /// <example><code>let json = Json.fromElement element</code></example>
    let rec fromElement (element:JsonElement) =
        match element.ValueKind with
        | Kind.String -> JStr <| element.GetString()
        | Kind.Number -> JNum <| Value (element.GetRawText())
        | Kind.True -> JBit true
        | Kind.False -> JBit false
        | Kind.Object ->
            JObj [
                for prop in element.EnumerateObject()
                    -> prop.Name, fromElement prop.Value
            ]
        | Kind.Array ->
            JArr [
                for item in element.EnumerateArray()
                    -> fromElement item
            ]
        | Kind.Null -> JNil
        | Kind.Undefined -> invalidArg (nameof element) "Element was undefined."

    let inline private parseDocument ([<InlineIfLambda>] fn) =
        try use document: JsonDocument = fn ()
            Ok <| fromElement document.RootElement
        with
            | :? JsonException
            | :? ArgumentException as exn -> Error exn

    let inline private parseDocumentAsync ([<InlineIfLambda>] fn) =
        task {
            try use! document: JsonDocument = fn ()
                return Ok <| fromElement document.RootElement
            with
                | :? JsonException
                | :? ArgumentException as exn -> return Error exn
        }

    /// <summary>Parses a <c>string</c> into a <c>Json</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Json.fromString json</code></example>
    let fromString ([<StringSyntax("Json")>] json:string) =
        parseDocument (fun () -> JsonDocument.Parse(json, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>Stream</c> asynchronously into a <c>Json</c>.</summary>
    /// <remarks>
    ///     The <c>Stream</c> is read to completion.<br/><br/>
    ///     Uses the following <c>JsonDocumentOptions</c>.
    ///     <code>
    ///         JsonDocumentOptions (
    ///             AllowTrailingCommas = true,
    ///             CommentHandling = JsonCommentHandling.Skip
    ///         )
    ///     </code>
    /// </remarks>
    /// <exception cref="System.OperationCanceledException">Thrown when the token is cancelled.</exception>
    /// <example><code>let! result = Json.fromStreamAsync token stream</code></example>
    let fromStreamAsync token stream =
        parseDocumentAsync (fun () -> JsonDocument.ParseAsync(stream, JsonDocumentOptions.Default, token))

    /// <summary>Parses a UTF-8 encoded <c>byte array</c> into a <c>Json</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Json.fromBytes bytes</code></example>
    let fromBytes (bytes:byte array) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlyMemory&lt;byte&gt;</c> into a <c>Json</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Json.fromMemory bytes</code></example>
    let fromMemory (bytes:ReadOnlyMemory<byte>) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlySequence&lt;byte&gt;</c> into a <c>Json</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Json.fromSequence bytes</code></example>
    let fromSequence (bytes:ReadOnlySequence<byte>) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

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
        let options = writer.Options
        let getPrefix () =
            // Indentation is not supported when writing raw values.
            if options.Indented then
                let size = options.IndentSize * writer.CurrentDepth
                options.NewLine + String(options.IndentCharacter, size)
            else String.Empty

        let rec write (prefix:string) = function
            | JStr str -> writer.WriteStringValue(str)
            | JNum num -> writer.WriteRawValue(prefix + num.ToString(), skipInputValidation = true)
            | JBit bit -> writer.WriteBooleanValue(bit)
            | JObj obj ->
                writer.WriteStartObject()
                for key, value in obj do
                    writer.WritePropertyName(key)
                    write String.Empty value
                writer.WriteEndObject()
            | JArr arr ->
                writer.WriteStartArray()
                let prefix = getPrefix ()
                for item in arr do
                    write prefix item
                writer.WriteEndArray()
            | JNil -> writer.WriteNullValue()

        write String.Empty json

    /// <summary>Converts a <c>Json</c> to a <c>JsonNode</c>.</summary>
    /// <remarks>
    ///     Returns <c>null</c> for <c>JNil</c>.<br/>
    ///     The last occurrence is chosen when duplicate properties exist.
    /// </remarks>
    /// <example><code>let node = Json.asJsonNode json</code></example>
    let rec asJsonNode json : JsonNode | null =
        match json with
        | JStr str -> JsonValue.Create(str)
        | JNum num -> JsonNode.Parse(num.ToString()) // Parsed as a number.
        | JBit bit -> JsonValue.Create(bit)
        | JObj obj ->
            let object = JsonObject()
            // Takes last instead of throwing an exception.
            for key, value in obj do
                object[key] <- asJsonNode value
            object
        | JArr arr ->
            let array = JsonArray()
            for item in arr do
                array.Add(asJsonNode item)
            array
        | JNil -> null

    let private writeToBuffer format json =
        let buffer = ArrayBufferWriter<byte>()
        let options = JsonFormat.getOptions format
        use writer = new Utf8JsonWriter(buffer, options)
        writeTo writer json
        writer.Flush()
        buffer

    /// <summary>Converts a <c>Json</c> to a <c>JsonElement</c>.</summary>
    /// <example><code>let element = Json.asJsonElement json</code></example>
    let asJsonElement json =
        let buffer = writeToBuffer Raw json
        let mutable reader = Utf8JsonReader(buffer.WrittenSpan)
        JsonElement.ParseValue(&reader)

    /// <summary>Converts a <c>Json</c> to a <c>JsonDocument</c>.</summary>
    /// <example><code>use document = Json.asJsonDocument json</code></example>
    let asJsonDocument json =
        let buffer = writeToBuffer Raw json
        JsonDocument.Parse(buffer.WrittenMemory)

    /// <summary>Converts a <c>Json</c> to a formatted JSON string.</summary>
    /// <example><code>let string = Json.asString Indented json</code></example>
    let asString format json =
        let buffer = writeToBuffer format json
        Encoding.UTF8.GetString(buffer.WrittenSpan)

    /// <summary>Converts a <c>Json</c> to a UTF-8 encoded <c>byte array</c>.</summary>
    /// <example><code>let bytes = Json.asBytes Indented json</code></example>
    let asBytes format json =
        let buffer = writeToBuffer format json
        buffer.WrittenSpan.ToArray()

    /// <summary>Determines whether two <c>Json</c> values are equal.</summary>
    /// <remarks>
    ///     Properties are compared regardless of order.
    ///     The last occurrence is chosen when duplicate properties exist.
    ///     Numbers are compared by value, so 1, 1.0 and 1e0 are equal.
    /// </remarks>
    /// <example><code>let equal = Json.equal x y</code></example>
    let equal x y =
        let lastByKey props =
            let dict = Dictionary<string, Json>(StringComparer.Ordinal)
            for key, value in props do dict[key] <- value
            dict

        let rec isEqual x y =
            match x, y with
            | JStr null, y -> isEqual JNil y
            | x, JStr null -> isEqual x JNil
            | JStr a, JStr b -> String.Equals(a, b, StringComparison.Ordinal)
            | JNum a, JNum b ->
                let a, b = a.ToString(), b.ToString()
                a = b || JsonNode.DeepEquals(JsonNode.Parse(a), JsonNode.Parse(b))
            | JBit a, JBit b -> a = b
            | JArr a, JArr b ->
                a.Length = b.Length
                && List.forall2 isEqual a b
            | JObj a, JObj b ->
                let a, b = lastByKey a, lastByKey b
                let sameLength = a.Count = b.Count
                sameLength && Seq.forall (fun (KeyValue (key, a)) ->
                    match b.TryGetValue(key) with
                    | true, b -> isEqual a b
                    | _ -> false
                ) a
            | JNil, JNil -> true
            | _ -> false

        // Avoids allocations when identical.
        x = y || isEqual x y

    /// <summary>Compares two <c>Json</c> values and returns a message when they differ.</summary>
    /// <remarks>
    ///     Properties are compared regardless of order.
    ///     The last occurrence is chosen when duplicate properties exist.
    ///     Numbers are compared by value, so 1, 1.0 and 1e0 are equal.
    /// </remarks>
    /// <example>
    /// <code>
    ///     match Json.diff x y with
    ///     | Some msg -> failwith msg
    ///     | None -> ()
    /// </code>
    /// </example>
    let diff x y =
        let render = function
            | JObj _ | JArr _ as json ->
                asString Indented json
                |> String.indent 4
                |> (+) "\n"
            | json -> asString Raw json

        let missing fn x y =
            Set.difference x y
            |> Set.toList
            |> List.map fn

        let pathKey path key =
            $"%s{path}%s{JsonPath.segment key}"

        let renderDiff x y (path:string) =
            multiline {
                path
                $"  x: %s{x}"
                $"  y: %s{y}"
            }

        let rec diff path x y =
            match x, y with
            | JStr null, y -> diff path JNil y
            | x, JStr null -> diff path x JNil
            | JStr x, JStr y when x = y -> []
            | JNum x, JNum y when equal (JNum x) (JNum y) -> []
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
                    |> List.collect (fun key -> diff (pathKey path key) xMap[key] yMap[key])

                yMissing @ xMissing @ differing
            | JArr x, JArr y when x.Length = y.Length ->
                y
                |> List.mapi2 (fun i -> diff $"%s{path}[%d{i}]") x
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

module JArr =

    /// <summary>An empty JSON array.</summary>
    /// <example><code>let array = JArr.empty</code></example>
    let empty = JArr []

    /// <summary>Creates a JSON array from <c>'a seq</c>.</summary>
    /// <example><code>let array = JArr.from JNum [ 1; 2; 3 ]</code></example>
    let inline from fn x =
        JArr [ for x in x -> fn x ]

module JStr =

    /// <summary>An empty JSON string.</summary>
    /// <example><code>let string = JStr.empty</code></example>
    let empty = JStr String.Empty

    /// <summary>Creates a JSON string from an <c>INumber</c>.</summary>
    /// <remarks>
    ///     Use <c>JStr.number&lt;int&gt;</c> to be explicit.<br/><br/>
    ///     Formats the number with invariant culture. Floating-point values use the shortest representation that round-trips to the same type.
    ///     NaN and infinity are written as <c>NaN</c>, <c>Infinity</c> and <c>-Infinity</c>.
    /// </remarks>
    /// <example><code>let string = JStr.number 1</code></example>
    let inline number<'a when 'a :> INumber<'a>>(x:'a) =
        JStr <| x.ToString(null, CultureInfo.InvariantCulture)

    /// <summary>Creates a JSON string or null from an <c>option</c>.</summary>
    /// <example><code>let string = JStr.option id (Some "string")</code></example>
    let inline option fn x =
        match x with
        | Some x -> JStr <| fn x
        | None -> JNil

    /// <summary>Creates a JSON string array from <c>'a seq</c>.</summary>
    /// <example><code>let array = JStr.array id [ "string" ]</code></example>
    let inline array fn x =
        JArr [ for x in x -> JStr <| fn x ]

    /// <summary>Creates a JSON string array with a single element from <c>'a</c>.</summary>
    /// <example><code>let array = JStr.single id "string"</code></example>
    let inline single fn x =
        JArr [ JStr <| fn x ]

module JNum =

    /// <summary>A JSON number with the value 0.</summary>
    /// <example><code>let number = JNum.zero</code></example>
    let zero = JNum <| Value "0"

    /// <summary>Creates a JSON number from an <c>INumber</c>.</summary>
    /// <remarks>
    ///     Use <c>JNum.number&lt;int&gt;</c> to be explicit.<br/><br/>
    ///     Formats the number with invariant culture. Floating-point values use the shortest representation that round-trips to the same type.
    ///     NaN and infinity are written as <c>0</c>.
    /// </remarks>
    /// <example><code>let number = JNum.number 1</code></example>
    let number<'a when 'a :> INumber<'a>>(x:'a) =
        JNum <| Number.From x

    /// <summary>Creates a JSON number or null from an <c>option</c>.</summary>
    /// <remarks>Use <c>JNum.option&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>let number = JNum.option id (Some 1)</code></example>
    let inline option<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        match x with
        | Some x -> number <| fn x
        | None -> JNil

    /// <summary>Creates a JSON number array from <c>'a seq</c>.</summary>
    /// <remarks>Use <c>JNum.array&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>let array = JNum.array id [ 1 ]</code></example>
    let inline array<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr [ for x in x -> number <| fn x ]

    /// <summary>Creates a JSON number array with a single element from <c>'a</c>.</summary>
    /// <remarks>Use <c>JNum.single&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>let array = JNum.single id 1</code></example>
    let inline single<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr [ number <| fn x ]

module JBit =

    /// <summary>Creates a JSON bool or null from an <c>option</c>.</summary>
    /// <example><code>let bool = JBit.option id (Some true)</code></example>
    let inline option fn x =
        match x with
        | Some x -> JBit <| fn x
        | None -> JNil

    /// <summary>Creates a JSON bool array from <c>'a seq</c>.</summary>
    /// <example><code>let array = JBit.array id [ true ]</code></example>
    let inline array fn x =
        JArr [ for x in x -> JBit <| fn x ]

    /// <summary>Creates a JSON bool array with a single element from <c>'a</c>.</summary>
    /// <example><code>let array = JBit.single id true</code></example>
    let inline single fn x =
        JArr [ JBit <| fn x ]

module JObj =

    /// <summary>An empty JSON object.</summary>
    /// <example><code>let object = JObj.empty</code></example>
    let empty = JObj []

    /// <summary>Creates a JSON object from <c>'a</c>.</summary>
    /// <example><code>let object = JObj.from (fun x -> [ "prop", JStr x.Prop ]) x</code></example>
    let inline from fn x =
        JObj <| fn x

    /// <summary>Creates a JSON object or null from an <c>option</c>.</summary>
    /// <example><code>let object = JObj.option (fun x -> [ "prop", JStr x.Prop ]) (Some {| Prop = "value" |})</code></example>
    let inline option fn x =
        match x with
        | Some x -> JObj <| fn x
        | None -> JNil

    /// <summary>Creates a JSON object array from <c>'a seq</c>.</summary>
    /// <example><code>let array = JObj.array (fun x -> [ "prop", JStr x.Prop ]) [ {| Prop = "value" |} ]</code></example>
    let inline array fn x =
        JArr [ for x in x -> JObj <| fn x ]

    /// <summary>Creates a JSON object array with a single element from <c>'a</c>.</summary>
    /// <example><code>let array = JObj.single (fun x -> [ "prop", JStr x.Prop ]) {| Prop = "value" |}</code></example>
    let inline single fn x =
        JArr [ JObj <| fn x ]