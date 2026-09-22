namespace Farse

open System
open System.Buffers
open System.Diagnostics.CodeAnalysis
open System.Numerics
open System.Text
open System.Text.Json
open System.Text.Json.Nodes

type Number =
    private | Value of string

    override this.ToString() =
        let (Value value) = this
        value

    static member private From(x) =
        Value <| INumber.format x

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

[<NoComparison>]
type Json =
    /// <summary>A JSON string.</summary>
    | JStr of string
    /// <summary>A JSON number.</summary>
    /// <remarks>Use <c>JNum.number</c> or suppress <c>FS3391</c> as <c>Number</c> uses implicit conversions.</remarks>
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

[<NoComparison>]
type JsonFormat =
    | Indented
    | Custom of JsonSerializerOptions
    | Raw

module Json =

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
    /// <exception cref="System.ArgumentException">Thrown for undefined elements.</exception>
    /// <example><code>let json = Json.fromElement element</code></example>
    let rec fromElement (element:JsonElement) =
        match element.ValueKind with
        | Kind.String -> JStr <| element.GetString()
        | Kind.Number -> JNum <| Value (element.GetRawText())
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
    /// <example><code>let result = Json.fromString json</code></example>
    let fromString ([<StringSyntax("Json")>] json:string) =
        parseDocument (fun () -> JsonDocument.Parse(json, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>Stream</c> asynchronously into a <c>Json</c>.</summary>
    /// <remarks>The <c>Stream</c> is read to completion.</remarks>
    /// <example><code>let! result = Json.fromStreamAsync token stream</code></example>
    let fromStreamAsync token stream =
        parseDocumentAsync (fun () -> JsonDocument.ParseAsync(stream, JsonDocumentOptions.Default, token))

    /// <summary>Parses a UTF-8 encoded <c>byte array</c> into a <c>Json</c>.</summary>
    /// <example><code>let result = Json.fromBytes bytes</code></example>
    let fromBytes (bytes:byte array) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlyMemory&lt;bytes&gt;</c> into a <c>Json</c>.</summary>
    /// <example><code>let result = Json.fromMemory bytes</code></example>
    let fromMemory (bytes:ReadOnlyMemory<byte>) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlySequence&lt;bytes&gt;</c> into a <c>Json</c>.</summary>
    /// <example><code>let result = Json.fromSequence bytes</code></example>
    let fromSequence (bytes:ReadOnlySequence<byte>) =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default))

    /// <summary>Converts a <c>Json</c> to a <c>JsonNode</c>.</summary>
    /// <remarks>
    ///     Returns <c>null</c> for <c>JNil</c>.<br/>
    ///     The last occurrence is chosen when duplicate properties exist.
    /// </remarks>
    /// <example><code>let node = Json.asJsonNode json</code></example>
    let rec asJsonNode json : JsonNode | null =
        match json with
        | JStr str -> JsonValue.Create(str)
        | JNum str -> JsonNode.Parse(str.ToString())
        | JBit bit -> JsonValue.Create(bit)
        | JObj obj ->
            let object = JsonObject()
            // Take last instead of throwing an exception.
            for name, json in obj do object[name] <- asJsonNode json
            object
        | JArr arr ->
            let array = JsonArray()
            for json in arr do array.Add(asJsonNode json)
            array
        | JNil -> null

    /// <summary>Converts a <c>Json</c> to a <c>JsonElement</c>.</summary>
    /// <remarks>The last occurrence is chosen when duplicate properties exist.</remarks>
    /// <example><code>let element = Json.asJsonElement json</code></example>
    let asJsonElement = asJsonNode >> JsonSerializer.SerializeToElement

    /// <summary>Converts a <c>Json</c> to a <c>JsonDocument</c>.</summary>
    /// <remarks>The last occurrence is chosen when duplicate properties exist.</remarks>
    /// <example><code>use document = Json.asJsonDocument json</code></example>
    let asJsonDocument = asJsonNode >> JsonSerializer.SerializeToDocument

    /// <summary>Converts a <c>Json</c> to a formatted JSON string.</summary>
    /// <remarks>The last occurrence is chosen when duplicate properties exist.</remarks>
    /// <example><code>let string = Json.asString Indented json</code></example>
    let asString format json =
        match format, asJsonNode json with
        | _, null -> "null"
        | Indented, node -> node.ToJsonString(JsonSerializerOptions.Default)
        | Custom options, node -> node.ToJsonString(options)
        | Raw, node -> node.ToJsonString()

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
            | JNum str -> writer.WriteRawValue(str.ToString())
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
    /// <remarks>The last occurrence is chosen when duplicate properties exist.</remarks>
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
            | JNum str -> str.ToString()
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

module internal JNil =

    let inline from map fn = function
        | Some x -> (map >> fn) x
        | None -> JNil

module JArr =

    /// <summary>An empty JSON array.</summary>
    /// <example><code>"prop", JArr.empty</code></example>
    let empty = JArr []

    /// <summary>Creates a JSON array from <c>'a seq</c>.</summary>
    /// <example><code>"prop", JArr.from JNum.number [ 1; 2; 3 ] </code></example>
    let inline from fn x =
        x
        |> Seq.map fn
        |> Seq.toList
        |> JArr

module JStr =

    /// <summary>An empty JSON string.</summary>
    /// <example><code>"prop", JStr.empty</code></example>
    let empty = JStr String.Empty

    /// <summary>Creates a JSON string from an <c>INumber</c>.</summary>
    /// <remarks>
    ///     Use <c>JStr.number&lt;int&gt;</c> to be explicit.
    ///     Formats the number with invariant culture and round-trippable precision.
    /// </remarks>
    /// <example><code>"prop", JStr.number 1</code></example>
    let inline number<'a when 'a :> INumber<'a>>(x:'a) =
        JStr <| INumber.format x

    /// <summary>Creates a JSON string or null from an <c>option</c>.</summary>
    /// <example><code>"prop", JStr.option id (Some "string")</code></example>
    let inline option fn x =
        JNil.from fn JStr x

    /// <summary>Creates a JSON string array from <c>'a seq</c>.</summary>
    /// <example><code>"prop", JStr.array id [ "string" ]</code></example>
    let inline array fn x =
        JArr.from (fn >> JStr) x

    /// <summary>Creates a JSON string array with a single element from <c>'a</c>.</summary>
    /// <example><code>"prop", JStr.single id "string"</code></example>
    let inline single fn x =
        JArr.from (fn >> JStr) [ x ]

module JNum =

    /// <summary>A JSON number with the value 0.</summary>
    /// <example><code>"prop", JNum.zero</code></example>
    let zero = JNum <| Value "0"

    /// <summary>Creates a JSON number from an <c>INumber</c>.</summary>
    /// <remarks>
    ///     Use <c>JNum.number&lt;int&gt;</c> to be explicit.
    ///     Formats the number with round-trippable precision.
    /// </remarks>
    /// <example><code>"prop", JNum.number 1</code></example>
    let number<'a when 'a :> INumber<'a>>(x:'a) =
        JNum <| Value (INumber.format x)

    /// <summary>Creates a JSON number or null from an <c>option</c>.</summary>
    /// <remarks>Use <c>JNum.option&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>"prop", JNum.option id (Some 1)</code></example>
    let inline option<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JNil.from fn number x

    /// <summary>Creates a JSON number array from <c>'a seq</c>.</summary>
    /// <remarks>Use <c>JNum.array&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>"prop", JNum.array id [ 1 ]</code></example>
    let inline array<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr.from (fn >> number) x

    /// <summary>Creates a JSON number array with a single element from <c>'a</c>.</summary>
    /// <remarks>Use <c>JNum.single&lt;_, int&gt;</c> to be explicit.</remarks>
    /// <example><code>"prop", JNum.single id 1</code></example>
    let inline single<'a, 'b when 'b :> INumber<'b>> (fn:'a -> 'b) x =
        JArr.from (fn >> number) [ x ]

module JBit =

    /// <summary>Creates a JSON bool or null from an <c>option</c>.</summary>
    /// <example><code>"prop", JBit.option id (Some true)</code></example>
    let inline option fn x =
        JNil.from fn JBit x

    /// <summary>Creates a JSON bool array from <c>'a seq</c>.</summary>
    /// <example><code>"prop", JBit.array id [ true ]</code></example>
    let inline array fn x =
        JArr.from (fn >> JBit) x

    /// <summary>Creates a JSON bool array with a single element from <c>'a</c>.</summary>
    /// <example><code>"prop", JBit.single id true</code></example>
    let inline single fn x =
        JArr.from (fn >> JBit) [ x ]

module JObj =

    /// <summary>An empty JSON object.</summary>
    /// <example><code>"prop", JObj.empty</code></example>
    let empty = JObj []

    /// <summary>Creates a JSON object from <c>'a</c>.</summary>
    /// <example><code>"prop", JObj.from (fun x -> [ "prop", JStr x.Prop ]) x</code></example>
    let inline from fn x =
        JObj <| fn x

    /// <summary>Creates a JSON object or null from an <c>option</c>.</summary>
    /// <example><code>"prop", JObj.option (fun x -> [ "prop", JStr x.Prop ]) (Some {| Prop = "value" |})</code></example>
    let inline option fn x =
        JNil.from fn JObj x

    /// <summary>Creates a JSON object array from <c>'a seq</c>.</summary>
    /// <example><code>"prop", JObj.array (fun x -> [ "prop", JStr x.Prop ]) [ {| Prop = "value" |} ]</code></example>
    let inline array fn x =
        JArr.from (fn >> JObj) x

    /// <summary>Creates a JSON object array with a single element from <c>'a</c>.</summary>
    /// <example><code>"prop", JObj.single (fun x -> [ "prop", JStr x.Prop ]) {| Prop = "value" |}</code></example>
    let inline single fn x =
        JArr.from (fn >> JObj) [ x ]