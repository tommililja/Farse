namespace Farse

open System
open System.Buffers
open System.Diagnostics.CodeAnalysis
open System.Text.Json

[<Struct; NoComparison; NoEquality>]
type Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    /// <summary>Runs a <c>Parser</c> against a <c>JsonElement</c>.</summary>
    /// <example><code>let result = Parser.run element parser</code></example>
    let inline run element (Parser parse) =
        parse element

    /// <summary>Creates a <c>Parser</c> from a value.</summary>
    /// <remarks>This parser will always succeed.</remarks>
    /// <example><code>let! int = Parser.from 1</code></example>
    let from x = Parser (fun _ -> Ok x)

    /// <summary>Creates a <c>Parser</c> from a <c>Result&lt;'r, string&gt;</c>.</summary>
    /// <example><code>let! int = Ok 1 |> Parser.fromResult</code></example>
    let fromResult x : Parser<'r> =
        Parser (fun element ->
            match x with
            | Ok x -> Ok x
            | Error msg ->
                element
                |> ParseError.details msg typeof<'r>
                |> Error.list
        )

    /// <summary>Creates a <c>Parser</c> that will fail.</summary>
    /// <example><code>do! Parser.fail "message"</code></example>
    let fail msg =
        Error msg
        |> fromResult

    /// <summary>Recovers from an error with a default value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parser.fail "msg" |> Parser.recover 0</code></example>
    let recover x (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> Ok x
            | Error _ -> Ok x
        )

    /// <summary>Binds a parsed value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code></example>
    let inline bind ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> fn x |> run element
            | Error e -> Error e
        )

    /// <summary>Maps a parsed value.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code></example>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> Ok <| fn x
            | Error e -> Error e
        )

    /// <summary>Ignores a parsed value.</summary>
    /// <remarks>Use <c>Parser.ignore&lt;int&gt;</c> to be explicit.</remarks>
    /// <example><code>do! "prop" &amp;= Parse.int |> Parser.ignore</code></example>
    let ignore<'r> (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (_:'r) -> Ok ()
            | Error e -> Error e
        )

    /// <summary>Returns the parsed value or a default value.</summary>
    /// <example><code>let! int = "prop" ?= Parse.int |> Parser.defaultValue 0</code></example>
    let defaultValue x (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (Some x) -> Ok x
            | Ok None -> Ok x
            | Error e -> Error e
        )

    /// <summary>Returns the parsed value or a default value from a function.</summary>
    /// <example><code>let! int = "prop" ?= Parse.int |> Parser.defaultWith (fun () -> 0)</code></example>
    let inline defaultWith ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (Some x) -> Ok x
            | Ok None -> Ok <| fn ()
            | Error e -> Error e
        )

    // Parsing

    let private parseDocument fn (Parser parse) =
        try use document: JsonDocument = fn ()
            parse document.RootElement
            |> Result.mapError Errors
        with
            | :? JsonException
            | :? ArgumentException
            | :? ArgumentNullException as exn -> Error <| Json exn

    let private parseDocumentAsync fn (Parser parse) =
        task {
            try use! document: JsonDocument = fn ()
                return
                    parse document.RootElement
                    |> Result.mapError Errors
            with
                | :? JsonException
                | :? ArgumentException
                | :? ArgumentNullException as exn -> return Error <| Json exn
        }

    /// <summary>Parses a <c>string</c> with a <c>Parser</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Parser.parse json parser</code></example>
    let parse ([<StringSyntax("Json")>] json:string) parser =
        parseDocument (fun () -> JsonDocument.Parse(json, JsonDocumentOptions.Default)) parser

    /// <summary>Parses a <c>string</c> with a <c>Parser</c> and <c>JsonDocumentOptions</c>.</summary>
    /// <example><code>let result = Parser.parseWith json options parser</code></example>
    let parseWith ([<StringSyntax("Json")>] json:string) options parser =
        parseDocument (fun () -> JsonDocument.Parse(json, options)) parser

    /// <summary>Parses a UTF-8 encoded <c>Stream</c> asynchronously with a <c>Parser</c>.</summary>
    /// <remarks>The <c>Stream</c> is read to completion.<br/><br/>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let! result = Parser.parseAsync stream token parser</code></example>
    let parseAsync stream token parser =
        parseDocumentAsync (fun () -> JsonDocument.ParseAsync(stream, JsonDocumentOptions.Default, token)) parser

    /// <summary>Parses a UTF-8 encoded <c>Stream</c> asynchronously with a <c>Parser</c> and <c>JsonDocumentOptions</c>.</summary>
    /// <remarks>The <c>Stream</c> is read to completion.</remarks>
    /// <example><code>let! result = Parser.parseWithAsync stream options token parser</code></example>
    let parseWithAsync stream options token parser =
        parseDocumentAsync (fun () -> JsonDocument.ParseAsync(stream, options, token)) parser

    /// <summary>Parses a UTF-8 encoded <c>byte array</c> with a <c>Parser</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Parser.parseBytes bytes parser</code></example>
    let parseBytes (bytes:byte array) parser =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default)) parser

    /// <summary>Parses a UTF-8 encoded <c>byte array</c> with a <c>Parser</c> and <c>JsonDocumentOptions</c>.</summary>
    /// <example><code>let result = Parser.parseBytesWith bytes options parser</code></example>
    let parseBytesWith (bytes:byte array) options parser =
        parseDocument (fun () -> JsonDocument.Parse(bytes, options)) parser

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlyMemory&lt;bytes&gt;</c> with a <c>Parser</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Parser.parseMemory bytes parser</code></example>
    let parseMemory (bytes:ReadOnlyMemory<byte>) parser =
        parseDocument (fun () -> JsonDocument.Parse(bytes, JsonDocumentOptions.Default)) parser

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlyMemory&lt;bytes&gt;</c> with a <c>Parser</c> and <c>JsonDocumentOptions</c>.</summary>
    /// <example><code>let result = Parser.parseMemoryWith bytes options parser</code></example>
    let parseMemoryWith (bytes:ReadOnlyMemory<byte>) options parser =
        parseDocument (fun () -> JsonDocument.Parse(bytes, options)) parser

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlySequence&lt;bytes&gt;</c> with a <c>Parser</c>.</summary>
    /// <remarks>Uses the following <c>JsonDocumentOptions</c>.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Parser.parseSequence sequence parser</code></example>
    let parseSequence (sequence:ReadOnlySequence<byte>) parser =
        parseDocument (fun () -> JsonDocument.Parse(sequence, JsonDocumentOptions.Default)) parser

    /// <summary>Parses a UTF-8 encoded <c>ReadOnlySequence&lt;byte&gt;</c> with a <c>Parser</c> and <c>JsonDocumentOptions</c>.</summary>
    /// <example><code>let result = Parser.parseSequenceWith sequence options parser</code></example>
    let parseSequenceWith (sequence:ReadOnlySequence<byte>) options parser =
        parseDocument (fun () -> JsonDocument.Parse(sequence, options)) parser