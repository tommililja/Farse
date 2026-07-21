namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

[<Struct; NoComparison; NoEquality>]
type Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    /// <summary>Runs the parser against an element.</summary>
    /// <example><code>let result = Parser.run element parser</code></example>
    let inline run element (Parser parse) =
        parse element

    /// <summary>Creates a parser from a value.</summary>
    /// <remarks>This parser will always succeed.</remarks>
    /// <example><code>let! int = Parser.from 1</code></example>
    let from x = Parser (fun _ -> Ok x)

    /// <summary>Creates a parser from a result.</summary>
    /// <example><code>let! int = Ok 1 |> Parser.fromResult</code></example>
    let fromResult x : Parser<'r> =
        Parser (fun element ->
            match x with
            | Ok x -> Ok x
            | Error msg ->
                element
                |> ParseError.invalid msg typeof<'r>
                |> Error.list
        )

    /// <summary>Creates a parser that will fail.</summary>
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

    /// <summary>Parses a JSON string with options.</summary>
    /// <example><code>let result = Parser.parseWith json options parser</code></example>
    let parseWith ([<StringSyntax("Json")>] json:string) options (Parser parse) =
        try
            use document = JsonDocument.Parse(json, options)
            parse document.RootElement
            |> Result.mapError Errors
        with
            | :? JsonException
            | :? ArgumentException
            | :? ArgumentNullException as exn -> Error <| Json exn

    /// <summary>Parses a JSON stream asynchronously with options.</summary>
    /// <example><code>let! result = Parser.parseWithAsync stream options ct parser</code></example>
    let parseWithAsync stream options token (Parser parse) =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream, options, token)
                return
                    parse document.RootElement
                    |> Result.mapError Errors
            with
                | :? JsonException
                | :? ArgumentException
                | :? ArgumentNullException as exn -> return Error <| Json exn
        }

    /// <summary>Parses a JSON string.</summary>
    /// <remarks>Uses the following JsonDocumentOptions.
    /// <code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code>
    /// </remarks>
    /// <example><code>let result = Parser.parse json parser</code></example>
    let parse ([<StringSyntax("Json")>] json:string) parser =
        parseWith json JsonDocumentOptions.preset parser

    /// <summary>Parses a JSON stream asynchronously.</summary>
    /// <remarks>Uses the following JsonDocumentOptions.<code>
    ///     JsonDocumentOptions (
    ///         AllowTrailingCommas = true,
    ///         CommentHandling = JsonCommentHandling.Skip
    ///     )
    /// </code></remarks>
    /// <example><code>let! result = Parser.parseAsync stream ct parser</code></example>
    let parseAsync stream token parser =
        parseWithAsync stream JsonDocumentOptions.preset token parser