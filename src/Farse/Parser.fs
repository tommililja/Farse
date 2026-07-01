namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

[<Struct; NoComparison; NoEquality>]
type Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    /// <summary>Runs the parser against an element.</summary>
    /// <example><code>let result = Parser.run element parser</code></example>
    /// <param name="element">The element to parse.</param>
    let inline run element (Parser parse) =
        parse element

    /// <summary>Creates a parser from a value.</summary>
    /// <remarks>This parser will always succeed.</remarks>
    /// <example><code>let! int = Parser.from 1</code></example>
    /// <param name="x">The value to return.</param>
    let inline from x =
        Parser (fun _ -> Ok x)

    /// <summary>Creates a parser from a result.</summary>
    /// <example><code>let! int = Ok 1 |> Parser.fromResult</code></example>
    /// <param name="x">The result to return.</param>
    let inline fromResult x : Parser<'r> =
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
    /// <param name="msg">The error message to return.</param>
    let inline fail msg =
        Error msg
        |> fromResult

    /// <summary>Recovers from an error with a default value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parser.fail "msg" |> Parser.recover 0</code></example>
    /// <param name="x">The default value to return.</param>
    let inline recover x (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> Ok x
            | Error _ -> Ok x
        )

    /// <summary>Binds a parsed value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code></example>
    /// <param name="fn">The binding function.</param>
    let inline bind ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> fn x |> run element
            | Error e -> Error e
        )

    /// <summary>Maps a parsed value.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code></example>
    /// <param name="fn">The mapping function.</param>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x -> Ok <| fn x
            | Error e -> Error e
        )

    /// <summary>Ignores a parsed value.</summary>
    /// <example><code>do! "prop" &amp;= Parse.int |> Parser.ignore</code></example>
    let inline ignore<'r> (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (_:'r) -> Ok ()
            | Error e -> Error e
        )

    /// <summary>Returns the parsed value or a default value.</summary>
    /// <example><code>let! int = "prop" ?= Parse.int |> Parser.defaultValue 0</code></example>
    /// <param name="x">The default value to return.</param>
    let inline defaultValue x (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (Some x) -> Ok x
            | Ok None -> Ok x
            | Error e -> Error e
        )

    /// <summary>Returns the parsed value or a default value from a function.</summary>
    /// <example><code>let! int = "prop" ?= Parse.int |> Parser.defaultWith (fun () -> 0)</code></example>
    /// <param name="fn">The function to run.</param>
    let inline defaultWith fn (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok (Some x) -> Ok x
            | Ok None -> Ok <| fn ()
            | Error e -> Error e
        )

    /// <summary>Parses a JSON string.</summary>
    /// <example><code>let result = Parser.parse json parser</code></example>
    /// <param name="json">The JSON string to parse.</param>
    let parse ([<StringSyntax("Json")>] json:string) (Parser parse) =
        try
            use document = JsonDocument.Parse(json, JsonDocumentOptions.preset)
            parse document.RootElement
            |> Result.mapError Errors
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error <| Json exn

    /// <summary>Parses a JSON stream asynchronously.</summary>
    /// <example><code>let! result = Parser.parseAsync stream ct parser</code></example>
    /// <param name="stream">The JSON stream to parse.</param>
    /// <param name="token">The CancellationToken to monitor.</param>
    let parseAsync stream token (Parser parse) =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream, JsonDocumentOptions.preset, token)
                return
                    parse document.RootElement
                    |> Result.mapError Errors
            with
                | :? JsonException
                | :? ArgumentNullException as exn -> return Error <| Json exn
        }