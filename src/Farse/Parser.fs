namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

type [<Struct>] Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    let private options = JsonDocumentOptions(AllowTrailingCommas = true)

    /// <summary>Returns a parser with the given value.</summary>
    /// <example><code>let! int = Parser.from 1</code></example>
    /// <param name="x">The value to return.</param>
    let inline from x =
        Parser (fun _ -> Ok x)

    /// <summary>Returns a parser with the given result.</summary>
    /// <example><code>let! int = Ok 1 |> Parser.fromResult</code></example>
    /// <param name="x">The result to return.</param>
    let fromResult x : Parser<'r> =
        Parser (fun element ->
            x
            |> Result.mapError (fun msg ->
                element
                |> ParseError.invalid msg typeof<'r>
                |> List.singleton
            )
        )

    /// <summary>Returns a parser that will fail.</summary>
    /// <example><code>do! Parser.fail "message"</code></example>
    /// <param name="msg">The error message to return.</param>
    let inline fail msg =
        Error msg
        |> fromResult

    /// <summary>Recover from an error with the given value.</summary>
    /// <example><code>let! int = "prop" &= Parser.fail "msg" |> Parser.recover 0</code></example>
    /// <param name="x">The value to return.</param>
    let inline recover x (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.defaultValue x
            |> Ok
        )

    /// <summary>Binds a parsed value with the given function.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code></example>
    /// <param name="fn">The binding function.</param>
    let inline bind ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.bind (fun x ->
                let (Parser next) = fn x
                next element
            )
        )

    /// <summary>Maps a parsed value with the given function.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code></example>
    /// <param name="fn">The mapping function.</param>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map fn
        )

    /// <summary>Ignores a parsed value.</summary>
    /// <example><code>do! "prop" &amp;= Parse.int |> Parser.ignore</code></example>
    let inline ignore<'a> (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map ignore<'a>
        )

    /// <summary>Sets a default value for a parsed option.</summary>
    /// <example><code>let! int = "prop" ?= Parse.int |> Parser.defaultValue 0</code></example>
    /// <param name="x">The default value.</param>
    let inline defaultValue x (Parser parse) =
        Parser (fun element ->
            parse element
            |> ResultOption.defaultValue x
        )

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    let parse ([<StringSyntax("Json")>] json:string) (Parser parse) =
        try
            use document = JsonDocument.Parse(json, options)
            parse document.RootElement
            |> Result.mapError Errors
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error <| Json exn

    /// <summary>Parses a JSON stream asynchronously with the given parser.</summary>
    /// <param name="stream">The JSON stream to parse.</param>
    let parseAsync (stream:Stream) (Parser parse) =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream, options)
                return
                    parse document.RootElement
                    |> Result.mapError Errors
            with
                | :? JsonException
                | :? ArgumentNullException as exn -> return Error <| Json exn
        }