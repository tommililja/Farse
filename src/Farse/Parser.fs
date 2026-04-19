namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

[<Struct; NoComparison; NoEquality>]
type Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    /// <summary>Returns a parser with the given value.</summary>
    /// <example>let! int = Parser.from 1</example>
    /// <param name="x">The value to return.</param>
    let inline from x =
        Parser (fun _ -> Ok x)

    /// <summary>Returns a parser with the given result.</summary>
    /// <example>let! int = Ok 1 |> Parser.fromResult</example>
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
    /// <example>do! Parser.fail "message"</example>
    /// <param name="msg">The error message to return.</param>
    let inline fail msg =
        Error msg
        |> fromResult

    /// <summary>Recover from an error with the given value.</summary>
    /// <example>let! int = "prop" &amp;= Parser.fail "msg" |> Parser.recover 0</example>
    /// <param name="x">The value to return.</param>
    let inline recover x (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.defaultValue x
            |> Ok
        )

    /// <summary>Binds a parsed value with the given function.</summary>
    /// <example>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</example>
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
    /// <example>let! string = "prop" &amp;= Parse.int |> Parser.map string</example>
    /// <param name="fn">The mapping function.</param>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map fn
        )

    /// <summary>Filters the parsed sequence with the given predicate.</summary>
    /// <example>let! positive = "prop" &amp;= Parse.list Parse.int |> Parser.filter (fun x -> x > 0)</example>
    /// <param name="fn">The predicate.</param>
    let inline filter ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map (Seq.filter fn)
        )

    /// <summary>Ignores a parsed value.</summary>
    /// <example>do! "prop" &amp;= Parse.int |> Parser.ignore</example>
    let inline ignore<'a> (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map ignore<'a>
        )

    /// <summary>Sets a default value for a parsed option.</summary>
    /// <example>let! int = "prop" ?= Parse.int |> Parser.defaultValue 0</example>
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
            use document = JsonDocument.Parse(json, JsonDocumentOptions.preset)
            parse document.RootElement
            |> Result.mapError Errors
        with
            | :? JsonException
            | :? ArgumentNullException as exn -> Error <| Json exn

    /// <summary>Parses a JSON stream asynchronously with the given parser.</summary>
    /// <param name="stream">The JSON stream to parse.</param>
    /// <param name="token">The CancellationToken to use.</param>
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