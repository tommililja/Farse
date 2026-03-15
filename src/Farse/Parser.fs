namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

type [<Struct>] Parser<'r> = Parser of (JsonElement -> Result<'r, ParseError list>)

module Parser =

    let private options = JsonDocumentOptions(AllowTrailingCommas = true)

    /// <summary>Returns a parser with the given value.</summary>
    /// <code>let! int = Parser.from 1</code>
    /// <param name="x">The value to return.</param>
    let inline from x =
        Parser (fun _ -> Ok x)

    /// <summary>Returns a parser with the given result.</summary>
    /// <code>let! int = Ok 1 |> Parser.fromResult</code>
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
    /// <code>do! Parser.fail "message"</code>
    /// <param name="msg">The error message to return.</param>
    let inline fail msg =
        Error msg
        |> fromResult

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code>
    /// <param name="fn">The binding function.</param>
    /// <typeparam name="parser">The parser to bind.</typeparam>
    let inline bind ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.bind (fun x ->
                let (Parser next) = fn x
                next element
            )
        )

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code>
    /// <param name="fn">The mapping function.</param>
    /// <typeparam name="parser">The parser to map.</typeparam>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map fn
        )

    /// <summary>Ignores the parsed value.</summary>
    /// <code>do! "prop" &amp;= Parse.int |> Parser.ignore</code>
    /// <typeparam name="parser">The parser to ignore.</typeparam>
    let inline ignore<'a> (Parser parse) =
        Parser (fun element ->
            parse element
            |> Result.map ignore<'a>
        )

    /// <summary>Sets a default value for the parsed optional value.</summary>
    /// <code>let! int = "prop" ?= Parse.int |> Parser.defaultValue 0</code>
    /// <typeparam name="parser">The parser used to parse the property value.</typeparam>
    let inline defaultValue x (Parser parse) =
        Parser (fun element ->
            parse element
            |> ResultOption.defaultValue x
        )

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <typeparam name="parser">The parser used to parse the JSON string.</typeparam>
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
    /// <typeparam name="parser">The parser used to parse the JSON stream.</typeparam>
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