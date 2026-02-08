namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

type Validate =

    static member inline Validate(Parser parser, fn) : Parser<'b option> =
        Parser (fun element ->
            parser element
            |> ResultOption.bind (fun x ->
                match fn x with
                | Ok x -> Ok <| Some x
                | Error msg ->
                    let value = Some $"%A{x}"
                    InvalidValue.create (Some msg) typeof<'b> value
            )
        )

    static member inline Validate(Parser parser, fn) : Parser<'b> =
        Parser (fun element ->
            parser element
            |> Result.bind (fun x ->
                fn x
                |> Result.bindError (fun msg ->
                    let value = Some $"%A{x}"
                    InvalidValue.create (Some msg) typeof<'b> value
                )
            )
        )

and [<Struct>] Parser<'a> = Parser of (JsonElement -> Result<'a, ParserError>)

module Parser =

    /// <summary>Returns a parser with the given value.</summary>
    /// <code>let! int = Parser.from 1</code>
    /// <param name="x">The value to return.</param>
    let inline from x =
        Parser (fun _ -> Ok x)

    /// <summary>Returns a parser with the given result.</summary>
    /// <code>let! int = Ok 1 |> Parser.fromResult</code>
    /// <param name="x">The result to return.</param>
    let inline fromResult x : Parser<'a> =
        Parser (fun element ->
            Result.bindError (fun msg ->
                let value = JsonElement.getValue element
                InvalidValue.create (Some msg) typeof<'a> value
            ) x
        )

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code>
    /// <param name="fn">The binding function.</param>
    /// <typeparam name="parser">The parser to bind.</typeparam>
    let inline bind ([<InlineIfLambda>] fn) (Parser parser) =
        Parser (fun element ->
            result {
                let! x = parser element
                let (Parser next) = fn x
                return! next element
            }
        )

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code>
    /// <param name="fn">The mapping function.</param>
    /// <typeparam name="parser">The parser to map.</typeparam>
    let inline map ([<InlineIfLambda>] fn) (Parser parser) =
        Parser (parser >> Result.map fn)

    /// <summary>Ignores the parsed value.</summary>
    /// <code>do! "prop" &amp;= Parse.int |> Parser.ignore</code>
    /// <typeparam name="parser">The parser to ignore.</typeparam>
    let inline ignore<'a> (Parser parser) =
        Parser (parser >> Result.map ignore<'a>)

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>
    ///     <para>Produces detailed error messages when validation fails.</para>
    ///     <para>Works with both required and optional values.</para>
    /// </remarks>
    /// <code>let! age = "age" ?= Parse.byte |> Parser.validate Age.fromByte</code>
    /// <code>let! email = "email" &amp;= Parse.string |> Parser.validate Email.fromString</code>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let inline validate ([<InlineIfLambda>] fn) parser =
        ((^T or Validate) : (static member Validate : ^T * (_ -> Result<_, string>) -> Parser<_>) (parser, fn))

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <typeparam name="parser">The parser used to parse the JSON string.</typeparam>
    let parse ([<StringSyntax("Json")>] json:string) (Parser parser) =
        try
            use document = JsonDocument.Parse(json)
            parser document.RootElement
            |> Result.mapError ParserError.asString
        with
            | :? JsonException
            | :? ArgumentNullException as exn ->
                Error.invalidJson exn json

    /// <summary>Parses a JSON stream asynchronously with the given parser.</summary>
    /// <param name="stream">The JSON stream to parse.</param>
    /// <typeparam name="parser">The parser used to parse the JSON stream.</typeparam>
    let parseAsync (stream:Stream) (Parser parser) =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream)
                return
                    parser document.RootElement
                    |> Result.mapError ParserError.asString
            with
                | :? JsonException
                | :? ArgumentNullException as exn ->
                    return Error.invalidStream exn
        }