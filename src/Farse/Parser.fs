namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

type Validate =

    static member inline Validate(parser, fn) : Parser<'b option> =
        fun element ->
            parser element
            |> ResultOption.bind (fun x ->
                match fn x with
                | Ok x -> Ok <| Some x
                | Error msg ->
                    let value = Some $"%A{x}"
                    InvalidValue.create (Some msg) typeof<'b> value
            )

    static member inline Validate(parser, fn) : Parser<'b> =
        fun element ->
            parser element
            |> Result.bind (fun x ->
                fn x
                |> Result.bindError (fun msg ->
                    let value = Some $"%A{x}"
                    InvalidValue.create (Some msg) typeof<'b> value
                )
            )

and Parser<'a> = JsonElement -> Result<'a, ParserError>

module Parser =

    /// <summary>Returns a parser with the given value.</summary>
    /// <code>let! int = Parser.from 1</code>
    /// <param name="x">The value to return.</param>
    let inline from x : Parser<_> =
        fun _ -> Ok x
        |> id

    /// <summary>Returns a parser with the given result.</summary>
    /// <code>let! int = Ok 1 |> Parser.fromResult</code>
    /// <param name="x">The result to return.</param>
    let inline fromResult x : Parser<'a> =
        fun element ->
            Result.bindError (fun msg ->
                let value = JsonElement.getValue element
                InvalidValue.create (Some msg) typeof<'a> value
            ) x
        |> id

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code>
    /// <param name="fn">The binding function.</param>
    /// <param name="parser">The parser to bind.</param>
    let inline bind ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        fun element ->
            result {
                let! x = parser element
                return! element |> fn x
            }
        |> id

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code>
    /// <param name="fn">The mapping function.</param>
    /// <param name="parser">The parser to map.</param>
    let inline map ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        parser >> Result.map fn

    /// <summary>Ignores the parsed value.</summary>
    /// <code>do! "prop" &amp;= Parse.int |> Parser.ignore</code>
    /// <param name="parser">The parser to ignore.</param>
    let inline ignore<'a> (parser:Parser<'a>) : Parser<_> =
        parser >> Result.map ignore<'a>

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
    /// <param name="parser">The parser used to parse the JSON string.</param>
    let parse ([<StringSyntax("Json")>] json:string) (parser:Parser<_>) =
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
    /// <param name="parser">The parser used to parse the JSON stream.</param>
    let parseAsync (stream:Stream) (parser:Parser<_>) =
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