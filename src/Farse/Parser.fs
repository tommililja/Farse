namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

// Ignore type alias fix (|> id) warning.
// ReSharper disable FSharpRedundantApplication

type Validate =

    static member inline Validate(parser:Parser<_ option>, fn) =
        fun element ->
            match parser element with
            | Ok (Some x) ->
                match fn x with
                | Ok x -> Ok <| Some x
                | Error msg -> Error <| Other msg
            | Ok None -> Ok None
            | Error e -> Error e

    static member inline Validate(parser:Parser<_>, fn) =
        fun element ->
            match parser element with
            | Ok x ->
                match fn x with
                | Ok x -> Ok x
                | Error msg -> Error <| Other msg
            | Error e -> Error e

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
    let inline fromResult x : Parser<_> =
        fun _ -> Result.mapError Other x
        |> id

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code>
    /// <param name="fn">The binding function.</param>
    /// <param name="parser">The parser to bind.</param>
    let inline bind ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> element |> fn x
            | Error e -> Error e
        |> id

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code>
    /// <param name="fn">The mapping function.</param>
    /// <param name="parser">The parser to map.</param>
    let inline map ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> Ok <| fn x
            | Error e -> Error e
        |> id

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>Works with both required and optional values.</remarks>
    /// <code>let! email = "prop" &amp;= Parse.string |> Parser.validate Email.fromString</code>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let inline validate ([<InlineIfLambda>] fn) parser =
        ((^T or Validate) : (static member Validate : ^T * (_ -> Result<_, string>) -> Parser<_>) (parser, fn))

    /// <summary>Ignores the parsed value.</summary>
    /// <code>do! "prop" &amp;= Parse.int |> Parser.ignore</code>
    /// <param name="parser">The parser to ignore.</param>
    let inline ignore (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok _ -> Ok ()
            | Error e -> Error e
        |> id

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
                Some json
                |> Error.invalidJson exn
                |> Error

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
                    return
                        None
                        |> Error.invalidJson exn
                        |> Error
        }