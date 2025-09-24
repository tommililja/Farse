namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

type Parser<'a> = JsonElement -> Result<'a, ParserError>

module Parser =

    /// <summary>Returns a parser with the given value.</summary>
    /// <param name="x">The value to return.</param>
    let inline from x : Parser<_> =
        fun _ -> Ok x
        |> id

    /// <summary>Returns a parser with the given result.</summary>
    /// <param name="x">The result to return.</param>
    let inline fromResult x : Parser<_> =
        fun _ -> Result.mapError Other x
        |> id

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <param name="fn">The binder function.</param>
    /// <param name="parser">The parser to bind.</param>
    let inline bind ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> element |> fn x
            | Error e -> Error e
        |> id

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="parser">The parser to map.</param>
    let inline map ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> Ok <| fn x
            | Error e -> Error e
        |> id

    /// <summary>Ignores the parsed value.</summary>
    /// <param name="parser">The parser whose value to ignore.</param>
    let inline ignore (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok _ -> Ok ()
            | Error e -> Error e
        |> id

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>
    ///     Adds the error string as details to the parser error message.
    ///     <example>
    ///         Do
    ///         <code>
    ///             let! plan = "plan" &amp;= (string |> Parser.validate Plan.fromString)
    ///             let! plan = "plan" &amp;= Parser.validate Plan.fromString string
    ///         </code>
    ///     </example>
    ///     <example>
    ///         Don't
    ///         <code>
    ///             let! plan = "plan" &amp;= string |> Parser.validate Plan.fromString
    ///         </code>
    ///     </example>
    /// </remarks>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let inline validate ([<InlineIfLambda>] fn) (parser:Parser<_>) : Parser<'b> =
        fun element ->
            match parser element with
            | Ok x ->
                match fn x with
                | Ok x -> Ok x
                | Error msg ->
                    InvalidValue (Some msg, typeof<'b>, element)
                    |> Error
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
                json
                |> Error.invalidJson exn
                |> Error