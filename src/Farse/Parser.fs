namespace Farse

#nowarn "3390"

open System.Text.Json

module Parser =

    /// <summary>Returns a parser with the given value.</summary>
    /// <param name="x">The value to return.</param>
    let from x : Parser<_> =
        fun _ -> Ok x

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <param name="fn">The binder function.</param>
    /// <param name="parser">The parser to bind.</param>
    let bind (fn:_ -> Parser<_>) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> element |> fn x
            | Error e -> Error e

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="parser">The parser to map.</param>
    let map fn (parser:Parser<_>) : Parser<_> =
        bind (fn >> from) parser

    /// <summary>Ignores the parsed value.</summary>
    /// <param name="parser">The parser whose value to ignore.</param>
    let ignore (parser:Parser<_>) : Parser<_> =
        map ignore parser

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let validate fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> fn x
            | Error e -> Error e

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <param name="parser">The parser used to parse the JSON string.</param>
    let parse (json:string) (parser:Parser<_>) =
        try
            match json with
            | Invalid -> Error.invalidString ()
            | String json ->
                use document = JsonDocument.Parse(json)
                parser document.RootElement
        with :? JsonException as exn -> Error.invalidJson json exn