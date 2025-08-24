namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

// The type alias does not work in some function signatures.
// Fixing it increases memory allocations, so leaving it for now.
type Parser<'a> = JsonElement -> Result<'a, string>

module Parser =

    let inline internal bindImpl fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> element |> fn x
            | Error e -> Error e

    let inline internal mapImpl fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> Ok <| fn x
            | Error e -> Error e

    let inline internal ignoreImpl (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok _ -> Ok ()
            | Error e -> Error e

    let inline internal validateImpl fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> fn x
            | Error e -> Error e

    /// <summary>Returns a parser with the given value.</summary>
    /// <param name="x">The value to return.</param>
    let from x : Parser<_> =
        fun _ -> Ok x

    /// <summary>Returns a parser with the given result.</summary>
    /// <param name="x">The result to return.</param>
    let fromResult x : Parser<_> =
        fun _ -> x

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <param name="fn">The binder function.</param>
    /// <param name="parser">The parser to bind.</param>
    let bind fn parser =
        bindImpl fn parser

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <param name="fn">The mapping function.</param>
    /// <param name="parser">The parser to map.</param>
    let map fn parser =
        mapImpl fn parser

    /// <summary>Ignores the parsed value.</summary>
    /// <param name="parser">The parser whose value to ignore.</param>
    let ignore parser =
        ignoreImpl parser

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let validate fn parser =
        validateImpl fn parser

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <param name="parser">The parser used to parse the JSON string.</param>
    let parse
        #if NET7_0_OR_GREATER
        ([<StringSyntax("Json")>] json:string) (parser:Parser<_>) =
        #else
        (json:string) (parser:Parser<_>) =
        #endif
        try
            match json with
            | Invalid -> Error.invalidString ()
            | String json ->
                use document = JsonDocument.Parse(json)
                parser document.RootElement
        with :? JsonException as exn -> Error.invalidJson json exn