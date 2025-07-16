namespace Farse

open System.Text.Json

type Parser<'a> = JsonElement -> Result<'a, string>

module Parser =

    let from x : Parser<_> =
        fun _ -> Ok x

    let bind (fn:_ -> Parser<_>) (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> element |> fn x
            | Error e -> Error e

    let validate fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> fn x
            | Error e -> Error e

    let map fn (x:Parser<_>) : Parser<_> =
        bind (fn >> from) x

    let ignore (x:Parser<_>) : Parser<_> =
        map ignore x

    /// <summary>Parses a JSON string using the supplied parser.</summary>
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