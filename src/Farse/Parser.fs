namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.Text.Json

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
    /// <code>do! "prop" &amp;= Parse.string |> Parser.ignore</code>
    /// <param name="parser">The parser to ignore.</param>
    let inline ignore (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok _ -> Ok ()
            | Error e -> Error e
        |> id

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Parser.req "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.getProperty name element
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich name element
                        |> Error
                | _ ->
                   CouldNotRead (name, element)
                   |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok element
                let mutable name = path[0]
                let mutable object = element

                for segment in path do
                    prop <- prop
                    |> Result.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            name <- segment
                            object <- element

                            element
                            |> JsonElement.getProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok prop ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich name object
                        |> Error
                | Error element ->
                    NotObject (name, object, element)
                    |> Error

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <code>let! int = Parser.opt "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.tryGetProperty name element
                    match prop with
                    | Some prop ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error error ->
                            error
                            |> ParserError.enrich name element
                            |> Error
                    | None -> Ok None
                | _ ->
                    CouldNotRead (name, element)
                    |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok <| Some element
                let mutable name = path[0]
                let mutable object = element

                for segment in path do
                    prop <- prop
                    |> ResultOption.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            name <- segment
                            object <- element

                            element
                            |> JsonElement.tryGetProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok (Some prop) ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error ->
                        error
                        |> ParserError.enrich name object
                        |> Error
                | Ok None -> Ok None
                | Error element ->
                    NotObject (name, object, element)
                    |> Error

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