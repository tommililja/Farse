namespace Farse

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Text.Json

type Validate =

    static member inline Validate(Parser parse, fn) : Parser<'b option> =
        Parser (fun element ->
            parse element
            |> ResultOption.bind (fun x ->
                match fn x with
                | Ok x -> Ok <| Some x
                | Error msg ->
                    Other.create msg
                    |> Error.list
            )
        )

    static member inline Validate(Parser parse, fn) : Parser<'b> =
        Parser (fun element ->
            parse element
            |> Result.bind (fun x ->
                fn x
                |> Result.bindError (Other.create >> Error.list)
            )
        )

    static member inline Seq(Parser parse, fn, convert) =
        Parser (fun element ->
            parse element
            |> Result.bind (fun (items:#seq<_>) ->
                let mutable error = None
                let mutable enumerator = items.GetEnumerator()

                let array =
                    Seq.length items
                    |> ResizeArray

                while error.IsNone && enumerator.MoveNext() do
                    let item = enumerator.Current
                    match fn item with
                    | Ok x -> array.Add x
                    | Error e -> error <- Some e

                match error with
                | None -> Ok <| convert (array :> seq<_>)
                | Some msg ->
                    Other.create msg
                    |> Error.list
            )
        )

    static member inline Validate(parser:Parser<'a list>, fn) : Parser<'b list> =
        Validate.Seq(parser, fn, List.ofSeq)

    static member inline Validate(parser:Parser<'a array>, fn) : Parser<'b array> =
        Validate.Seq(parser, fn, Array.ofSeq)

    static member inline Validate(parser:Parser<'a Set>, fn) : Parser<'b Set> =
        Validate.Seq(parser, fn, Set.ofSeq)

    static member inline Validate(parser:Parser<'a seq>, fn) : Parser<'b seq> =
        Validate.Seq(parser, fn, id)

and [<Struct>] Parser<'a> = Parser of (JsonElement -> Result<'a, ParserError list>)

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
            match x with
            | Ok x -> Ok x
            | Error msg ->
                let value = JsonElement.getValue element
                InvalidValue.create (Some msg) typeof<'a> value
                |> Error.list
        )

    /// <summary>Binds the parsed value with the given function.</summary>
    /// <code>let! int = "prop" &amp;= Parse.int |> Parser.bind Parser.from</code>
    /// <param name="fn">The binding function.</param>
    /// <typeparam name="parser">The parser to bind.</typeparam>
    let inline bind ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (fun element ->
            match parse element with
            | Ok x ->
                let (Parser next) = fn x
                next element
            | Error e -> Error e
        )

    /// <summary>Maps the parsed value with the given function.</summary>
    /// <code>let! string = "prop" &amp;= Parse.int |> Parser.map string</code>
    /// <param name="fn">The mapping function.</param>
    /// <typeparam name="parser">The parser to map.</typeparam>
    let inline map ([<InlineIfLambda>] fn) (Parser parse) =
        Parser (parse >> Result.map fn)

    /// <summary>Ignores the parsed value.</summary>
    /// <code>do! "prop" &amp;= Parse.int |> Parser.ignore</code>
    /// <typeparam name="parser">The parser to ignore.</typeparam>
    let inline ignore<'a> (Parser parse) =
        Parser (parse >> Result.map ignore<'a>)

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>Works with options and sequences.</remarks>
    /// <code>let! age = "age" ?= Parse.byte |> Parser.validate Age.fromByte</code>
    /// <code>let! email = "email" &amp;= Parse.string |> Parser.validate Email.fromString</code>
    /// <param name="fn">The validation function.</param>
    /// <param name="parser">The parser to validate.</param>
    let inline validate ([<InlineIfLambda>] fn) parser =
        ((^T or Validate) : (static member Validate : ^T * (_ -> Result<_, string>) -> Parser<_>) (parser, fn))

    /// <summary>Parses a JSON string with the given parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <typeparam name="parser">The parser used to parse the JSON string.</typeparam>
    let parse ([<StringSyntax("Json")>] json:string) (Parser parse) =
        try
            use document = JsonDocument.Parse(json)
            parse document.RootElement
            |> Result.mapError ParserErrors.asString
        with
            | :? JsonException
            | :? ArgumentNullException as exn ->
                json
                |> Error.invalidJson exn
                |> Error

    /// <summary>Parses a JSON stream asynchronously with the given parser.</summary>
    /// <param name="stream">The JSON stream to parse.</param>
    /// <typeparam name="parser">The parser used to parse the JSON stream.</typeparam>
    let parseAsync (stream:Stream) (Parser parse) =
        task {
            try
                use! document = JsonDocument.ParseAsync(stream)
                return
                    parse document.RootElement
                    |> Result.mapError ParserErrors.asString
            with
                | :? JsonException
                | :? ArgumentNullException as exn ->
                    return
                        exn
                        |> Error.invalidStream
                        |> Error
        }