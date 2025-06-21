namespace Farse

open System
open System.Globalization
open System.Text.Json

module Parse =

    /// <summary>Parses a required property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let req (path:string) (parser:Parser<_>) : Parser<_> =
        if not <| path.Contains(".") then
            fun (element:JsonElement) ->
                try
                    match element.TryGetProperty(path) with
                    | true, prop when prop.ValueKind <> JsonValueKind.Null ->
                        match parser prop with
                        | Ok x -> Ok x
                        | Error msg -> Error.parseError path None msg element
                    | false, _ -> Error.missingProperty path None element
                    | _ -> Error.nullProperty path None element
                with
                    | ArrayException msg -> Error.parseError path None msg element
                    | :? InvalidOperationException -> Error.notObject path None element
        else
            Parser.traverse path parser

    /// <summary>Parses an optional property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let opt (path:string) (parser:Parser<_>) : Parser<_> =
        if not <| path.Contains(".") then
            fun (element:JsonElement) ->
                try
                    match element.TryGetProperty(path) with
                    | true, prop when prop.ValueKind <> JsonValueKind.Null ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error msg -> Error.parseError path None msg element
                    | _ -> Ok None
                with
                    | ArrayException msg -> Error.parseError path None msg element
                    | :? InvalidOperationException -> Error.notObject path None element
        else
            Parser.tryTraverse path parser

    let private enumerable convert (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Array ->
                element.EnumerateArray()
                |> Seq.map (parser >> Result.defaultWith (fun e -> raise <| ArrayException e))
                |> convert
                |> Ok
            | _ ->
                element.ValueKind
                |> Error.invalidElement JsonValueKind.Array
                |> Error

    let private getValue (parse:JsonElement -> 'a) : Parser<_> =
        fun element ->
            try Ok <| parse element with
            | :? FormatException
            | :? InvalidOperationException ->
                typeof<'a>
                |> Error.couldNotParse element
                |> Error

    /// Parses an element as System.Int32.
    let int = getValue _.GetInt32()

    /// Parses an element as System.Double.
    let float = getValue _.GetDouble()

    /// Parses an element as System.Decimal.
    let decimal = getValue _.GetDecimal()

    /// Parses an element as System.String.
    let string = getValue _.GetString()

    /// Parses an element as System.Boolean.
    let bool = getValue _.GetBoolean()

    /// Parses an element as System.Guid.
    let guid = getValue _.GetGuid()

    /// Parses an element as System.DateTime (ISO 8601).
    let dateTime = getValue _.GetDateTime()

    /// Parses an element as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue (_.GetDateTime() >> _.ToUniversalTime())

    /// Parses an element as System.DateTime with a specific format.
    let dateTimeExact (format:string) =
        fun (element:JsonElement) ->
            try
                let str = element.GetString()
                Ok <| DateTime.ParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None)
            with
                | :? ArgumentNullException
                | :? FormatException
                | :? InvalidOperationException ->
                    format
                    |> Error.couldNotParseDateTime element
                    |> Error

    /// Parses an element as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue _.GetDateTimeOffset()

    /// <summary>Parses an element as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element in the list.</param>
    let list (parser:Parser<_>) : Parser<_> =
        enumerable Seq.toList parser

    /// <summary>Parses an element as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element in the array.</param>
    let array (parser:Parser<_>) : Parser<_> =
        enumerable Seq.toArray parser