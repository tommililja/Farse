namespace Farse

open System
open System.Collections.Generic
open System.Globalization
open System.Text.Json

module Parse =

    /// <summary>Parses a required property with the supplied parser.</summary>
    /// <param name="name">The name of the property.</param>
    /// <param name="parser">The parser used for the property value. For example, Parse.int.</param>
    /// <param name="element">The element to parse from. Must be an object.</param>
    let req (name:string) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            try
                let prop = element.GetProperty(name)
                match prop.ValueKind with
                | JsonValueKind.Null -> Error.nullProperty name element
                | _ ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error msg -> Error.parseError name msg element
            with
                | ArrayException msg -> Error.parseError name msg element
                | :? KeyNotFoundException -> Error.nullProperty name element
                | :? InvalidOperationException -> Error.notObject element

    /// <summary>Parses an optional property with the supplied parser.</summary>
    /// <param name="name">The name of the property.</param>
    /// <param name="parser">The parser used for the property value. For example, Parse.int.</param>
    /// <param name="element">The element to parse from. Must be an object.</param>
    let opt (name:string) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            try
                match element.TryGetProperty(name) with
                | true, prop ->
                    match prop.ValueKind with
                    | JsonValueKind.Null -> Ok None
                    | _ ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error msg -> Error.parseError name msg element
                | false, _ -> Ok None
            with
                | ArrayException msg -> Error.parseError name msg element
                | :? InvalidOperationException -> Error.notObject element

    /// <summary>Parses an element as a Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element in the list.</param>
    /// <param name="element">The element to parse from. Must be an array.</param>
    let list (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Array ->
                element.EnumerateArray()
                |> Seq.map (parser >> Result.defaultWith (fun e -> raise <| ArrayException e))
                |> Seq.toList
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

    /// Parses an element as System.DateTime.
    let dateTime = getValue _.GetDateTime()

    /// Parses an element as System.DateTime and converts it to UTC.
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

    /// Parses an element as System.DateTimeOffset.
    let dateTimeOffset = getValue _.GetDateTimeOffset()