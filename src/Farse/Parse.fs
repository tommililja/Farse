namespace Farse

open System
open System.Globalization
open System.Text.Json

module Parse =

    let private parse (name:string) parser =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Object ->
                match element.TryGetProperty(name) with
                | true, prop when prop.ValueKind <> JsonValueKind.Null ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error e -> Error.parseError name e element
                | false, _ -> Error.missingProperty name element
                | _ -> Error.nullProperty name element
            | _ -> Error.notObject name element

    let private tryParse (name:string) parser =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Object ->
                match element.TryGetProperty(name) with
                | true, prop when prop.ValueKind <> JsonValueKind.Null ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error e -> Error.parseError name e element
                | _ -> Ok None
            | _ -> Error.notObject name element

    /// <summary>Parses a required property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let req (path:string) (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name -> parse name parser
        | Nested path ->
            parser
            |> Array.foldBack (fun name parser -> parse name parser)
                (path.Split('.', StringSplitOptions.RemoveEmptyEntries))

    /// <summary>Parses an optional property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let opt (path:string) (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name -> tryParse name parser
        | Nested path ->
            parser
            |> Parser.map Some
            |> Array.foldBack (fun name parser element ->
                match tryParse name parser element with
                | Ok (Some x) -> Ok x
                | Ok None -> Ok None
                | Error e -> Error e
            ) (path.Split('.', StringSplitOptions.RemoveEmptyEntries))

    let private seq convert (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | JsonValueKind.Array ->
                element.EnumerateArray()
                |> Seq.map parser
                |> fun seq ->
                    let array = ResizeArray<'a>()
                    let mutable error = None

                    use e = seq.GetEnumerator()
                    while error.IsNone && e.MoveNext() do
                        match e.Current with
                        | Ok x -> array.Add x
                        | Error e -> error <- Some e

                    match error with
                    | Some e -> Error e
                    | None -> Ok <| convert array
            | _ ->
                element.ValueKind
                |> Error.invalidElement JsonValueKind.Array
                |> Error

    let private getValue (tryParse:JsonElement -> bool * 'a) validateKind : Parser<_> =
        fun element ->
            if validateKind element then
                match tryParse element with
                | true, x -> Ok x
                | _ ->
                    typeof<'a>
                    |> Error.couldNotParse element
                    |> Error
            else // TODO: Show different error message.
                typeof<'a>
                |> Error.couldNotParse element
                |> Error

    let private isString (e:JsonElement) =
        e.ValueKind = JsonValueKind.String

    let private isNumber (e:JsonElement) =
        e.ValueKind = JsonValueKind.Number

    let private isBool (e:JsonElement) =
        e.ValueKind = JsonValueKind.True || e.ValueKind = JsonValueKind.False

    /// Parses an element as System.Int32.
    let int = getValue _.TryGetInt32() isNumber

    /// Parses an element as System.Double.
    let float = getValue _.TryGetDouble() isNumber

    /// Parses an element as System.Decimal.
    let decimal = getValue _.TryGetDecimal() isNumber

    /// Parses an element as System.String.
    let string = getValue _.TryGetString() isString

    /// Parses an element as System.Boolean.
    let bool = getValue _.TryGetBoolean() isBool

    /// Parses an element as System.Guid.
    let guid = getValue _.TryGetGuid() isString

    /// Parses an element as System.DateTime (ISO 8601).
    let dateTime = getValue _.TryGetDateTime() isString

    /// Parses an element as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue _.TryGetDateTimeUtc() isString

    /// Parses an element as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue _.TryGetDateTimeOffset() isString

    /// Parses an element as System.DateTime with a specific format.
    let dateTimeExact (format:string) =
        fun (element:JsonElement) ->
            if isString element then
                let str = element.GetString()
                match DateTime.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, date -> Ok date
                | _ ->
                    format
                    |> Error.couldNotParseDateTime element
                    |> Error
            else
                element.ValueKind
                |> Error.invalidElement JsonValueKind.String
                |> Error

    /// <summary>Parses an element as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element in the list.</param>
    let list (parser:Parser<_>) : Parser<_> =
        seq Seq.toList parser

    /// <summary>Parses an element as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element in the array.</param>
    let array (parser:Parser<_>) : Parser<_> =
        seq Seq.toArray parser

    /// <summary>Parses an element as Microsoft.FSharp.Collections.Set.</summary>
    /// <param name="parser">The parser used for every element in the set.</param>
    let set (parser:Parser<_>) : Parser<_> =
        seq Set.ofSeq parser