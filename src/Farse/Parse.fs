namespace Farse

open System
open System.Globalization
open System.Text.Json

module Parse =

    let private getProperty (name:string) (element:JsonElement) =
        match element.ValueKind with
        | Kind.Object ->
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Ok prop
            | false, _ -> Error.missingProperty name element
            | _ -> Error.nullProperty name element
        | _ -> Error.notObject name element

    let private tryGetProperty (name:string) (element:JsonElement) =
        match element.ValueKind with
        | Kind.Object ->
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Ok (Some prop)
            | false, _ -> Ok None
            | _ -> Ok None
        | _ -> Error.notObject name element

    let private tryParse name parser =
        fun (element:JsonElement) ->
            match tryGetProperty name element with
            | Ok (Some prop) ->
                match parser prop with
                | Ok x -> Ok (Some x)
                | Error msg when String.startsWith "Error" msg -> Error msg
                | Error msg -> Error.parseError name msg element
            | Ok None -> Ok None
            | Error e -> Error e

    let private parse name parser =
        fun (element:JsonElement) ->
            match getProperty name element with
            | Ok prop ->
                match parser prop with
                | Ok x -> Ok x
                | Error msg when String.startsWith "Error" msg -> Error msg
                | Error msg -> Error.parseError name msg element
            | Error e -> Error e

    let private trav (path:string array) parser =
        fun element ->
            let mutable last = Ok element
            let mutable previous = element

            for name in path do
                match last with
                | Ok element ->
                    last <- getProperty name element
                    previous <- element
                | Error _ -> ()

            match last with
            | Ok prop ->
                match parser prop with
                | Ok x -> Ok x
                | Error msg when String.startsWith "Error" msg -> Error msg
                | Error msg ->
                    let name = Array.last path
                    Error.parseError name msg previous
            | Error e -> Error e

    let tryTrav (path:string array) parser =
        fun element ->
            let mutable last = Ok (Some element)
            let mutable previous = element

            for name in path do
                match last with
                | Ok (Some element) ->
                    last <- tryGetProperty name element
                    previous <- element
                | _ -> ()

            match last with
            | Ok (Some prop) ->
                match parser prop with
                | Ok x -> Ok (Some x)
                | Error msg when String.startsWith "Error" msg -> Error msg
                | Error msg ->
                    let name = Array.last path
                    Error.parseError name msg previous
            | Ok None -> Ok None
            | Error e -> Error e

    /// <summary>Parses a required property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let req (path:string) (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name -> parse name parser
        | Nested path -> trav path parser

    /// <summary>Parses an optional property with the supplied parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let opt (path:string) (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name -> tryParse name parser
        | Nested path -> tryTrav path parser

    let private seq convert (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Array ->
                let mutable error = None
                let array =
                    element.GetArrayLength()
                    |> ResizeArray

                for element in element.EnumerateArray() do
                    if error.IsNone then
                        match parser element with
                        | Ok x -> array.Add x
                        | Error e -> error <- Some e

                match error with
                | Some e -> Error e
                | None -> Ok (convert array)
            | _ ->
                element.ValueKind
                |> Error.invalidElement Kind.Array
                |> Error

    let private getValue (tryParse:JsonElement -> bool * 'a) expectedKind : Parser<_> =
        fun element ->
            let isExpectedKind =
                if expectedKind = Kind.True
                then element.ValueKind = Kind.True || element.ValueKind = Kind.False
                else element.ValueKind = expectedKind

            if isExpectedKind then
                match tryParse element with
                | true, x -> Ok x
                | _ ->
                    element
                    |> Error.couldNotParse typeof<'a>
                    |> Error
            else
                element.ValueKind
                |> Error.invalidElement expectedKind
                |> Error

    /// Parses an element as System.Int32.
    let int = getValue _.TryGetInt32() Kind.Number

    /// Parses an element as System.Double.
    let float = getValue _.TryGetDouble() Kind.Number

    /// Parses an element as System.Decimal.
    let decimal = getValue _.TryGetDecimal() Kind.Number

    /// Parses an element as System.String.
    let string = getValue _.TryGetString() Kind.String

    /// Parses an element as System.Boolean.
    let bool = getValue _.TryGetBoolean() Kind.True

    /// Parses an element as System.Guid.
    let guid = getValue _.TryGetGuid() Kind.String

    /// Parses an element as System.DateTime (ISO 8601).
    let dateTime = getValue _.TryGetDateTime() Kind.String

    /// Parses an element as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue _.TryGetDateTimeUtc() Kind.String

    /// Parses an element as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue _.TryGetDateTimeOffset() Kind.String

    /// Parses an element as System.DateTime with a specific format.
    let dateTimeExact (format:string) =
        fun (element:JsonElement) ->
            if element.ValueKind = Kind.String then
                let dateString = element.GetString()
                match DateTime.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, date -> Ok date
                | _ ->
                    element
                    |> Error.couldNotParseDateTime format
                    |> Error
            else
                element.ValueKind
                |> Error.invalidElement Kind.String
                |> Error

    /// <summary>Parses an element as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let list (parser:Parser<_>) : Parser<_> =
        seq Seq.toList parser

    /// <summary>Parses an element as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let array (parser:Parser<_>) : Parser<_> =
        seq Seq.toArray parser

    /// <summary>Parses an element as Microsoft.FSharp.Collections.Set.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let set (parser:Parser<_>) : Parser<_> =
        seq Set.ofSeq parser