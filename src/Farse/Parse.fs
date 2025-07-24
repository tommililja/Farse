namespace Farse

open System.Text.Json

module Parse =

    let private getProperty (name:string) (element:JsonElement) =
        match element.ValueKind with
        | Kind.Object -> Ok (snd (element.TryGetProperty(name)))
        | _ -> Error.notObject name element

    let private tryGetProperty (name:string) (element:JsonElement) =
        match element.ValueKind with
        | Kind.Object ->
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Ok (Some prop)
            | _ -> Ok None
        | _ -> Error.notObject name element

    let private tryParse name parser element =
        match tryGetProperty name element with
        | Ok (Some prop) ->
            match parser prop with
            | Ok x -> Ok (Some x)
            | Error msg when String.startsWith "Error" msg -> Error msg
            | Error msg -> Error.parseError name msg element
        | Ok None -> Ok None
        | Error e -> Error e

    let private parse name parser element =
        match getProperty name element with
        | Ok prop ->
            match parser prop with
            | Ok x -> Ok x
            | Error msg when String.startsWith "Error" msg -> Error msg
            | Error msg -> Error.parseError name msg element
        | Error e -> Error e

    let private trav (path:string array) parser element =
        let mutable last = Ok element
        let mutable previous = element
        let mutable previousName = path[0]

        // Pretty rowdy, but it works.
        for i in 0 .. path.Length - 1 do
            match last with
            | Ok (element:JsonElement) when element.ValueKind = Kind.Object ->
                last <- getProperty path[i] element
                previous <- element
                previousName <- path[i]
            | Ok element when element.ValueKind = Kind.Null || element.ValueKind = Kind.Undefined ->
                if i = path.Length
                then last <- Ok element
                else last <- Error.notObjectTrav previousName previous element
            | Ok element -> last <- Error.notObject path[i] element
            | _ -> ()

        match last with
        | Ok prop ->
            match parser prop with
            | Ok x -> Ok x
            | Error msg when String.startsWith "Error: Could not read" msg ->
                let name = Array.last path
                let expected =
                    if prop.ValueKind = Kind.Array
                    then prop.Item 0
                    else prop

                Error.notObjectTrav name previous expected
            | Error msg when String.startsWith "Error" msg -> Error msg
            | Error msg -> Error.parseError previousName msg previous
        | Error e -> Error e

    let private tryTrav (path:string array) parser element =
        let mutable last = Ok (Some element)
        let mutable previous = element
        let mutable previousName = path[0]

        // Pretty rowdy, but it works.
        for i in 0 .. path.Length - 1 do
            match last with
            | Ok (Some (element:JsonElement)) when element.ValueKind = Kind.Object ->
                last <- tryGetProperty path[i] element
                previous <- element
                previousName <- path[i]
            | Ok (Some element) -> last <- Error.notObject path[i] element
            | _ -> ()

        match last with
        | Ok (Some prop) ->
            match parser prop with
            | Ok x -> Ok (Some x)
            | Error msg when String.startsWith "Error: Could not read" msg ->
                let name = Array.last path
                let expected =
                    if prop.ValueKind = Kind.Array
                    then prop.Item 0
                    else prop

                Error.notObjectTrav name previous expected
            | Error msg when String.startsWith "Error" msg -> Error msg
            | Error msg -> Error.parseError previousName msg previous
        | Ok None -> Ok None
        | Error e -> Error e

    /// <summary>Parses a required property with the given parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let req path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name -> parse name parser
        | Nested path -> trav path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <param name="path">The path to the property. For example, "prop" or "prop.prop2".</param>
    /// <param name="parser">The parser used to parse the property value. For example, Parse.int.</param>
    let opt path (parser:Parser<_>) : Parser<_> =
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
                if element.ValueKind = expectedKind then true
                else if expectedKind = Kind.True then element.ValueKind = Kind.False
                else expectedKind = Kind.Undefined

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

    /// Parses a number as System.Int32.
    let int = getValue _.TryGetInt32() Kind.Number

    /// Parses a number as System.Int16.
    let int16 = getValue _.TryGetInt16() Kind.Number

    /// Parses a number as System.Int64.
    let int64 = getValue _.TryGetInt64() Kind.Number

    /// Parses a number as System.UInt16.
    let uint16 = getValue _.TryGetUInt16() Kind.Number

    /// Parses a number as System.UInt32.
    let uint32 = getValue _.TryGetUInt32() Kind.Number

    /// Parses a number as System.UInt64.
    let uint64 = getValue _.TryGetUInt64() Kind.Number

    /// Parses a number as System.Double.
    let float = getValue _.TryGetDouble() Kind.Number

    /// Parses a number as System.Single.
    let float32 = getValue _.TryGetSingle() Kind.Number

    /// Parses a number as System.Decimal.
    let decimal = getValue _.TryGetDecimal() Kind.Number

    /// Parses a number as System.Byte.
    let byte = getValue _.TryGetByte() Kind.Number

    /// Parses a number as System.SByte.
    let sbyte = getValue _.TryGetSByte() Kind.Number

    /// Parses a string as System.Char.
    let char = getValue _.TryGetChar() Kind.String

    /// Parses a string as System.String.
    let string = getValue _.TryGetString() Kind.String

    /// Parses a bool as System.Boolean.
    let bool = getValue _.TryGetBoolean() Kind.True

    /// Parses a string as System.Guid.
    let guid = getValue _.TryGetGuid() Kind.String

    // Dates

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = getValue _.TryGetDateTime() Kind.String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue _.TryGetDateTimeUtc() Kind.String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue _.TryGetDateTimeOffset() Kind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) = getValue _.TryGetDateTimeExact(format) Kind.String

    // Sequences

    /// <summary>Parses an array as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let list parser =
        seq Seq.toList parser

    /// <summary>Parses an array as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let array parser =
        seq Seq.toArray parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.Set.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let set parser =
        seq Set.ofSeq parser

    // Misc

    /// Parses an object's property count as System.Int32.
    let propertyCount = getValue _.TryGetPropertyCount() Kind.Object

    /// Parses an array's length as System.Int32.
    let arrayLength = getValue _.TryGetArrayLength() Kind.Array

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = getValue _.TryGetKind() Kind.Undefined

    /// Does not parse element and returns FSharp.Core.Unit.
    let none = Parser.from ()