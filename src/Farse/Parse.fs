namespace Farse

open System.Text.Json

module Parse =

    let private rewriteError (e:string) (current:JsonElement) previous path =
        let name = Array.last path
        if current.ValueKind = Kind.Array
        then
            let msg = e.Split("\n")[1]
            Error.couldNotParse name msg previous
        else Error.notObject name previous  current

    let private parse name (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let prop = JsonElement.getProperty name element
                match parser prop with
                | Ok x -> Ok x
                | Error e when String.startsWith "Error: Could not read" e -> rewriteError e prop element [| name |]
                | Error e when String.startsWith "Error:" e -> Error e
                | Error msg -> Error.couldNotParse name msg element
            | _ -> Error.couldNotRead name element

    let private tryParse name (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                match JsonElement.tryGetProperty name element with
                | Some prop ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error e when String.startsWith "Error: Could not read" e -> rewriteError e prop element [| name |]
                    | Error e when String.startsWith "Error:" e -> Error e
                    | Error msg -> Error.couldNotParse name msg element
                | None -> Ok None
            | _ -> Error.couldNotRead name element

    // Pretty rowdy, but it works.
    let private trav (path:string array) (parser:Parser<_>) : Parser<_> =
        fun element ->
            let mutable last = Ok element
            let mutable previous = element
            let mutable previousName = path[0]

            for i in 0 .. path.Length - 1 do
                match last with
                | Ok element when JsonElement.getKind element = Kind.Object ->
                    last <- Ok <| JsonElement.getProperty path[i] element
                    previous <- element
                    previousName <- path[i]
                | Ok element ->
                    if i = path.Length
                    then last <- Ok element
                    else last <- Error.notObject previousName previous element
                | _ -> ()

            match last with
            | Ok prop ->
                match parser prop with
                | Ok x -> Ok x
                | Error e when String.startsWith "Error: Could not read" e -> rewriteError e prop previous path
                | Error e when String.startsWith "Error:" e -> Error e
                | Error msg -> Error.couldNotParse previousName msg previous
            | Error e -> Error e

    // Pretty rowdy, but it works.
    let private tryTrav (path:string array) (parser:Parser<_>) : Parser<_> =
        fun element ->
            let mutable last = Ok (Some element)
            let mutable previous = element
            let mutable previousName = path[0]

            for i in 0 .. path.Length - 1 do
                match last with
                | Ok (Some element) when JsonElement.getKind element = Kind.Object ->
                    last <- Ok <| JsonElement.tryGetProperty path[i] element
                    previous <- element
                    previousName <- path[i]
                | Ok (Some element) ->
                    if i = path.Length
                    then last <- Ok <| Some element
                    else last <- Error.notObject previousName previous element
                | _ -> ()

            match last with
            | Ok (Some prop) ->
                match parser prop with
                | Ok x -> Ok <| Some x
                | Error e when String.startsWith "Error: Could not read" e -> rewriteError e prop previous path
                | Error e when String.startsWith "Error:" e -> Error e
                | Error msg -> Error.couldNotParse previousName msg previous
            | Ok None -> Ok None
            | Error e -> Error e

    /// <summary>Parses a required property with the given parser.</summary>
    /// <example>
    /// <code>
    /// "prop.prop2" &amp;= Parse.int
    /// </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path parser =
        match path with
        | Flat name -> parse name parser
        | Nested path -> trav path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <example>
    /// <code>
    /// "prop.prop2" ?= Parse.int
    /// </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path parser =
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
                | None -> Ok <| convert array
            | _ ->
                element.ValueKind
                |> Error.invalidKind Kind.Array
                |> Error

    let inline private getValue (tryParse:JsonElement -> bool * 'a) expectedKind : Parser<_> =
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
                    |> Error.invalidValue typeof<'a>
                    |> Error
            else
                element.ValueKind
                |> Error.invalidKind expectedKind
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

    /// Parses null as FSharp.Core.Unit.
    let unit = getValue _.TryGetUnit() Kind.Null

    // Dates

    #if NET7_0_OR_GREATER

    /// Parses a string as System.TimeOnly.
    let timeOnly = getValue _.TryGetTimeOnly() Kind.String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeOnlyExact (format:string) = getValue _.TryGetTimeOnlyExact(format) Kind.String

    /// Parses a string as System.DateOnly (ISO 8601).
    let dateOnly = getValue _.TryGetDateOnly() Kind.String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateOnlyExact (format:string) = getValue _.TryGetDateOnlyExact(format) Kind.String

    #endif

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = getValue _.TryGetDateTime() Kind.String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue _.TryGetDateTimeUtc() Kind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) = getValue _.TryGetDateTimeExact(format) Kind.String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue _.TryGetDateTimeOffset() Kind.String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact (format:string) = getValue _.TryGetDateTimeOffsetExact(format) Kind.String

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

    /// Parses an array's length as System.Int32.
    let arrayLength = getValue _.TryGetArrayLength() Kind.Array

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = getValue _.TryGetKind() Kind.Undefined

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()