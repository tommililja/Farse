namespace Farse

open System
open System.Text.Json

module Parse =
    open JsonElement

    let private parse name (parser:Parser<_>) : Parser<_> =
        fun element ->
            match element.ValueKind with
            | Kind.Object ->
                match getProperty name element |> parser with
                | Ok x -> Ok x
                | Error error ->
                    error
                    |> ParserError.enrich name element
                    |> Error
            | _ ->
               CouldNotRead (name, element)
               |> Error

    let private tryParse name (parser:Parser<_>) : Parser<_> =
        fun element ->
            match element.ValueKind with
            | Kind.Object ->
                match tryGetProperty name element with
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

    let private traverse (path:string array) (parser:Parser<_>) : Parser<_> =
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
                        Ok <| getProperty name element
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

    let private tryTraverse (path:string array) (parser:Parser<_>) : Parser<_> =
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
                        Ok <| tryGetProperty name element
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

    /// <summary>Parses a required property with the given parser.</summary>
    /// <example>
    ///     <code>
    ///         Parse.req "prop.prop2" Parse.int
    ///     </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path parser =
        match path with
        | Flat name -> parse name parser
        | Nested path -> traverse path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <example>
    ///     <code>
    ///         Parse.opt "prop.prop2" Parse.int
    ///     </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path parser =
        match path with
        | Flat name -> tryParse name parser
        | Nested path -> tryTraverse path parser

    // Internal

    let inline internal getValue (tryParse: JsonElement -> Result<'a, string>) expectedKind : Parser<_> =
        fun element ->
            let isExpectedKind =
                match expectedKind with
                | kind when kind = element.ValueKind -> true
                | kind when Kind.isBool kind -> isBool element
                | kind -> kind = Kind.Undefined

            if isExpectedKind then
                match tryParse element with
                | Ok x -> Ok x
                | Error msg ->
                    InvalidValue (msg, typeof<'a>, element)
                    |> Error
            else
                 InvalidKind (expectedKind, element)
                 |> Error

    let inline private seq convert (parser:Parser<_>) : Parser<_> =
        fun element ->
            match element.ValueKind with
            | Kind.Array ->
                let array =
                    element.GetArrayLength()
                    |> ResizeArray

                let mutable error = None
                for item in element.EnumerateArray() do
                    error <- error
                    |> Option.orElse (
                        match parser item with
                        | Ok x -> array.Add x; None
                        | Error error ->
                            (array.Count, element, error)
                            |> ArrayError
                            |> Some
                    )

                match error with
                | Some e -> Error e
                | None -> Ok <| convert array
            | _ ->
                InvalidKind (Kind.Array, element)
                |> Error

    // Custom

    /// <summary>Creates a custom parser with the given try parse function.</summary>
    /// <param name="tryParse">The try parse function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let custom tryParse expectedKind =
        getValue tryParse expectedKind

    // Basic types

    /// Parses a number as System.Int32.
    let int = getValue tryGetInt Kind.Number

    /// Parses a number as a System.Int32 enum.
    let intEnum<'a when 'a : enum<int>> =
        getValue tryGetIntEnum<'a> Kind.Number

    /// Parses a number as System.Int16.
    let int16 = getValue tryGetInt16 Kind.Number

    /// Parses a number as a System.Int16 enum.
    let int16Enum<'a when 'a : enum<int16>> =
        getValue tryGetInt16Enum<'a> Kind.Number

    /// Parses a number as System.Int64.
    let int64 = getValue tryGetInt64 Kind.Number

    /// Parses a number as a System.Int16 enum.
    let int64Enum<'a when 'a : enum<int64>> =
        getValue tryGetInt64Enum<'a> Kind.Number

    /// Parses a number as System.UInt16.
    let uint16 = getValue tryGetUInt16 Kind.Number

    /// Parses a number as a System.UInt16 enum.
    let uint16Enum<'a when 'a : enum<uint16>> =
        getValue tryGetUInt16Enum<'a> Kind.Number

    /// Parses a number as System.UInt32.
    let uint32 = getValue tryGetUInt32 Kind.Number

    /// Parses a number as a System.UInt32 enum.
    let uint32Enum<'a when 'a : enum<uint32>> =
        getValue tryGetUInt32Enum<'a> Kind.Number

    /// Parses a number as System.UInt64.
    let uint64 = getValue tryGetUInt64 Kind.Number

    /// Parses a number as a System.UInt64 enum.
    let uint64Enum<'a when 'a : enum<uint64>> =
        getValue tryGetUInt64Enum<'a> Kind.Number

    /// Parses a number as System.Double.
    let float = getValue tryGetFloat Kind.Number

    /// Parses a number as System.Single.
    let float32 = getValue tryGetFloat32 Kind.Number

    /// Parses a number as System.Decimal.
    let decimal = getValue tryGetDecimal Kind.Number

    /// Parses a number as System.Byte.
    let byte = getValue tryGetByte Kind.Number

    /// Parses a number as a System.Byte enum.
    let byteEnum<'a when 'a : enum<byte>> =
        getValue tryGetByteEnum<'a> Kind.Number

    /// Parses a number as System.SByte.
    let sbyte = getValue tryGetSByte Kind.Number

    /// Parses a number as a System.SByte enum.
    let sbyteEnum<'a when 'a : enum<sbyte>> =
        getValue tryGetSByteEnum<'a> Kind.Number

    /// Parses a string as System.Char.
    let char = getValue tryGetChar Kind.String

    /// Parses a string as System.String.
    let string = getValue tryGetString Kind.String

    /// Parses a string as an enum.
    let enum<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)> =
        getValue tryGetEnum<'a> Kind.String

    /// Parses a bool as System.Boolean.
    let bool = getValue tryGetBool Kind.True

    /// Parses a string as System.Guid.
    let guid = getValue tryGetGuid Kind.String

    /// Parses null as FSharp.Core.Unit.
    let unit = getValue tryGetUnit Kind.Null

    // Date and time

    /// Parses a string as System.TimeOnly.
    let timeOnly = getValue tryGetTimeOnly Kind.String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeOnlyExact (format:string) =
        getValue (tryGetTimeOnlyExact format) Kind.String

    /// Parses a string as System.TimeSpan.
    let timeSpan = getValue tryGetTimeSpan Kind.String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeSpanExact (format:string) =
        getValue (tryGetTimeSpanExact format) Kind.String

    /// Parses a string as System.DateOnly (ISO 8601).
    let dateOnly = getValue tryGetDateOnly Kind.String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateOnlyExact (format:string) =
        getValue (tryGetDateOnlyExact format) Kind.String

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = getValue tryGetDateTime Kind.String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = getValue tryGetDateTimeUtc Kind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) =
        getValue (tryGetDateTimeExact format) Kind.String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = getValue tryGetDateTimeOffset Kind.String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact (format:string) =
        getValue (tryGetDateTimeOffsetExact format) Kind.String

    // Sequences

    /// <summary>Parses an array as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let list parser = seq Seq.toList parser

    /// <summary>Parses an array as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let array parser = seq Seq.toArray parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.Set.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let set parser = seq Set.ofSeq parser

    // Json

    /// Parses an element's raw text as System.String.
    let rawText = getValue tryGetRawText Kind.Undefined

    /// Parses an array's length as System.Int32.
    let arrayLength = getValue tryGetArrayLength Kind.Array

    /// Parses an object's property count as System.Int32.
    let propertyCount = getValue tryGetPropertyCount Kind.Object

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = getValue tryGetKind Kind.Undefined

    /// Parses an element as System.Text.Json.JsonElement.
    let element = getValue tryGetElement Kind.Undefined

    // Misc

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()

    /// <summary>Always succeeds and returns the given value.</summary>
    /// <param name="x">The value to return.</param>
    let noneWith x = Parser.from x