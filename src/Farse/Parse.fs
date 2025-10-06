namespace Farse

open System
open System.Text.Json

module Parse =
    open JsonElement
    open type ExpectedKind

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

    /// <summary>Creates a custom parser with the given function.</summary>
    /// <param name="fn">The parsing function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let inline custom (fn: JsonElement -> Result<'a, _>) expectedKind : Parser<_> =
        fun element ->
            let isExpectedKind =
                match expectedKind with
                | Any -> true
                | Bool -> isBool element
                | kind -> isExpectedKind kind element

            if isExpectedKind then
                match fn element with
                | Ok x -> Ok x
                | Error msg ->
                    InvalidValue (msg, typeof<'a>, element)
                    |> Error
            else
                 InvalidKind (expectedKind, element)
                 |> Error
        |> id

    // Basic types

    /// Parses a number as System.Int32.
    let int = custom tryGetInt Number

    /// Parses a number as a System.Int32 enum.
    let intEnum<'a when 'a : enum<int>> =
        custom tryGetIntEnum<'a> Number

    /// Parses a number as System.Int16.
    let int16 = custom tryGetInt16 Number

    /// Parses a number as a System.Int16 enum.
    let int16Enum<'a when 'a : enum<int16>> =
        custom tryGetInt16Enum<'a> Number

    /// Parses a number as System.Int64.
    let int64 = custom tryGetInt64 Number

    /// Parses a number as a System.Int16 enum.
    let int64Enum<'a when 'a : enum<int64>> =
        custom tryGetInt64Enum<'a> Number

    /// Parses a number as System.UInt16.
    let uint16 = custom tryGetUInt16 Number

    /// Parses a number as a System.UInt16 enum.
    let uint16Enum<'a when 'a : enum<uint16>> =
        custom tryGetUInt16Enum<'a> Number

    /// Parses a number as System.UInt32.
    let uint32 = custom tryGetUInt32 Number

    /// Parses a number as a System.UInt32 enum.
    let uint32Enum<'a when 'a : enum<uint32>> =
        custom tryGetUInt32Enum<'a> Number

    /// Parses a number as System.UInt64.
    let uint64 = custom tryGetUInt64 Number

    /// Parses a number as a System.UInt64 enum.
    let uint64Enum<'a when 'a : enum<uint64>> =
        custom tryGetUInt64Enum<'a> Number

    /// Parses a number as System.Double.
    let float = custom tryGetFloat Number

    /// Parses a number as System.Single.
    let float32 = custom tryGetFloat32 Number

    /// Parses a number as System.Decimal.
    let decimal = custom tryGetDecimal Number

    /// Parses a number as System.Byte.
    let byte = custom tryGetByte Number

    /// Parses a number as a System.Byte enum.
    let byteEnum<'a when 'a : enum<byte>> =
        custom tryGetByteEnum<'a> Number

    /// Parses a number as System.SByte.
    let sbyte = custom tryGetSByte Number

    /// Parses a number as a System.SByte enum.
    let sbyteEnum<'a when 'a : enum<sbyte>> =
        custom tryGetSByteEnum<'a> Number

    /// Parses a string as System.Char.
    let char = custom tryGetChar String

    /// Parses a string as System.String.
    let string = custom tryGetString String

    /// Parses a string as an enum.
    let enum<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)> =
        custom tryGetEnum<'a> String

    /// Parses a bool as System.Boolean.
    let bool = custom tryGetBool Bool

    /// Parses a string as System.Guid.
    let guid = custom tryGetGuid String

    /// Parses null as FSharp.Core.Unit.
    let unit = custom tryGetUnit Null

    // Date and time

    /// Parses a string as System.TimeOnly.
    let timeOnly = custom tryGetTimeOnly String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeOnlyExact (format:string) =
        custom (tryGetTimeOnlyExact format) String

    /// Parses a string as System.TimeSpan.
    let timeSpan = custom tryGetTimeSpan String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeSpanExact (format:string) =
        custom (tryGetTimeSpanExact format) String

    /// Parses a string as System.DateOnly (ISO 8601).
    let dateOnly = custom tryGetDateOnly String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateOnlyExact (format:string) =
        custom (tryGetDateOnlyExact format) String

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = custom tryGetDateTime String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc = custom tryGetDateTimeUtc String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) =
        custom (tryGetDateTimeExact format) String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = custom tryGetDateTimeOffset String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact (format:string) =
        custom (tryGetDateTimeOffsetExact format) String

    // Sequences

    let inline private arr convert (parser:Parser<_>) : Parser<_> =
        fun element ->
            match element.ValueKind with
            | Kind.Array ->
                let mutable error = None
                let mutable enumerator = element.EnumerateArray()
                let array =
                    element.GetArrayLength()
                    |> ResizeArray

                while error.IsNone && enumerator.MoveNext() do
                    match parser enumerator.Current with
                    | Ok x -> array.Add x
                    | Error e -> error <- Some e

                match error with
                | None -> Ok <| convert array
                | Some error ->
                    ArrayError (array.Count, element, error)
                    |> Error
            | _ ->
                InvalidKind (Array, element)
                |> Error

    /// <summary>Parses an array as Microsoft.FSharp.Collections.list.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let list parser = arr List.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Core.array.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let array parser = arr Array.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.Set.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let set parser = arr Set.ofSeq parser

    // Json

    /// Parses an element's raw text as System.String.
    let rawText = custom tryGetRawText Any

    /// Parses an array's length as System.Int32.
    let arrayLength = custom tryGetArrayLength Array

    /// Parses an object's property count as System.Int32.
    let propertyCount = custom tryGetPropertyCount Object

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = custom tryGetKind Any

    /// Parses an element as System.Text.Json.JsonElement.
    let element = custom tryGetElement Any

    // Misc

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()

    /// <summary>Always succeeds and returns the given value.</summary>
    /// <param name="x">The value to return.</param>
    let noneWith x = Parser.from x