namespace Farse

open System
open System.Collections.Generic
open System.Globalization
open System.Text.Json

module Parse =

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Parse.req "prop.prop2" Parse.int</code>
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
    /// <code>let! int = Parse.opt "prop.prop2" Parse.int</code>
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

    /// <summary>Creates a custom parser with the given function.</summary>
    /// <param name="fn">The parsing function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let inline custom (fn:JsonElement -> Result<'a, _>) expectedKind : Parser<_> =
        fun (element:JsonElement) ->
            let isExpectedKind =
                match expectedKind with
                | Any -> true
                | Bool -> element.ValueKind = Kind.True || element.ValueKind = Kind.False
                | kind ->
                    element.ValueKind
                    |> ExpectedKind.fromKind
                    |> (=) kind

            if isExpectedKind then
                try match fn element with
                    | Ok x -> Ok x
                    | Error msg ->
                        InvalidValue (msg, typeof<'a>, element)
                        |> Error
                with ex ->
                    InvalidValue (Some ex.Message, typeof<'a>, element)
                    |> Error
            else
                 InvalidKind (expectedKind, element)
                 |> Error
        |> id

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>Produces detailed error messages when validation fails.</remarks>
    /// <code>let! email = "prop" &amp;= Parse.valid Parse.string Email.fromString</code>
    /// <param name="parser">The parser to validate.</param>
    /// <param name="fn">The validation function.</param>
    let inline valid (parser:Parser<_>) ([<InlineIfLambda>] fn) : Parser<'b> =
        fun element ->
            match parser element with
            | Ok x ->
                match fn x with
                | Ok x -> Ok x
                | Error msg ->
                    InvalidValue (Some msg, typeof<'b>, element)
                    |> Error
            | Error e -> Error e
        |> id

    // Parsing

    open type ExpectedKind

    // Basic types

    let inline private tryParse fn =
        match fn () with
        | true, x -> Ok x
        | _ -> Error None

    /// Parses a number as System.Int32.
    let int = custom (_.TryGetInt32 >> tryParse) Number

    /// Parses a number as System.Int16.
    let int16 = custom (_.TryGetInt16 >> tryParse) Number

    /// Parses a number as System.Int64.
    let int64 = custom (_.TryGetInt64 >> tryParse) Number

    /// Parses a number as System.UInt16.
    let uint16 = custom (_.TryGetUInt16 >> tryParse) Number

    /// Parses a number as System.UInt32.
    let uint32 = custom (_.TryGetUInt32 >> tryParse) Number

    /// Parses a number as System.UInt64.
    let uint64 = custom (_.TryGetUInt64 >> tryParse) Number

    /// Parses a number as System.Double.
    let float = custom (_.TryGetDouble >> tryParse) Number

    /// Parses a number as System.Single.
    let float32 = custom (_.TryGetSingle >> tryParse) Number

    /// Parses a number as System.Decimal.
    let decimal = custom (_.TryGetDecimal >> tryParse) Number

    /// Parses a number as System.Byte.
    let byte = custom (_.TryGetByte >> tryParse) Number

    /// Parses a number as System.SByte.
    let sbyte = custom (_.TryGetSByte >> tryParse) Number

    /// Parses a string as System.Char.
    let char =
        custom (fun element ->
            match element.GetString() with
            | str when str.Length = 1 -> Ok str[0]
            | _ -> Error <| Some "Expected a string length of 1."
        ) String

    /// Parses a string as System.String.
    let string = custom (_.GetString() >> Ok) String

    /// Parses a bool as System.Boolean.
    let bool = custom (_.GetBoolean() >> Ok) Bool

    /// Parses a string as System.Guid.
    let guid = custom (_.TryGetGuid >> tryParse) String

    /// Parses null as FSharp.Core.Unit.
    let unit = custom (ignore >> Ok) Null

    // Enums

    let inline private parseEnum<'a, 'b when 'a: enum<'b>> fn =
        let enumType = typeof<'a>
        match fn () with
        | true, x when Enum.IsDefined(enumType, x) -> Ok <| LanguagePrimitives.EnumOfValue<'b, 'a> x
        | true, _ -> Error <| Some $"Expected %s{enumType.Name} enum."
        | _ -> Error None

    /// Parses a string as an enum.
    let enum<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)> =
        custom (fun element ->
            let str = element.GetString()
            match Enum.TryParse<'a>(str, true) with
            | true, enum -> Ok enum
            | _ -> Error None
        ) String

    /// Parses a number as a System.Int32 enum.
    let intEnum<'a when 'a : enum<int>> =
        custom (_.TryGetInt32 >> parseEnum<'a, int>) Number

    /// Parses a number as a System.Int16 enum.
    let int16Enum<'a when 'a : enum<int16>> =
        custom (_.TryGetInt16 >> parseEnum<'a, int16>) Number

    /// Parses a number as a System.Int16 enum.
    let int64Enum<'a when 'a : enum<int64>> =
        custom (_.TryGetInt64 >> parseEnum<'a, int64>) Number

    /// Parses a number as a System.UInt16 enum.
    let uint16Enum<'a when 'a : enum<uint16>> =
        custom (_.TryGetUInt16 >> parseEnum<'a, uint16>) Number

    /// Parses a number as a System.UInt32 enum.
    let uint32Enum<'a when 'a : enum<uint32>> =
        custom (_.TryGetUInt32 >> parseEnum<'a, uint32>) Number

    /// Parses a number as a System.UInt64 enum.
    let uint64Enum<'a when 'a : enum<uint64>> =
        custom (_.TryGetUInt64 >> parseEnum<'a, uint64>) Number

    /// Parses a number as a System.Byte enum.
    let byteEnum<'a when 'a : enum<byte>> =
        custom (_.TryGetByte >> parseEnum<'a, byte>) Number

    /// Parses a number as a System.SByte enum.
    let sbyteEnum<'a when 'a : enum<sbyte>> =
        custom (_.TryGetSByte >> parseEnum<'a, sbyte>) Number

    // Date and time

    /// Parses a string as System.TimeOnly.
    let timeOnly =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error None
        ) String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeOnlyExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error <| Some $"Expected %s{format}."
        ) String

    /// Parses a string as System.TimeSpan.
    let timeSpan =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error None
        ) String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeSpanExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParseExact(str, format, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error <| Some $"Expected %s{format}."
        ) String

    /// Parses a string as System.DateOnly (ISO 8601).
    let dateOnly =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error None
        ) String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateOnlyExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error <| Some $"Expected %s{format}."
        ) String

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = custom (_.TryGetDateTime >> tryParse) String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc =
        custom (fun element ->
            match element.TryGetDateTime() with
            | true, dateTime -> Ok <| dateTime.ToUniversalTime()
            | _ -> Error None
        ) String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTime.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTime -> Ok dateTime
            | _ -> Error <| Some $"Expected %s{format}."
        ) String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = custom (_.TryGetDateTimeOffset >> tryParse) String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTimeOffset.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error <| Some $"Expected %s{format}."
        ) String

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

    /// <summary>Parses an array as Microsoft.FSharp.Collections.seq.</summary>
    /// <param name="parser">The parser used for every element.</param>
    let seq parser = arr Seq.ofResizeArray parser

    /// <summary>Parses an array at a specific index.</summary>
    /// <param name="i">The index to parse in the array.</param>
    /// <param name="parser">The parser used for the element.</param>
    let index i (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when i >= 0 && arrayLength >= i + 1 -> element.Item(i) |> parser
            | Kind.Array -> Error <| ArrayLengthError (i, arrayLength, element)
            | _ -> Error <| InvalidKind (Array, element)
        |> id

    // Key/Value

    let inline private getDuplicateKey (pairs:('k * 'v) seq) =
        let seen = HashSet<'k>()
        pairs
        |> Seq.tryPick (fun (k, _) ->
            if seen.Add(k) then None
            else Some k
        )

    let inline private keyValue convert (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let mutable error = None
                let mutable enumerator = element.EnumerateObject()

                let array =
                    element.GetPropertyCount()
                    |> ResizeArray

                while error.IsNone && enumerator.MoveNext() do
                    match parser enumerator.Current.Value with
                    | Ok x -> array.Add(enumerator.Current.Name, x)
                    | Error e -> error <- Some e

                match error with
                | None ->
                    match getDuplicateKey array with
                    | Some key -> Error <| DuplicateKeys (key, element)
                    | None -> Ok <| convert array
                | Some e -> Error e
            | _ ->
                InvalidKind (Object, element)
                |> Error
        |> id

    /// <summary>Parses an object's properties as Microsoft.FSharp.Collections.Map.</summary>
    /// <param name="parser">The parser used for every property.</param>
    let map parser = keyValue Map.ofSeq parser

    /// <summary>Parses an object's properties as System.Collections.Generic.IDictionary.</summary>
    /// <param name="parser">The parser used for every property.</param>
    let dict parser = keyValue Dictionary.ofSeq parser

    /// <summary>Parses an object's properties as System.Collections.Generic.KeyValuePair seq.</summary>
    /// <param name="parser">The parser used for every property.</param>
    let keyValuePairs parser = keyValue KeyValuePairs.ofSeq parser

    /// <summary>Parses an object's properties as tuple seq.</summary>
    /// <param name="parser">The parser used for every property.</param>
    let tuples parser = keyValue Seq.ofResizeArray parser

    // Tuples

    let inline private parseItem i parser (element:JsonElement) =
        element.Item(i)
        |> parser
        |> Result.mapError (fun e -> ArrayError(i, element, e))

    let inline private tuple length fn : Parser<_> =
        fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when arrayLength = length -> fn element
            | Kind.Array -> Error <| InvalidTuple(length, arrayLength, element)
            | _ -> Error <| InvalidKind(Array, element)

    /// <summary>Parses an array with two values as a tuple.</summary>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    let tuple2 (a:Parser<_>) (b:Parser<_>) =
        tuple 2 (fun e ->
            result {
                let! a = parseItem 0 a e
                let! b = parseItem 1 b e

                return a, b
            }
        )

    /// <summary>Parses an array with three values as a tuple of three.</summary>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    /// <param name="c">The parser used for the third value.</param>
    let tuple3 (a:Parser<_>) (b:Parser<_>) (c:Parser<_>) =
        tuple 3 (fun e ->
            result {
                let! a = parseItem 0 a e
                let! b = parseItem 1 b e
                let! c = parseItem 2 c e

                return a, b, c
            }
        )

    // Json

    /// Parses an element's raw text as System.String.
    let rawText = custom (_.GetRawText() >> Ok) Any

    /// Parses an array's length as System.Int32.
    let arrayLength = custom (_.GetArrayLength() >> Ok) Array

    /// Parses an object's property count as System.Int32.
    let propertyCount = custom (_.GetPropertyCount() >> Ok) Object

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = custom (_.ValueKind >> Ok) Any

    /// Parses an element as System.Text.Json.JsonElement.
    let element = custom (_.Clone() >> Ok) Any

    // Misc

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()

    /// <summary>Always succeeds and returns the given value.</summary>
    /// <param name="x">The value to return.</param>
    let noneWith x = Parser.from x