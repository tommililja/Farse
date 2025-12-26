namespace Farse

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Text.Json

module Parse =

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()

    /// <summary>Parses an optional value with the given parser.</summary>
    /// <code>let! int = "prop" &amp;= Parse.optional Parse.int</code>
    /// <param name="parser">The parser used to parse the property value.</param>
    let optional (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok None
            | _ ->
                match parser element with
                | Ok x -> Ok <| Some x
                | Error e -> Error e
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

    /// <summary>Creates a custom parser with the given function.</summary>
    /// <param name="fn">The parsing function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let inline custom (fn:JsonElement -> Result<'a, _>) expectedKind : Parser<_> =
        fun (element:JsonElement) ->
            let isExpectedKind =
                match expectedKind with
                | ExpectedKind.Any -> true
                | ExpectedKind.Bool -> element.ValueKind = Kind.True || element.ValueKind = Kind.False
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

    // Basic types

    let inline private tryParse fn =
        match fn () with
        | true, x -> Ok x
        | _ -> Error None

    /// Parses a number as System.Int32.
    let int = custom (_.TryGetInt32 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Int16.
    let int16 = custom (_.TryGetInt16 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Int64.
    let int64 = custom (_.TryGetInt64 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.UInt16.
    let uint16 = custom (_.TryGetUInt16 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.UInt32.
    let uint32 = custom (_.TryGetUInt32 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.UInt64.
    let uint64 = custom (_.TryGetUInt64 >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Double.
    let float = custom (_.TryGetDouble >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Single.
    let float32 = custom (_.TryGetSingle >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Decimal.
    let decimal = custom (_.TryGetDecimal >> tryParse) ExpectedKind.Number

    /// Parses a number as System.Byte.
    let byte = custom (_.TryGetByte >> tryParse) ExpectedKind.Number

    /// Parses a number as System.SByte.
    let sbyte = custom (_.TryGetSByte >> tryParse) ExpectedKind.Number

    /// Parses a string as System.Char.
    let char =
        custom (fun element ->
            match element.GetString() with
            | str when str.Length = 1 -> Ok str[0]
            | _ -> Error <| Some "Expected a string length of 1."
        ) ExpectedKind.String

    /// Parses a string as System.ExpectedKind.String.
    let string = custom (_.GetString() >> Ok) ExpectedKind.String

    /// Parses a string as System.Numerics.BigInteger.
    let bigInt =
        custom (fun element ->
            let str = element.GetString()
            match BigInteger.TryParse(str, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, bigInt -> Ok bigInt
            | false, _ -> Error None
        ) ExpectedKind.String

    /// Parses a bool as System.Boolean.
    let bool = custom (_.GetBoolean() >> Ok) ExpectedKind.Bool

    /// Parses a string as System.Guid.
    let guid = custom (_.TryGetGuid >> tryParse) ExpectedKind.String

    /// Parses null as FSharp.Core.Unit.
    let unit = custom (ignore >> Ok) ExpectedKind.Null

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
        ) ExpectedKind.String

    /// Parses a number as a System.Int32 enum.
    let intEnum<'a when 'a : enum<int>> =
        custom (_.TryGetInt32 >> parseEnum<'a, int>) ExpectedKind.Number

    /// Parses a number as a System.Int16 enum.
    let int16Enum<'a when 'a : enum<int16>> =
        custom (_.TryGetInt16 >> parseEnum<'a, int16>) ExpectedKind.Number

    /// Parses a number as a System.Int16 enum.
    let int64Enum<'a when 'a : enum<int64>> =
        custom (_.TryGetInt64 >> parseEnum<'a, int64>) ExpectedKind.Number

    /// Parses a number as a System.UInt16 enum.
    let uint16Enum<'a when 'a : enum<uint16>> =
        custom (_.TryGetUInt16 >> parseEnum<'a, uint16>) ExpectedKind.Number

    /// Parses a number as a System.UInt32 enum.
    let uint32Enum<'a when 'a : enum<uint32>> =
        custom (_.TryGetUInt32 >> parseEnum<'a, uint32>) ExpectedKind.Number

    /// Parses a number as a System.UInt64 enum.
    let uint64Enum<'a when 'a : enum<uint64>> =
        custom (_.TryGetUInt64 >> parseEnum<'a, uint64>) ExpectedKind.Number

    /// Parses a number as a System.Byte enum.
    let byteEnum<'a when 'a : enum<byte>> =
        custom (_.TryGetByte >> parseEnum<'a, byte>) ExpectedKind.Number

    /// Parses a number as a System.SByte enum.
    let sbyteEnum<'a when 'a : enum<sbyte>> =
        custom (_.TryGetSByte >> parseEnum<'a, sbyte>) ExpectedKind.Number

    // Date and time

    /// Parses a string as System.TimeOnly.
    let timeOnly =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error None
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeOnlyExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// Parses a string as System.TimeSpan.
    let timeSpan =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error None
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let timeSpanExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParseExact(str, format, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// Parses a string as System.DateOnly (ISO 8601).
    let dateOnly =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error None
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateOnlyExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// Parses a string as System.DateTime (ISO 8601).
    let dateTime = custom (_.TryGetDateTime >> tryParse) ExpectedKind.String

    /// Parses a string as System.DateTime (ISO 8601) and converts it to UTC.
    let dateTimeUtc =
        custom (fun element ->
            match element.TryGetDateTime() with
            | true, dateTime -> Ok <| dateTime.ToUniversalTime()
            | _ -> Error None
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTime.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTime -> Ok dateTime
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// Parses a string as System.DateTimeOffset (ISO 8601).
    let dateTimeOffset = custom (_.TryGetDateTimeOffset >> tryParse) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact (format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTimeOffset.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

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
                    ArrayItem (array.Count, element, error)
                    |> Error
            | _ ->
                InvalidKind (ExpectedKind.Array, element)
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
    let seq parser = arr Seq.ofSeq parser

    /// <summary>Parses an array at a specific index.</summary>
    /// <param name="i">The index to parse in the array.</param>
    /// <param name="parser">The parser used for the element.</param>
    let index i (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when i >= 0 && arrayLength >= i + 1 ->
                match element.Item i |> parser with
                | Ok x -> Ok x
                | Error e ->
                    ArrayItem (i, element, e)
                    |> Error
            | Kind.Array ->
                ArrayItem (i, element, ArrayLength)
                |> Error
            | _ ->
                InvalidKind (ExpectedKind.Array, element)
                |> Error
        |> id

    // Key/Value

    let inline private getDuplicateKey (pairs:('k * 'v) seq) =
        let seen = HashSet<'k>()
        pairs
        |> Seq.tryPick (fun (k, _) ->
            if seen.Add(k) then None
            else Some k
        )

    let inline private keyValue convert (parser:Parser<_>) : Parser<'b> =
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
                    | Error e -> error <- Some <| KeyValue (enumerator.Current.Name, e, element)

                match error with
                | None ->
                    match getDuplicateKey array with
                    | Some key -> Error <| InvalidValue (Some <| Error.duplicateKey key, typeof<'b>, element)
                    | None -> Ok <| convert array
                | Some e -> Error e
            | _ -> Error <| InvalidKind (ExpectedKind.Object, element)

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
    let tuples parser = keyValue Seq.ofSeq parser

    /// <summary>Parses an object's keys as System.ExpectedKind.String Microsoft.FSharp.Collections.seq</summary>
    let keys = keyValue (Seq.ofSeq >> Seq.map fst) none

    // Tuples

    let inline private parseItem i parser (element:JsonElement) =
        element.Item i
        |> parser
        |> Result.mapError (fun e -> ArrayItem (i, element, e))

    let inline private tuple length fn : Parser<'b> =
        fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when arrayLength = length -> fn element
            | Kind.Array ->
                let details = Error.invalidTuple length arrayLength
                InvalidValue (Some details, typeof<'b>, element)
                |> Error
            | _ ->
                InvalidKind (ExpectedKind.Array, element)
                |> Error

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

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = custom (_.ValueKind >> Ok) ExpectedKind.Any

    /// Parses an element as System.Text.Json.JsonElement.
    let element = custom (_.Clone() >> Ok) ExpectedKind.Any

    /// Parses an element's raw text as System.ExpectedKind.String.
    let rawText = custom (_.GetRawText() >> Ok) ExpectedKind.Any

    /// Parses an array's length as System.Int32.
    let arrayLength = custom (_.GetArrayLength() >> Ok) ExpectedKind.Array

    /// Parses an object's property count as System.Int32.
    let propertyCount = custom (_.GetPropertyCount() >> Ok) ExpectedKind.Object