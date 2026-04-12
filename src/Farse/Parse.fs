namespace Farse

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Numerics
open System.Text.Json

module Parse =

    /// <summary>Always succeeds and returns FSharp.Core.Unit.</summary>
    /// <example>do! "prop" &amp;= Parse.none</example>
    let none = Parser.from ()

    /// <summary>Parses an optional value with the given parser but returns a default value when null.</summary>
    /// <example>let! int = "prop" &amp;= Parse.nil Parse.int 1</example>
    /// <param name="x">The default value.</param>
    let nil (Parser parse) x =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok x
            | _ -> parse element
        )

    /// <summary>Parses an optional value with the given parser.</summary>
    /// <example>let! int = "prop" &amp;= Parse.optional Parse.int</example>
    let optional (Parser parse) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok None
            | _ ->
                parse element
                |> Result.map Some
        )

    /// <summary>Validates a parsed value with the given function.</summary>
    /// <example>let! type' = "prop" &amp;= Parse.valid Parse.string Type.fromString</example>
    /// <remarks>Produces detailed error messages when validation fails.</remarks>
    /// <param name="fn">The validation function.</param>
    let valid (Parser parse) fn : Parser<'r> =
        Parser (fun element ->
            parse element
            |> Result.bind (fun x ->
                fn x
                |> Result.mapError (fun msg ->
                    element
                    |> ParseError.validation msg typeof<'r> $"%A{x}"
                    |> List.singleton
                )
            )
        )

    /// <summary>Creates a custom parser from the given function.</summary>
    /// <example>
    ///     let parser =
    ///         Parse.custom (fun element ->
    ///             let string = element.GetString()
    ///             match InstantPattern.General.Parse(string) with
    ///             | result when result.Success -> Ok result.Value
    ///             | result -> Error result.Exception.Message
    ///         ) ExpectedKind.String
    /// </example>
    /// <remarks>Produces detailed error messages when validation fails.</remarks>
    /// <param name="fn">The parsing function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let custom fn expectedKind : Parser<'r> =
        Parser (fun element ->
            let isExpectedKind =
                match expectedKind with
                | ExpectedKind.Any -> true
                | ExpectedKind.Bool -> JsonElement.isBool element
                | kind ->
                    element.ValueKind
                    |> ExpectedKind.fromKind
                    |> (=) kind

            if isExpectedKind then
                try
                    fn element
                    |> Result.mapError (fun msg ->
                        element
                        |> ParseError.invalid msg typeof<'r>
                        |> List.singleton
                    )
                with ex ->
                    element
                    |> ParseError.invalidEx ex.Message typeof<'r> ex
                    |> Error.list
            else
                element
                |> ParseError.expectedKind expectedKind JsonPath.empty typeof<'r>
                |> Error.list
        )

    // Basic types

    let inline private tryParse fn : Result<'r, string> =
        match fn () with
        | true, x -> Ok x
        | _ -> Error $"Invalid %s{typeof<'r>.Name}."

    /// <summary>Parses a number as System.Int32.</summary>
    /// <example>let! int = "prop" &amp;= Parse.int</example>
    let int = custom (_.TryGetInt32 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Int16.</summary>
    /// <example>let! int16 = "prop" &amp;= Parse.int16</example>
    let int16 = custom (_.TryGetInt16 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Int64.</summary>
    /// <example>let! int64 = "prop" &amp;= Parse.int64</example>
    let int64 = custom (_.TryGetInt64 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt16.</summary>
    /// <example>let! uint64 = "prop" &amp;= Parse.uint16</example>
    let uint16 = custom (_.TryGetUInt16 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt32.</summary>
    /// <example>let! uint32 = "prop" &amp;= Parse.uint32</example>
    let uint32 = custom (_.TryGetUInt32 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt64.</summary>
    /// <example>let! uint64 = "prop" &amp;= Parse.uint64</example>
    let uint64 = custom (_.TryGetUInt64 >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Double.</summary>
    /// <example>let! float = "prop" &amp;= Parse.float</example>
    let float = custom (_.TryGetDouble >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Single.</summary>
    /// <example>let! float32 = "prop" &amp;= Parse.float32</example>
    let float32 = custom (_.TryGetSingle >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Decimal.</summary>
    /// <example>let! decimal = "prop" &amp;= Parse.decimal</example>
    let decimal = custom (_.TryGetDecimal >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.Byte.</summary>
    /// <example>let! byte = "prop" &amp;= Parse.byte</example>
    let byte = custom (_.TryGetByte >> tryParse) ExpectedKind.Number

    /// <summary>Parses a number as System.SByte.</summary>
    /// <example>let! sbyte = "prop" &amp;= Parse.sbyte</example>
    let sbyte = custom (_.TryGetSByte >> tryParse) ExpectedKind.Number

    /// <summary>Parses a string as System.Char.</summary>
    /// <example>let! char = "prop" &amp;= Parse.char</example>
    let char =
        custom (fun element ->
            match element.GetString() with
            | str when str.Length = 1 -> Ok str[0]
            | _ -> Error "Expected a string length of 1."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.String.</summary>
    /// <example>let! string = "prop" &amp;= Parse.string</example>
    let string = custom (_.GetString() >> Ok) ExpectedKind.String

    /// <summary>Parses a string as System.Numerics.BigInteger.</summary>
    /// <example>let! bigInt = "prop" &amp;= Parse.bigInt</example>
    let bigInt =
        custom (fun element ->
            let str = element.GetString()
            match BigInteger.TryParse(str, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, bigInt -> Ok bigInt
            | false, _ -> Error "Invalid BigInteger."
        ) ExpectedKind.String

    /// <summary>Parses a bool as System.Boolean.</summary>
    /// <example>let! bool = "prop" &amp;= Parse.bool</example>
    let bool = custom (_.GetBoolean() >> Ok) ExpectedKind.Bool

    /// <summary>Parses a string as System.Guid.</summary>
    /// <example>let! guid = "prop" &amp;= Parse.guid</example>
    let guid = custom (_.TryGetGuid >> tryParse) ExpectedKind.String

    /// <summary>Parses null as FSharp.Core.Unit.</summary>
    /// <example>do! "prop" &amp;= Parse.unit</example>
    let unit = custom (ignore >> Ok) ExpectedKind.Null

    // Enums

    let inline private parseEnum<'a, 'b when 'a: enum<'b>> fn =
        let enumType = typeof<'a>
        match fn () with
        | true, x when Enum.IsDefined(enumType, x) -> Ok <| LanguagePrimitives.EnumOfValue<'b, 'a> x
        | true, _ -> Error $"Expected %s{enumType.Name} enum."
        | _ -> Error $"Invalid %s{enumType.Name}."

    /// <summary>Parses a string as an enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.enum&lt;Enum&gt;</example>
    let enum<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)> =
        custom (fun element ->
            let str = element.GetString()
            match Enum.TryParse<'a>(str, true) with
            | true, enum -> Ok enum
            | _ -> Error $"Invalid %s{typeof<'a>.Name}."
        ) ExpectedKind.String

    /// <summary>Parses a number as a System.Int32 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.intEnum&lt;Enum&gt;</example>
    let intEnum<'a when 'a : enum<int>> =
        custom (_.TryGetInt32 >> parseEnum<'a, int>) ExpectedKind.Number

    /// <summary>Parses a number as a System.Int16 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.int16Enum&lt;Enum&gt;</example>
    let int16Enum<'a when 'a : enum<int16>> =
        custom (_.TryGetInt16 >> parseEnum<'a, int16>) ExpectedKind.Number

    /// <summary>Parses a number as a System.Int64 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.int64Enum&lt;Enum&gt;</example>
    let int64Enum<'a when 'a : enum<int64>> =
        custom (_.TryGetInt64 >> parseEnum<'a, int64>) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt16 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.uint16Enum&lt;Enum&gt;</example>
    let uint16Enum<'a when 'a : enum<uint16>> =
        custom (_.TryGetUInt16 >> parseEnum<'a, uint16>) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt32 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.uint32Enum&lt;Enum&gt;</example>
    let uint32Enum<'a when 'a : enum<uint32>> =
        custom (_.TryGetUInt32 >> parseEnum<'a, uint32>) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt64 enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.uint64Enum&lt;Enum&gt;</example>
    let uint64Enum<'a when 'a : enum<uint64>> =
        custom (_.TryGetUInt64 >> parseEnum<'a, uint64>) ExpectedKind.Number

    /// <summary>Parses a number as a System.Byte enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.byteEnum&lt;Enum&gt;</example>
    let byteEnum<'a when 'a : enum<byte>> =
        custom (_.TryGetByte >> parseEnum<'a, byte>) ExpectedKind.Number

    /// <summary>Parses a number as a System.SByte enum.</summary>
    /// <example>let! enum = "prop" &amp;= Parse.sbyteEnum&lt;Enum&gt;</example>
    let sbyteEnum<'a when 'a : enum<sbyte>> =
        custom (_.TryGetSByte >> parseEnum<'a, sbyte>) ExpectedKind.Number

    // Date and time

    /// <summary>Parses a string as System.TimeOnly.</summary>
    /// <example>let! timeOnly = "prop" &amp;= Parse.timeOnly</example>
    let timeOnly =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error "Invalid TimeOnly."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <example>let! timeOnly = "prop" &amp;= Parse.timeOnlyExact "HH:mm:ss"</example>
    /// <param name="format">The required format.</param>
    let timeOnlyExact ([<StringSyntax("TimeOnlyFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeSpan.</summary>
    /// <example>let! timeSpan = "prop" &amp;= Parse.timeSpan</example>
    let timeSpan =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParse(str, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error "Invalid TimeSpan."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <example>let! timeSpan = "prop" &amp;= Parse.timeSpanExact "c"</example>
    /// <param name="format">The required format.</param>
    let timeSpanExact ([<StringSyntax("TimeSpanFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match TimeSpan.TryParseExact(str, format, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateOnly (ISO 8601).</summary>
    /// <example>let! dateOnly = "prop" &amp;= Parse.dateOnly</example>
    let dateOnly =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParse(str, CultureInfo.InvariantCulture) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error "Invalid DateOnly."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <example>let! dateOnly = "prop" &amp;= Parse.dateOnlyExact "yyyy-MM-dd"</example>
    /// <param name="format">The required format.</param>
    let dateOnlyExact ([<StringSyntax("DateOnlyFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime (ISO 8601).</summary>
    /// <example>let! dateTime = "prop" &amp;= Parse.dateTime</example>
    let dateTime = custom (_.TryGetDateTime >> tryParse) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime (ISO 8601) and converts it to UTC.</summary>
    /// <example>let! dateTime = "prop" &amp;= Parse.dateTimeUtc</example>
    let dateTimeUtc =
        custom (fun element ->
            match element.TryGetDateTime() with
            | true, dateTime -> Ok <| dateTime.ToUniversalTime()
            | _ -> Error "Invalid DateTime."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <example>let! dateTime = "prop" &amp;= Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"</example>
    /// <param name="format">The required format.</param>
    let dateTimeExact ([<StringSyntax("DateTimeFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTime.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTime -> Ok dateTime
            | _ -> Error $"Expected '%s{format}'."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset (ISO 8601).</summary>
    /// <example>let! dateTimeOffset = "prop" &amp;= Parse.dateTimeOffset</example>
    let dateTimeOffset = custom (_.TryGetDateTimeOffset >> tryParse) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <example>let! dateTimeOffset = "prop" &amp;= Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm:ss zzz"</example>
    /// <param name="format">The required format.</param>
    let dateTimeOffsetExact ([<StringSyntax("DateTimeFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTimeOffset.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error $"Expected '%s{format}'."
        ) ExpectedKind.String

    // Sequences

    let inline private parseIndex n (Parser parse) (element:JsonElement) =
        parse (element.Item n)
        |> Result.mapError (fun errors ->
            errors
            |> List.map (ParseError.withIndex n)
        )

    let inline private arr ([<InlineIfLambda>] convert) (Parser parse) : Parser<'r> =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Array ->
                let mutable error = false
                let mutable enumerator = element.EnumerateArray()
                let mutable i = 0

                let items =
                    element.GetArrayLength()
                    |> Array.zeroCreate

                while not error && enumerator.MoveNext() do
                    match parse enumerator.Current with
                    | Ok x -> items[i] <- x; i <- i + 1
                    | Error _ -> error <- true

                match error with
                | false -> Ok <| convert items
                | true ->
                    let errors = ResizeArray()
                    let elements = element.EnumerateArray () |> Seq.indexed

                    for i, element in elements do
                        match parse element with
                        | Ok _ -> ()
                        | Error list ->
                            list
                            |> List.iter (fun error ->
                                error
                                |> ParseError.withIndex i
                                |> errors.Add
                            )

                    errors
                    |> List.ofSeq
                    |> Error
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Array JsonPath.empty typeof<'r>
                |> Error.list
        )

    /// <summary>Parses an array as Microsoft.FSharp.Collections.list.</summary>
    /// <example>let! list = "prop" &amp;= Parse.list Parse.int</example>
    /// <param name="parser">The parser used for every element.</param>
    let list parser = arr List.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Core.array.</summary>
    /// <example>let! array = "prop" &amp;= Parse.array Parse.int</example>
    /// <param name="parser">The parser used for every element.</param>
    let array parser = arr Array.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.Set.</summary>
    /// <example>let! set = "prop" &amp;= Parse.set Parse.int</example>
    /// <param name="parser">The parser used for every element.</param>
    let set parser = arr Set.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.seq.</summary>
    /// <example>let! seq = "prop" &amp;= Parse.seq Parse.int</example>
    /// <param name="parser">The parser used for every element.</param>
    let seq parser = arr Seq.ofSeq parser

    /// <summary>Parses an array at a specific index.</summary>
    /// <example>let! int = "prop" &amp;= Parse.index 0 Parse.int</example>
    /// <param name="n">The index to parse in the array.</param>
    /// <param name="parser">The parser used for the element.</param>
    let index n parser : Parser<'r> =
        Parser (fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when n >= 0 && arrayLength >= n + 1 -> parseIndex n parser element
            | Kind.Array ->
                element
                |> ParseError.invalidIndex n typeof<'r>
                |> Error.list
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Array JsonPath.empty typeof<'r>
                |> Error.list
        )

    // Key/Value

    let inline private getDuplicateKeys (pairs:('k * 'v) seq) =
        let seen = HashSet<'k>()
        pairs
        |> Seq.choose (fun (k, _) ->
            if seen.Add(k) then None
            else Some k
        )
        |> Seq.distinct
        |> List.ofSeq

    let inline private keyValue ([<InlineIfLambda>] convert) (Parser parse) : Parser<'r> =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let mutable error = false
                let mutable enumerator = element.EnumerateObject()
                let mutable i = 0

                let items =
                    element.GetPropertyCount()
                    |> Array.zeroCreate

                while not error && enumerator.MoveNext() do
                    let current = enumerator.Current
                    match parse current.Value with
                    | Ok x -> items[i] <- current.Name, x; i <- i + 1
                    | Error _ -> error <- true

                match error with
                | false ->
                    match getDuplicateKeys items with
                    | [] -> Ok <| convert items
                    | keys ->
                        keys
                        |> List.map (fun key -> ParseError.duplicateKey key typeof<'r> element)
                        |> Error
                | true ->
                    let errors = ResizeArray()
                    let elements = element.EnumerateObject ()

                    for element in elements do
                        match parse element.Value with
                        | Ok _ -> ()
                        | Error list ->
                            list
                            |> List.iter (fun error ->
                                error
                                |> ParseError.withProp element.Name
                                |> errors.Add
                            )

                    errors
                    |> List.ofSeq
                    |> Error
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Array JsonPath.empty typeof<'r>
                |> Error.list
        )

    /// <summary>Parses an object's properties as Microsoft.FSharp.Collections.Map.</summary>
    /// <example>let! map = "prop" &amp;= Parse.map Parse.int</example>
    /// <param name="parser">The parser used for every property value.</param>
    let map parser = keyValue Map.ofSeq parser

    /// <summary>Parses an object's properties as System.Collections.Generic.IDictionary.</summary>
    /// <example>let! dict = "prop" &amp;= Parse.dict Parse.int</example>
    /// <param name="parser">The parser used for every property value.</param>
    let dict parser = keyValue dict parser

    /// <summary>Parses an object's properties as System.Collections.Generic.KeyValuePair seq.</summary>
    /// <example>let! keyValuePairs = "prop" &amp;= Parse.keyValuePairs Parse.int</example>
    /// <param name="parser">The parser used for every property value.</param>
    let keyValuePairs parser = keyValue (Seq.map KeyValuePair.Create) parser

    /// <summary>Parses an object's properties as tuple Microsoft.FSharp.Collections.seq.</summary>
    /// <example>let! tuples = "prop" &amp;= Parse.tuples Parse.int</example>
    /// <param name="parser">The parser used for every property value.</param>
    let tuples parser = keyValue Seq.ofSeq parser

    /// <summary>Parses an object's keys as System.String Microsoft.FSharp.Collections.seq.</summary>
    /// <example>let! keys = "prop" &amp;= Parse.keys</example>
    let keys = keyValue (Seq.map fst) none

    // Tuples

    let inline private tuple expected ([<InlineIfLambda>] fn) : Parser<'r> =
        Parser (fun (element:JsonElement) ->
            let actual = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when actual = expected -> fn element
            | Kind.Array ->
                element
                |> ParseError.invalidTuple actual expected typeof<'r>
                |> Error.list
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Array JsonPath.empty typeof<'r>
                |> Error.list
        )

    /// <summary>Parses an array with two values as a tuple.</summary>
    /// <example>let! tuple = "prop" &amp;= Parse.tuple2 Parse.int Parse.string</example>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    let tuple2 a b =
        tuple 2 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e

                return a, b
            }
        )

    /// <summary>Parses an array with three values as a tuple of three.</summary>
    /// <example>let! tuple = "prop" &amp;= Parse.tuple3 Parse.int Parse.string Parse.bool</example>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    /// <param name="c">The parser used for the third value.</param>
    let tuple3 a b c =
        tuple 3 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e
                and! c = parseIndex 2 c e

                return a, b, c
            }
        )

    /// <summary>Parses an array with four values as a tuple of four.</summary>
    /// <example>let! tuple = "prop" &amp;= Parse.tuple4 Parse.int Parse.string Parse.bool Parse.float</example>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    /// <param name="c">The parser used for the third value.</param>
    /// <param name="d">The parser used for the fourth value.</param>
    let tuple4 a b c d =
        tuple 4 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e
                and! c = parseIndex 2 c e
                and! d = parseIndex 3 d e

                return a, b, c, d
            }
        )

    /// <summary>Parses an array with five values as a tuple of five.</summary>
    /// <example>let! tuple = "prop" &amp;= Parse.tuple5 Parse.int Parse.string Parse.bool Parse.float Parse.int</example>
    /// <param name="a">The parser used for the first value.</param>
    /// <param name="b">The parser used for the second value.</param>
    /// <param name="c">The parser used for the third value.</param>
    /// <param name="d">The parser used for the fourth value.</param>
    /// <param name="e">The parser used for the fifth value.</param>
    let tuple5 a b c d e =
        tuple 5 (fun el ->
            result {
                let! a = parseIndex 0 a el
                and! b = parseIndex 1 b el
                and! c = parseIndex 2 c el
                and! d = parseIndex 3 d el
                and! e = parseIndex 4 e el

                return a, b, c, d, e
            }
        )

    // One-of

    /// <summary>Parses an object based on a discriminator property.</summary>
    /// <example>let! x = Parse.oneOf "type" [ "A", a; "B", b ]</example>
    /// <param name="name">The discriminator property name.</param>
    /// <param name="parsers">The parsers to match the discriminator property against.</param>
    let oneOf name parsers : Parser<'r> =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Object ->
                let (Parser parse) = Prop.req name string
                parse element
                |> Result.bind (fun x ->
                    let parser = List.tryFind (fun (key, _) -> key = x) parsers
                    match parser with
                    | Some (_, Parser parse) -> parse element
                    | None ->
                        element
                        |> ParseError.invalidOneOf x typeof<'r>
                        |> Error.list
                )
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Object JsonPath.empty typeof<'r>
                |> Error.list
        )

    /// <summary>Parses an element by trying each parser in order, returning the first success.</summary>
    /// <example>let! x = Parse.attempt [ a; b ]</example>
    /// <param name="parsers">The list of parsers to try.</param>
    let attempt parsers : Parser<'r> =
        Parser (fun element ->
            let rec loop errors = function
                | [] ->
                    element
                    |> ParseError.attempt errors typeof<'r>
                    |> Error.list
                | Parser parse :: rest ->
                    match parse element with
                    | Ok x -> Ok x
                    | Error _ -> loop (errors + 1) rest
            loop 0 parsers
        )

    // Json

    /// <summary>Parses an element's kind as System.Text.Json.JsonValueKind.</summary>
    /// <example>let! jsonValueKind = "prop" &amp;= Parse.kind</example>
    let kind = custom (_.ValueKind >> Ok) ExpectedKind.Any

    /// <summary>Parses an element as System.Text.Json.JsonElement.</summary>
    /// <example>let! jsonElement = "prop" &amp;= Parse.element</example>
    let element = custom (JsonElement.clone >> Ok) ExpectedKind.Any

    /// <summary>Parses an element's raw text as System.String.</summary>
    /// <example>let! string = "prop" &amp;= Parse.rawText</example>
    let rawText = custom (_.GetRawText() >> Ok) ExpectedKind.Any

    /// <summary>Parses an array's length as System.Int32.</summary>
    /// <example>let! int = "prop" &amp;= Parse.arrayLength</example>
    let arrayLength = custom (_.GetArrayLength() >> Ok) ExpectedKind.Array

    /// <summary>Parses an object's property count as System.Int32.</summary>
    /// <example>let! int = "prop" &amp;= Parse.propertyCount</example>
    let propertyCount = custom (_.GetPropertyCount() >> Ok) ExpectedKind.Object