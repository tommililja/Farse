namespace Farse

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Numerics
open System.Text.Json

module Parse =

    /// Always succeeds and returns FSharp.Core.Unit.
    let none = Parser.from ()

    /// <summary>Parses an optional value with the given parser.</summary>
    /// <remarks>Returns a default value when the value is null.</remarks>
    /// <code>let! int = "prop" &amp;= Parse.def Parse.int 1</code>
    /// <typeparam name="parser">The parser used to parse the property value.</typeparam>
    let def (Parser parse) x =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok x
            | _ -> parse element
        )

    /// <summary>Parses an optional value with the given parser.</summary>
    /// <code>let! int = "prop" &amp;= Parse.optional Parse.int</code>
    /// <typeparam name="parser">The parser used to parse the property value.</typeparam>
    let optional (Parser parse) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok None
            | _ ->
                parse element
                |> Result.map Some
        )

    /// <summary>Validates the parsed value with the given function.</summary>
    /// <remarks>Produces detailed error messages when validation fails.</remarks>
    /// <code>let! email = "prop" &amp;= Parse.valid Parse.string Email.fromString</code>
    /// <typeparam name="parser">The parser to validate.</typeparam>
    /// <param name="fn">The validation function.</param>
    let inline valid (Parser parse) ([<InlineIfLambda>] fn) : Parser<'b> =
        Parser (fun element ->
            parse element
            |> Result.bind (fun x ->
                fn x
                |> Result.bindError (fun msg ->
                    Some $"%A{x}"
                    |> InvalidValue.create (Some msg) typeof<'b>
                    |> Error.list
                )
            )
        )

    /// <summary>Creates a custom parser with the given function.</summary>
    /// <remarks>Produces detailed error messages when validation fails.</remarks>
    /// <param name="fn">The parsing function.</param>
    /// <param name="expectedKind">The expected element kind.</param>
    let inline custom ([<InlineIfLambda>] fn) expectedKind : Parser<'a> =
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
                    |> Result.bindError (fun msg ->
                        element
                        |> InvalidValue.fromElement msg typeof<'a>
                        |> Error.list
                    )
                with ex ->
                    element
                    |> InvalidValue.fromElement (Some ex.Message) typeof<'a>
                    |> Error.list
            else
                expectedKind
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

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

    /// Parses a string as System.String.
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
    let timeOnlyExact ([<StringSyntax("TimeOnlyFormat")>] format:string) =
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
    let timeSpanExact ([<StringSyntax("TimeSpanFormat")>] format:string) =
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
    let dateOnlyExact ([<StringSyntax("DateOnlyFormat")>] format:string) =
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
    let dateTimeExact ([<StringSyntax("DateTimeFormat")>] format:string) =
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
    let dateTimeOffsetExact ([<StringSyntax("DateTimeFormat")>] format:string) =
        custom (fun element ->
            let str = element.GetString()
            match DateTimeOffset.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error <| Some $"Expected '%s{format}'."
        ) ExpectedKind.String

    // Sequences

    let inline private parseIndex n (Parser parse) (element:JsonElement) =
        parse (element.Item n)
        |> Result.bindError (fun errors ->
            errors
            |> List.map (ArrayItem.create n)
            |> Error
        )

    let inline private arr ([<InlineIfLambda>] convert) (Parser parse) =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Array ->
                let mutable error = None
                let mutable enumerator = element.EnumerateArray()

                let array =
                    element.GetArrayLength()
                    |> ResizeArray

                while error.IsNone && enumerator.MoveNext() do
                    match parse enumerator.Current with
                    | Ok x -> array.Add(x)
                    | Error errors -> error <- Some (errors, array.Count)

                match error with
                | None -> Ok <| convert (array :> seq<_>)
                | Some (errors, index) ->
                    let array = ResizeArray()
                    array.Add(errors, index)

                    let mutable i = index + 1
                    while enumerator.MoveNext() do
                        match parse enumerator.Current with
                        | Ok _ -> ()
                        | Error errors -> array.Add(errors, i)
                        i <- i + 1

                    array
                    |> List.ofSeq
                    |> List.map (fun (errors, n) ->
                        errors
                        |> List.map (ArrayItem.create n)
                    )
                    |> List.concat
                    |> Error
            | _ ->
                ExpectedKind.Array
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

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
    let seq parser = arr id parser

    /// <summary>Parses an array at a specific index.</summary>
    /// <param name="n">The index to parse in the array.</param>
    /// <param name="parser">The parser used for the element.</param>
    let index n parser =
        Parser (fun (element:JsonElement) ->
            let arrayLength = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when n >= 0 && arrayLength >= n + 1 -> parseIndex n parser element
            | Kind.Array -> Error.list <| ArrayIndex.create n
            | _ ->
                ExpectedKind.Array
                |> InvalidKind.create JsonPath.empty element
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

    let inline private keyValue ([<InlineIfLambda>] convert) (Parser parse) : Parser<'b> =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let mutable error = None
                let mutable enumerator = element.EnumerateObject()

                let array =
                    element.GetPropertyCount()
                    |> ResizeArray

                while error.IsNone && enumerator.MoveNext() do
                    let current = enumerator.Current
                    match parse current.Value with
                    | Ok x -> array.Add(current.Name, x)
                    | Error errors -> error <- Some (errors, current.Name)

                match error with
                | None ->
                    match getDuplicateKeys array with
                    | [] -> Ok <| convert (array :> seq<_>)
                    | keys ->
                        keys
                        |> List.map (fun key ->
                            let message = Error.duplicateKey key
                            element
                            |> InvalidValue.fromElement (Some message) typeof<'b>
                        )
                        |> Error
                | Some (errors, name) ->
                    let array = ResizeArray()
                    array.Add(errors, name)

                    while enumerator.MoveNext() do
                        match parse enumerator.Current.Value with
                        | Ok _ -> ()
                        | Error errors -> array.Add(errors, name)

                    array
                    |> List.ofSeq
                    |> List.map (fun (errors, name) ->
                        errors
                        |> List.map (KeyValue.create name)
                    )
                    |> List.concat
                    |> Error
            | _ ->
                ExpectedKind.Array
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

    /// <summary>Parses an object's properties as Microsoft.FSharp.Collections.Map.</summary>
    /// <param name="parser">The parser used for every property value.</param>
    let map parser = keyValue Map.ofSeq parser

    /// <summary>Parses an object's properties as System.Collections.Generic.IDictionary.</summary>
    /// <param name="parser">The parser used for every property value.</param>
    let dict parser = keyValue dict parser

    /// <summary>Parses an object's properties as System.Collections.Generic.KeyValuePair seq.</summary>
    /// <param name="parser">The parser used for every property value.</param>
    let keyValuePairs parser = keyValue (Seq.map KeyValuePair.Create) parser

    /// <summary>Parses an object's properties as tuple Microsoft.FSharp.Collections.seq.</summary>
    /// <param name="parser">The parser used for every property value.</param>
    let tuples parser = keyValue id parser

    /// <summary>Parses an object's keys as System.String Microsoft.FSharp.Collections.seq.</summary>
    let keys = keyValue (Seq.map fst) none

    // Tuples

    let inline private tuple expected ([<InlineIfLambda>] fn) : Parser<'b> =
        Parser (fun (element:JsonElement) ->
            let actual = element.GetArrayLength()
            match element.ValueKind with
            | Kind.Array when actual = expected -> fn element
            | Kind.Array ->
                let details = Error.invalidTuple expected actual
                element
                |> InvalidValue.fromElement (Some details) typeof<'b>
                |> Error.list
            | _ ->
                ExpectedKind.Array
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

    /// <summary>Parses an array with two values as a tuple.</summary>
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

    // One of

    /// <summary>Parses an object's property with a specific name.</summary>'
    let oneOf disc cases =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Object ->
                result {
                    let (Parser parser) = Prop.req disc string

                    let! disc = parser element
                    match List.tryFind (fun (key, _) -> key = disc) cases with
                    | Some (_, Parser p) -> return! p element
                    | None -> return! Error.list <| Other.create $"Discriminator %A{disc} not found."
                }
            | _ ->
                ExpectedKind.Object
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

    // Json

    /// Parses an element's kind as System.Text.Json.JsonValueKind.
    let kind = custom (_.ValueKind >> Ok) ExpectedKind.Any

    /// Parses an element as System.Text.Json.JsonElement.
    let element = custom (_.Clone() >> Ok) ExpectedKind.Any

    /// Parses an element's raw text as System.String.
    let rawText = custom (_.GetRawText() >> Ok) ExpectedKind.Any

    /// Parses an array's length as System.Int32.
    let arrayLength = custom (_.GetArrayLength() >> Ok) ExpectedKind.Array

    /// Parses an object's property count as System.Int32.
    let propertyCount = custom (_.GetPropertyCount() >> Ok) ExpectedKind.Object