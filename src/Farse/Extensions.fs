namespace Farse

open System
open System.Collections.Generic
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes

type ExpectedKind =
    | Undefined
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null
    | Any

module ExpectedKind =

    let fromJsonValueKind = function
        | JsonValueKind.Undefined -> Undefined
        | JsonValueKind.Object -> Object
        | JsonValueKind.Array -> Array
        | JsonValueKind.String -> String
        | JsonValueKind.Number -> Number
        | JsonValueKind.True -> Bool
        | JsonValueKind.False -> Bool
        | JsonValueKind.Null -> Null
        | _ -> raise <| ArgumentOutOfRangeException()

    let asString = function
        | Undefined -> "Undefined"
        | Object -> "Object"
        | Array -> "Array"
        | String -> "String"
        | Number -> "Number"
        | Bool -> "Bool"
        | Null -> "Null"
        | Any -> "Any"

module JsonElement =

    let inline isBool (element:JsonElement) =
        element.ValueKind = JsonValueKind.True
        || element.ValueKind = JsonValueKind.False

    let inline isExpectedKind (expectedKind:ExpectedKind) (element:JsonElement) =
        element.ValueKind
        |> ExpectedKind.fromJsonValueKind
        |> (=) expectedKind

[<AutoOpen>]
module internal Extensions =

    type Kind = JsonValueKind

    module Kind =

        let inline isBool (kind:Kind) =
            kind = Kind.True || kind = Kind.False

        let asString kind =
            if isBool kind then "Bool"
            else string kind

    module JsonSerializerOptions =

        let preset = JsonSerializerOptions (
            WriteIndented = true,
            IndentSize = 4
        )

    module JsonElement =

        let inline getProperty (name:string) (element:JsonElement) =
            element.TryGetProperty(name) |> snd

        let inline tryGetProperty (name:string) (element:JsonElement) =
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Some prop
            | _ -> None

        let asString (element:JsonElement) =
            element.ToString()

        let asRawString (element:JsonElement) =
            element.GetRawText()

        let asJsonString (element:JsonElement) =
            JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

        // Parsing

        let inline private tryParse parse =
            match parse () with
            | true, x -> Ok x
            | _ -> Error None

        let inline private parseEnum<'a, 'b when 'a: enum<'b>> parse =
            let enumType = typeof<'a>
            match parse () with
            | true, x when Enum.IsDefined(enumType, x) ->
                Ok <| LanguagePrimitives.EnumOfValue<'b, 'a> x
            | true, _ -> Error <| Some $"Expected %s{enumType.Name} enum."
            | _ -> Error None

        let inline tryGetInt (element:JsonElement) =
            tryParse element.TryGetInt32

        let inline tryGetIntEnum<'a when 'a: enum<int>> (element:JsonElement) =
            parseEnum<'a, int> element.TryGetInt32

        let inline tryGetInt16 (element:JsonElement) =
            tryParse element.TryGetInt16

        let inline tryGetInt16Enum<'a when 'a: enum<int16>> (element:JsonElement) =
            parseEnum<'a, int16> element.TryGetInt16

        let inline tryGetInt64 (element:JsonElement) =
            tryParse element.TryGetInt64

        let inline tryGetInt64Enum<'a when 'a: enum<int64>> (element:JsonElement) =
            parseEnum<'a, int64> element.TryGetInt64

        let inline tryGetUInt16 (element:JsonElement) =
            tryParse element.TryGetUInt16

        let inline tryGetUInt16Enum<'a when 'a: enum<uint16>> (element:JsonElement) =
            parseEnum<'a, uint16> element.TryGetUInt16

        let inline tryGetUInt32 (element:JsonElement) =
            tryParse element.TryGetUInt32

        let inline tryGetUInt32Enum<'a when 'a: enum<uint32>> (element:JsonElement) =
            parseEnum<'a, uint32> element.TryGetUInt32

        let inline tryGetUInt64 (element:JsonElement) =
            tryParse element.TryGetUInt64

        let inline tryGetUInt64Enum<'a when 'a: enum<uint64>> (element:JsonElement) =
            parseEnum<'a, uint64> element.TryGetUInt64

        let inline tryGetFloat (element:JsonElement) =
            tryParse element.TryGetDouble

        let inline tryGetFloat32 (element:JsonElement) =
            tryParse element.TryGetSingle

        let inline tryGetDecimal (element:JsonElement) =
            tryParse element.TryGetDecimal

        let inline tryGetByte (element:JsonElement) =
            tryParse element.TryGetByte

        let inline tryGetByteEnum<'a when 'a: enum<byte>> (element:JsonElement) =
            parseEnum<'a, byte> element.TryGetByte

        let inline tryGetSByte (element:JsonElement) =
            tryParse element.TryGetSByte

        let inline tryGetSByteEnum<'a when 'a: enum<sbyte>> (element:JsonElement) =
            parseEnum<'a, sbyte> element.TryGetSByte

        let inline tryGetChar (element:JsonElement) =
            match element.GetString() with
            | str when str.Length = 1 -> Ok str[0]
            | _ -> Error <| Some "Expected a string length of 1."

        let inline tryGetString (element:JsonElement) =
            Ok <| element.GetString()

        let inline tryGetBool (element:JsonElement) =
            Ok <| element.GetBoolean()

        let inline tryGetGuid (element:JsonElement) =
            tryParse element.TryGetGuid

        let inline tryGetUnit _ = Ok ()

        let inline tryGetTimeOnly (element:JsonElement) =
            let str = element.GetString()
            tryParse (fun _ -> TimeOnly.TryParse(str, CultureInfo.InvariantCulture))

        let inline tryGetTimeOnlyExact (format:string) (element:JsonElement) =
            let str = element.GetString()
            match TimeOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error <| Some $"Expected %s{format}."

        let inline tryGetTimeSpan (element:JsonElement) =
            let str = element.GetString()
            tryParse (fun _ -> TimeSpan.TryParse(str, CultureInfo.InvariantCulture))

        let inline tryGetTimeSpanExact (format:string) (element:JsonElement) =
            let str = element.GetString()
            match TimeSpan.TryParseExact(str, format, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error <| Some $"Expected %s{format}."

        let inline tryGetDateOnly (element:JsonElement) =
            let str = element.GetString()
            tryParse (fun _ -> DateOnly.TryParse(str, CultureInfo.InvariantCulture))

        let inline tryGetDateOnlyExact (format:string) (element:JsonElement) =
            let str = element.GetString()
            match DateOnly.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error <| Some $"Expected %s{format}."

        let inline tryGetDateTime (element:JsonElement) =
            tryParse element.TryGetDateTime

        let inline tryGetDateTimeUtc (element:JsonElement) =
            match element.TryGetDateTime() with
            | true, dateTime -> Ok <| dateTime.ToUniversalTime()
            | _ -> Error None

        let inline tryGetDateTimeExact (format:string) (element:JsonElement) =
            let str = element.GetString()
            match DateTime.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTime -> Ok dateTime
            | _ -> Error <| Some $"Expected %s{format}."

        let inline tryGetDateTimeOffset (element:JsonElement) =
            tryParse element.TryGetDateTimeOffset

        let inline tryGetDateTimeOffsetExact (format:string) (element:JsonElement)  =
            let str = element.GetString()
            match DateTimeOffset.TryParseExact(str, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error <| Some $"Expected %s{format}."

        let inline tryGetArrayLength (element:JsonElement) =
            Ok <| element.GetArrayLength()

        let inline tryGetPropertyCount (element:JsonElement) =
            Ok <| element.GetPropertyCount()

        let inline tryGetKind (element:JsonElement) =
            Ok element.ValueKind

        let inline tryGetElement (element:JsonElement) =
            Ok <| element.Clone()

        let inline tryGetEnum<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)> (element:JsonElement) : Result<'a, string option> =
            let str = element.GetString()
            match Enum.TryParse<'a>(str, true) with
            | true, enum -> Ok enum
            | _ -> Error None

        let tryGetRawText (element:JsonElement) =
            Ok <| element.GetRawText()

    module JsonNode =

        let create x =
            JsonValue.Create<'a>(x)
                .Root

        let asString (node:JsonNode) =
            if node = null
            then "null"
            else node.ToJsonString(JsonSerializerOptions.preset)

    [<AutoOpen>]
    module ResultBuilder =

        type ResultBuilder() =

            member inline _.Return(x) = Ok x

            member inline _.ReturnFrom(x) = x

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Ok ()

            member inline _.Bind(x, [<InlineIfLambda>] fn) = Result.bind fn x

        let result = ResultBuilder ()

    module ResultOption =

        let inline bind fn = function
            | Ok x ->
                match x with
                | Some x -> fn x
                | None -> Ok None
            | Error e -> Error e

    module String =

        let isNotEmpty =
            String.IsNullOrWhiteSpace
            >> not

    module Seq =

        let inline ofResizeArray (x:ResizeArray<_>) = x :> seq<_>

    module KeyValuePairs =

        let inline ofSeq x = Seq.map KeyValuePair.Create x

    module Dictionary =

        let inline ofSeq x = dict x

    [<AutoOpen>]
    module ActivePatterns =

        let (|Flat|Nested|) (str:string) =
            if str.Contains('.')
            then Nested (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Flat str