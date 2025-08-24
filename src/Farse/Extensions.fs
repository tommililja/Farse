namespace Farse

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes

type internal Kind = JsonValueKind

module internal JsonSerializerOptions =

    let preset = JsonSerializerOptions(WriteIndented = true)

module internal JsonElement =

    let inline getProperty (name:string) (element:JsonElement) =
        element.TryGetProperty(name) |> snd

    let inline tryGetProperty (name:string) (element:JsonElement) =
        match element.TryGetProperty(name) with
        | true, prop when prop.ValueKind <> Kind.Null -> Some prop
        | _ -> None

    let getKind (element:JsonElement) =
        element.ValueKind

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

    // Parsing

    let inline private tryParse x =
        match x with
        | true, x -> Some x
        | _ -> None

    let inline tryGetInt (element:JsonElement) =
        tryParse <| element.TryGetInt32()

    let inline tryGetInt16 (element:JsonElement) =
        tryParse <| element.TryGetInt16()

    let inline tryGetInt64 (element:JsonElement) =
        tryParse <| element.TryGetInt64()

    let inline tryGetUInt16 (element:JsonElement) =
        tryParse <| element.TryGetUInt16()

    let inline tryGetUInt32 (element:JsonElement) =
        tryParse <| element.TryGetUInt32()

    let inline tryGetUInt64 (element:JsonElement) =
        tryParse <| element.TryGetUInt64()

    let inline tryGetFloat (element:JsonElement) =
        tryParse <| element.TryGetDouble()

    let inline tryGetFloat32 (element:JsonElement) =
        tryParse <| element.TryGetSingle()

    let inline tryGetDecimal (element:JsonElement) =
        tryParse <| element.TryGetDecimal()

    let inline tryGetByte (element:JsonElement) =
        tryParse <| element.TryGetByte()

    let inline tryGetSByte (element:JsonElement) =
        tryParse <| element.TryGetSByte()

    let inline tryGetChar (element:JsonElement) =
        match element.GetString() with
        | str when str.Length = 1 -> Some str[0]
        | _ -> None

    let inline tryGetString (element:JsonElement) =
        Some <| element.GetString()

    let inline tryGetBool (element:JsonElement) =
        Some <| element.GetBoolean()

    let inline tryGetGuid (element:JsonElement) =
        tryParse <| element.TryGetGuid()

    let inline tryGetUnit _ = Some ()

    let inline tryGetTimeOnly (element:JsonElement) =
        let timeString = element.GetString()
        tryParse <| TimeOnly.TryParse(timeString, CultureInfo.InvariantCulture)

    let inline tryGetTimeOnlyExact (format:string) (element:JsonElement) =
        let timeString = element.GetString()
        tryParse <| TimeOnly.TryParseExact(timeString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    let inline tryGetDateOnly (element:JsonElement) =
        let dateString = element.GetString()
        tryParse <| DateOnly.TryParse(dateString, CultureInfo.InvariantCulture)

    let inline tryGetDateOnlyExact (format:string) (element:JsonElement) =
        let dateString = element.GetString()
        tryParse <| DateOnly.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    let inline tryGetDateTime (element:JsonElement) =
        tryParse <| element.TryGetDateTime()

    let inline tryGetDateTimeUtc (element:JsonElement) =
        match element.TryGetDateTime() with
        | true, dateTime -> Some <| dateTime.ToUniversalTime()
        | _ -> None

    let inline tryGetDateTimeExact (format:string) (element:JsonElement) =
        let dateString = element.GetString()
        tryParse <| DateTime.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    let inline tryGetDateTimeOffset (element:JsonElement) =
        tryParse <| element.TryGetDateTimeOffset()

    let inline tryGetDateTimeOffsetExact (format:string) (element:JsonElement)  =
        let dateString = element.GetString()
        tryParse <| DateTimeOffset.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    let inline tryGetArrayLength (element:JsonElement) =
        Some <| element.GetArrayLength()

    let inline tryGetKind (element:JsonElement) =
        Some <| element.ValueKind

    let inline tryGetElement (element:JsonElement) =
        Some <| element.Clone()

module internal JsonNode =

    let create x =
        JsonValue.Create<'a>(x)
            .Root

    let asString (node:JsonNode) =
        if node = null
        then "null"
        else node.ToJsonString(JsonSerializerOptions.preset)

module internal String =

    let startsWith (startsWith:string) (str:string) =
        str.StartsWith(startsWith)

[<AutoOpen>]
module internal ActivePatterns =

    let (|String|Invalid|) (str:string) =
        if String.IsNullOrEmpty(str)
        then Invalid
        else String str

    let (|Flat|Nested|) (str:string) =
        if str.Contains('.')
        then Nested (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
        else Flat str