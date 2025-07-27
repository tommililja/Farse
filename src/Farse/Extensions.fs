namespace Farse

open System
open System.Globalization
open System.Runtime.CompilerServices
open System.Text.Json

module internal JsonSerializerOptions =

    let preset = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)

module internal JsonElement =

    let getKind (element:JsonElement) =
        element.ValueKind

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

type internal JsonElementExtensions() =

    [<Extension>]
    static member TryGetChar(element:JsonElement) =
        match element.GetString() with
        | str when str.Length = 1 -> true, str[0]
        | _ -> false, Char.MinValue

    #if NET8_0_OR_GREATER

    [<Extension>]
    static member TryGetTimeOnly(element:JsonElement) =
        let timeString = element.GetString()
        TimeOnly.TryParse(timeString, CultureInfo.InvariantCulture)

    [<Extension>]
    static member TryGetTimeOnlyExact(element:JsonElement, format:string) =
        let timeString = element.GetString()
        TimeOnly.TryParseExact(timeString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    [<Extension>]
    static member TryGetDateOnly(element:JsonElement) =
        let dateString = element.GetString()
        DateOnly.TryParse(dateString, CultureInfo.InvariantCulture)

    [<Extension>]
    static member TryGetDateOnlyExact (element:JsonElement, format:string) =
        let dateString = element.GetString()
        DateOnly.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    #endif

    [<Extension>]
    static member TryGetDateTimeUtc(element:JsonElement) =
        match element.TryGetDateTime() with
        | true, dateTime -> true, dateTime.ToUniversalTime()
        | invalid -> invalid

    [<Extension>]
    static member TryGetDateTimeExact(element:JsonElement, format:string) =
        let dateString = element.GetString()
        DateTime.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    [<Extension>]
    static member TryGetDateTimeOffsetExact(element:JsonElement, format:string) =
        let dateString = element.GetString()
        DateTimeOffset.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None)

    [<Extension>]
    static member TryGetString(element:JsonElement) =
        true, element.GetString()

    [<Extension>]
    static member TryGetBoolean(element:JsonElement) =
        true, element.GetBoolean()

    [<Extension>]
    static member TryGetUnit(element:JsonElement) =
        element.ValueKind = Kind.Null, ()

    [<Extension>]
    static member TryGetPropertyCount(element:JsonElement) =
        true, element.GetPropertyCount()

    [<Extension>]
    static member TryGetArrayLength(element:JsonElement) =
        true, element.GetArrayLength()

    [<Extension>]
    static member TryGetKind(element:JsonElement) =
        true, element.ValueKind

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