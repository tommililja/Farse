namespace Farse

open System
open System.Globalization
open System.Runtime.CompilerServices
open System.Text.Json

module internal JsonSerializerOptions =

    let preset = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)

module internal JsonElement =

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

    [<Extension>]
    static member TryGetDateTimeUtc(element:JsonElement) =
        match element.TryGetDateTime() with
        | true, dateTime -> true, dateTime.ToUniversalTime()
        | false, invalid -> false, invalid

    [<Extension>]
    static member TryGetDateTimeExact(element:JsonElement, format:string) =
        let dateString = element.GetString()
        match DateTime.TryParseExact(dateString, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, date -> true, date
        | _ -> false, DateTime.MinValue

    [<Extension>]
    static member TryGetString(element:JsonElement) =
        true, element.GetString()

    [<Extension>]
    static member TryGetBoolean(element:JsonElement) =
        true, element.GetBoolean()

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