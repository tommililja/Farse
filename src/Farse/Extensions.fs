namespace Farse

open System
open System.Runtime.CompilerServices
open System.Text.Json

type internal Kind = JsonValueKind

module internal JsonSerializerOptions =

    let preset = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)

module internal JsonElement =

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

type internal JsonElementExtensions() =

    [<Extension>]
    static member TryGetChar(e:JsonElement) =
        match e.GetString() with
        | str when str.Length = 1 -> true, str[0]
        | _ -> false, Char.MinValue

    [<Extension>]
    static member TryGetDateTimeUtc(e:JsonElement) =
        match e.TryGetDateTime() with
        | true, dateTime -> true, dateTime.ToUniversalTime()
        | false, invalid -> false, invalid

    [<Extension>]
    static member TryGetString(e:JsonElement) =
        true, e.GetString()

    [<Extension>]
    static member TryGetBoolean(e:JsonElement) =
        true, e.GetBoolean()

    [<Extension>]
    static member TryGetPropertyCount(e:JsonElement) =
        true, e.GetPropertyCount()

    [<Extension>]
    static member TryGetArrayLength(e:JsonElement) =
        true, e.GetArrayLength()

    [<Extension>]
    static member TryGetKind(e:JsonElement) =
        true, e.ValueKind

module internal String =

    let startsWith (startsWith:string) (str:string) =
        str.StartsWith(startsWith)

[<AutoOpen>]
module internal ActivePatterns =

    let (|String|Invalid|) (str:string) =
        if String.IsNullOrEmpty str
        then Invalid
        else String str

    let (|Flat|Nested|) (str:string) =
        if str.Contains('.')
        then Nested (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
        else Flat str