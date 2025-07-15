namespace Farse

open System
open System.Runtime.CompilerServices
open System.Text.Json

type Kind = JsonValueKind

module internal JsonSerializerOptions =

    let preset = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)

module internal JsonElement =

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        JsonSerializer.Serialize(element, JsonSerializerOptions.preset)

type internal JsonElementExtensions() =

    [<Extension>]
    static member TryGetString(e:JsonElement) =
        true, e.GetString()

    [<Extension>]
    static member TryGetBoolean(e:JsonElement) =
        true, e.GetBoolean()

    [<Extension>]
    static member TryGetDateTimeUtc(e:JsonElement) =
        match e.TryGetDateTime() with
        | true, dateTime -> true, dateTime.ToUniversalTime()
        | false, invalid -> false, invalid

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