namespace Farse

open System
open System.Runtime.CompilerServices
open System.Text.Json

module internal JsonElement =

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        let json = element.GetRawText()
        use document = JsonDocument.Parse(json)
        let settings = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)
        JsonSerializer.Serialize(document.RootElement, settings)

type internal JsonElementExtensions() =

    [<Extension>]
    static member TryGetString (e:JsonElement) =
        true, e.GetString()

    [<Extension>]
    static member TryGetBoolean (e:JsonElement) =
        true, e.GetBoolean()

    [<Extension>]
    static member TryGetDateTimeUtc (e:JsonElement) =
        match e.TryGetDateTime() with
        | true, dateTime -> true, dateTime.ToUniversalTime()
        | false, invalid -> false, invalid

[<AutoOpen>]
module internal ActivePatterns =

    let (|String|Invalid|) (str:string) =
        if String.IsNullOrEmpty str
        then Invalid
        else String str

    let (|Flat|Nested|) (str:string) =
        if str.Contains(".")
        then Nested str
        else Flat str