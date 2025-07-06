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
        e.TryGetDateTime()
        |> fun (bool, dateTime) -> bool, dateTime.ToUniversalTime()

[<AutoOpen>]
module internal ActivePatterns =

    let (|NullOrEmpty|String|) (str:string) =
        if String.IsNullOrEmpty str
        then NullOrEmpty
        else String str

    let (|Nested|Flat|) (str:string) =
        if str.Contains(".")
        then Nested str
        else Flat str