namespace Farse

open System
open System.Text.Json

module internal JsonElement =

    let getRawText (element:JsonElement) =
        element.GetRawText()

    let getJson (element:JsonElement) =
        let json = element.GetRawText()
        use document = JsonDocument.Parse(json)
        let settings = JsonSerializerOptions(IndentSize = 4, WriteIndented = true)
        JsonSerializer.Serialize(document.RootElement, settings)

[<AutoOpen>]
module internal ActivePatterns =

    let (|NullOrEmpty|String|) (str:string) =
        if String.IsNullOrEmpty str
        then NullOrEmpty
        else String str