namespace Farse

open System.Text.Json

type internal Kind = JsonValueKind

type Parser<'a> = JsonElement -> Result<'a, string>