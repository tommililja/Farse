namespace Farse

open System
open System.Text.Json
open System.Text.RegularExpressions

[<AutoOpen>]
module internal ActivePatterns =

    let private separator = Regex(@"(?<!\\)\.")

    let inline (|IsExpectedKind|_|) (e:JsonElement) = function
        | ExpectedKind.Any -> not e.isUndefined
        | ExpectedKind.Array -> e.ValueKind = Kind.Array
        | ExpectedKind.Bool -> e.ValueKind = Kind.True || e.ValueKind = Kind.False
        | ExpectedKind.Null -> e.ValueKind = Kind.Null
        | ExpectedKind.Number -> e.ValueKind = Kind.Number
        | ExpectedKind.Object -> e.ValueKind = Kind.Object
        | ExpectedKind.String -> e.ValueKind = Kind.String

    let (|Prop|Path|Invalid|) (path:string) =
        if path = null then Invalid
        else
            match separator.Split(path) with
            | [| name |] -> Prop (name.Replace("\\.", "."))
            | segments when Array.exists String.IsNullOrEmpty segments -> Invalid
            | segments -> Path (segments |> Array.map _.Replace("\\.", "."))

    let inline (|Empty|_|) string =
        String.IsNullOrWhiteSpace(string)