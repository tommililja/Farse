namespace Farse

open System.Text.Json
open System.Text.RegularExpressions

[<AutoOpen>]
module internal ActivePatterns =

    let private pathRegex = Regex(@"(?:\\\.|[^.])+")

    let inline (|IsExpectedKind|_|) (e:JsonElement) = function
        | ExpectedKind.Any -> not e.isUndefined
        | ExpectedKind.Array -> e.ValueKind = Kind.Array
        | ExpectedKind.Bool -> e.ValueKind = Kind.True || e.ValueKind = Kind.False
        | ExpectedKind.Null -> e.ValueKind = Kind.Null
        | ExpectedKind.Number -> e.ValueKind = Kind.Number
        | ExpectedKind.Object -> e.ValueKind = Kind.Object
        | ExpectedKind.String -> e.ValueKind = Kind.String

    let (|Prop|Path|) (path:string) =
        let segments =
            pathRegex.Matches(path)
            |> Seq.map _.Value.Replace("\\.", ".")
            |> Seq.toArray

        match segments with
        | [||] -> Prop path
        | [| name |] -> Prop name
        | segments -> Path segments

    let inline (|Empty|_|) string =
        String.isEmpty(string)