namespace Farse

open System
open System.Text.Json

[<AutoOpen>]
module internal Extensions =

    module JsonDocumentOptions =

        let preset =
            JsonDocumentOptions (
                AllowTrailingCommas = true,
                CommentHandling = JsonCommentHandling.Skip
            )

    module JsonElement =

        let inline getProperty (name:string) (element:JsonElement) =
            element.TryGetProperty(name) |> snd

        let inline tryGetProperty (name:string) (element:JsonElement) =
            match element.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Some prop
            | _ -> None

        let inline tryGetValue (element:JsonElement) =
            match element.ValueKind with
            | Kind.Null
            | Kind.Undefined
            | Kind.Object
            | Kind.Array -> None
            | _ -> Some <| element.GetRawText()

        // Undefined elements are not clonable.
        let inline clone (element:JsonElement) =
            match element.ValueKind with
            | Kind.Undefined -> JsonElement() // Undefined.
            | _ -> element.Clone()

        let inline isBool (element:JsonElement) =
            element.ValueKind = Kind.True || element.ValueKind = Kind.False

    module String =

        let indentLines (str:string) =
            str
            |> _.Split('\n')
            |> Array.map (sprintf "  %s")
            |> String.concat "\n"

    module Type =

        let rec getName (x:Type) =
            match x with
            | x when x.IsGenericType ->
                let name = x.Name.Substring(0, x.Name.IndexOf('`'))
                let args =
                    x.GetGenericArguments()
                    |> Array.map getName
                    |> String.concat ", "

                $"%s{name}<%s{args}>"
            | x -> x.Name

    module Error =

        let list x =
            List.singleton x
            |> Error

    module Result =

        // Significantly decreases memory allocations.
        let inline mapError ([<InlineIfLambda>] fn) = function
            | Ok x -> Ok x
            | Error e -> Error <| fn e

    module ResultOption =

        let inline bind ([<InlineIfLambda>] fn) = function
            | Ok (Some x) -> fn x
            | Ok None -> Ok None
            | Error e -> Error e

        let inline defaultValue v =
            Result.map (Option.defaultValue v)

    [<AutoOpen>]
    module ActivePatterns =

        let inline (|Prop|Path|) (str:string) =
            if str.Contains('.')
            then Path (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Prop str