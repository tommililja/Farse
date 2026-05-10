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

        // Returns Null and Undefined elements.
        let inline getProperty (name:string) (e:JsonElement) =
            e.TryGetProperty(name) |> snd

        let inline tryGetProperty (name:string) (e:JsonElement) =
            match e.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Some prop
            | _ -> None

        let inline tryGetNullProperty (name:string) (e:JsonElement) =
            match e.TryGetProperty(name) with
            | true, prop -> Some prop
            | _ -> None

        let inline tryGetValue (e:JsonElement) =
            match e.ValueKind with
            | Kind.Null | Kind.Undefined | Kind.Object | Kind.Array -> None
            | _ -> Some <| e.GetRawText()

        // Undefined elements are not clonable.
        let inline clone (e:JsonElement) =
            match e.ValueKind with
            | Kind.Undefined -> JsonElement() // Undefined.
            | _ -> e.Clone()

        let inline isBool (e:JsonElement) =
            e.ValueKind = Kind.True || e.ValueKind = Kind.False

    module String =

        let isNotEmpty = String.IsNullOrWhiteSpace >> not

    module Type =

        let rec getName = function
            | (x:Type) when x.IsGenericType ->
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

        let inline defaultValue x =
            Result.map (Option.defaultValue x)

    module Seq =

        let inline ofSeq x = x :> seq<_>

    [<AutoOpen>]
    module ActivePatterns =

        let inline (|Prop|Path|) (str:string) =
            if str.Contains('.')
            then Path (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Prop str