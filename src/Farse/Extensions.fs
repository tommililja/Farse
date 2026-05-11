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

        let inline tryGetProperty (name:string) (e:JsonElement) =
            match e.TryGetProperty(name) with
            | true, prop when prop.ValueKind <> Kind.Null -> Element prop
            | true, prop -> Null prop
            | _, prop -> Undefined prop

        let inline tryGetValue (e:JsonElement) =
            match e.ValueKind with
            | Kind.Null | Kind.Undefined | Kind.Object | Kind.Array -> None
            | _ -> Some <| e.GetRawText()

        // Undefined elements are not clonable.
        let inline clone (e:JsonElement) =
            match e.ValueKind with
            | Kind.Undefined -> JsonElement() // Undefined.
            | _ -> e.Clone()

    module String =

        let isNotEmpty =
            String.IsNullOrWhiteSpace
            >> not

    module Type =

        let rec getName type' =
            match type' with
            | x when x = typeof<unit> -> "unit"
            | x when x.IsArray -> $"%s{getName (x.GetElementType())} array"
            | x when x.IsGenericType ->
                let name = x.Name.Substring(0, x.Name.IndexOf('`'))
                let args =
                    x.GetGenericArguments()
                    |> Array.map getName
                    |> String.concat ", "

                match name with
                | "FSharpOption" -> $"%s{args} option"
                | "FSharpList" -> $"%s{args} list"
                | "FSharpSet" -> $"%s{args} Set"
                | "FSharpMap" -> $"Map<%s{args}>"
                | "FSharpResult" -> $"Result<%s{args}>"
                | "IEnumerable" -> $"%s{args} seq"
                | "Tuple" -> $"(%s{args})"
                | name -> $"%s{name}<%s{args}>"
            | x ->
                match x.Name with
                | "Int16" -> "int16"
                | "Int32" -> "int"
                | "Int64" -> "int64"
                | "Byte" -> "byte"
                | "SByte" -> "int8"
                | "UInt16" -> "uint16"
                | "UInt32" -> "uint"
                | "UInt64" -> "uint64"
                | "Double" -> "float"
                | "Single" -> "float32"
                | "Boolean" -> "bool"
                | "String" -> "string"
                | "Char" -> "char"
                | "Decimal" -> "decimal"
                | "BigInteger" -> "bigint"
                | "Object" -> "obj"
                | name -> name

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

        let inline ofSeq x =
            x :> seq<_>

    [<AutoOpen>]
    module ActivePatterns =

        let inline (|Prop|Path|) (str:string) =
            if str.Contains('.')
            then Path (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Prop str