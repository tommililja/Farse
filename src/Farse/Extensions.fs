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

    type JsonElement with

        member inline this.isNull =
            this.ValueKind = Kind.Null

        member inline this.isUndefined =
            this.ValueKind = Kind.Undefined

        member inline this.isNullOrUndefined =
            this.ValueKind = Kind.Null || this.ValueKind = Kind.Undefined

        member inline this.isNotNull =
            this.ValueKind <> Kind.Null

    module JsonElement =

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

        let inline isNotEmpty str =
            String.IsNullOrWhiteSpace(str)
            |> not

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

        let inline list x =
            List.singleton x
            |> Error

    module Seq =

        let inline ofSeq x =
            x :> seq<_>

    [<AutoOpen>]
    module ActivePatterns =

        let (|Prop|Path|) (str:string) =
            if str.Contains('.')
            then Path (str.Split('.', StringSplitOptions.RemoveEmptyEntries))
            else Prop str