namespace Farse

open System
open System.Text.Encodings.Web
open System.Text.Json

[<AutoOpen>]
module internal Common =

    let inline (!) x = not x

    let inline (!!) x y = x y |> not

    module JsonDocumentOptions =

        let Default =
            JsonDocumentOptions (
                AllowTrailingCommas = true,
                CommentHandling = JsonCommentHandling.Skip
            )

    module JsonSerializerOptions =

        let Default =
            JsonSerializerOptions (
                WriteIndented = true,
                IndentSize = 4,
                NewLine = "\n",
                Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
            )

        let Raw =
            JsonSerializerOptions (
                Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
            )

    type JsonElement with

        member inline this.isNull =
            this.ValueKind = Kind.Null

        member inline this.isUndefined =
            this.ValueKind = Kind.Undefined

        member inline this.isNullOrUndefined =
            this.ValueKind = Kind.Null || this.ValueKind = Kind.Undefined

        member inline this.IsString =
            this.ValueKind = Kind.String

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

        let inline isEmpty string =
            String.IsNullOrWhiteSpace(string)

        let inline indent n (string:string) =
            string.Split('\n')
            |> Array.map (fun line -> String.replicate n " " + line)
            |> String.concat "\n"

    module Type =

        let private fromType = function
            | "Int16" -> "int16"
            | "Int32" -> "int"
            | "Int64" -> "int64"
            | "Int128" -> "int128"
            | "Byte" -> "byte"
            | "SByte" -> "sbyte"
            | "UInt16" -> "uint16"
            | "UInt32" -> "uint"
            | "UInt64" -> "uint64"
            | "UInt128" -> "uint128"
            | "Double" -> "float"
            | "Single" -> "float32"
            | "Boolean" -> "bool"
            | "String" -> "string"
            | "Char" -> "char"
            | "Decimal" -> "decimal"
            | "BigInteger" -> "bigint"
            | "Object" -> "obj"
            | name -> name

        let private fromGenericType (args:string array) name =
            let first = args[0]
            let joined = String.concat ", " args

            match name with
            | "FSharpOption" -> $"%s{first} option"
            | "FSharpValueOption" -> $"%s{first} voption"
            | "FSharpList" -> $"%s{first} list"
            | "FSharpSet" -> $"%s{first} Set"
            | "FSharpMap" -> $"Map<%s{joined}>"
            | "FSharpResult" -> $"Result<%s{joined}>"
            | "IEnumerable" -> $"%s{first} seq"
            | "Tuple" -> $"""(%s{String.concat " * " args})"""
            | "ValueTuple" -> $"""struct (%s{String.concat " * " args})"""
            | _ -> $"%s{name}<%s{joined}>"

        let rec getName = function
            | x when x = typeof<unit> -> "unit"
            | x when x.IsArray -> $"%s{getName (x.GetElementType())} array"
            | x when x.IsGenericType ->
                let name = x.Name.Substring(0, x.Name.IndexOf('`'))
                let args =
                    x.GetGenericArguments()
                    |> Array.map getName

                fromGenericType args name
            | x -> fromType x.Name

        // Currently only used for numbers.
        let inline articleOf<'a> =
            match typeof<'a>.Name with
            | "Int16" | "Int32" | "Int64" | "Int128" | "SByte" -> "an"
            | _ -> "a"

    module Error =

        let inline toList x =
            match x with
            | Ok _ -> []
            | Error x -> x

        let inline list x =
            List.singleton x
            |> Error

    module Seq =

        let inline ofSeq x =
            x :> seq<_>