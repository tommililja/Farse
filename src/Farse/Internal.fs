namespace Farse

open System
open System.Text.Json
open System.Text.RegularExpressions

[<AutoOpen>]
module internal Internal =

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
                NewLine = "\n"
            )

    type JsonElement with

        member inline this.isNull =
            this.ValueKind = Kind.Null

        member inline this.isUndefined =
            this.ValueKind = Kind.Undefined

        member inline this.isNullOrUndefined =
            this.ValueKind = Kind.Null || this.ValueKind = Kind.Undefined

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

        let inline isNotEmpty string =
            isEmpty string |> not

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

        let private fromGenericType args = function
            | "FSharpOption" -> $"%s{args} option"
            | "FSharpList" -> $"%s{args} list"
            | "FSharpSet" -> $"%s{args} Set"
            | "FSharpMap" -> $"Map<%s{args}>"
            | "FSharpResult" -> $"Result<%s{args}>"
            | "IEnumerable" -> $"%s{args} seq"
            | "Tuple" -> $"""(%s{args.Replace(", ", " * ")})"""
            | name -> $"%s{name}<%s{args}>"

        let rec getName = function
            | x when x = typeof<unit> -> "unit"
            | x when x.IsArray -> $"%s{getName (x.GetElementType())} array"
            | x when x.IsGenericType ->
                let name = x.Name.Substring(0, x.Name.IndexOf('`'))
                let args =
                    x.GetGenericArguments()
                    |> Array.map getName
                    |> String.concat ", "

                fromGenericType args name
            | x -> fromType x.Name

        // Currently only used for numbers.
        let inline getArticle<'r> =
            match typeof<'r> with
            | r when r.Name.StartsWith("Int")
                  || r.Name.StartsWith("SByte") -> "an"
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

    [<AutoOpen>]
    module ActivePatterns =

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

    [<AutoOpen>]
    module Builders =

        type StringBuilder() =

            member inline _.Yield(line:string) =
                List.singleton line

            member inline _.Yield(line:string option) =
                line
                |> Option.map List.singleton
                |> Option.defaultValue []

            member inline _.YieldFrom(lines:string list) = lines

            member inline _.Combine(a, b) = List.append a b

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = []

            member inline _.Run(lines) =
                lines
                |> List.filter String.isNotEmpty
                |> String.concat "\n"

        type ResultBuilder() =

            member inline _.Bind(x, [<InlineIfLambda>] fn) = Result.bind fn x

            member inline _.BindReturn(a, [<InlineIfLambda>] fn) =
                match a with
                | Ok a -> Ok <| fn a
                | Error e -> Error e

            member inline _.Bind2Return(a, b, [<InlineIfLambda>] fn) =
                match a, b with
                | Ok a, Ok b -> Ok <| fn (a, b)
                | a, b ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                    ]

            member inline _.Bind3Return(a, b, c, [<InlineIfLambda>] fn) =
                match a, b, c with
                | Ok a, Ok b, Ok c -> Ok <| fn (a, b, c)
                | a, b, c ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                    ]

            member inline _.Bind4Return(a, b, c, d, [<InlineIfLambda>] fn) =
                match a, b, c, d with
                | Ok a, Ok b, Ok c, Ok d -> Ok <| fn (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                    ]

            member inline _.Bind5Return(a, b, c, d, e, [<InlineIfLambda>] fn) =
                match a, b, c, d, e with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok <| fn (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                        yield! Error.toList e
                    ]

            member inline _.MergeSources(a, b) =
                match a, b with
                | Ok a, Ok b -> Ok (a, b)
                | a, b ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                    ]

            member inline _.MergeSources3(a, b, c) =
                match a, b, c with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | a, b, c ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                    ]

            member inline _.MergeSources4(a, b, c, d) =
                match a, b, c, d with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                    ]

            member inline _.MergeSources5(a, b, c, d, e) =
                match a, b, c, d, e with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                        yield! Error.toList e
                    ]

            member inline _.Return(x) = Ok x

            member inline _.ReturnFrom(x) = x

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Ok ()

        let string = StringBuilder()

        let result = ResultBuilder()