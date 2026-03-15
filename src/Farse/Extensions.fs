namespace Farse

open System
open System.Text.Json

// Ignore enum match warning.
#nowarn 104

[<AutoOpen>]
module internal Extensions =

    type Kind = JsonValueKind

    module Kind =

        let asString = function
            | Kind.Undefined -> "Undefined"
            | Kind.Object -> "Object"
            | Kind.Array -> "Array"
            | Kind.String -> "String"
            | Kind.Number -> "Number"
            | Kind.True | Kind.False -> "Bool"
            | Kind.Null -> "Null"

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

    [<AutoOpen>]
    module Builders =

        type StringBuilder() =

            member inline _.Yield(line:string) =
                Seq.singleton line

            member inline _.Yield(line:string option) =
                line
                |> Option.map Seq.singleton
                |> Option.defaultValue Seq.empty

            member inline _.YieldFrom(lines:string seq) = lines

            member inline _.Combine(a, b) = Seq.append a b

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Seq.empty

            member inline _.Run(lines) =
                lines
                |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                |> String.concat "\n"

        type ResultBuilder() =

            member inline _.Bind(x, [<InlineIfLambda>] fn) = Result.bind fn x

            member inline _.Bind2(a, b, [<InlineIfLambda>] fn) =
                match a, b with
                | Ok a, Ok b -> fn (a, b)
                | a, b ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind3(a, b, c, [<InlineIfLambda>] fn) =
                match a, b, c with
                | Ok a, Ok b, Ok c -> fn (a, b, c)
                | a, b, c ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind4(a, b, c, d, [<InlineIfLambda>] fn) =
                match a, b, c, d with
                | Ok a, Ok b, Ok c, Ok d -> fn (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind5(a, b, c, d, e, [<InlineIfLambda>] fn) =
                match a, b, c, d, e with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> fn (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                        match e with Error e -> yield! e | _ -> ()
                    ]

            member inline _.BindReturn(a, [<InlineIfLambda>] fn) =
                match a with
                | Ok a -> Ok <| fn a
                | Error e -> Error e

            member inline _.Bind2Return(a, b, [<InlineIfLambda>] fn) =
                match a, b with
                | Ok a, Ok b -> Ok <| fn (a, b)
                | a, b ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind3Return(a, b, c, [<InlineIfLambda>] fn) =
                match a, b, c with
                | Ok a, Ok b, Ok c -> Ok <| fn (a, b, c)
                | a, b, c ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind4Return(a, b, c, d, [<InlineIfLambda>] fn) =
                match a, b, c, d with
                | Ok a, Ok b, Ok c, Ok d -> Ok <| fn (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Bind5Return(a, b, c, d, e, [<InlineIfLambda>] fn) =
                match a, b, c, d, e with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok <| fn (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                        match e with Error e -> yield! e | _ -> ()
                    ]

            member inline _.MergeSources(a, b) =
                match a, b with
                | Ok a, Ok b -> Ok (a, b)
                | a, b ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                    ]

            member inline _.MergeSources3(a, b, c) =
                match a, b, c with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | a, b, c ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                    ]

            member inline _.MergeSources4(a, b, c, d) =
                match a, b, c, d with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                    ]

            member inline _.MergeSources5(a, b, c, d, e) =
                match a, b, c, d, e with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        match a with Error e -> yield! e | _ -> ()
                        match b with Error e -> yield! e | _ -> ()
                        match c with Error e -> yield! e | _ -> ()
                        match d with Error e -> yield! e | _ -> ()
                        match e with Error e -> yield! e | _ -> ()
                    ]

            member inline _.Return(x) = Ok x

            member inline _.ReturnFrom(x) = x

            member inline _.Delay([<InlineIfLambda>] fn) = fn ()

            member inline _.Zero() = Ok ()

        let string = StringBuilder()

        let result = ResultBuilder()