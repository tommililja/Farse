namespace Farse

[<AutoOpen>]
module internal Builders =

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
