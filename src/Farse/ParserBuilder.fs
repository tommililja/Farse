namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member inline _.Return(x) = from x

        member inline _.ReturnFrom(x) = x

        member inline _.Delay([<InlineIfLambda>] fn) = fn ()

        member inline _.Zero() = from ()

        member inline _.Bind(x, [<InlineIfLambda>] fn) = bind fn x

        member inline _.Bind2(Parser a, Parser b, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element with
                | Ok a, Ok b -> fn (a, b) |> run element
                | a, b ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                    ]
            )

        member inline _.Bind3(Parser a, Parser b, Parser c, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> fn (a, b, c) |> run element
                | a, b, c ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                    ]
            )

        member inline _.Bind4(Parser a, Parser b, Parser c, Parser d, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> fn (a, b, c, d) |> run element
                | a, b, c, d ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                    ]
            )

        member inline _.Bind5(Parser a, Parser b, Parser c, Parser d, Parser e, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> fn (a, b, c, d, e) |> run element
                | a, b, c, d, e ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                        yield! Error.toList e
                    ]
            )

        member inline _.BindReturn(Parser a, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element with
                | Ok a -> Ok <| fn a
                | Error e -> Error e
            )

        member inline _.Bind2Return(Parser a, Parser b, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok <| fn (a, b)
                | a, b ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                    ]
            )

        member inline _.Bind3Return(Parser a, Parser b, Parser c, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok <| fn (a, b, c)
                | a, b, c ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                    ]
            )

        member inline _.Bind4Return(Parser a, Parser b, Parser c, Parser d, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok <| fn (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                    ]
            )

        member inline _.Bind5Return(Parser a, Parser b, Parser c, Parser d, Parser e, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok <| fn (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                        yield! Error.toList e
                    ]
            )

        member inline _.MergeSources(Parser a, Parser b) =
            Parser (fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok (a, b)
                | a, b ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                    ]
            )

        member inline _.MergeSources3(Parser a, Parser b, Parser c) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | a, b, c ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                    ]
            )

        member inline _.MergeSources4(Parser a, Parser b, Parser c, Parser d) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | a, b, c, d ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                    ]
            )

        member inline _.MergeSources5(Parser a, Parser b, Parser c, Parser d, Parser e) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | a, b, c, d, e ->
                    Error [
                        yield! Error.toList a
                        yield! Error.toList b
                        yield! Error.toList c
                        yield! Error.toList d
                        yield! Error.toList e
                    ]
            )

    let parser = ParserBuilder()