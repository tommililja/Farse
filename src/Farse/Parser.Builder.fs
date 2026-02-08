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
                | Ok a, Ok b -> fn (a, b) <| element
                | Error e, _ -> Error e
                | _, Error e -> Error e
            )

        member inline _.Bind3(Parser a, Parser b, Parser c, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> fn (a, b, c) <| element
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e
            )

        member inline _.Bind4(Parser a, Parser b, Parser c, Parser d, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> fn (a, b, c, d) <| element
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e
            )

        member inline _.Bind5(Parser a, Parser b, Parser c, Parser d, Parser e, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> fn (a, b, c, d, e) <| element
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e
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
                | Error e, _ -> Error e
                | _, Error e -> Error e
            )

        member inline _.Bind3Return(Parser a, Parser b, Parser c, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok <| fn (a, b, c)
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e
            )

        member inline _.Bind4Return(Parser a, Parser b, Parser c, Parser d, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok <| fn (a, b, c, d)
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e
            )

        member inline _.Bind5Return(Parser a, Parser b, Parser c, Parser d, Parser e, [<InlineIfLambda>] fn) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok <| fn (a, b, c, d, e)
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e
            )

        member inline _.MergeSources(Parser a, Parser b) =
            Parser (fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok (a, b)
                | Error e, _ -> Error e
                | _, Error e -> Error e
            )

        member inline _.MergeSources3(Parser a, Parser b, Parser c) =
            Parser (fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e
            )

        member inline _.MergeSources4(Parser a, Parser b, Parser c, Parser d) =
            Parser (fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e
            )

        member inline _.MergeSources5(Parser a, Parser b, Parser c, Parser d, Parser e) =
            Parser (fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e
            )

    let parser = ParserBuilder()