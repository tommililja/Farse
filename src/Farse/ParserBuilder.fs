namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member inline _.Return(x) = from x

        member inline _.ReturnFrom(x) = x

        member inline _.Zero() = from ()

        member inline _.Combine(a:Parser<unit>, fn:unit -> Parser<'r>) = bind fn a

        member inline _.Delay(fn) = fn

        member _.Run(fn) =
            let deferred = lazy (fn ())
            Parser (fun element -> run element deferred.Value)

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

    /// <summary>Builds a <c>Parser</c> by combining parsers.</summary>
    /// <remarks>
    ///     The body is lazy and evaluated on first use.
    ///     Use <c>and!</c> to collect errors instead of returning on the first.
    /// </remarks>
    /// <example>
    /// <code>
    ///     let parser =
    ///         parser {
    ///             let! x = "x" &amp;= Parse.int
    ///             and! y = "y" &amp;= Parse.int
    /// &#160;
    ///             do! "z" &amp;= Parse.unit
    /// &#160;
    ///             if x = 0 then
    ///                 do! Parser.fail "Parser failed."
    /// &#160;
    ///             return x + y
    ///         }
    /// </code>
    /// </example>
    let parser = ParserBuilder()