namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member _.Return(x) = from x

        member _.ReturnFrom(x) = x

        member _.Bind(x, fn) = bind fn x

        member _.MergeSources(a:Parser<'a>, b:Parser<'b>) : Parser<_> =
            fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok (a, b)
                | Error e, _ -> Error e
                | _, Error e -> Error e

        member _.MergeSources3(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>) : Parser<_> =
            fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e

        member _.MergeSources4(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e

        member _.MergeSources5(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, e: Parser<'e>) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e

        member _.Bind2(a: Parser<'a>, b: Parser<'b>, fn: 'a * 'b -> Parser<'c>) : Parser<_> =
            fun element ->
                match a element, b element with
                | Ok a, Ok b -> fn (a, b) <| element
                | Error e, _ -> Error e
                | _, Error e -> Error e

        member _.Bind3(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, fn: 'a * 'b * 'c -> Parser<'d>) : Parser<_> =
            fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> fn (a, b, c) <| element
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e

        member _.Bind4(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, fn: 'a * 'b * 'c * 'd -> Parser<'e>) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> fn (a, b, c, d) <| element
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e

        member _.Bind5(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, e: Parser<'e>, fn: 'a * 'b * 'c * 'd * 'e -> Parser<'f>) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> fn (a, b, c, d, e) <| element
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e

        member _.BindReturn(a: Parser<'a>, fn: 'a -> 'b) : Parser<_> =
            fun element ->
                match a element with
                | Ok a -> Ok <| fn a
                | Error e -> Error e

        member _.Bind2Return(a: Parser<'a>, b: Parser<'b>, fn: 'a * 'b -> 'c) : Parser<_> =
            fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok <| fn (a, b)
                | Error e, _ -> Error e
                | _, Error e -> Error e

        member _.Bind3Return(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, fn: 'a * 'b * 'c -> 'd) : Parser<_> =
            fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok <| fn (a, b, c)
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e

        member _.Bind4Return(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, fn: 'a * 'b * 'c * 'd -> 'e) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok <| fn (a, b, c, d)
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e

        member _.Bind5Return(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, e: Parser<'e>, fn: 'a * 'b * 'c * 'd * 'e -> 'f) : Parser<_> =
            fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok <| fn (a, b, c, d, e)
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e

        member _.Zero() = from ()

        member _.Delay(fn) = fn ()

    let parser = ParserBuilder()