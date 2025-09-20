namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member _.Return(x) = from x

        member _.ReturnFrom(x) = x

        member _.Bind(x, fn) = bind fn x

        member _.Bind(x:Result<_,_>, fn) = fromResult x |> bind fn

        member _.Combine (a, b) = bind (fun () -> b) a

        member _.MergeSources(a:Parser<'a>, b:Parser<'b>) =
            fun element ->
                match a element, b element with
                | Ok a, Ok b -> Ok (a, b)
                | Error e, _ -> Error e
                | _, Error e -> Error e

        member _.MergeSources3(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>) =
            fun element ->
                match a element, b element, c element with
                | Ok a, Ok b, Ok c -> Ok (a, b, c)
                | Error e, _, _ -> Error e
                | _, Error e, _ -> Error e
                | _, _, Error e -> Error e

        member _.MergeSources4(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>) =
            fun element ->
                match a element, b element, c element, d element with
                | Ok a, Ok b, Ok c, Ok d -> Ok (a, b, c, d)
                | Error e, _, _, _ -> Error e
                | _, Error e, _, _ -> Error e
                | _, _, Error e, _ -> Error e
                | _, _, _, Error e -> Error e

        member _.MergeSources5(a: Parser<'a>, b: Parser<'b>, c: Parser<'c>, d: Parser<'d>, e: Parser<'e>) =
            fun element ->
                match a element, b element, c element, d element, e element with
                | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (a, b, c, d, e)
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e

        member _.Zero() = from ()

        member _.Delay(fn) = fn ()

    let parser = ParserBuilder()