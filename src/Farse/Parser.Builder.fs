namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member _.Return(x) = from x

        member _.ReturnFrom(x) = x

        member _.Bind(x, fn) = bind fn x

        member _.Zero() = from ()

        member _.Delay(fn) = fn ()

    let parser = ParserBuilder()