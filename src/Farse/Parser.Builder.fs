namespace Farse

[<AutoOpen>]
module ParserBuilder =
    open Parser

    type ParserBuilder() =

        member _.Return(x) : Parser<_> = from x

        member _.ReturnFrom(x) : Parser<_> = x

        member _.Bind(x:Parser<_>, fn:_ -> Parser<_>) : Parser<_> = bind fn x

        member _.Zero() : Parser<_> = from ()

        member _.Delay(fn:unit -> Parser<_>) : Parser<_> = fn ()

    let parser = ParserBuilder()
