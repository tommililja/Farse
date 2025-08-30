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

        member _.Zero() = from ()

        member _.Delay(fn) = fn ()

    let parser = ParserBuilder()