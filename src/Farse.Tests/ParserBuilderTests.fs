namespace Farse.Tests

open Expecto.Flip
open Xunit
open Farse

module ParserBuilderTests =

    let private example =
        parser {
             let! x = Prop.get "x" Parse.int
             and! y = Prop.get "y" Parse.int

             do! Prop.get "z" Parse.unit

             if x = 0 then
                 do! Parser.fail "Parser failed."

             return x + y
         }

    [<Fact>]
    let ``Should parse with let!, and!, do! and if`` () =
        let actual =
            example
            |> Parser.parse """{ "x": 1, "y": 2, "z": null }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none 3 actual

    [<Fact>]
    let ``Should fail when Parser.fail is reached`` () =
        example
        |> Parser.parse """{ "x": 0, "y": 2, "z": null }"""
        |> Expect.parserError

    [<Fact>]
    let ``Should collect errors with and!`` () =
        example
        |> Parser.parse "{}"
        |> Expect.parserError