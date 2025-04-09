namespace Farse.Tests

open Xunit
open Farse

module Parser =

    [<Fact>]
    let ``Should return expected value`` () =
        let expected = ()
        let parser = Parser.from expected |> Parser.parse "1"
        let actual = Expect.ok parser
        Expect.equal actual expected

    [<Fact>]
    let ``Should bind values and return expected value`` () =
        let expected = 2
        let result =
            Parse.int
            |> Parser.bind (fun x -> Parse.int >> Result.map ((+)x))
            |> Parser.parse "1"
        let actual = Expect.ok result
        Expect.equal actual expected

    [<Fact>]
    let ``Should map value and return expected value`` () =
        let expected = "1"
        let result =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
        let actual = Expect.ok result
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok with expected value when validation succeeds`` () =
        let expected = 1
        let custom = Parse.int |> Parser.validate Ok
        let result = custom |> Parser.parse "1"
        let actual = Expect.ok result
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Error with expected value when validation fails`` () =
        let expected = "Error"
        let custom = Parse.int |> Parser.validate (fun _ -> Error expected)
        let result = custom |> Parser.parse "1"
        let actual = Expect.error result
        Expect.equal actual expected