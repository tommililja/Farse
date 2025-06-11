namespace Farse.Tests

open Xunit
open Farse

module Parser =

    [<Fact>]
    let ``Should lift and return expected value`` () =
        let expected = ()
        let actual =
            expected
            |> Parser.from
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should bind parsers and return expected value`` () =
        let expected = 2
        let actual =
            Parse.int
            |> Parser.bind (fun x -> Parser.from (x + 1))
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should map parser and return expected value`` () =
        let expected = "1"
        let actual =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return ok with expected value when validation succeeds`` () =
        let expected = 1
        let actual =
            Parse.int
            |> Parser.validate Ok
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return error with expected value when validation fails`` () =
        let expected = "Error"
        let actual =
            Parse.int
            |> Parser.validate (fun _ -> Error expected)
            |> Parser.parse "1"
            |> Expect.error
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse value from nested object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": 100
                        }
                    }
                }
            """

        let expected = 100
        let actual =
            Parse.req "prop.prop2.prop3" Parse.int
            |> Parser.parse json
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should try parse value from nested object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": null
                        }
                    }
                }
            """

        let expected = None
        let actual =
            Parse.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse json
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse required value from nested object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": 100
                        }
                    }
                }
            """

        let expected = 100
        let actual =
            Parse.req "prop.prop2.prop3" Parse.int
            |> Parser.parse json
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should parse optional value from nested object`` () =
        let json =
            """
                {
                    "prop": {
                        "prop2": {
                            "prop3": null
                        }
                    }
                }
            """

        let expected = None
        let actual =
            Parse.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse json
            |> Expect.ok
        Expect.equal actual expected
