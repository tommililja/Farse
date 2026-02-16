namespace Farse.Tests

open Xunit
open Farse

module ParserTests =

    [<Fact>]
    let ``Should create Parser and return expected value`` () =
        let expected = ()
        let actual =
            expected
            |> Parser.from
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should create Parser and return expected Result`` () =
        let expected = Ok ()
        let actual =
            expected
            |> Parser.fromResult
            |> Parser.parse "1"
        Expect.equal actual expected

    [<Fact>]
    let ``Should bind Parser value and return expected value`` () =
        let expected = 2
        let actual =
            Parse.int
            |> Parser.bind (fun x -> Parser.from (x + 1))
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should map Parser value and return expected value`` () =
        let expected = "1"
        let actual =
            Parse.int
            |> Parser.map string
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should ignore Parser value and return unit`` () =
        let expected = ()
        let actual =
            Parse.int
            |> Parser.ignore<int>
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok with expected value when validation succeeds`` () =
        let expected = 1
        let actual =
            Parse.int
            |> Parser.validate Ok
            |> Parser.parse "1"
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok with expected value when optional validation succeeds`` () =
        let expected = Some 1
        let actual =
            Prop.opt "prop" Parse.int
            |> Parser.validate (fun (x:int) -> Ok x)
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.ok
        Expect.equal actual expected

    [<Fact>]
    let ``Should return Ok with expected value when list validation succeeds`` () =
        let expected = [ 1; 2; 3 ]
        let actual: _ list =
            Prop.req "prop" (Parse.list Parse.string)
            |> Parser.validate (fun x -> Ok <| int x)
            |> Parser.parse """{ "prop": [ "1", "2", "3" ] }"""
            |> Expect.ok
        Expect.equalSeq actual expected

    [<Fact>]
    let ``Should return Ok with expected value when array validation succeeds`` () =
        let expected = [| 1; 2; 3 |]
        let actual: _ array =
            Prop.req "prop" (Parse.array Parse.string)
            |> Parser.validate (fun x -> Ok <| int x)
            |> Parser.parse """{ "prop": [ "1", "2", "3" ] }"""
            |> Expect.ok
        Expect.equalSeq actual expected

    [<Fact>]
    let ``Should return Ok with expected value when Set validation succeeds`` () =
        let expected = set [ 1; 2; 3 ]
        let actual: _ Set =
            Prop.req "prop" (Parse.set Parse.string)
            |> Parser.validate (fun x -> Ok <| int x)
            |> Parser.parse """{ "prop": [ "1", "2", "3" ] }"""
            |> Expect.ok
        Expect.equalSeq actual expected

    [<Fact>]
    let ``Should return Ok with expected value when seq validation succeeds`` () =
        let expected = seq [ 1; 2; 3 ]
        let actual: _ seq =
            Prop.req "prop" (Parse.seq Parse.string)
            |> Parser.validate (fun x -> Ok <| int x)
            |> Parser.parse """{ "prop": [ "1", "2", "3" ] }"""
            |> Expect.ok
        Expect.equalSeq actual expected

    [<Fact>]
    let ``Should return Ok when parsing JSON async`` () =
        task {
            let expected = 1
            let! actual =
                Prop.req "prop" Parse.int
                |> Parser.parseAsync (MemoryStream.create """{ "prop": 1 }""")
                |> Task.map Expect.ok
            Expect.equal actual expected
        }