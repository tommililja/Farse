namespace Farse.Tests

open Expecto.Flip
open Xunit
open Farse
open Farse.Operators

module OperatorTests =

    let private run json parser =
        match Parser.parse json parser with
        | Ok x -> sprintf "%A" x
        | Error _ -> "Error"

    module Properties =

        [<Theory>]
        [<InlineData("{}", "Error")>]
        [<InlineData("""{ "a": null }""", "Error")>]
        [<InlineData("""{ "a": 1 }""", "1")>]
        [<InlineData("""{ "a": "x" }""", "Error")>]
        let ``"a" &= int`` (json:string, expected:string) =
            "a" &= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "Error")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": 1 }""", "Some 1")>]
        [<InlineData("""{ "a": "x" }""", "Error")>]
        let ``"a" &= option int`` (json:string, expected:string) =
            "a" &= Parse.option Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "None")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": 1 }""", "Some 1")>]
        [<InlineData("""{ "a": "x" }""", "Error")>]
        let ``"a" ?= int`` (json:string, expected:string) =
            "a" ?= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "None")>]
        [<InlineData("""{ "a": null }""", "Some None")>]
        [<InlineData("""{ "a": 1 }""", "Some (Some 1)")>]
        [<InlineData("""{ "a": "x" }""", "Error")>]
        let ``"a" ??= int`` (json:string, expected:string) =
            "a" ??= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

    module NestedProperties =

        [<Theory>]
        [<InlineData("{}", "Error")>]
        [<InlineData("""{ "a": null }""", "Error")>]
        [<InlineData("""{ "a": {} }""", "Error")>]
        [<InlineData("""{ "a": { "b": null } }""", "Error")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "1")>]
        [<InlineData("""{ "a": { "b": "x" } }""", "Error")>]
        [<InlineData("""{ "a": 1 }""", "Error")>]
        let ``"a.b" &= int`` (json:string, expected:string) =
            "a.b" &= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "Error")>]
        [<InlineData("""{ "a": null }""", "Error")>]
        [<InlineData("""{ "a": {} }""", "Error")>]
        [<InlineData("""{ "a": { "b": null } }""", "None")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "Some 1")>]
        [<InlineData("""{ "a": { "b": "x" } }""", "Error")>]
        [<InlineData("""{ "a": 1 }""", "Error")>]
        let ``"a.b" &= option int`` (json:string, expected:string) =
            "a.b" &= Parse.option Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "None")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": {} }""", "None")>]
        [<InlineData("""{ "a": { "b": null } }""", "None")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "Some 1")>]
        [<InlineData("""{ "a": { "b": "x" } }""", "Error")>]
        [<InlineData("""{ "a": 1 }""", "Error")>]
        let ``"a.b" ?= int`` (json:string, expected:string) =
            "a.b" ?= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "None")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": {} }""", "None")>]
        [<InlineData("""{ "a": { "b": null } }""", "Some None")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "Some (Some 1)")>]
        [<InlineData("""{ "a": { "b": "x" } }""", "Error")>]
        [<InlineData("""{ "a": 1 }""", "Error")>]
        let ``"a.b" ??= int`` (json:string, expected:string) =
            "a.b" ??= Parse.int
            |> run json
            |> Expect.equal Msg.none expected

    module Mixed =

        [<Theory>]
        [<InlineData("{}", "None")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": {} }""", "Error")>]
        [<InlineData("""{ "a": { "b": null } }""", "Error")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "Some 1")>]
        let ``"a" ?= ("b" &= int)`` (json:string, expected:string) =
            "a" ?= ("b" &= Parse.int)
            |> run json
            |> Expect.equal Msg.none expected

        [<Theory>]
        [<InlineData("{}", "Error")>]
        [<InlineData("""{ "a": null }""", "None")>]
        [<InlineData("""{ "a": {} }""", "None")>]
        [<InlineData("""{ "a": { "b": null } }""", "None")>]
        [<InlineData("""{ "a": { "b": 1 } }""", "Some 1")>]
        let ``"a" &= ("b" ?= int)`` (json:string, expected:string) =
            "a" &= ("b" ?= Parse.int)
            |> run json
            |> Expect.equal Msg.none expected