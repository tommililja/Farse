namespace Farse.Tests

open Expecto.Flip
open Xunit
open Farse

module PropTests =

    module Optional =

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.opt "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse undefined property as None`` () =
            let expected = None
            let actual =
                Prop.opt "missing" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.opt "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.opt "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.opt "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.opt "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module OptionalTraverse =

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.opt "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse first null property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop.prop2.pro3" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse intermediate null property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse last null property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse first undefined property as None`` () =
            let expected = None
            let actual =
                Prop.opt "missing.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse intermediate undefined property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop.missing.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse last undefined property as None`` () =
            let expected = None
            let actual =
                Prop.opt "prop.prop2.missing" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.opt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.opt "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError

    module Required =

        [<Fact>]
        let ``Should parse property`` () =
            let expected = 1
            let actual =
                Prop.req "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.req "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.req "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when property is null`` () =
            Prop.req "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is undefined`` () =
            Prop.req "missing" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.req "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.req "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.req "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.req "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module RequiredTraverse =

        [<Fact>]
        let ``Should parse property`` () =
            let expected = 1
            let actual =
                Prop.req "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.req "prop.prop2" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.req "prop.prop2" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when first property is null`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is null`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when last property is null`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is undefined`` () =
            Prop.req "missing.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is undefined`` () =
            Prop.req "prop.missing.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when last property is undefined`` () =
            Prop.req "prop.prop2.missing" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.req "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.req "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError

    module TryOptional =

        [<Fact>]
        let ``Should parse property as Some Some value`` () =
            let expected = Some (Some 1)
            let actual =
                Prop.tryOpt "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse null property as Some None`` () =
            let expected = Some None
            let actual =
                Prop.tryOpt "prop" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "missing" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryOpt "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryOpt "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryOpt "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryOpt "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module TryOptionalTraverse =

        [<Fact>]
        let ``Should parse property as Some Some value`` () =
            let expected = Some (Some 1)
            let actual =
                Prop.tryOpt "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse null property as Some None`` () =
            let expected = Some None
            let actual =
                Prop.tryOpt "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse first null property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "prop.prop2.pro3" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse intermediate null property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse last null property as None`` () =
            let expected = Some None
            let actual =
                Prop.tryOpt "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse first undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "missing.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse intermediate undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "prop.missing.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should parse last undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryOpt "prop.prop2.missing" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none actual expected

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryOpt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryOpt "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryOpt "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryOpt "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.tryOpt "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.tryOpt "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError