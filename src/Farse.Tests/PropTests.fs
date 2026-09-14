namespace Farse.Tests

open Expecto.Flip
open Xunit
open Farse

module PropTests =

    module TryGet =

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.tryGet "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "missing" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryGet "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryGet "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryGet "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryGet "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module TryGetTraverse =

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.tryGet "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse first null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop.prop2.pro3" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse intermediate null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse last null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse first undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "missing.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse intermediate undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop.missing.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse last undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop.prop2.missing" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryGet "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryGet "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryGet "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryGet "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.tryGet "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.tryGet "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError

    module Get =

        [<Fact>]
        let ``Should parse property`` () =
            let expected = 1
            let actual =
                Prop.get "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.get "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.get "prop" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when property is null`` () =
            Prop.get "prop" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is undefined`` () =
            Prop.get "missing" Parse.int
            |> Parser.parse """{ "prop": 1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.get "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.get "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.get "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.get "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module GetTraverse =

        [<Fact>]
        let ``Should parse property`` () =
            let expected = 1
            let actual =
                Prop.get "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse property as Some value`` () =
            let expected = Some 1
            let actual =
                Prop.get "prop.prop2" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse null property as None`` () =
            let expected = None
            let actual =
                Prop.get "prop.prop2" (Parse.option Parse.int)
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when first property is null`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": null }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is null`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": null } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when last property is null`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is undefined`` () =
            Prop.get "missing.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is undefined`` () =
            Prop.get "prop.missing.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when last property is undefined`` () =
            Prop.get "prop.prop2.missing" Parse.int
            |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.get "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.get "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.get "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError

    module TryGet2 =

        [<Fact>]
        let ``Should parse property as Some Some value`` () =
            let expected = Some (Some 1)
            let actual =
                Prop.tryGet2 "prop" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse null property as Some None`` () =
            let expected = Some None
            let actual =
                Prop.tryGet2 "prop" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "missing" Parse.int
                |> Parser.parse """{ "prop": 1 }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryGet2 "prop" Parse.int
            |> Parser.parse """{ "prop": "1" }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryGet2 "prop" Parse.int
            |> Parser.parse """{ "prop": 1.1 }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryGet2 "prop" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryGet2 "prop" Parse.int
            |> Parser.parse "null"
            |> Expect.parserError

    module TryGet2Traverse =

        [<Fact>]
        let ``Should parse property as Some Some value`` () =
            let expected = Some (Some 1)
            let actual =
                Prop.tryGet2 "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse null property as Some None`` () =
            let expected = Some None
            let actual =
                Prop.tryGet2 "prop.prop2" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse first null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "prop.prop2.pro3" Parse.int
                |> Parser.parse """{ "prop": null }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse intermediate null property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": null } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse last null property as None`` () =
            let expected = Some None
            let actual =
                Prop.tryGet2 "prop.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": null } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse first undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "missing.prop2.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse intermediate undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "prop.missing.prop3" Parse.int
                |> Parser.parse """{ "prop": { "prop2": 1 } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse last undefined property as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "prop.prop2.missing" Parse.int
                |> Parser.parse """{ "prop": { "prop2": { "prop3": 1 } } }"""
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should fail when property is a different kind``() =
            Prop.tryGet2 "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": "1" } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when property is invalid``() =
            Prop.tryGet2 "prop.prop2" Parse.int
            |> Parser.parse """{ "prop": { "prop2": 1.1 } }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when element is null`` () =
            Prop.tryGet2 "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

        [<Fact>]
        let ``Should fail when element is not an object`` () =
            Prop.tryGet2 "prop.prop2.prop3" Parse.int
            |> Parser.parse "[]"
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when first property is not an object`` () =
            Prop.tryGet2 "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": [] }"""
            |> Expect.parserError

        [<Fact>]
        let ``Should fail when intermediate property is not an object`` () =
            Prop.tryGet2 "prop.prop2.prop3" Parse.int
            |> Parser.parse """{ "prop": { "prop2": [] } }"""
            |> Expect.parserError

module PathTests =

    [<Fact>]
    let ``Should parse a name containing an escaped dot`` () =
        let expected = 1
        let actual =
            Prop.get "prop\.prop2" Parse.int
            |> Parser.parse """{ "prop.prop2": 1 }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should traverse a path with an escaped dot in a middle segment`` () =
        let expected = 1
        let actual =
            Prop.get "a.b\.c.d" Parse.int
            |> Parser.parse """{ "a": { "b.c": { "d": 1 } } }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should parse a name containing a backslash and an escaped dot`` () =
        let expected = 1
        let actual =
            Prop.get @"prop\\.prop2" Parse.int
            |> Parser.parse """{ "prop\\.prop2": 1 }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should parse a name ending with a backslash`` () =
        let expected = 1
        let actual =
            Prop.get @"prop\" Parse.int
            |> Parser.parse """{ "prop\\": 1 }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should parse an empty name`` () =
        let expected = 1
        let actual =
            Prop.get "" Parse.int
            |> Parser.parse """{ "": 1 }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should parse a name containing a single quote`` () =
        let expected = 1
        let actual =
            Prop.get "prop's" Parse.int
            |> Parser.parse """{ "prop's": 1 }"""
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
        Expect.equal Msg.none expected actual

    [<Fact>]
    let ``Should render a bracketed path for a name with an escaped dot`` () =
        Prop.get "prop\.prop2" Parse.int
        |> Parser.parse """{ "prop.prop2": "1" }"""
        |> Expect.parserError

    [<Fact>]
    let ``Should render a bracketed path for a name with a quote and a dot`` () =
        Prop.get "prop's\.prop2" Parse.int
        |> Parser.parse """{ "prop's.prop2": "1" }"""
        |> Expect.parserError

    [<Fact>]
    let ``Should render dotted and bracketed segments in the same path`` () =
        Prop.get "a.b\.c.d" Parse.int
        |> Parser.parse """{ "a": { "b.c": { "d": "1" } } }"""
        |> Expect.parserError

    [<Fact>]
    let ``Should treat a backslash before a dot as a path separator when not escaped`` () =
        Prop.get "prop\\.prop2" Parse.int
        |> Parser.parse """{ "prop\\.prop2": "1" }"""
        |> Expect.parserError