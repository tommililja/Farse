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
        let ``Should parse null element as None`` () =
            let expected = None
            let actual =
                Prop.tryGet "prop" Parse.int
                |> Parser.parse "null"
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
        let ``Should fail when path is null`` () =
            Prop.tryGet null Parse.int
            |> Parser.parse """{ "": 1 }"""
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
        let ``Should parse null element as None`` () =
            Prop.tryGet "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

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

        [<Fact>]
        let ``Should fail when path is null`` () =
            Prop.get null Parse.int
            |> Parser.parse """{ "": 1 }"""
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
        let ``Should parse null element as None`` () =
            let expected = None
            let actual =
                Prop.tryGet2 "prop" Parse.int
                |> Parser.parse "null"
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
        let ``Should fail when path is null`` () =
            Prop.tryGet2 null Parse.int
            |> Parser.parse """{ "": 1 }"""
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
        let ``Should parse null element as None`` () =
            Prop.tryGet2 "prop.prop2.prop3" Parse.int
            |> Parser.parse "null"
            |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."

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

    [<Theory>]
    [<InlineData(@"prop\.prop2", """{ "prop.prop2": 1 }""")>]
    [<InlineData(@"a.b\.c.d", """{ "a": { "b.c": { "d": 1 } } }""")>]
    [<InlineData(@"prop\\.prop2", """{ "prop\\.prop2": 1 }""")>]
    [<InlineData(@"prop\", """{ "prop\\": 1 }""")>]
    [<InlineData(@"", """{ "": 1 }""")>]
    [<InlineData(@"\.", """{ ".": 1 }""")>]
    let ``Should parse an escaped or unusual path`` (path:string, json:string) =
        let get =
            Prop.get path Parse.int
            |> Parser.parse json
            |> Expect.wantOk $"Expected %s{nameof Prop.get} to succeed."

        let tryGet =
            Prop.tryGet path Parse.int
            |> Parser.parse json
            |> Expect.wantOk $"Expected %s{nameof Prop.tryGet} to succeed."

        let tryGet2 =
            Prop.tryGet2 path Parse.int
            |> Parser.parse json
            |> Expect.wantOk $"Expected %s{nameof Prop.tryGet2} to succeed."

        Expect.equal Msg.none 1 get
        Expect.equal Msg.none (Some 1) tryGet
        Expect.equal Msg.none (Some (Some 1)) tryGet2

    [<Theory>]
    [<InlineData(".")>]
    [<InlineData("..")>]
    [<InlineData(".prop")>]
    [<InlineData("prop.")>]
    [<InlineData("prop..prop2")>]
    [<InlineData("prop.prop2.")>]
    let ``Should fail when path has an empty segment`` (path:string) =
        let json = """{ "prop": { "prop2": 1 }, ".prop": 1, "prop.": 1, ".": 1 }"""
        Prop.get path Parse.int
        |> Parser.parse json
        |> Expect.isError $"Expected %s{nameof Prop.get} to fail."
        Prop.tryGet path Parse.int
        |> Parser.parse json
        |> Expect.isError $"Expected %s{nameof Prop.tryGet} to fail."
        Prop.tryGet2 path Parse.int
        |> Parser.parse json
        |> Expect.isError $"Expected %s{nameof Prop.tryGet2} to fail."

    [<Fact>]
    let ``Should fail when path ends with a dot`` () =
        Prop.get "prop." Parse.int
        |> Parser.parse """{ "prop": 1 }"""
        |> Expect.parserError

    [<Fact>]
    let ``Should fail when path has consecutive dots`` () =
        Prop.get "prop..prop2" Parse.int
        |> Parser.parse """{ "prop": { "prop2": 1 } }"""
        |> Expect.parserError

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

module JsonPathTests =

    [<Theory>]
    [<InlineData("prop", ".prop")>]
    [<InlineData("_", "._")>]
    [<InlineData("_a1", "._a1")>]
    [<InlineData("camelCase42", ".camelCase42")>]
    let ``Should render a dotted segment for a shorthand name`` (name:string, expected:string) =
        Expect.equal Msg.none expected (JsonPath.segment name)

    [<Theory>]
    [<InlineData("my key", "['my key']")>]
    [<InlineData("first-name", "['first-name']")>]
    [<InlineData("1abc", "['1abc']")>]
    [<InlineData("a$", "['a$']")>]
    [<InlineData("$ref", "['$ref']")>]
    [<InlineData("*", "['*']")>]
    [<InlineData("prop.prop2", "['prop.prop2']")>]
    [<InlineData("[0]", "['[0]']")>]
    [<InlineData("名前", "['名前']")>]
    let ``Should render a bracketed segment for a non-shorthand name`` (name:string, expected:string) =
        Expect.equal Msg.none expected (JsonPath.segment name)

    [<Theory>]
    [<InlineData("")>]
    [<InlineData(null)>]
    let ``Should render an empty bracketed segment for a null or empty name`` (name:string) =
        Expect.equal Msg.none "['']" (JsonPath.segment name)

    [<Theory>]
    [<InlineData("prop's", @"['prop\'s']")>]
    [<InlineData(@"a\b", @"['a\\b']")>]
    [<InlineData(@"prop\", @"['prop\\']")>]
    [<InlineData(@"a'b\c", @"['a\'b\\c']")>]
    [<InlineData("tab\there", @"['tab\there']")>]
    [<InlineData("line\nbreak", @"['line\nbreak']")>]
    [<InlineData("cr\rlf", @"['cr\rlf']")>]
    [<InlineData("\b", @"['\b']")>]
    [<InlineData("\u000C", @"['\f']")>]
    [<InlineData("\u0001", @"['\u0001']")>]
    [<InlineData("\u001F", @"['\u001f']")>]
    let ``Should escape special characters in a bracketed segment`` (name:string, expected:string) =
        Expect.equal Msg.none expected (JsonPath.segment name)

    [<Theory>]
    [<InlineData(" ")>]
    [<InlineData("~")>]
    [<InlineData("\u007F")>]
    let ``Should not escape printable non-shorthand characters`` (name:string) =
        Expect.equal Msg.none $"['%s{name}']" (JsonPath.segment name)