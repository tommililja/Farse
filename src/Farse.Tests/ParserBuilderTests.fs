namespace Farse.Tests

open Expecto.Flip
open Xunit
open Farse
open Farse.Operators

#nowarn 40
#nowarn 21

type Tree =
    | Leaf of int
    | Branch of Tree * Tree

type Value = {
    Id: string
    Fields: Field array
}

and Field = {
    Name: string
    Values: Value array
}

module ParserBuilderTests =

    let private example =
        parser {
             let! x = "x" &= Parse.int
             and! y = "y" &= Parse.int

             do! "z" &= Parse.unit

             if x = 0 then
                 do! Parser.fail "Parser failed."

             return x + y
         }

    module Self =

        let private json =
            """
                {
                    "type": "branch",
                    "left": { "type": "leaf", "value": 1 },
                    "right": {
                        "type": "branch",
                        "left": { "type": "leaf", "value": 2 },
                        "right": { "type": "leaf", "value": 3 }
                    }
                }
           """

        [<Fact>]
        let ``Should parse self with recursive parser value`` () =
            let rec tree =
                Parse.oneOf "type" [
                    "leaf",
                        parser {
                            let! value = Prop.get "value" Parse.int
                            return Leaf value
                        }
                    "branch",
                        parser {
                            let! left = Prop.get "left" tree
                            let! right = Prop.get "right" tree
                            return Branch (left, right)
                        }
                ]

            let expected = Branch (Leaf 1, Branch (Leaf 2, Leaf 3))
            let actual =
                tree
                |> Parser.parse json
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse with self with recursive parser function`` () =
            let rec tree () =
                Parse.oneOf "type" [
                    "leaf",
                        parser {
                            let! value = Prop.get "value" Parse.int
                            return Leaf value
                        }
                    "branch",
                        parser {
                            let! left = Prop.get "left" (tree ())
                            let! right = Prop.get "right" (tree())
                            return Branch (left, right)
                        }
                ]

            let expected = Branch (Leaf 1, Branch (Leaf 2, Leaf 3))
            let actual =
                tree ()
                |> Parser.parse json
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

    module Mutual =

        [<Fact>]
        let ``Should parse mutually recursive parser starting from value`` () =
            let json =
                """
                    {
                        "id": "root",
                        "fields": [
                            {
                                "name": "color",
                                "values": [
                                    {
                                        "id": "red",
                                        "fields": []
                                    },
                                    {
                                        "id": "blue",
                                        "fields": [
                                            {
                                                "name": "shade",
                                                "values": [
                                                    { "id": "light", "fields": [] },
                                                    { "id": "dark", "fields": [] }
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            },
                            {
                                "name": "size",
                                "values": [
                                    { "id": "small", "fields": [] },
                                    { "id": "large", "fields": [] }
                                ]
                            }
                        ]
                    }
                """

            let expected =
                { Id = "root"
                  Fields =
                    [| { Name = "color"
                         Values =
                            [| { Id = "red"; Fields = [||] }
                               { Id = "blue"
                                 Fields =
                                    [| { Name = "shade"
                                         Values =
                                            [| { Id = "light"; Fields = [||] }
                                               { Id = "dark"; Fields = [||] } |] } |] } |] }
                       { Name = "size"
                         Values =
                            [| { Id = "small"; Fields = [||] }
                               { Id = "large"; Fields = [||] } |] } |] }

            let rec valueParser =
                parser {
                    let! id = Prop.get "id" Parse.string
                    and! fields = Prop.get "fields" (Parse.array fieldParser)
                    return { Id = id; Fields = fields }
                }

            and fieldParser =
                parser {
                    let! name = Prop.get "name" Parse.string
                    and! values = Prop.get "values" (Parse.array valueParser)
                    return { Name = name; Values = values }
                }

            let actual =
                valueParser
                |> Parser.parse json
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual

        [<Fact>]
        let ``Should parse mutually recursive parser starting from field`` () =
            let json =
                """
                    {
                        "name": "color",
                        "values": [
                            {
                                "id": "red",
                                "fields": []
                            },
                            {
                                "id": "blue",
                                "fields": [
                                    {
                                        "name": "shade",
                                        "values": [
                                            { "id": "light", "fields": [] },
                                            { "id": "dark", "fields": [] }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                """

            let expected =
                { Name = "color"
                  Values =
                    [| { Id = "red"; Fields = [||] }
                       { Id = "blue"
                         Fields =
                            [| { Name = "shade"
                                 Values =
                                    [| { Id = "light"; Fields = [||] }
                                       { Id = "dark"; Fields = [||] } |] } |] } |] }

            let rec valueParser =
                parser {
                    let! id = Prop.get "id" Parse.string
                    and! fields = Prop.get "fields" (Parse.array fieldParser)
                    return { Id = id; Fields = fields }
                }

            and fieldParser =
                parser {
                    let! name = Prop.get "name" Parse.string
                    and! values = Prop.get "values" (Parse.array valueParser)
                    return { Name = name; Values = values }
                }

            let actual =
                fieldParser
                |> Parser.parse json
                |> Expect.wantOk $"Expected %s{nameof Parser.parse} to succeed."
            Expect.equal Msg.none expected actual