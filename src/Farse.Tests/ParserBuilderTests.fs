namespace Farse.Tests

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