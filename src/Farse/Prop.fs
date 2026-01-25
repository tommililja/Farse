namespace Farse

open System.Text.Json

module Prop =

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Prop.req "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.getProperty name element
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich (JsonPath.prop name)
                        |> Error
                | _ ->
                   InvalidKind (ExpectedKind.Object, element)
                   |> ParserError.fromType
                   |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok element
                let mutable jsonPath = JsonPath.empty

                for name in path do
                    prop <- prop
                    |> Result.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            jsonPath <-
                                JsonPath.prop name
                                |> JsonPath.append jsonPath

                            element
                            |> JsonElement.getProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok prop ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich jsonPath
                        |> Error
                | Error element ->
                    InvalidKind (ExpectedKind.Object, element)
                    |> ParserError.enriched jsonPath
                    |> Error

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <code>let! int = Prop.opt "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.tryGetProperty name element
                    match prop with
                    | Some prop ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error error ->
                            error
                            |> ParserError.enrich (JsonPath.prop name)
                            |> Error
                    | None -> Ok None
                | _ ->
                    InvalidKind (ExpectedKind.Object, element)
                    |> ParserError.fromType
                    |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok <| Some element
                let mutable jsonPath = JsonPath.empty

                for name in path do
                    prop <- prop
                    |> ResultOption.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            jsonPath <-
                                JsonPath.prop name
                                |> JsonPath.append jsonPath

                            element
                            |> JsonElement.tryGetProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok (Some prop) ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error ->
                        error
                        |> ParserError.enrich jsonPath
                        |> Error
                | Ok None -> Ok None
                | Error element ->
                    InvalidKind (ExpectedKind.Object, element)
                    |> ParserError.enriched jsonPath
                    |> Error