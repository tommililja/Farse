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
                let prop, path =
                    path
                    |> Seq.fold (fun (prop, path) name ->
                        match prop with
                        | Ok (element:JsonElement) ->
                            match element.ValueKind with
                            | Kind.Object ->
                                element
                                |> JsonElement.getProperty name
                                |> Ok, JsonPath.append path (JsonPath.prop name)
                            | _ -> Error element, path
                        | Error e -> Error e, path
                    ) (Ok element, JsonPath.empty)

                match prop with
                | Ok prop ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich path
                        |> Error
                | Error element ->
                    InvalidKind (ExpectedKind.Object, element)
                    |> ParserError.enriched path
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
                let prop, path =
                    path
                    |> Seq.fold (fun (prop, path) name ->
                        match prop with
                        | Ok (Some (element:JsonElement)) ->
                            match element.ValueKind with
                            | Kind.Object ->
                                element
                                |> JsonElement.tryGetProperty name
                                |> Ok, JsonPath.append path (JsonPath.prop name)
                            | _ -> Error element, path
                        | _ -> prop, path
                    ) (Ok (Some element), JsonPath.empty)

                match prop with
                | Ok (Some prop) ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error ->
                        error
                        |> ParserError.enrich path
                        |> Error
                | Ok None -> Ok None
                | Error element ->
                    InvalidKind (ExpectedKind.Object, element)
                    |> ParserError.enriched path
                    |> Error