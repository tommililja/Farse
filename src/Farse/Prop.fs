namespace Farse

open System.Text.Json

module Prop =

    let inline private parse name (Parser parse) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let prop = JsonElement.getProperty name element
                parse prop
                |> Result.bindError (fun errors ->
                    errors
                    |> List.map (ParserError.appendProp name)
                    |> Error
                )
            | _ ->
                ExpectedKind.Object
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

    let inline private tryParse name (Parser parse) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let prop = JsonElement.tryGetProperty name element
                match prop with
                | Some prop ->
                    match parse prop with
                    | Ok x -> Ok <| Some x
                    | Error errors ->
                        errors
                        |> List.map (ParserError.appendProp name)
                        |> Error
                | None -> Ok None
            | _ ->
                ExpectedKind.Object
                |> InvalidKind.create JsonPath.empty element
                |> Error.list
        )

    let inline private traverse path (Parser parse) =
        Parser (fun element ->
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
                parse prop
                |> Result.mapError (fun errors ->
                    errors
                    |> List.map (ParserError.appendPath path)
                )
            | Error element ->
                ExpectedKind.Object
                |> InvalidKind.create path element
                |> Error.list
        )

    let inline private tryTraverse path (Parser parse) =
        Parser (fun element ->
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
                match parse prop with
                | Ok x -> Ok <| Some x
                | Error errors ->
                    errors
                    |> List.map (ParserError.appendPath path)
                    |> Error
            | Ok None -> Ok None
            | Error element ->
                ExpectedKind.Object
                |> InvalidKind.create path element
                |> Error.list
        )

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Prop.req "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path parser =
        match path with
        | Prop name -> parse name parser
        | Path path -> traverse path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <code>let! int = Prop.opt "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path parser =
        match path with
        | Prop name -> tryParse name parser
        | Path path -> tryTraverse path parser