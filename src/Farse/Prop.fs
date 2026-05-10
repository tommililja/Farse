namespace Farse

open System.Text.Json

module Prop =

    let inline private isObject<'a, 'b> fn : Parser<'b> =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object -> fn element
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Object JsonPath.empty typeof<'a>
                |> Error.list
        )

    let inline private parse name (Parser parse) : Parser<'r> =
        isObject<'r, 'r> (fun element ->
            match JsonElement.getProperty name element with
            | prop when prop.ValueKind <> Kind.Undefined ->
                parse prop
                |> Result.mapError (fun errors ->
                    errors
                    |> List.map (ParseError.withProp name)
                )
            | prop ->
                prop
                |> ParseError.required (JsonPath.prop name) typeof<'r>
                |> Error.list
        )

    let inline private tryParse name (Parser parse) =
        isObject<'r, 'r option> (fun element ->
            match JsonElement.tryGetProperty name element with
            | Some prop ->
                match parse prop with
                | Ok x -> Ok <| Some x
                | Error errors ->
                    errors
                    |> List.map (ParseError.withProp name)
                    |> Error
            | None -> Ok None
        )

    let inline private tryParseNull name (Parser parse) =
        isObject<'r, 'r option option> (fun element ->
            match JsonElement.tryGetNullProperty name element with
            | Some prop when prop.ValueKind <> Kind.Null ->
                match parse prop with
                | Ok x -> Ok <| (Some (Some x))
                | Error errors ->
                    errors
                    |> List.map (ParseError.withProp name)
                    |> Error
            | Some _ -> Ok (Some None)
            | None -> Ok None
        )

    let inline private traverse path (Parser parse) : Parser<'r> =
        Parser (fun element ->
            let prop, path =
                path
                |> Seq.fold (fun (prop, path) name ->
                    match prop with
                    | Ok (element:JsonElement) ->
                        match element.ValueKind with
                        | Kind.Object ->
                            let path = JsonPath.append path (JsonPath.prop name)
                            match JsonElement.getProperty name element with
                            | prop when prop.ValueKind <> Kind.Undefined -> Ok prop, path
                            | prop -> Error (prop, true), path
                        | _ -> Error (element, false), path
                    | Error e -> Error e, path
                ) (Ok element, JsonPath.empty)

            match prop with
            | Ok prop ->
                parse prop
                |> Result.mapError (fun errors ->
                    errors
                    |> List.map (ParseError.withPath path)
                )
            | Error (element, true) ->
                element
                |> ParseError.required path typeof<'r>
                |> Error.list
            | Error (element, false) ->
                element
                |> ParseError.expectedKind ExpectedKind.Object path typeof<'r>
                |> Error.list
        )

    let inline private tryTraverse path (Parser parse) : Parser<'r option> =
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
                    |> List.map (ParseError.withPath path)
                    |> Error
            | Ok None -> Ok None
            | Error element ->
                element
                |> ParseError.expectedKind ExpectedKind.Object path typeof<'r>
                |> Error.list
        )

    let inline private tryTraverseNull path (Parser parse) : Parser<'r option option> =
        Parser (fun element ->
            let prop, path =
                path
                |> Seq.fold (fun (prop, path) name ->
                    match prop with
                    | Ok (Some (element:JsonElement)) ->
                        match element.ValueKind with
                        | Kind.Object ->
                            element
                            |> JsonElement.tryGetNullProperty name
                            |> Ok, JsonPath.append path (JsonPath.prop name)
                        | Kind.Null -> Ok None, path
                        | _ -> Error element, path
                    | _ -> prop, path
                ) (Ok (Some element), JsonPath.empty)

            match prop with
            | Ok (Some prop) when prop.ValueKind <> Kind.Null ->
                match parse prop with
                | Ok x -> Ok <| Some (Some x)
                | Error errors ->
                    errors
                    |> List.map (ParseError.withPath path)
                    |> Error
            | Ok (Some _) -> Ok (Some None)
            | Ok None -> Ok None
            | Error element ->
                element
                |> ParseError.expectedKind ExpectedKind.Object path typeof<'r>
                |> Error.list
        )

    /// <summary>Parses a required property.</summary>
    /// <example>let! int = Prop.req "prop.prop2" Parse.int</example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path parser =
        match path with
        | Prop name -> parse name parser
        | Path path -> traverse path parser

    /// <summary>Parses an optional property.</summary>
    /// <example>let! int = Prop.opt "prop.prop2" Parse.int</example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path parser =
        match path with
        | Prop name -> tryParse name parser
        | Path path -> tryTraverse path parser

    /// <summary>Parses an optional property.</summary>
    /// <remarks>Distinguishing between a missing property and a null value.</remarks>
    /// <example>let! int = Prop.optNull "prop.prop2" Parse.int</example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let tryOpt path parser =
        match path with
        | Prop name -> tryParseNull name parser
        | Path path -> tryTraverseNull path parser