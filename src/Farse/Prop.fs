namespace Farse

open System.Text.Json

module Prop =

    let inline private parse name (Parser parser) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let prop = JsonElement.getProperty name element
                parser prop
                |> Result.bindError (fun error ->
                    let path = JsonPath.prop name
                    ParserError.enrich path error
                )
            | _ -> InvalidKind.create ExpectedKind.Object element
        )

    let inline private tryParse name (Parser parser) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Object ->
                let prop = JsonElement.tryGetProperty name element
                match prop with
                | Some prop ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error ->
                        let path = JsonPath.prop name
                        ParserError.enrich path error
                | None -> Ok None
            | _ -> InvalidKind.create ExpectedKind.Object element
        )

    let inline private traverse path (Parser parser) =
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
                parser prop
                |> Result.bindError (ParserError.enrich path)
            | Error element ->
                ExpectedKind.Object
                |> CouldNotParse.invalidKind path element
        )

    let inline private tryTraverse path (Parser parser) =
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
                match parser prop with
                | Ok x -> Ok <| Some x
                | Error error -> ParserError.enrich path error
            | Ok None -> Ok None
            | Error element ->
                ExpectedKind.Object
                |> CouldNotParse.invalidKind path element
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